{-# LANGUAGE OverloadedStrings #-}
{-|
Module:      Octobunce.Parser
Description: Parse IRC messages
Copyright:   © 2014 Josh Holland
License:     MIT
Maintainer:  josh@inv.alid.pw
Stability:   experimental
Portability: any
-}

module Octobunce.Parser
    ( ircMessage
    ) where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (pack)
import           Data.Char                        (digitToInt)
import           Control.Applicative              ((*>), (<$>))
import           Octobunce.Types

-- | Parse a full IRC message line, including the line ending. Mostly based on
-- RFC 2812 § 2.3.1.
ircMessage :: Parser IrcMessage
ircMessage = do
    source <- option "" $ do
        source <- ircSource
        space
        return source
    command <- ircCommand
    space
    args <- ircArgs
    endOfLine
    return IrcMessage
        { ircMsgSource  = source
        , ircMsgCommand = command
        , ircMsgArgs    = args
        }

-- | Parse the initial source part of an IRC line.
ircSource :: Parser ByteString
ircSource = char ':' *> takeWhile1 (not . isSpace)

-- | Parse the command or numeric response part of an IRC line.
ircCommand :: Parser IrcCommand
ircCommand = choice [ircCommandNumeric, ircCommandTextual]

-- | Parse a numeric IRC response code.
ircCommandNumeric :: Parser IrcCommand
ircCommandNumeric = do
    d1 <- digitToInt <$> digit
    d2 <- digitToInt <$> digit
    d3 <- digitToInt <$> digit
    return $ IrcNumeric $ 100*d1 + 10*d2 + d3

-- | Parse an IRC command.
ircCommandTextual :: Parser IrcCommand
ircCommandTextual = do
    name <- many1 letter_ascii
    case name of
        "PASS"     -> return IrcPass
        "NICK"     -> return IrcNick
        "USER"     -> return IrcUser
        "OPER"     -> return IrcOper
        "MODE"     -> return IrcMode
        "SERVICE"  -> return IrcService
        "QUIT"     -> return IrcQuit
        "SQUIT"    -> return IrcSquit
        "JOIN"     -> return IrcJoin
        "PART"     -> return IrcPart
        "TOPIC"    -> return IrcTopic
        "NAMES"    -> return IrcNames
        "LIST"     -> return IrcList
        "INVITE"   -> return IrcInvite
        "KICK"     -> return IrcKick
        "PRIVMSG"  -> return IrcPrivmsg
        "NOTICE"   -> return IrcNotice
        "MOTD"     -> return IrcMotd
        "LUSERS"   -> return IrcLusers
        "VERSION"  -> return IrcVersion
        "STATS"    -> return IrcStats
        "LINKS"    -> return IrcLinks
        "TIME"     -> return IrcTime
        "CONNECT"  -> return IrcConnect
        "TRACE"    -> return IrcTrace
        "ADMIN"    -> return IrcAdmin
        "INFO"     -> return IrcInfo
        "SERVLIST" -> return IrcServlist
        "SQUERY"   -> return IrcSquery
        "WHO"      -> return IrcWho
        "WHOIS"    -> return IrcWhois
        "WHOWAS"   -> return IrcWhowas
        "KILL"     -> return IrcKill
        "PING"     -> return IrcPing
        "PONG"     -> return IrcPong
        "ERROR"    -> return IrcError
        "AWAY"     -> return IrcAway
        "REHASH"   -> return IrcRehash
        "DIE"      -> return IrcDie
        "RESTART"  -> return IrcRestart
        "SUMMON"   -> return IrcSummon
        "USERS"    -> return IrcUsers
        "WALLOPS"  -> return IrcWallops
        "USERHOST" -> return IrcUserhost
        "ISON"     -> return IrcIson
        _          -> return $ IrcUnknown $ pack name

-- | Parse the arguments for the IRC command.
ircArgs :: Parser [ByteString]
ircArgs = do
    args <- ircWord `sepBy` space
    trail <- option [] $ do
        space
        trail <- char ':' *> ircTrailing
        return [trail]
    return $ args ++ trail

-- | Parse an individual IRC word argument.
ircWord :: Parser ByteString
ircWord = do
    first <- ircChar
    rest <- many' $ choice [ircChar, char ':']
    return $ pack (first:rest)

-- | Parse the trailing part of IRC arguments, not including the initial colon.
ircTrailing :: Parser ByteString
ircTrailing = A.takeWhile isTrailChar
  where
    isTrailChar c =  isIrcChar c
                  || c == ' '
                  || c == ':'

-- | Parse a single IRC character, as defined by 'isIrcChar'.
ircChar :: Parser Char
ircChar = A.satisfy isIrcChar

-- | Test whether a 'Char' is valid in an IRC word, i.e. it is not null, colon,
-- space or a newline/carriage return.
isIrcChar :: Char -> Bool
isIrcChar c =  c /= '\0'
            && c /= ' '
            && c /= ':'
            && c /= '\n'
            && c /= '\r'
