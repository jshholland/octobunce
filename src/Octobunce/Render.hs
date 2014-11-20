{-|
Module:      Octobunce.Render
Description: Render an IrcMessage to a ByteString.
Copyright:   © 2014 Josh Holland
License:     MIT
Maintainer:  josh@inv.alid.pw
Stability:   experimental
Portability: any
-}
module Octobunce.Render
    ( renderMessage
    , renderMessage'
    ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.ByteString.Lazy    (toStrict)
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.List               (intersperse)
import           Octobunce.Types

-- | Render an 'IrcMessage' ready for transmission.
renderMessage :: IrcMessage -> ByteString
renderMessage = toStrict . toLazyByteString . renderMessage'

-- | Get the raw 'Builder' for use with 'hPutBuilder'.
renderMessage' :: IrcMessage -> Builder
renderMessage' msg
    | BS.null (ircMsgSource msg) = renderMessageNoSource msg
    | otherwise                  = renderMessageWithSource msg

renderMessageNoSource :: IrcMessage -> Builder
renderMessageNoSource (IrcMessage _src cmd args) = unwordsB (cmd':args') <> byteString "\r\n"
  where
    cmd'  = renderCommand cmd
    args' = renderArgs args

renderMessageWithSource :: IrcMessage -> Builder
renderMessageWithSource (IrcMessage src cmd args) = unwordsB (src':cmd':args') <> byteString "\r\n"
  where
    src'  = charUtf8 ':' <> byteString src
    cmd'  = renderCommand cmd
    args' = renderArgs args

renderCommand :: IrcCommand -> Builder
renderCommand (IrcNumeric n)  = intDec n
renderCommand (IrcUnknown bs) = byteString bs
renderCommand IrcPass         = byteString "PASS"
renderCommand IrcNick         = byteString "NICK"
renderCommand IrcUser         = byteString "USER"
renderCommand IrcOper         = byteString "OPER"
renderCommand IrcMode         = byteString "MODE"
renderCommand IrcService      = byteString "SERVICE"
renderCommand IrcQuit         = byteString "QUIT"
renderCommand IrcSquit        = byteString "SQUIT"
renderCommand IrcJoin         = byteString "JOIN"
renderCommand IrcPart         = byteString "PART"
renderCommand IrcTopic        = byteString "TOPIC"
renderCommand IrcNames        = byteString "NAMES"
renderCommand IrcList         = byteString "LIST"
renderCommand IrcInvite       = byteString "INVITE"
renderCommand IrcKick         = byteString "KICK"
renderCommand IrcPrivmsg      = byteString "PRIVMSG"
renderCommand IrcNotice       = byteString "NOTICE"
renderCommand IrcMotd         = byteString "MOTD"
renderCommand IrcLusers       = byteString "LUSERS"
renderCommand IrcVersion      = byteString "VERSION"
renderCommand IrcStats        = byteString "STATS"
renderCommand IrcLinks        = byteString "LINKS"
renderCommand IrcTime         = byteString "TIME"
renderCommand IrcConnect      = byteString "CONNECT"
renderCommand IrcTrace        = byteString "TRACE"
renderCommand IrcAdmin        = byteString "ADMIN"
renderCommand IrcInfo         = byteString "INFO"
renderCommand IrcServlist     = byteString "SERVLIST"
renderCommand IrcSquery       = byteString "SQUERY"
renderCommand IrcWho          = byteString "WHO"
renderCommand IrcWhois        = byteString "WHOIS"
renderCommand IrcWhowas       = byteString "WHOWAS"
renderCommand IrcKill         = byteString "KILL"
renderCommand IrcPing         = byteString "PING"
renderCommand IrcPong         = byteString "PONG"
renderCommand IrcError        = byteString "ERROR"
renderCommand IrcAway         = byteString "AWAY"
renderCommand IrcRehash       = byteString "REHASH"
renderCommand IrcDie          = byteString "DIE"
renderCommand IrcRestart      = byteString "RESTART"
renderCommand IrcSummon       = byteString "SUMMON"
renderCommand IrcUsers        = byteString "USERS"
renderCommand IrcWallops      = byteString "WALLOPS"
renderCommand IrcUserhost     = byteString "USERHOST"
renderCommand IrcIson         = byteString "ISON"

renderArgs :: [ByteString] -> [Builder]
renderArgs [] = []
renderArgs [x] = [charUtf8 ':' <> byteString x]
renderArgs (x:xs) = byteString x : renderArgs xs

unwordsB :: [Builder] -> Builder
unwordsB = mconcat . intersperse (charUtf8 ' ')
