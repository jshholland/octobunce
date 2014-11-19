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
    ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Octobunce.Types

-- | Render an IrcMessage ready for transmission.
renderMessage :: IrcMessage -> ByteString
renderMessage msg
    | BS.null $ ircMsgSource msg = renderMessageNoSource msg
    | otherwise                  = renderMessageWithSource msg

renderMessageNoSource :: IrcMessage -> ByteString
renderMessageNoSource (IrcMessage _src cmd args) = BS.unwords $ cmd':args'
  where
    cmd'  = renderCommand cmd
    args' = renderArgs args

renderMessageWithSource (IrcMessage src cmd args) = BS.unwords $ src':cmd':args'
  where
    src'  = ':':src
    cmd'  = renderCommand cmd
    args' = renderArgs args

renderCommand :: IrcCommand -> ByteString
renderCommand (IrcNumeric n)  = BS.pack $ show n
renderCommand (IrcUnknown bs) = bs
renderCommand IrcPass         = "PASS"
renderCommand IrcNick         = "NICK"
renderCommand IrcUser         = "USER"
renderCommand IrcOper         = "OPER"
renderCommand IrcMode         = "MODE"
renderCommand IrcService      = "SERVICE"
renderCommand IrcQuit         = "QUIT"
renderCommand IrcSquit        = "SQUIT"
renderCommand IrcJoin         = "JOIN"
renderCommand IrcPart         = "PART"
renderCommand IrcTopic        = "TOPIC"
renderCommand IrcNames        = "NAMES"
renderCommand IrcList         = "LIST"
renderCommand IrcInvite       = "INVITE"
renderCommand IrcKick         = "KICK"
renderCommand IrcPrivmsg      = "PRIVMSG"
renderCommand IrcNotice       = "NOTICE"
renderCommand IrcMotd         = "MOTD"
renderCommand IrcLusers       = "LUSERS"
renderCommand IrcVersion      = "VERSION"
renderCommand IrcStats        = "STATS"
renderCommand IrcLinks        = "LINKS"
renderCommand IrcTime         = "TIME"
renderCommand IrcConnect      = "CONNECT"
renderCommand IrcTrace        = "TRACE"
renderCommand IrcAdmin        = "ADMIN"
renderCommand IrcInfo         = "INFO"
renderCommand IrcServlist     = "SERVLIST"
renderCommand IrcSquery       = "SQUERY"
renderCommand IrcWho          = "WHO"
renderCommand IrcWhois        = "WHOIS"
renderCommand IrcWhowas       = "WHOWAS"
renderCommand IrcKill         = "KILL"
renderCommand IrcPing         = "PING"
renderCommand IrcPong         = "PONG"
renderCommand IrcError        = "ERROR"
renderCommand IrcAway         = "AWAY"
renderCommand IrcRehash       = "REHASH"
renderCommand IrcDie          = "DIE"
renderCommand IrcRestart      = "RESTART"
renderCommand IrcSummon       = "SUMMON"
renderCommand IrcUsers        = "USERS"
renderCommand IrcWallops      = "WALLOPS"
renderCommand IrcUserhost     = "USERHOST"
renderCommand IrcIson         = "ISON"

renderArgs :: [ByteString] -> [ByteString]
renderArgs [] = []
renderArgs [x] = [':' `BS.cons` x]
renderArgs (x:xs) = x : renderArgs xs
