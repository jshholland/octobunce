{-|
Module: Octobunce.Types
Description: Define types for the Octobunce IRC bot library
Copyright: © 2014 Josh Holland
License: MIT
Maintainer: josh@inv.alid.pw
Stability: experimental
Portability: any
-}

module Octobunce.Types
    ( IrcMessage(..)
    , IrcCommand(..)
    , 
    ) where

import Data.ByteString     (ByteString)
import Control.Applicative
import Control.Monad.Trans

-- | Represent an IRC message, in either direction.
data IrcMessage = IrcMessage
    { ircMsgSource  :: ByteString -- ^ The prefixed source part of the message.
    , ircMsgCommand :: IrcCommand -- ^ The command or numeric response code.
    , ircMsgArgs    :: [ByteString] -- ^ Any arguments to the command.
    } deriving Show

-- | Represent an IRC command or numeric response code.
data IrcCommand = IrcNumeric Int -- ^ A numeric response code; should be in [0,1000)
                | IrcUnknown ByteString -- ^ Unrecognised command.
                | IrcPass
                | IrcNick
                | IrcUser
                | IrcOper
                | IrcMode
                | IrcService
                | IrcQuit
                | IrcSquit
                | IrcJoin
                | IrcPart
                | IrcTopic
                | IrcNames
                | IrcList
                | IrcInvite
                | IrcKick
                | IrcPrivmsg
                | IrcNotice
                | IrcMotd
                | IrcLusers
                | IrcVersion
                | IrcStats
                | IrcLinks
                | IrcTime
                | IrcConnect
                | IrcTrace
                | IrcAdmin
                | IrcInfo
                | IrcServlist
                | IrcSquery
                | IrcWho
                | IrcWhois
                | IrcWhowas
                | IrcKill
                | IrcPing
                | IrcPong
                | IrcError
                | IrcAway
                | IrcRehash
                | IrcDie
                | IrcRestart
                | IrcSummon
                | IrcUsers
                | IrcWallops
                | IrcUserhost
                | IrcIson
    deriving Show

data Nick bot = NickBS ByteString
              | NickBot (bot -> ByteString)

class Octobunce bot where
    nick :: Nick bot
    nick = NickBS "octobunce"

type Address = ByteString
type Channel = ByteString

newtype ActionT bot m a = ActionT
    { unActionT :: m a 
    }

instance Monad m => Monad (ActionT bot m) where
    return = ActionT . return
    act >>= f = ActionT $ unActionT act >>= unActionT . f

instance Applicative m => Applicative (ActionT bot m) where
    pure = ActionT . pure
    f <*> act = ActionT $ unActionT f <*> unActionT act

instance Functor m => Functor (ActionT bot m) where
    fmap f = ActionT . fmap f . unActionT

instance MonadTrans (ActionT bot) where
    lift = ActionT
