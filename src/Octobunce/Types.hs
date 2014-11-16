module Octobunce.Types
    ( IrcMessage(..)
    , IrcCommand(..)
    , 
    ) where

import Data.ByteString     (ByteString)
import Control.Applicative
import Control.Monad.Trans

data IrcMessage = IrcMessage
    { ircMsgSource  :: ByteString
    , ircMsgCommand :: IrcCommand
    , ircMsgArgs    :: [ByteString]
    } deriving Show

data IrcCommand = IrcNumeric Int
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
                | IrcUnknown ByteString
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
