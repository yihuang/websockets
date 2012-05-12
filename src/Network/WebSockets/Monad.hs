{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}
module Network.WebSockets.Monad 
  ( ServerT(..)
  , Server
  , Sender
  , runServerT
  , recvBS
  , recvWith
  , sendBS
  , getSender

  , recvUtf8
  , sendUtf8
  ) where

import qualified Data.Conduit           as C
import qualified Data.Conduit.Internal  as C
import qualified Data.Conduit.List      as CL
import Data.Conduit.Network     (Application)

import Data.Typeable            (Typeable)
import Data.ByteString          (ByteString)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Control.Applicative      (Applicative)
import Control.Exception        (Exception)
import Control.Monad.Trans.State
import Control.Monad.Trans      (MonadTrans(lift))
import Control.Monad.IO.Class   (MonadIO(liftIO))

data ServerError
    = ParseError String
    | ConnectionClosed              
    deriving (Show, Typeable)

instance Exception ServerError

data ServerState m = ServerState
    { source :: C.Source m ByteString
    , sink   :: C.Sink ByteString m ()
    }

newtype ServerT m a = ServerT
    { unServerT :: StateT (ServerState m) m a
    } deriving (Applicative, Functor, Monad)

type Server = ServerT (C.ResourceT IO)
type Sender = ByteString -> (C.ResourceT IO) ()

instance MonadTrans ServerT where
    lift m = ServerT (lift m)

deriving instance MonadIO m         => MonadIO (ServerT m)
deriving instance C.MonadThrow m    => C.MonadThrow (ServerT m)
deriving instance C.MonadResource m => C.MonadResource (ServerT m)

runServerT :: Monad m => ServerT m () -> Application m
runServerT ws src snk = do
    let st = ServerState src snk
    evalStateT (unServerT ws) st

recvBS :: C.MonadThrow m => ServerT m ByteString
recvBS = do
    st <- ServerT get
    let src = source st
    (src', ma) <- lift $ src C.$$+ CL.head
    ServerT $ put st{ source=src' }
    case ma of
        Nothing -> C.monadThrow ConnectionClosed
        Just  a -> return a

recvWith :: (ByteString -> Either String a) -> Server a
recvWith p = do
    bs <- recvBS
    case p bs of
        Left err -> C.monadThrow $ ParseError err
        Right a  -> return a

recvUtf8 :: C.MonadThrow m => ServerT m T.Text
recvUtf8 = liftM T.decodeUtf8 recvBS

runPipeM :: Monad m => C.Pipe i o m r -> m (C.Pipe i o m r)
runPipeM (C.PipeM mp _) = mp >>= runPipeM
runPipeM s              = return s

sinkPush :: C.MonadThrow m => i -> C.Sink i m r -> m (C.Sink i m r)
sinkPush i (C.NeedInput p _)    = runPipeM (p i)
sinkPush i (C.PipeM mp _)       = mp >>= sinkPush i
sinkPush _ (C.HaveOutput{})     = error "impossible: Sink HaveOutput"
sinkPush _ (C.Done _ _)         = C.monadThrow ConnectionClosed

sendBS :: C.MonadThrow m => ByteString -> ServerT m ()
sendBS bs = do
    st <- ServerT get
    let snk = sink st
    snk' <- lift $ sinkPush bs snk
    ServerT $ put st{ sink=snk' }

sendUtf8 :: C.MonadThrow m => T.Text -> ServerT m ()
sendUtf8 = sendBS . T.encodeUtf8

getSender :: Monad m => ServerT m (ByteString -> m ())
getSender = do
    snk <- ServerT $ gets sink
    let sender bs = C.yield bs C.$$ snk
    return sender
