{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module Data.Conduit.SharedSink
  ( SharedSink(..)
  , newSharedSink
  , pushSharedSink
  , wrapSharedSink
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Data.Conduit

{- |
`SharedSink' is a sink protected by a `MVar', so it can be passed around and used at multiple places concurrently. 

The sink should return `()' and should not enter `Done' state.
-}

newtype SharedSink a m = SharedSink (MVar (Sink a m ()))

data SharedSinkClosed = SharedSinkClosed
  deriving (Show, Typeable)

instance Exception SharedSinkClosed

newSharedSink :: (Functor m, MonadIO m) => Sink a m' () -> m (SharedSink a m')
newSharedSink snk = fmap SharedSink $ liftIO $ newMVar snk

sinkEval :: MonadBase IO m => Sink i m r -> m (Sink i m r)
sinkEval (PipeM mp _) = mp >>= sinkEval
sinkEval s            = return s

sinkPush :: MonadBase IO m => i -> Sink i m r -> m (Sink i m r)
sinkPush i (NeedInput p _)    = return (p i)
sinkPush i (PipeM mp _)       = mp >>= sinkPush i
sinkPush _ (HaveOutput _ _ _) = error "impossible: Sink HaveOutput"
sinkPush _ (Done _ _)         = liftBase $ throwIO SharedSinkClosed

-- | run a step of the sink, save the new state of sink in the `MVar'.
pushSharedSink :: MonadBaseControl IO m => SharedSink a m -> a -> m ()
pushSharedSink (SharedSink snk) a = modifyMVar_ snk (\s -> sinkPush a s >>= sinkEval)

-- | Wrap a `SharedSink' as a normal sink.
wrapSharedSink :: MonadBaseControl IO m => SharedSink a m -> Sink a m ()
wrapSharedSink snk = sink
  where
    sink = NeedInput push close
    push a = lift (pushSharedSink snk a) >> sink
    close = return ()
