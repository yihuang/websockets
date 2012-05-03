{-# LANGUAGE DeriveDataTypeable #-}
module Data.Conduit.SharedSink
  ( SharedSink(..)
  , newSharedSink
  , pushSharedSink
  , wrapSharedSink
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
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

newSharedSink :: Sink a m () -> IO (SharedSink a m)
newSharedSink snk = fmap SharedSink $ newMVar snk

sinkPush :: i -> Sink i IO r -> IO (Sink i IO r)
sinkPush i (NeedInput p _) = return $ p i
sinkPush i (PipeM mp _) = mp
sinkPush i (HaveOutput _ _ _) = error "impossible: Sink HaveOutput"
sinkPush i (Done _ _) = throwIO SharedSinkClosed

-- | run a step of the sink, save the new state of sink in the `MVar'.
pushSharedSink :: SharedSink a IO -> a -> IO ()
pushSharedSink (SharedSink snk) a = modifyMVar_ snk (sinkPush a)

-- | Wrap a `SharedSink' as a normal sink.
wrapSharedSink :: SharedSink a IO -> Sink a (ResourceT IO) ()
wrapSharedSink snk = NeedInput push close
  where
    push a = liftIO (pushSharedSink snk a) >> wrapSharedSink snk
    close = return ()
