{-# LANGUAGE DeriveDataTypeable #-}
module Data.Conduit.SharedSink
  ( SharedSink(..)
  , newSharedSink
  , pushSharedSink
  , wrapSharedSink
  ) where

import Control.Monad.Trans (lift)
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

-- | run a step of the sink, save the new state of sink in the `MVar'.
pushSharedSink :: SharedSink a IO -> a -> IO ()
pushSharedSink (SharedSink snk) a = modifyMVar_ snk go
  where
    go (SinkData push _) = do
        r <- runResourceT $ push a
        case r of
            Processing push' close' -> return $ SinkData push' close'
            Done _ _ -> throwIO SharedSinkClosed
    go s = return s

-- | Wrap a `SharedSink' as a normal sink.
wrapSharedSink :: SharedSink a IO -> Sink a IO ()
wrapSharedSink snk = SinkData push close
  where
    push a = lift (pushSharedSink snk a) >> return (Processing push close)
    close = return ()
