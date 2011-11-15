{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Emulate
  ( StreamChan
  , EmulateProtocol(..)
  , runEmulator
  , iterChan
  , enumChan
  , encodeFrame
  , decodeFrame
  ) where

import Data.ByteString (ByteString)
import Data.Enumerator
import Control.Concurrent.Chan
import Blaze.ByteString.Builder
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Network.WebSockets.Monad
import Network.WebSockets.Demultiplex (emptyDemultiplexState)
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Emulate (EmulateProtocol(..))
import Control.Monad.Trans (liftIO)

type StreamChan a = Chan (Stream a)

iterChan :: StreamChan a -> Iteratee a IO ()
iterChan ch = continue go
  where
    go EOF = liftIO $ writeChan ch EOF
    go stream = liftIO (writeChan ch stream) >> continue go

enumChan :: StreamChan a -> Enumerator a IO b
enumChan ch = checkContinue0 $ \loop f -> do
    stream <- liftIO $ readChan ch
    f stream >>== loop

runEmulator :: StreamChan ByteString -> StreamChan ByteString -> WebSockets EmulateProtocol a -> IO a
runEmulator inChan outChan ws = do
    let sender x = writeChan outChan (Chunks [toByteString x])
        env      = WebSocketsEnv defaultWebSocketsOptions sender EmulateProtocol
        state    = runReaderT (unWebSockets ws) env
        iter     = evalStateT state emptyDemultiplexState
    r <- run $ enumChan inChan $$ iter
    writeChan outChan EOF
    either (error . show) return r
