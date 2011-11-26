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

import Prelude hiding (catch)
import Data.ByteString (ByteString)
import Data.Enumerator
import Control.Exception
import Control.Concurrent.STM
import Network.WebSockets.Handshake.Http (Request, RequestHttpPart)
import Network.WebSockets.Monad
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Emulate (EmulateProtocol(..))
import Control.Monad.Trans (liftIO)

type StreamChan a = TChan (Stream a)

iterChan :: StreamChan a -> Iteratee a IO ()
iterChan ch = continue go
  where
    go EOF = liftIO $ atomically $ writeTChan ch EOF
    go stream = liftIO (atomically $ writeTChan ch stream) >> continue go

enumChan :: StreamChan a -> Enumerator a IO b
enumChan ch = checkContinue0 $ \loop f -> do
    stream <- liftIO $ atomically $ readTChan ch
    f stream >>== loop

runEmulator :: StreamChan ByteString -> StreamChan ByteString -> RequestHttpPart -> (Request -> WebSockets EmulateProtocol a) -> IO a
runEmulator inChan outChan req app = do
    r <- (run $ enumChan inChan $$ runWebSockets req app $ iterChan outChan)
           `catch` (ioError)
           `finally` (atomically $ writeTChan outChan EOF)
    either (error . show) return r
