{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Chan
  ( StreamChan
  , ChanProtocol
  , iterChan
  , enumChan
  , runWithChans
  ) where

import Data.ByteString (ByteString)
import Data.Enumerator
import Data.Attoparsec
import Control.Applicative
import Control.Concurrent.Chan
import Blaze.ByteString.Builder
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Network.WebSockets.Monad
import Network.WebSockets.Demultiplex (emptyDemultiplexState)
import Network.WebSockets.Protocol
import Network.WebSockets.Types hiding (fromLazyByteString)
import Network.WebSockets.Handshake.Http
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

data ChanProtocol = ChanProtocol
instance Protocol ChanProtocol where
    version _ = "sockjs"
    headerVersion _ = "sockjs"
    encodeFrame _ _ (Frame _ _ payload) = fromLazyByteString payload
    decodeFrame _ = Frame True TextFrame <$> takeLazyByteString
    finishRequest _ (RequestHttpPart path hs) = return $ Right $ Request path hs (Response 101 "" [] "")
    implementations = [ChanProtocol]
instance TextProtocol ChanProtocol
instance BinaryProtocol ChanProtocol

runWithChans :: StreamChan ByteString -> StreamChan ByteString -> WebSockets ChanProtocol a -> IO a
runWithChans inChan outChan ws = do
    let sender x = writeChan outChan (Chunks [toByteString x])
        env      = WebSocketsEnv defaultWebSocketsOptions sender ChanProtocol
        state    = runReaderT (unWebSockets ws) env
        iter     = evalStateT state emptyDemultiplexState
    r <- run $ enumChan inChan $$ iter
    writeChan outChan EOF
    either (error . show) return r
