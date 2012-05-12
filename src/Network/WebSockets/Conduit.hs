{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.WebSockets.Conduit
  ( intercept
  , WebSocketsOptions(..)
  , WS.Request(..)
  , WS.Hybi00
  , WS.Hybi10
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)
import qualified Control.Exception.Lifted as Lifted

import Data.Char (toLower)
import Data.Conduit ( ($$+), (=$), ($=) )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Conduit as C
import qualified Data.Conduit.Network as C
import qualified Data.Conduit.List as CL
import qualified Blaze.ByteString.Builder as B

import Control.Concurrent (newMVar, withMVar)

import Network.Wai
import Network.Wai.Handler.Warp (Connection(..))
import qualified Network.WebSockets.Handshake.Http as WS
import qualified Network.WebSockets.Handshake as WS
import qualified Network.WebSockets.Types as WS
import qualified Network.WebSockets.Protocol as WS
import qualified Network.WebSockets.Protocol.Hybi10 as WS
import qualified Network.WebSockets.Protocol.Hybi00 as WS
import qualified Network.WebSockets.Protocol.Unsafe as Unsafe

data WebSocketsOptions = WebSocketsOptions
    { onPong       :: IO ()
    }

connSink :: Connection -> IO (C.Sink ByteString (C.ResourceT IO) ())
connSink conn = do
    lock <- liftIO $ newMVar ()
    return $ sink lock
  where
    sink l = C.NeedInput (push l) close
    push l x = liftIO (withMVar l (\_ -> connSendAll conn x)) >> sink l
    close = return ()

handleControlMessage :: (WS.TextProtocol p, WS.WebSocketsData str)
                     => WebSocketsOptions
                     -> C.Sink (WS.Message p) (C.ResourceT IO) ()
                     -> C.Conduit (WS.Message p) (C.ResourceT IO) str
handleControlMessage opt snk = CL.concatMapM step
  where step msg = case msg of
            (WS.DataMessage am) -> case am of
                WS.Text s   -> return [WS.fromLazyByteString s]
                WS.Binary s -> return [WS.fromLazyByteString s]
            (WS.ControlMessage cm) -> case cm of
                WS.Close _  -> liftIO $ throwIO WS.ConnectionClosed
                WS.Pong _   -> liftIO (onPong opt) >> return []
                WS.Ping pl  -> C.yield (Unsafe.pong pl) C.$$ snk >> return []

runWebSockets :: (WS.TextProtocol p)
              => p
              -> WebSocketsOptions
              -> C.Application (C.ResourceT IO)
              -> C.Source (C.ResourceT IO) ByteString
              -> C.Sink ByteString (C.ResourceT IO) ()
              -> C.ResourceT IO ()
runWebSockets p opts app src snk = do
    let snk' = WS.encodeMessages p =$ CL.map B.toByteString =$ snk
    let src' = src $= WS.decodeMessages p $= handleControlMessage opts snk'
        snk'' = CL.map WS.textData =$ snk'
    app src' snk''

intercept :: forall p. (WS.TextProtocol p)
          => p
          -> WebSocketsOptions
          -> (WS.Request -> C.Application (C.ResourceT IO))
          -> Request
          -> Maybe (C.Source (C.ResourceT IO) S.ByteString -> Connection -> C.ResourceT IO ())
intercept _ opts app req =
    case lookup "upgrade" (requestHeaders req) of
        Just s
            | S.map toLower s == "websocket" ->
                Just $ \src conn -> do
                    (src', req', (p::p)) <- handshake' src conn
                    snk <- liftIO $ connSink conn
                    runWebSockets p opts (app req') src' snk
            | otherwise                      -> Nothing
        _                                    -> Nothing
  where
    part = WS.RequestHttpPart (rawPathInfo req) (requestHeaders req) (isSecure req)
    sendRsp conn = liftIO . connSendAll conn . B.toByteString . WS.encodeResponse
    handshake' src conn = do
        (src', (req', p)) <- (src $$+ WS.handshake part) `Lifted.catch`
                                   ( \e -> do sendRsp conn $ WS.responseError (undefined::p) e
                                              Lifted.throwIO e )
        sendRsp conn $ WS.requestResponse req'
        return (src', req', p)
