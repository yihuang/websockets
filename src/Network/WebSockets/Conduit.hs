{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.WebSockets.Conduit
  ( intercept
  , Application
  , WebSocketsOptions(..)
  , WS.Request(..)
  , WS.Hybi00
  , WS.Hybi10
  ) where
import Debug.Trace
import System.IO (stdout)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)
import qualified Control.Exception.Lifted as Lifted

import Data.Char (toLower)
import Data.Monoid
import Data.Conduit ( ($$+), (=$), ($=) )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as C
import Data.Conduit.Blaze (builderToByteString)
import qualified Blaze.ByteString.Builder as B
import Data.Conduit.SharedSink (SharedSink, newSharedSink, wrapSharedSink, pushSharedSink)

import Network.Wai.Handler.Warp (Connection(..))
import Network.Wai hiding (Application)
import qualified Network.WebSockets.Handshake.Http as WS
import qualified Network.WebSockets.Handshake as WS
import qualified Network.WebSockets.Types as WS
import qualified Network.WebSockets.Protocol as WS
import qualified Network.WebSockets.Protocol.Hybi10 as WS
import qualified Network.WebSockets.Protocol.Hybi00 as WS
import qualified Network.WebSockets.Protocol.Unsafe as Unsafe

type Application str = C.Source (C.ResourceT IO) str -> C.Sink str (C.ResourceT IO) () -> C.ResourceT IO ()

data WebSocketsOptions = WebSocketsOptions
    { onPong       :: IO ()
    }

connSink :: Connection -> C.Sink ByteString IO ()
connSink conn = sink
  where
    sink = C.NeedInput push close
    push x = lift (trace ("sink:" ++ show x) $ connSendAll conn x) >> sink
    close = return ()

handleControlMessage :: (WS.TextProtocol p, WS.WebSocketsData str)
                     => WebSocketsOptions
                     -> SharedSink (WS.Message p) IO
                     -> C.Conduit (WS.Message p) (C.ResourceT IO) str
handleControlMessage opt sharedSink = CL.concatMapM step
  where step msg = case msg of
            (WS.DataMessage am) -> return . (:[]) $ case am of
                WS.Text s -> WS.fromLazyByteString s
                WS.Binary s -> WS.fromLazyByteString s
            (WS.ControlMessage cm) -> case cm of
                WS.Close _ -> liftIO $ throwIO WS.ConnectionClosed
                WS.Pong _ -> liftIO (onPong opt) >> return []
                WS.Ping pl -> liftIO (pushSharedSink sharedSink (Unsafe.pong pl)) >> return []

autoFlush :: Monad m => C.Conduit B.Builder m B.Builder
autoFlush = CL.map (`mappend` B.flush)

runWebSockets :: (WS.TextProtocol p, WS.WebSocketsData str)
              => p
              -> WebSocketsOptions
              -> Application str
              -> C.Source (C.ResourceT IO) ByteString
              -> C.Sink ByteString IO ()
              -> C.ResourceT IO ()
runWebSockets p opts app src snk = do
    let msgSink = WS.encodeMessages p =$ autoFlush =$ builderToByteString =$ snk
    sharedSink <- liftIO $ newSharedSink msgSink
    let src' = src $= WS.decodeMessages p $= handleControlMessage opts sharedSink
        snk' = CL.map WS.textData =$ wrapSharedSink sharedSink
    liftIO $ putStrLn "trace0"
    app src' snk'
    liftIO $ putStrLn "trace1"

intercept :: forall p str. (WS.TextProtocol p, WS.WebSocketsData str)
          => p
          -> WebSocketsOptions
          -> (WS.Request -> Application str)
          -> Request
          -> Maybe (C.Source (C.ResourceT IO) S.ByteString -> Connection -> C.ResourceT IO ())
intercept _ opts app req =
    case lookup "upgrade" (requestHeaders req) of
        Just s
            | S.map toLower s == "websocket" ->
                Just $ \src conn -> do
                    (src', req', (p::p)) <- handshake' src conn
                    liftIO $ putStrLn $ WS.version p
                    -- src' C.$$ C.sinkHandle stdout
                    runWebSockets p opts (app req') src' (connSink conn)
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
