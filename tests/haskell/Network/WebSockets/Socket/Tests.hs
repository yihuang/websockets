{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets.Socket.Tests
    ( tests
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, forM_, replicateM)
import Prelude hiding (catch)

import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee, ($$))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Enumerator as E
import qualified Network.Socket as S
import qualified Network.Socket.Enumerator as SE

import Network.WebSockets
import Network.WebSockets.Monad
import Network.WebSockets.Socket
import Network.WebSockets.Protocol.Hybi00.Internal
import Network.WebSockets.Protocol.Hybi10.Internal
import Network.WebSockets.Tests.Util
import Network.WebSockets.Tests.Util.Http

tests :: Test
tests = testGroup "Network.WebSockets.Socket.Tests"
    [ testProperty "sendReceive-hybi10" (sendReceive Hybi10_)
    , testProperty "sendReceive-hybi00" (sendReceive Hybi00_)
    ]

client :: Int -> (Iteratee ByteString IO () -> Iteratee ByteString IO a) -> IO a
client port app = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    hostAddr <- S.inet_addr "127.0.0.1"
    let addr = S.SockAddrInet (fromIntegral port) hostAddr
    S.connect sock addr
    res <- E.run_ $ SE.enumSocket 4096 sock $$ app $ iterSocket sock
    S.sClose sock
    return res

webSocketsClient :: Protocol p => Int -> p -> WebSockets p a -> IO a
webSocketsClient port proto ws =
    client port $ runWebSocketsWith' defaultWebSocketsOptions proto ws

sendReceive :: forall p. (ExampleRequest p, TextProtocol p) => p -> Property
sendReceive proto = monadicIO $ do
    serverThread <- run $ forkIO $ retry $ runServer "0.0.0.0" 42940 server
    waitSome
    texts <- map unArbitraryUtf8 <$> pick arbitrary
    texts' <- run $ retry $ webSocketsClient 42940 proto $ client' texts
    waitSome
    run $ killThread serverThread
    assert $ texts == texts'
  where
    waitSome = run $ threadDelay $ 200 * 1000

    server :: Request -> WebSockets p ()
    server _ = flip catchWsError (\_ -> return ()) $ forever $ do
        text <- receiveData
        sendTextData (text :: BL.ByteString)

    client' :: [BL.ByteString] -> WebSockets p [BL.ByteString]
    client' texts = do
        sendBuilder $ encodeRequestBody $ exampleRequest proto
        forM_ texts sendTextData
        replicateM (length texts) $ do
            receiveData

    -- HOLY SHIT WHAT SORT OF ATROCITY IS THIS?!?!?!
    --
    -- The problem is that sometimes, the server hasn't been brought down yet
    -- before the next test, which will cause it not to be able to bind to the
    -- same port again. In this case, we just retry.
    --
    -- The same is true for our client: possibly, the server is not up yet
    -- before we run the client. We also want to retry in that case.
    retry :: IO a -> IO a
    retry action = action `catch` \(_ :: SomeException) -> action
