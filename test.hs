{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Debug.Trace

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Blaze.ByteString.Builder as B

import Data.Conduit ( ($$), ($=), runResourceT )
import qualified Data.Conduit.Network as Net
import qualified Data.Conduit.List as CL

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (ResourceT, resourceForkIO)

import Network.WebSockets.Monad
import Network.WebSockets.Conduit

import Network.Wai.Handler.Warp

echo :: Server ()
echo = forever $ recvBS >>= sendBS

close :: Server ()
close = return ()

fork' :: ResourceT IO () -> ResourceT IO (MVar ())
fork' ac = do
    m <- liftIO newEmptyMVar
    resourceForkIO $ ac >> liftIO (putMVar m ())
    return m

concurrent :: Server ()
concurrent = do
    bs <- recvBS
    send <- getSender
    vars <- lift $ forM [1..50] $ \i ->
                fork' $ replicateM_ 50 $
                    send $ S.concat ["hello ", S.pack (show i), bs]
    liftIO $ mapM_ takeMVar vars

route :: Request -> Server ()
route req = case requestPath req of
    "/echo" -> echo
    "/close" -> close
    "/concurrent" -> concurrent

main :: IO ()
main = do
    let tcpApp = runServerT concurrent
    forkIO $ runResourceT $
        Net.runTCPServer
            (Net.ServerSettings 3001 Net.HostAny)
            tcpApp
    runSettings defaultSettings
        { settingsPort = 3000
        , settingsIntercept = intercept (undefined::Hybi00) (WebSocketsOptions (return ())) (fmap runServerT route)
        } undefined
