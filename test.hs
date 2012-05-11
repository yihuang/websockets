{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Debug.Trace
import Data.ByteString (ByteString)
import qualified Blaze.ByteString.Builder as B
import Data.Conduit ( ($$), ($=), runResourceT )
import qualified Data.Conduit.Network as Net
import qualified Data.Conduit.List as CL
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)

import qualified Network.WebSockets.Monad as WS
import qualified Network.WebSockets.Conduit as WS
import Network.Wai
import Network.Wai.Handler.Warp

echo :: WS.Server ()
echo = forever $ WS.recvBS >>= WS.sendBS

close = return ()

route :: WS.Request -> WS.Server ()
route req = case WS.requestPath req of
    "/echo" -> echo
    "/close" -> close

main :: IO ()
main = do
    let echoApp = WS.runServerT echo
    forkIO $ runResourceT $
        Net.runTCPServer
            (Net.ServerSettings 3001 Net.HostAny)
            echoApp
    runSettings defaultSettings
        { settingsPort = 3000
        , settingsIntercept = WS.intercept (undefined::WS.Hybi00) (WS.WebSocketsOptions (return ())) (fmap WS.runServerT route)
        } undefined
