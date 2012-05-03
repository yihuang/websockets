{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Debug.Trace
import Data.ByteString (ByteString)
import qualified Blaze.ByteString.Builder as B
import Data.Conduit ( ($$), ($=), runResourceT )
import qualified Data.Conduit.Network as Net
import qualified Data.Conduit.List as CL
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)

import Network.WebSockets.Conduit as WS
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.Wai (Response(ResponseBuilder))

echo :: WS.Application ByteString
echo src snk = do
    src $$ snk

main :: IO ()
main = do
    forkIO $ runResourceT $
        Net.runTCPServer
            (Net.ServerSettings 3001 Net.HostAny)
            echo
    runSettings defaultSettings
         { settingsPort = 3000
         , settingsIntercept = intercept (undefined::WS.Hybi00) (WebSocketsOptions (return ())) (const echo)
         } (\_ -> return $ ResponseBuilder status200 [] (B.fromByteString "Pong"))