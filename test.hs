{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Data.ByteString (ByteString)
import qualified Blaze.ByteString.Builder as B
import Data.Conduit ( ($$) )
import qualified Data.Conduit.Network as Net
import Control.Concurrent (forkIO)

import Network.WebSockets.Conduit as WS
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.Wai (Response(ResponseBuilder))

echo :: WS.Application ByteString
echo src snk = src $$ snk

main :: IO ()
main = do
    forkIO $ Net.runTCPServer 
                (Net.ServerSettings 3001 Nothing)
                echo
    runSettings defaultSettings
         { settingsPort = 3000
         , settingsIntercept = intercept (undefined::WS.Hybi00) (WebSocketsOptions (return ())) (const echo)
         } (\_ -> return $ ResponseBuilder status200 [] (B.fromByteString "Pong"))
