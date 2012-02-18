{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Data.ByteString (ByteString)
import Data.Conduit ( ($$) )
import qualified Blaze.ByteString.Builder as B

import Network.WebSockets.Conduit as WS
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.Wai (Response(ResponseBuilder))

echo :: WS.Request -> WS.Application ByteString
echo _ src snk = src $$ snk

main :: IO ()
main = runSettings defaultSettings
         { settingsPort = 3000
         , settingsIntercept = intercept (undefined::WS.Hybi00) (WebSocketsOptions (return ())) echo
         } (\_ -> return $ ResponseBuilder status200 [] (B.fromByteString "Pong"))
