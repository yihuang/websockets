{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Protocol.Emulate
  ( EmulateProtocol(..)
  ) where

import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi10 (Hybi10_(Hybi10_))
import Network.WebSockets.Handshake.Http

data EmulateProtocol = EmulateProtocol

instance Protocol EmulateProtocol where
    version _ = "sockjs"
    headerVersion _ = "sockjs"
    encodeFrame _ = encodeFrame Hybi10_
    decodeFrame _ = decodeFrame Hybi10_
    finishRequest _ (RequestHttpPart path hs) = return $ Right $ Request path hs (Response 101 "" [] "")
    implementations = [EmulateProtocol]

instance TextProtocol EmulateProtocol
instance BinaryProtocol EmulateProtocol

