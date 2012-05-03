{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol.Hybi10
    ( Hybi10
    ) where

import Data.Conduit ((=$=))
import Data.Conduit.List as CL
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi10.Internal
import Network.WebSockets.Protocol.Unsafe

data Hybi10 = forall p. Protocol p => Hybi10 p

instance Protocol Hybi10 where
    version        (Hybi10 p)   = version p
    headerVersions (Hybi10 p)   = headerVersions p
    supported      (Hybi10 p) h = supported p h
    encodeMessages (Hybi10 p)   = CL.map castMessage =$= encodeMessages p
    decodeMessages (Hybi10 p)   = decodeMessages p =$= CL.map castMessage
    finishRequest  (Hybi10 p)   = finishRequest p
    implementations             = [Hybi10 Hybi10_]

instance TextProtocol Hybi10
instance BinaryProtocol Hybi10
