-- | Implementation of the WebSocket handshake
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.WebSockets.Handshake
    ( HandshakeError (..)
    , handshake
    , responseError
    ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadThrow(monadThrow))

import Data.List (find)
import qualified Data.ByteString as B
import qualified Data.Conduit as C

import Network.WebSockets.Handshake.Http
import Network.WebSockets.Protocol

-- | Receives and checks the client handshake. If no suitable protocol is found
-- (or the client sends garbage), a 'HandshakeError' will be thrown.
handshake :: (MonadThrow m, Protocol p)
          => RequestHttpPart
          -> C.Sink B.ByteString m (Request, p)
handshake rhp = case find (flip supported rhp) implementations of
    Nothing -> lift $ monadThrow NotSupported
    Just p  -> do
        rq <- finishRequest p rhp
        return (rq, p)

-- | Respond to errors encountered during handshake. First argument may be
-- bottom.
responseError :: forall p. Protocol p => p -> HandshakeError -> Response
responseError _ err = response400 $ case err of
    -- TODO: fix
    NotSupported -> versionHeader  -- Version negotiation
    _            -> []
  where
    versionHeader = [("Sec-WebSocket-Version",
        B.intercalate ", " $ concatMap headerVersions (implementations :: [p]))]
