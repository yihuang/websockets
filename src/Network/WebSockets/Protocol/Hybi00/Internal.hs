{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Network.WebSockets.Protocol.Hybi00.Internal
       ( Hybi00_ (..)
       ) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource ( MonadThrow(monadThrow) )
import Data.Char (isDigit)

import Data.Binary (encode)
import Data.Digest.Pure.MD5 (md5)
import Data.Int (Int32)
import qualified Blaze.ByteString.Builder as BB
import qualified Data.Attoparsec as A
import qualified Data.Conduit.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI

import Network.WebSockets.Handshake.Http
import Network.WebSockets.Protocol
import Network.WebSockets.Types

data Hybi00_ = Hybi00_

instance Protocol Hybi00_ where
    version         Hybi00_   = "hybi00"
    headerVersions  Hybi00_   = []  -- The client will elide it
    supported       Hybi00_ h = getSecWebSocketVersion h == Nothing
    encodeMessages  Hybi00_   = CL.map encodeMessage
    decodeMessages  Hybi00_   = C.sequence (A.sinkParser parseMessage)
    finishRequest   Hybi00_   = handshakeHybi00
    implementations           = [Hybi00_]

instance TextProtocol Hybi00_

encodeMessage :: Message p -> BB.Builder
encodeMessage (DataMessage (Text pl))    =
    BB.fromLazyByteString $ "\0" `BL.append` pl `BL.append` "\255"
encodeMessage (ControlMessage (Close _)) =
    BB.fromLazyByteString  "\255\0"
encodeMessage msg                        = error $
    "Network.WebSockets.Protocol.Hybi00.encodeFrame: unsupported message: " ++
    show msg

parseMessage :: A.Parser (Message p)
parseMessage = parseText <|> parseClose
  where
    parseText = do
        _ <- A.word8 0x00
        utf8string <- A.manyTill A.anyWord8 (A.try $ A.word8 0xff)
        return $ DataMessage $ Text $ BL.pack utf8string

    parseClose = do
        _ <- A.word8 0xff
        _ <- A.word8 0x00
        return $ ControlMessage $ Close ""

divBySpaces :: String -> Maybe Int32
divBySpaces str
    | spaces == 0 = Nothing
    | otherwise   = Just . fromIntegral $ number `div` spaces
  where
    number = read $ filter isDigit str :: Integer
    spaces = fromIntegral . length $ filter (== ' ') str

handshakeHybi00 :: MonadThrow m
                => RequestHttpPart
                -> C.Sink B.ByteString m Request
handshakeHybi00 reqHttp@(RequestHttpPart path h isSecure) = do
    keyPart3 <- A.sinkParser $ A.take 8
    keyPart1 <- numberFromToken =<< getHeader "Sec-WebSocket-Key1"
    keyPart2 <- numberFromToken =<< getHeader "Sec-WebSocket-Key2"

    let key = B.concat . BL.toChunks . encode . md5 $ BL.concat
                [keyPart1, keyPart2, BL.fromChunks [keyPart3]]

    host <- getHeader "Host"
    -- todo: origin right? (also applies to hybi10)
    origin <- getHeader "Origin"
    let schema = if isSecure then "wss://" else "ws://"
    let response = response101
            [ ("Sec-WebSocket-Location", B.concat [schema, host, path])
            , ("Sec-WebSocket-Origin", origin)
            ]
            key

    return $ Request path h response
  where
    getHeader k = case lookup k h of
        Just t  -> return t
        Nothing -> lift . monadThrow $ MalformedRequest reqHttp $
            "Header missing: " ++ BC.unpack (CI.original k)

    numberFromToken token = case divBySpaces (BC.unpack token) of
        Just n  -> return $ encode n
        Nothing -> lift . monadThrow $ MalformedRequest reqHttp
            "Security token does not contain enough spaces"
