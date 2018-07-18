{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Crypto.Macaroon.Serializer.Base64
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable

Base64 serializer/deserializer

-}
module Crypto.Macaroon.Serializer.Base64 (
        serialize
      , deserialize
      ) where

import           Control.Applicative
import           Control.Monad
import           Crypto.Macaroon.Internal
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.Bits
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base64.URL       as B64
import qualified Data.ByteString.Char8            as B8
import           Data.Char
import           Data.Hex
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Serialize
import           Data.Word


-- | Serialize a macaroon in an URL-safe Base64 encoding
serialize :: Macaroon -> BS.ByteString
serialize m = B8.filter (/= '=') . B64.encode . runPut $ do
    packetize "location" (location m)
    packetize "identifier" (identifier m)
    forM_ (caveats m) $ \c -> do
        packetize "cid" (cid c)
        unless (cl c == location m && vid c == BS.empty) $ do
            packetize "vid" (vid c)
            packetize "cl" (cl c)
    packetize "signature" (signature m)

packetize :: BS.ByteString -> BS.ByteString -> Put
packetize key dat = do
    let size = 4 + 2 + BS.length key + BS.length dat
    putByteString $ B8.map toLower . hex . encode $ (fromIntegral size :: Word16)
    putByteString key
    putByteString " "
    putByteString dat
    putByteString "\n"

-- | Deserialize a macaroon from a base64url-encoded ByteString
deserialize :: BS.ByteString -> Either String Macaroon
deserialize = parseOnly macaroon . B64.decodeLenient


macaroon :: Parser Macaroon
macaroon = do
    ps <- many packet <* endOfInput
    let (header,ps') = splitAt 2 ps
    (l, i) <- case header of
      [("location",l),("identifier",i)] -> pure (l, i)
      _                                 -> fail "missing macaroon header"
    let (caveats,sig) = splitAt (length ps' - 1) ps'
    s <- case sig of
        [("signature", s)] -> pure s
        _                  -> fail "missing macaroon signature"
    return $ MkMacaroon l i (map (mkCaveat l) (groupBy splitCavs caveats)) s
  where
    mkCaveat _ [("cid",c),("vid",v),("cl",l)] = MkCaveat c v l
    mkCaveat l [("cid",c)] = MkCaveat c BS.empty l
    mkCaveat _ _ = error "Malformed caveat"
    splitCavs _ ("cid",_) = False
    splitCavs _ _ = True

packet :: Parser (BS.ByteString, BS.ByteString)
packet = do
    size <- A8.take 4
    case A8.parseOnly (A8.hexadecimal :: Parser Word16) size of
        Left e -> fail e
        Right s -> do
            bs <- A8.take (fromIntegral $ s - 4)
            let (key, dat) = B8.break (== ' ') bs
            return (key, B8.tail $ B8.init dat)

