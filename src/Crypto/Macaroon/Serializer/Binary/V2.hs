{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : Crypto.Macaroon.Serializer.Binary.V2
Copyright   : (c) 2018 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable

Binary format serializer/deserializer

-}
module Crypto.Macaroon.Serializer.Binary.V2 (
  serialize,
  deserialize
  ) where

import           Control.Applicative
import           Control.Monad
import           Crypto.Macaroon.Internal
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.Bits
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Base64.URL       as B64
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy.Builder     as BL
import           Data.Char
import           Data.Hex
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Serialize
import           Data.Word


pattern FieldLocation = 1 :: Word64
pattern FieldIdentifier = 2 :: Word64
pattern FieldVid = 4 :: Word64
pattern FieldSignature = 6 :: Word64


-- | Serialize a macaroon in an URL-safe Base64 encoding
serialize :: Macaroon -> BS.ByteString
serialize = B8.filter (/= '=') . B64.encode . BL.toStrict . BL.toLazyByteString . putMacaroon

putMacaroon :: Macaroon -> BL.Builder
putMacaroon m = version <> opt_location <> ident <> putEOS <> cavs <> putEOS <> sig
  where 
    version = BL.word8 2
    opt_location = if BS.null (location m) then mempty else putField FieldLocation (location m)
    ident = putField FieldIdentifier (identifier m)
    cavs = foldMap (putCaveat (location m)) (caveats m)
    sig = putField FieldSignature (signature m)

putCaveat :: Location -> Caveat -> BL.Builder
putCaveat rootLocation c = opt_location <> ident <> opt_vid <> putEOS
  where
    opt_location = if rootLocation == (cl c) then mempty else putField FieldLocation (cl c)
    ident = putField FieldIdentifier (cid c)
    opt_vid = if (BS.null (vid c)) then mempty else putField FieldVid (vid c)
    

putField :: Word64 -> BS.ByteString -> BL.Builder
putField fieldType content = mconcat [
    putVarInt (fromIntegral fieldType :: Word64)
  , putVarInt (fromIntegral (BS.length content) :: Word64)
  , BL.byteString content 
  ]

-- | Encode a Word64.
putVarInt :: Word64 -> BL.Builder
putVarInt n
    | n < 128 = BL.word8 (fromIntegral n)
    | otherwise = BL.word8 (fromIntegral $ n .&. 127 .|. 128)
                        <> putVarInt (n `shiftR` 7)

putEOS :: BL.Builder
putEOS = BL.word8 0


-- | Deserialize a macaroon from a base64url-encoded ByteString
deserialize :: BS.ByteString -> Either String Macaroon
deserialize = parseOnly macaroonParser . B64.decodeLenient


macaroonParser :: Parser Macaroon
macaroonParser = do
  word8 2
  loc <- option BS.empty (parseField FieldLocation)
  ident <- parseField FieldIdentifier
  word8 0
  cavs <- manyTill (caveatParser loc) (word8 0)
  sig <- parseField FieldSignature
  return $ MkMacaroon loc ident cavs sig

caveatParser :: BS.ByteString -> Parser Caveat
caveatParser rootLocation = do
  loc <- option rootLocation (parseField FieldLocation)
  ident <- parseField FieldIdentifier
  v <- option BS.empty (parseField FieldVid)
  word8 0
  return $ MkCaveat ident v loc

parseField :: Word64 -> Parser BS.ByteString
parseField fieldType = do
    ty <- getVarInt
    size <- getVarInt
    if (ty /= fieldType) then mkError fieldType ty else A8.take (fromIntegral size)
  where
    mkError expected got = fail ("Unexpected field type value. Expected: " <> (show expected) <> " but got: " <> (show got))

 -- | Decode an unsigned varint.
getVarInt :: Parser Word64
getVarInt = loop 1 0
  where
    loop !s !n = do
        b <- anyWord8
        let n' = n + s * fromIntegral (b .&. 127)
        if (b .&. 128) == 0
            then return n'
            else loop (128*s) n'

