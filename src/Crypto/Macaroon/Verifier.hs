{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : Crypto.Macaroon.Verifier
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Crypto.Macaroon.Verifier (
    Verified(..)
  , verifySig
  , verifyExact
  , verifyCavs
  -- , module Data.Attoparsec.ByteString
  , module Data.Attoparsec.ByteString.Char8
) where


import           Crypto.Hash
import           Data.Bool
import qualified Data.ByteString            as BS
import           Data.Byteable
import           Data.Foldable
import           Data.Maybe
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8

import           Crypto.Macaroon.Internal


-- | Opaque datatype for now. Might need more explicit errors
data Verified = Ok | Failed deriving (Show,Eq)

instance Monoid Verified where
  mempty = Ok
  mappend Ok Ok = Ok
  mappend _ _ = Failed


type CaveatVerifier = Caveat -> Maybe Verified

verifySig :: Key -> Macaroon -> Verified
verifySig k m = bool Failed Ok $
      signature m == foldl' hash (toBytes (hmac derivedKey (identifier m) :: HMAC SHA256)) (caveats m)
  where
    hash s c = toBytes (hmac s (vid c `BS.append` cid c) :: HMAC SHA256)
    derivedKey = toBytes (hmac "macaroons-key-generator" k :: HMAC SHA256)

verifyCavs :: [Caveat -> Maybe Verified] -> Macaroon -> Verified
verifyCavs verifiers m = mconcat $ map (\c -> mconcat . catMaybes $ map ($ c) verifiers) (caveats m)

verifyExact :: (Show a, Eq a) => Key -> a -> Parser a -> Caveat -> Maybe Verified
verifyExact key expected parser cav = if key `BS.isPrefixOf` cid cav then
        case parseOnly kvparser (cid cav) of
          Right v -> verify <$> Just v
          Left _ -> Just Failed
        else Nothing
  where
    kvparser = do
      key <- string key
      skipSpace
      string "="
      skipSpace
      parser

      -- *> skipSpace *> string "=" *> skipSpace *> parser <* endOfInput
    verify a = bool Failed Ok (a == expected)
