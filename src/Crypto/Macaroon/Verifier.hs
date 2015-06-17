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
  , CaveatVerifier
  , (<???>)
  , verifyMacaroon
  , verifySig
  , verifyExact
  , verifyFun
  , module Data.Attoparsec.ByteString.Char8
  , verifyCavs
) where


import           Crypto.Hash
import           Data.Bool
import qualified Data.ByteString            as BS
import           Data.Byteable
import           Data.Foldable
import           Data.Function
import           Data.Maybe
import           Data.Traversable
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8

import           Crypto.Macaroon.Internal


-- | Opaque datatype for now. Might need more explicit errors
data Verified = Ok | Failed deriving (Show,Eq)

instance Monoid Verified where
  mempty = Ok
  mappend Ok Ok = Ok
  mappend _ _ = Failed


data CaveatVerifier = CV { vFun :: Caveat -> Maybe Verified , helpText :: String}

instance Eq CaveatVerifier where
  (==) = (==) `on` helpText

instance Show CaveatVerifier where
    show = helpText

(<???>) :: (Caveat -> Maybe Verified) -> String -> CaveatVerifier
f <???> t = CV f t

verifySig :: Key -> Macaroon -> Verified
verifySig k m = bool Failed Ok $
      signature m == foldl' hash (toBytes (hmac derivedKey (identifier m) :: HMAC SHA256)) (caveats m)
  where
    hash s c = toBytes (hmac s (vid c `BS.append` cid c) :: HMAC SHA256)
    derivedKey = toBytes (hmac "macaroons-key-generator" k :: HMAC SHA256)

verifyMacaroon :: Key -> [CaveatVerifier] -> Macaroon -> Verified
verifyMacaroon secret verifiers m = verifySig secret m `mappend` verifyCavs verifiers m


verifyCavs :: [CaveatVerifier] -> Macaroon -> Verified
verifyCavs verifiers m = foldMap (\c -> fromMaybe Failed $ foldMap (($ c) . vFun) verifiers) (caveats m)

verifyExact :: (Eq a) => Key -> a -> Parser a -> Caveat -> Maybe Verified
verifyExact k expected = verifyFun k (expected ==)

verifyFun :: Key -> (a -> Bool) -> Parser a -> Caveat -> Maybe Verified
verifyFun key f parser cav = if key `BS.isPrefixOf` cid cav then
        case parseOnly kvparser (cid cav) of
          Right v -> (bool Failed Ok . f) <$> Just v
          Left _ -> Just Failed
        else Nothing
  where
    kvparser = do
      key <- string key
      skipSpace
      string "="
      skipSpace
      parser <* endOfInput
