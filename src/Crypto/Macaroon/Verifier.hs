{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-|
Module      : Crypto.Macaroon.Verifier
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Crypto.Macaroon.Verifier (
    Verifier
  , verifyMacaroon
  , verifySig
  -- , verifyExact
  -- , verifyFun
  , module Data.Attoparsec.ByteString.Char8
  , verifyCavs
) where


import           Crypto.Hash
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import           Data.Bool
import           Data.Byteable
import qualified Data.ByteString                  as BS
import           Data.Either
import           Data.Either.Validation
import           Data.Foldable
import           Data.Function
import           Data.Maybe
import           Data.Traversable

import           Crypto.Macaroon.Internal

type Verifier = Caveat -> Maybe (Either String Caveat)

verifySig :: Key -> Macaroon -> Either String Macaroon
verifySig k m = bool (Left "Signatures do not match") (Right m) $
      signature m == foldl' hash (toBytes (hmac derivedKey (identifier m) :: HMAC SHA256)) (caveats m)
  where
    hash s c = toBytes (hmac s (vid c `BS.append` cid c) :: HMAC SHA256)
    derivedKey = toBytes (hmac "macaroons-key-generator" k :: HMAC SHA256)

verifyMacaroon :: Key -> [Verifier] -> Macaroon -> Either String Macaroon
verifyMacaroon secret verifiers m = verifySig secret m >>= verifyCavs verifiers

verifyCavs :: [Verifier] -> Macaroon -> Either String Macaroon
verifyCavs verifiers m = case partitionEithers verifiedCaveats of
    ([],_) -> Right m
    (errs,_) -> Left (mconcat errs)
  where
    verifiedCaveats = map (\c -> defaultFail c $ foldMap (fmap eitherToValidation . ($c)) verifiers) $ caveats m
    defaultFail c = maybe (Left ("No validation for this caveat: " ++ show c)) validationToEither


-- TODO: define API
