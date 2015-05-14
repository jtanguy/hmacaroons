{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Crypto.Macaroon.Verifier
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Crypto.Macaroon.Verifier where


import           Crypto.Hash
import           Data.Bool
import qualified Data.ByteString            as BS
import           Data.Byteable
import           Data.Foldable

import           Crypto.Macaroon.Internal


-- | Opaque datatype for now. Might need more explicit errors
data Result = Success | Failure deriving (Show,Eq)

verifySig :: Key -> Macaroon -> Result
verifySig k m = bool Failure Success $
      signature m == foldl' hash (toBytes (hmac derivedKey (identifier m) :: HMAC SHA256)) (caveats m)
  where
    hash s c = toBytes (hmac s (vid c `BS.append` cid c) :: HMAC SHA256)
    derivedKey = toBytes (hmac "macaroons-key-generator" k :: HMAC SHA256)
