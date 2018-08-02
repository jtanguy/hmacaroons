{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Crypto.Macaroon
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable

Pure haskell implementations of macaroons.

Warning: this implementation has not been audited by security experts.
Do not use in production


References:

- Macaroons: Cookies with Contextual Caveats for Decentralized Authorization in the Cloud <http://research.google.com/pubs/pub41892.html>
- Time for better security in NoSQL <http://hackingdistributed.com/2014/11/23/macaroons-in-hyperdex>
-}
module Crypto.Macaroon (
    -- * Types
      Macaroon
    , Caveat
    , Secret
    , Key
    , Location
    , Sig
    -- * Accessing functions
    -- ** Macaroons
    , location
    , identifier
    , caveats
    , signature
    -- ** Caveats
    , cl
    , cid
    , vid

    -- * Create Macaroons
    , create
    , inspect
    , addFirstPartyCaveat
    -- , addThirdPartyCaveat
    -- * Serialize
    , module Crypto.Macaroon.Serializer.Binary.V1
    -- * Verify
    , module Crypto.Macaroon.Verifier
    ) where

-- import           Crypto.Cipher.AES
import           Crypto.Hash
import           Data.Byteable
import qualified Data.ByteString                   as BS

import           Crypto.Macaroon.Internal
import           Crypto.Macaroon.Serializer.Binary.V1
import           Crypto.Macaroon.Verifier

-- | Create a Macaroon from its key, identifier and location
create :: Secret -> Key -> Location -> Macaroon
create secret ident loc = MkMacaroon loc ident [] (toBytes (hmac derivedKey ident :: HMAC SHA256))
  where
    derivedKey = toBytes (hmac "macaroons-key-generator" secret :: HMAC SHA256)

-- | Inspect a macaroon's contents. For debugging purposes.
inspect :: Macaroon -> String
inspect = show

-- | Add a first party Caveat to a Macaroon, with its identifier
addFirstPartyCaveat :: Key -> Macaroon -> Macaroon
addFirstPartyCaveat ident m = addCaveat (location m) ident BS.empty m

-- |Add a third party Caveat to a Macaroon, using its location, identifier and
-- verification key
-- addThirdPartyCaveat :: Key
--                     -> Key
--                     -> Location
--                     -> Macaroon
--                     -> Macaroon
-- addThirdPartyCaveat key cid loc m = addCaveat loc cid vid m
--   where
--     vid = encryptECB (initAES (signature m)) key
