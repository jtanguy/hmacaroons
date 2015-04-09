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
    , caveatLoc
    , caveatId
    , caveatVId

    -- * Create Macaroons
    , create
    , inspect
    , addFirstPartyCaveat
    , addThirdPartyCaveat

    -- * Prepare Macaroons for transfer
    , serialize
    ) where

import           Crypto.Cipher.AES
import           Crypto.Hash
import           Data.Char
import           Data.Byteable
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as B8
import           Data.Hex
import           Data.Word
import           Data.Serialize

import           Crypto.Macaroon.Internal

-- | Create a Macaroon from its key, identifier and location
create :: Key -> Key -> Location -> Macaroon
create secret ident loc = MkMacaroon loc ident [] (toBytes (hmac derivedKey ident :: HMAC SHA256))
  where
    derivedKey = toBytes (hmac "macaroons-key-generator" secret :: HMAC SHA256)

-- | Caveat target location
caveatLoc :: Caveat -> Location
caveatLoc = cl

-- | Caveat identifier
caveatId :: Caveat -> Key
caveatId = cid

-- | Caveat verification identifier
caveatVId :: Caveat -> Key
caveatVId = vid

-- | Inspect a macaroon's contents. For debugging purposes.
inspect :: Macaroon -> String
inspect = show

-- | Serialize a macaroon in an URL-safe Base64 encoding
serialize :: Macaroon -> BS.ByteString
serialize m = B8.filter (/= '=') . B64.encode $ packets
  where
    packets = BS.concat [ putPacket "location" (location m)
                        , putPacket "identifier" (identifier m)
                        , caveatPackets
                        , putPacket "signature" (signature m)
                        ]
    caveatPackets = BS.concat $ map (cavPacket (location m)) (caveats m)
    cavPacket loc c | cl c == loc && vid c == BS.empty = putPacket "cid" (cid c)
                    | otherwise = BS.concat [ putPacket "cid" (cid c)
                                            , putPacket "vid" (vid c)
                                            , putPacket "cl" (cl c)
                                            ]
    putPacket key dat = BS.concat [
        B8.map toLower . hex . encode $ (fromIntegral size :: Word16)
        , key
        , " "
        , dat
        , "\n"
        ]
      where
        size = 4 + 2 + BS.length key + BS.length dat




-- | Add a first party Caveat to a Macaroon, with its identifier
addFirstPartyCaveat :: Key -> Macaroon -> Macaroon
addFirstPartyCaveat ident m = addCaveat (location m) ident BS.empty m

-- |Add a third party Caveat to a Macaroon, using its location, identifier and
-- verification key
addThirdPartyCaveat :: Key
                    -> Key
                    -> Location
                    -> Macaroon
                    -> Macaroon
addThirdPartyCaveat key cid loc m = addCaveat loc cid vid m
  where
    vid = encryptECB (initAES (signature m)) key


