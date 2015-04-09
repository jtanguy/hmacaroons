{-|
Module      : Crypto.Macaroon.Binder
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Crypto.Macaroon.Binder where

import           Crypto.Hash
import           Data.Byteable
import qualified Data.ByteString          as BS

import           Crypto.Macaroon.Internal

-- | Datatype for binding discharging and authorizing macaroons together
newtype Binder = Binder { bind :: Macaroon -> Macaroon -> BS.ByteString }


-- | Binder which concatenates the two signatures and hashes them
hashSigs :: Binder
hashSigs = Binder $ \m m' -> toBytes (HMAC . hash $ BS.append (toBytes $ signature m') (toBytes $ signature m) :: HMAC SHA256)

