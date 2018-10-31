{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Crypto.Macaroon.Verifier.Internal
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Crypto.Macaroon.Verifier.Internal where

import           Control.Arrow            ((&&&))
import           Crypto.Hash
import           Data.Bool
import           Data.Byteable
import qualified Data.ByteString          as BS
import           Data.Foldable
import           Data.List.NonEmpty       (NonEmpty, nonEmpty)
import           Data.Maybe
import           Data.Text                (Text)

import           Crypto.Macaroon.Internal


-- | Type representing the result of a validator
data VerifierResult = Verified -- ^ The caveat is correctly parsed and verified
                    | Refused VerifierError -- ^ The caveat is refused
                    | Unrelated -- ^ The given verifier does not verify the caveat
                    deriving (Show, Eq)

-- | Type representing an error returned by a verifier (on a caveat it's supposed to handle)
data VerifierError = ParseError Text -- ^ The caveat couldn't be parsed
                   | VerifierError Text -- ^ The verifier understood the caveat but couldn't satisfy it.
                   deriving (Show, Eq)

-- | Type alias for a caveat that was not discharged
--   an empty list means that no verifiers were related to the caveat
type RemainingCaveat = (Caveat, [VerifierError])

-- | Type representing a macaroon validation error.
data ValidationError = SigMismatch -- ^ Signatures do not match
                     | RemainingCaveats (NonEmpty RemainingCaveat)
                     -- ^ There are remaining caveats
                     deriving (Show,Eq)

-- | Check that the given macaroon has a correct signature
verifySig :: Key -> Macaroon -> Either ValidationError Macaroon
verifySig k m = bool (Left SigMismatch) (Right m) $
    signature m == foldl' hash (toBytes (hmac derivedKey (identifier m) :: HMAC SHA256)) (caveats m)
  where
    hash s c = toBytes (hmac s (vid c `BS.append` cid c) :: HMAC SHA256)
    derivedKey = toBytes (hmac "macaroons-key-generator" k :: HMAC SHA256)

-- | Given a list of verifiers, verify each caveat of the given macaroon
verifyCavs :: forall m. (Applicative m)
           => [Caveat -> m VerifierResult]
           -> Macaroon
           -> m (Either ValidationError Macaroon)
verifyCavs verifiers m = toEither <$> errors
  where
    toEither = maybe (Right m) (Left . RemainingCaveats) . nonEmpty
    errors = fmap catMaybes $ traverse keepErrors $ caveats m

    -- apply the verifiers to a caveat and only keep errors
    keepErrors :: Caveat -> m (Maybe RemainingCaveat)
    keepErrors =
        fmap sequenceA . -- Move the Maybe outside the tuple
        sequenceA . -- Move the m outside the tuple
        getTaggedErrors

    -- annotated results
    getTaggedErrors :: Caveat -> (Caveat, m (Maybe [VerifierError]))
    getTaggedErrors = id &&& fmap collapseErrors . applyVerifiers

    -- collapse all the results, @Nothing@ means the caveat has been
    -- verified
    collapseErrors :: [VerifierResult] -> Maybe [VerifierError]
    collapseErrors rs =
      if Verified `elem` rs
        then Nothing
        else Just [e | Refused e <- rs]

    -- apply all the verifiers to a caveat.
    applyVerifiers :: Caveat -> m [VerifierResult]
    applyVerifiers c = traverse ($ c) verifiers
