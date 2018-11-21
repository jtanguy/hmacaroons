{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
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

import           Crypto.Macaroon.Internal


-- | Type representing the result of a validator
data VerifierResult pe ve = Verified -- ^ The caveat is correctly parsed and verified
                    | Refused (VerifierError pe ve) -- ^ The caveat is refused
                    | Unrelated -- ^ The given verifier does not verify the caveat

deriving instance (Show pe, Show ve) => Show (VerifierResult pe ve)
deriving instance (Eq pe, Eq ve) => Eq (VerifierResult pe ve)

-- | Type representing an error returned by a verifier (on a caveat it's supposed to handle)
data VerifierError pe ve = ParseError pe -- ^ The caveat couldn't be parsed
                   | VerifierError ve -- ^ The verifier understood the caveat but couldn't satisfy it.

deriving instance (Show pe, Show ve) => Show (VerifierError pe ve)
deriving instance (Eq pe, Eq ve) => Eq (VerifierError pe ve)

-- | Type alias for a caveat that was not discharged
--   an empty list means that no verifiers were related to the caveat
type RemainingCaveat pe ve = (Caveat, [VerifierError pe ve])

-- | Type representing a macaroon validation error.
data ValidationError pe ve = SigMismatch -- ^ Signatures do not match
                     | RemainingCaveats (NonEmpty (RemainingCaveat pe ve))
                     -- ^ There are remaining caveats
                     --
deriving instance (Show pe, Show ve) => Show (ValidationError pe ve)
deriving instance (Eq pe, Eq ve) => Eq (ValidationError pe ve)

-- | Check that the given macaroon has a correct signature
verifySig :: Key -> Macaroon -> Either (ValidationError pe ve) Macaroon
verifySig k m = bool (Left SigMismatch) (Right m) $
    signature m == foldl' hash (toBytes (hmac derivedKey (identifier m) :: HMAC SHA256)) (caveats m)
  where
    hash s c = toBytes (hmac s (vid c `BS.append` cid c) :: HMAC SHA256)
    derivedKey = toBytes (hmac "macaroons-key-generator" k :: HMAC SHA256)

-- | Given a list of verifiers, verify each caveat of the given
-- list. You shouldn't use it directly unless you're doing fancy multi-step validation
verifyCavs :: forall m pe ve. (Applicative m)
           => [Caveat -> m (VerifierResult pe ve)]
           -> [Caveat]
           -> m (Either (ValidationError pe ve) ())
verifyCavs verifiers caveats = toEither <$> errors
  where
    toEither = maybe (Right ()) (Left . RemainingCaveats) . nonEmpty
    errors = catMaybes <$> traverse keepErrors caveats

    -- apply the verifiers to a caveat and only keep errors
    keepErrors :: Caveat -> m (Maybe (RemainingCaveat pe ve))
    keepErrors =
        fmap sequenceA . -- Move the Maybe outside the tuple
        sequenceA . -- Move the m outside the tuple
        getTaggedErrors

    -- annotated results
    getTaggedErrors :: Caveat -> (Caveat, m (Maybe [VerifierError pe ve]))
    getTaggedErrors = id &&& fmap collapseErrors . applyVerifiers

    -- collapse all the results, @Nothing@ means the caveat has been
    -- verified
    collapseErrors :: [VerifierResult pe ve] -> Maybe [VerifierError pe ve]
    collapseErrors rs =
      -- using `elem` would require an @Eq@ constraint on pe and ve
      if any isVerified rs
        then Nothing
        else Just [e | Refused e <- rs]
    isVerified Verified = True
    isVerified _        = False

    -- apply all the verifiers to a caveat.
    applyVerifiers :: Caveat -> m [VerifierResult pe ve]
    applyVerifiers c = traverse ($ c) verifiers
