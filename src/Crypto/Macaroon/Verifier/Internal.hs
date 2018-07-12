{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-|
Module      : Crypto.Macaroon.Verifier.Internal
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Crypto.Macaroon.Verifier.Internal where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Bool
import           Data.Byteable
import qualified Data.ByteString          as BS
import           Data.Either
import           Data.Either.Validation
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup

import           Crypto.Macaroon.Internal


-- | Type representing the result of a validator
data VerifierResult = Verified -- ^ The caveat is correctly parsed and verified
                    | Refused ValidationError -- ^ The caveat is refused (Either a parse error or a validation error
                    | Unrelated -- ^ The given verifier does not verify the caveat
                    deriving (Show, Eq)

-- | Type representing different validation errors.
-- Only 'ParseError' and 'ValidatorError' are exported, @SigMismatch@ and
-- @NoVerifier@ are used internally and should not be used by the user
data ValidationError = SigMismatch -- ^ Signatures do not match
                     | NoVerifier -- ^ No verifier can handle a given caveat
                     | ParseError String -- ^ A verifier had a parse error
                     | ValidatorError String -- ^ A verifier failed
                     deriving (Show,Eq)


-- | The 'Semigroup' instance is written so @SigMismatch@ is an annihilator
instance Semigroup ValidationError where
    NoVerifier <> e = e
    e <> NoVerifier = e
    SigMismatch <> _ = SigMismatch
    _ <> SigMismatch = SigMismatch
    (ValidatorError e) <> (ParseError _) = ValidatorError e
    (ParseError _) <> (ValidatorError e) = ValidatorError e
    (ValidatorError e1) <> (ValidatorError e2) = ValidatorError $ e1 <> " " <> e2
    (ParseError e1) <> (ParseError e2) = ParseError $ e1 <> " " <> e2

-- | @NoVerifier@ is the identity element
instance Monoid ValidationError where
    mempty = NoVerifier

-- | Check that the given macaroon has a correct signature
verifySig :: Key -> Macaroon -> Either ValidationError Macaroon
verifySig k m = bool (Left SigMismatch) (Right m) $
    signature m == foldl' hash (toBytes (hmac derivedKey (identifier m) :: HMAC SHA256)) (caveats m)
  where
    hash s c = toBytes (hmac s (vid c `BS.append` cid c) :: HMAC SHA256)
    derivedKey = toBytes (hmac "macaroons-key-generator" k :: HMAC SHA256)

-- | Given a list of verifiers, verify each caveat of the given macaroon
verifyCavs :: (Functor m, MonadIO m)
           => [Caveat -> m VerifierResult]
           -> Macaroon
           -> m (Either ValidationError Macaroon)
verifyCavs verifiers m = gatherEithers <$> mapM validateCaveat (caveats m)
  where
    {-
     - validateCaveat :: Caveat -> m (Validation String Caveat)
     - We can use fromJust here safely since we use a `Just Failure` as a
     - starting value for the foldM. We are guaranteed to have a `Just something`
     - from it.
     -}
    validateCaveat c = fmap (const c) . fromJust <$> foldM (\res v -> mappend res . fmap eitherToValidation . vResult2MaybeEither <$> v c) (defErr c) verifiers
    -- vResult2MaybeEither :: VerifierResult -> Maybe (Either ValidationError ())
    vResult2MaybeEither Unrelated = Nothing
    vResult2MaybeEither Verified = Just (Right ())
    vResult2MaybeEither (Refused e)= Just (Left e)
    -- defErr :: Caveat -> Maybe (Validation String Caveat)
    defErr c = Just $ Failure NoVerifier
    -- gatherEithers :: [Validation String Caveat] -> Either String Caveat
    gatherEithers vs = case partitionEithers . map validationToEither $ vs of
        ([],_) ->  Right m
        (errs,_) -> Left (mconcat errs)
