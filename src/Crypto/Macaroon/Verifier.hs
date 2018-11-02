{-|
Module      : Crypto.Macaroon.Verifier
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Crypto.Macaroon.Verifier (
    verify
  , VerifierResult(..)
  , VerifierError(..)
  , ValidationError(..)
) where


import           Data.Functor.Identity

import           Crypto.Macaroon.Internal
import           Crypto.Macaroon.Verifier.Internal



-- | Verify a Macaroon's signature and caveats, given the corresponding Secret
-- and verifiers.
--
-- A verifier is a function of type
-- @'Monad' m => 'Caveat' -> m VerifierResult@.
--
-- It should return:
--
-- * 'Unrelated' if the caveat is not related to the verifier
-- (for instance a time verifier is given an action caveat);
-- * 'Refused' ('ParseError' reason) if the verifier  is related to the
-- caveat, but failed to parse it completely;
-- * 'Refused' ('VerifierError' reason) if the verifier is related to the
-- caveat, parsed it and invalidated it;
-- * 'Verified' if the verifier has successfully verified the
-- given caveat
verify :: (Applicative m)
       => Secret
       -> [Caveat
       -> m (VerifierResult pe ve)]
       -> Macaroon
       -> m (Either (ValidationError pe ve) Macaroon)
verify secret verifiers m =
    let checkSig = verifySig secret
        checkCavs = fmap (fmap $ const m) . either (pure . Left) (verifyCavs verifiers . caveats )
    in checkCavs . checkSig $ m

-- | Synchronously verify a macaroon signature and caveats, given the
-- corresponding Secret and verifiers.
-- This is a variant of @verify@ working with synchronous verifiers.
verifySync :: Secret -> [Caveat -> VerifierResult pe ve] -> Macaroon -> Either (ValidationError pe ve) Macaroon
verifySync secret verifiers m =
    let verifiersIdent = fmap (fmap Identity) verifiers
    in runIdentity $ verify secret verifiersIdent m
