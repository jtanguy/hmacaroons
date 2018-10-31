{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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


import           Control.Applicative
import           Control.Monad                     hiding (forM)
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import           Data.Bool
import qualified Data.ByteString                   as BS
import           Data.Either.Combinators
import           Data.Traversable

import           Crypto.Macaroon.Internal
import           Crypto.Macaroon.Verifier.Internal



-- | Verify a Macaroon's signature and caveats, given the corresponding Secret
-- and verifiers.
--
-- A verifier is a function of type
-- @'MonadIO' m => 'Caveat' -> m VerifierResult@.
--
-- It should return:
--
-- * 'Unrelated' if the caveat is not related to the verifier
-- (for instance a time verifier is given an action caveat);
-- * 'Refused' ('ParseError' reason) if the verifier  is related to the
-- caveat, but failed to parse it completely;
-- * 'Refused' ('ValidatorError' reason) if the verifier is related to the
-- caveat, parsed it and invalidated it;
-- * 'Verified' if the verifier has successfully verified the
-- given caveat
verify :: (Functor m, MonadIO m) => Secret -> [Caveat -> m VerifierResult] -> Macaroon -> m (Either ValidationError Macaroon)
verify secret verifiers m = join <$> forM (verifySig secret m) (verifyCavs verifiers)

