{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr


This test suite is based on the pymacaroons test suite:
<https://github.com/ecordell/pymacaroons>
-}
module Crypto.Macaroon.Verifier.Internal.Tests where

import qualified Data.ByteString.Char8             as B8
import           Data.List
import           Test.Tasty
-- import Test.Tasty.HUnit
import           Data.Either
import           Test.Tasty.QuickCheck             hiding (Failure, Success)

import           Crypto.Macaroon
import           Crypto.Macaroon.Verifier.Internal

import           Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Verifier.Internal" [ sigs
                                                      ]

sigs = testProperty "Signatures" $ \sm -> verifySig (secret sm) (macaroon sm) == Right (macaroon sm)
