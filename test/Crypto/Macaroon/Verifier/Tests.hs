{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr


This test suite is based on the pymacaroons test suite:
<https://github.com/ecordell/pymacaroons>
-}
module Crypto.Macaroon.Verifier.Tests where


import qualified Data.ByteString.Char8 as B8
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import           Crypto.Macaroon
import           Crypto.Macaroon.Verifier

import Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Verifier" [ sigs
                                             ]

{-
 - Test fixtures
 -}
sec = B8.pack "this is our super secret key; only we should know it"

m :: Macaroon
m = create sec key loc
  where
    key = B8.pack "we used our sec key"
    loc = B8.pack "http://mybank/"

m2 :: Macaroon
m2 = addFirstPartyCaveat "test = caveat" m

m3 :: Macaroon
m3 = addFirstPartyCaveat "test = acaveat" m

{-
 - Tests
 -}
sigs = testGroup "Signatures" [ basic
                              , one
                              , two
                              ]

basic = testGroup "Basic Macaroon" [ none , sigQC ]

none = testCase "No caveat" $
    VSuccess @=? verifySig sec m

sigQC = testProperty "Random" $
    \sm -> verifySig (secret sm) (macaroon sm) == VSuccess

one = testCase "Macaroon with one caveat" $
    VSuccess @=? verifySig sec m2

two = testCase "Macaroon with two caveats" $
    VSuccess @=? verifySig sec m3

