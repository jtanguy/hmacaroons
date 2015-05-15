{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr


This test suite is based on the pymacaroons test suite:
<https://github.com/ecordell/pymacaroons>
-}
module Crypto.Macaroon.Verifier.Tests where


import Data.List
import qualified Data.ByteString.Char8 as B8
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import           Crypto.Macaroon
import           Crypto.Macaroon.Verifier

import Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Verifier" [ sigs
                                             , exactCavs
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
m3 = addFirstPartyCaveat "value = 42" m2

exVerifiers = [ verifyExact "test" "caveat" (many' letter_ascii)
              , verifyExact "value" 42 decimal
              ]
exVerifiers' = [ verifyExact "test" "caveat" (many' letter_ascii)
               , verifyExact "value" 43 decimal
               ]
funVerifiers = [ verifyFun "test" ("cav" `isPrefixOf`) (many' letter_ascii)
               , verifyFun "value" (<= 43) decimal
               ]

{-
 - Tests
 -}
sigs = testGroup "Signatures" [ basic
                              , one
                              , two
                              ]

basic = testGroup "Basic Macaroon" [ none , sigQC ]

none = testCase "No caveat" $
    Ok @=? verifySig sec m

sigQC = testProperty "Random" $
    \sm -> verifySig (secret sm) (macaroon sm) == Ok

one = testCase "Macaroon with one caveat" $
    Ok @=? verifySig sec m2

two = testCase "Macaroon with two caveats" $
    Ok @=? verifySig sec m3

exactCavs = testGroup "Exact Caveats" [
    testGroup "Ignoring non-relevant" [
        testCase "Zero caveat" $ Ok @=? verifyCavs exVerifiers m
      , testCase "One caveat" $ Ok @=? verifyCavs exVerifiers' m2
      ]
  , testCase "One caveat win" $ Ok @=? verifyCavs exVerifiers m2
  , testCase "Two caveat win" $ Ok @=? verifyCavs exVerifiers m3
  , testCase "Two caveat fail" $ Failed @=? verifyCavs exVerifiers' m3
  ]

funCavs = testGroup "Function Caveats" [
    testCase "One caveat win" $ Ok @=? verifyCavs funVerifiers m2
  , testCase "Two caveat win" $ Ok @=? verifyCavs funVerifiers m3
  ]
