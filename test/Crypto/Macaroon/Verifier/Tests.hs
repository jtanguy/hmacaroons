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

exactCavs = testGroup "Exact Caveats" [ zero', one', two' , one'', two'']

zero' = testCase "Zero caveat win" $
    Ok @=? verifyCavs exVerifiers m
one' = testCase "One caveat win" $
    Ok @=? verifyCavs exVerifiers m2
one'' = testCase "Ignoring non-relevant" $
    Ok @=? verifyCavs exVerifiers' m2
two' = testCase "Two caveat win" $
    Ok @=? verifyCavs exVerifiers m3
two'' = testCase "Two caveat fail" $
    Failed @=? verifyCavs exVerifiers' m3
