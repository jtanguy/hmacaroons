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
-- import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import           Crypto.Macaroon
import           Crypto.Macaroon.Verifier

import Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Verifier" [ sigs
                                             , firstParty
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

exTC = verifyExact "test" "caveat" (many' letter_ascii) <???> "test = caveat"
exTZ = verifyExact "test" "bleh" (many' letter_ascii) <???> "test = bleh"
exV42 = verifyExact "value" 42 decimal <???> "value = 42"
exV43 = verifyExact "value" 43 decimal <???> "value = 43"

funTCPre = verifyFun "test" ("cav" `isPrefixOf`) (many' letter_ascii) <???> "test startsWith cav"
funTV43lte = verifyFun "value" (<= 43) decimal <???> "value <= 43"

allvs = [exTC, exTZ, exV42, exV43, funTCPre, funTV43lte]

{-
 - Tests
 -}
sigs = testProperty "Signatures" $ \sm -> verifySig (secret sm) (macaroon sm) == Ok

firstParty = testGroup "First party caveats" [
    testGroup "Pure verifiers" [ 
        testProperty "Zero caveat" $
                forAll (sublistOf allvs) (\vs -> Ok == verifyCavs vs m)
      , testProperty "One caveat" $
          forAll (sublistOf allvs) (\vs -> disjoin [
              Ok == verifyCavs vs m2 .&&. any (`elem` vs) [exTC,funTCPre] .&&. (exTZ `notElem` vs)
            , Failed === verifyCavs vs m2
            ])
      , testProperty "Two Exact" $
          forAll (sublistOf allvs) (\vs -> disjoin [
              Ok == verifyCavs vs m3 .&&.
                any (`elem` vs) [exTC,funTCPre] .&&.  (exTZ `notElem` vs) .&&.
                any (`elem` vs) [exV42,funTV43lte] .&&.  (exV43 `notElem` vs)
            , Failed === verifyCavs vs m3
            ])
      ]
    ]
