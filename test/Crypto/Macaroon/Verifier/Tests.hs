{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr


This test suite is based on the pymacaroons test suite:
<https://github.com/ecordell/pymacaroons>
-}
module Crypto.Macaroon.Verifier.Tests where


import qualified Data.ByteString.Char8     as B8
import           Data.List
import           Test.Tasty
-- import Test.Tasty.HUnit
import           Data.Either
import           Test.Tasty.QuickCheck     hiding (Failure, Success)

import           Crypto.Macaroon
import           Crypto.Macaroon.Verifier

import           Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Verifier" [ ]

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

{-
 - Tests
 -}

-- TODO: Re-do tests
{-
firstParty = testGroup "First party caveats" [
    testGroup "Pure verifiers" [
        testProperty "Zero caveat" $
                forAll (sublistOf allvs) (\vs -> Right m == verifyCavs vs m)
      , testProperty "One caveat" $
          forAll (sublistOf allvs) (\vs -> disjoin [
              Right m2 == verifyCavs vs m2 .&&. any (`elem` vs) [exTC,funTCPre] .&&. (exTZ `notElem` vs)
            , True === isLeft( verifyCavs vs m2)
            ])
      , testProperty "Two Exact" $
          forAll (sublistOf allvs) (\vs -> disjoin [
              Right m3 == verifyCavs vs m3 .&&.
                any (`elem` vs) [exTC,funTCPre] .&&.  (exTZ `notElem` vs) .&&.
                any (`elem` vs) [exV42,funTV43lte] .&&.  (exV43 `notElem` vs)
            , True === isLeft (verifyCavs vs m3)
            ])
      ]
    , testGroup "Pure verifiers with sig" [
        testProperty "Zero caveat" $
                forAll (sublistOf allvs) (\vs -> Right m == verifyMacaroon sec vs m)
      , testProperty "One caveat" $
          forAll (sublistOf allvs) (\vs -> disjoin [
              Right m2 == verifyMacaroon sec vs m2 .&&. any (`elem` vs) [exTC,funTCPre] .&&. (exTZ `notElem` vs)
            , True === isLeft (verifyMacaroon sec vs m2)
            ])
      , testProperty "Two Exact" $
          forAll (sublistOf allvs) (\vs -> disjoin [
              Right m3 == verifyMacaroon sec vs m3 .&&.
                any (`elem` vs) [exTC,funTCPre] .&&.  (exTZ `notElem` vs) .&&.
                any (`elem` vs) [exV42,funTV43lte] .&&.  (exV43 `notElem` vs)
            , True === isLeft (verifyMacaroon sec vs m3)
            ])
      ]
    ]
    -}
