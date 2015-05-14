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

import           Crypto.Macaroon
import           Crypto.Macaroon.Verifier

import Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Verifier" [ sigs
                                             ]

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

sigs = testGroup "Signatures" [ basic
                              , minted
                              ]

basic = testCase "Basic Macaroon Signature" $
    Success @=? verifySig sec m


minted :: TestTree
minted = testGroup "Macaroon with first party caveats" [ one
                                                       , two
                                                       ]
one = testCase "One caveat" $
    Success @=? verifySig sec m2

two = testCase "Two caveats" $
    Success @=? verifySig sec m3

