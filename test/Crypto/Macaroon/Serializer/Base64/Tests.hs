{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr


This test suite is based on the pymacaroons test suite:
<https://github.com/ecordell/pymacaroons>
-}
module Crypto.Macaroon.Serializer.Base64.Tests where


import qualified Data.ByteString.Char8 as B8
import Test.Tasty
import Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Crypto.Macaroon
import           Crypto.Macaroon.Serializer.Base64

import Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Serializer.Base64" [ basic
                                                      , basicQC
                                                      , minted
                                                      , minted2
                                                      -- , minted3
                                                      ]

basicQC = testProperty "Reversibility" $
    forAll (macaroon <$> arbitrary) (\m -> deserialize (serialize m) == Right m)

m :: Macaroon
m = create secret key loc
  where
    secret = B8.pack "this is our super secret key; only we should know it"
    key = B8.pack "we used our secret key"
    loc = B8.pack "http://mybank/"

basic :: TestTree
basic = testGroup "Basic macaroon" [ basicSerialize
                                   , basicDeserialize
                                   ]

basicSerialize = testCase "Serialization" $
    "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudG\
    \lmaWVyIHdlIHVzZWQgb3VyIHNlY3JldCBrZXkKMDAyZnNpZ25h\
    \dHVyZSDj2eApCFJsTAA5rhURQRXZf91ovyujebNCqvD2F9BVLwo" @=? serialize m

basicDeserialize = testCase "Deserialization" $
    Right m @=? (deserialize . serialize) m

m2 :: Macaroon
m2 = addFirstPartyCaveat "test = caveat" m

minted :: TestTree
minted = testGroup "Macaroon with first party caveat" [ mintSerialize
                                                      , mintDeserialize
                                                      ]


mintSerialize = testCase "Serialization" $
    "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudGlmaWVyIHdlIHVzZ\
    \WQgb3VyIHNlY3JldCBrZXkKMDAxNmNpZCB0ZXN0ID0gY2F2ZWF0CjAwMmZzaWduYXR1cmUgGXusegR\
    \K8zMyhluSZuJtSTvdZopmDkTYjOGpmMI9vWcK" @=? serialize m2

mintDeserialize = testCase "Deserialization" $
    Right m2 @=? (deserialize . serialize) m2


m3 :: Macaroon
m3 = addFirstPartyCaveat "test = acaveat" m

minted2 :: TestTree
minted2 = testGroup "Macaroon with first party caveats" [ mint2Trimmed
                                                        , mint2Des
                                                        ]

mint2Trimmed = testCase "Serialization" $
    "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudGlmaWVyIHdlIHVz\
    \ZWQgb3VyIHNlY3JldCBrZXkKMDAxN2NpZCB0ZXN0ID0gYWNhdmVhdAowMDJmc2ln\
    \bmF0dXJlIJRJ_V3WNJQnqlVq5eez7spnltwU_AXs8NIRY739sHooCg" @=? serialize m3

mint2Des = testCase "Deserialization" $
    Right m3 @=? (deserialize . serialize) m3

