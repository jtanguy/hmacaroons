{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr


This test suite is based on the pymacaroons test suite:
<https://github.com/ecordell/pymacaroons>
-}
module Crypto.Macaroon.Serializer.Binary.V2.Tests where


import qualified Data.ByteString.Char8 as B8
import Test.Tasty
import Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Crypto.Macaroon
import           Crypto.Macaroon.Serializer.Binary.V2

import Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Serializer.Binary.V2" [ basic
                                                      , minted
                                                      , minted2
                                                      -- , minted3
                                                      ]

basicQC = testProperty "Reversibility" $
    \sm -> deserialize (serialize (macaroon sm)) == Right (macaroon sm)

m :: Macaroon
m = create secret key loc
  where
    secret = B8.pack "this is the key"
    key = B8.pack "keyid"
    loc = B8.pack "http://example.org/"

basic :: TestTree
basic = testGroup "Basic macaroon" [ basicSerialize
                                   , basicDeserialize
                                   , basicQC
                                   ]

basicSerialize = testCase "Serialization" $
    "AgETaHR0cDovL2V4YW1wbGUub3JnLwIFa2V5aWQAAA\
    \YgfN7nklEcW8b1KEhYBd_psk54XijiqZMB-dcRxgnjjvc" @=? serialize m

basicDeserialize = testCase "Deserialization" $
    Right m @=? (deserialize . serialize) m

m2 :: Macaroon
m2 = addFirstPartyCaveat "account = 3735928559" m

minted :: TestTree
minted = testGroup "Macaroon with first party caveat" [ mintSerialize
                                                      , mintDeserialize
                                                      ]


mintSerialize = testCase "Serialization" $
    "AgETaHR0cDovL2V4YW1wbGUub3JnLwIFa2V5aWQAAh\
    \RhY2NvdW50ID0gMzczNTkyODU1OQAABiD1SAf23G7f\
    \iL8PcwazgiVio2JTPb9zObphdl2kvSWdhw" @=? serialize m2

mintDeserialize = testCase "Deserialization" $
    Right m2 @=? (deserialize . serialize) m2


m3 :: Macaroon
m3 = addFirstPartyCaveat "user = alice" m2

minted2 :: TestTree
minted2 = testGroup "Macaroon with first party caveats" [ mint2Trimmed
                                                        , mint2Des
                                                        ]

mint2Trimmed = testCase "Serialization" $
    "AgETaHR0cDovL2V4YW1wbGUub3JnLwIFa2V5aWQAAh\
    \RhY2NvdW50ID0gMzczNTkyODU1OQACDHVzZXIgPSBh\
    \bGljZQAABiBL6WfNHqDGsmuvakqU7psFsViG2guoXoxCqTyNDhJe_A" @=? serialize m3

mint2Des = testCase "Deserialization" $
    Right m3 @=? (deserialize . serialize) m3

