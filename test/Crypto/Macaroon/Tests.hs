{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr


This test suite is based on the pymacaroons test suite:
<https://github.com/ecordell/pymacaroons>
-}
module Crypto.Macaroon.Tests where

import           Data.Byteable
import qualified Data.ByteString.Char8 as B8
import           Data.Hex
import           Test.Tasty
import           Test.Tasty.HUnit

import           Crypto.Macaroon
import           Crypto.Macaroon.Serializer.Base64

tests :: TestTree
tests = testGroup "Crypto.Macaroon" [ basic
                                    , minted
                                    , minted2
                                    , minted3
                                    ]


m :: Macaroon
m = create secret key loc
  where
    secret = B8.pack "this is our super secret key; only we should know it"
    key = B8.pack "we used our secret key"
    loc = B8.pack "http://mybank/"

basic :: TestTree
basic = testGroup "Basic macaroon" [ basicSignature
                                   , basicSerialize
                                   , basicDeserialize
                                   ]

basicSignature = testCase "Signature" $
    "E3D9E02908526C4C0039AE15114115D97FDD68BF2BA379B342AAF0F617D0552F" @=? (hex . signature) m

basicSerialize = testCase "Serialization" $
    "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudG\
    \lmaWVyIHdlIHVzZWQgb3VyIHNlY3JldCBrZXkKMDAyZnNpZ25h\
    \dHVyZSDj2eApCFJsTAA5rhURQRXZf91ovyujebNCqvD2F9BVLwo" @=? serialize m

basicDeserialize = testCase "Deserialization" $
    Right m @=? (deserialize . serialize) m

m2 :: Macaroon
m2 = addFirstPartyCaveat "test = caveat" m

minted :: TestTree
minted = testGroup "Macaroon with first party caveat" [ mintInspect
                                                      , mintSerialize
                                                      , mintDeserialize
                                                      ]

mintInspect = testCase "Inspect" $
    "location http://mybank/\nidentifier we used\
    \ our secret key\ncid test = caveat\nsignature\
    \ 197BAC7A044AF33332865B9266E26D49\
    \3BDD668A660E44D88CE1A998C23DBD67" @=? inspect m2


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

m4 :: Macaroon
m4 = addThirdPartyCaveat caveat_key caveat_id caveat_loc n
  where
    n = addFirstPartyCaveat "account = 3735928559" $ create sec key loc
    key = B8.pack "we used our other secret key"
    loc = B8.pack "http://mybank/"
    sec = B8.pack "this is a different super-secret key; never use the same secret twice"
    caveat_key = B8.pack "4; guaranteed random by a fair toss of the dice"
    caveat_id = B8.pack "this was how we remind auth of key/pred"
    caveat_loc = B8.pack "http://auth.mybank/"

minted3 :: TestTree
minted3 = testGroup "Macaroon with first and third party caveats" [ mint3sig ]


mint3sig = testCase "Signature" $
    "6B99EDB2EC6D7A4382071D7D41A0BF7DFA27D87D2F9FEA86E330D7850FFDA2B2" @=? (hex . signature) m4
