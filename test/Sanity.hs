{-# LANGUAGE OverloadedStrings #-}
module Sanity where

import           Crypto.Hash
import           Data.Byteable
import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as B
import           Data.Hex

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Python HMAC Sanity check" [ checkKey
                                             , checkMac1
                                             , checkMac2
                                             , checkMac3
                                             , checkMac4
                                             ]


secret :: ByteString
secret = "this is our super secret key; only we should know it"

public :: ByteString
public = "we used our secret key"

key :: ByteString
key = B.take 32 secret

mac1 :: ByteString
mac1 = toBytes (hmac key public :: HMAC SHA256)

mac2 :: ByteString
mac2 = toBytes (hmac mac1 "account = 3735928559" :: HMAC SHA256)

mac3 :: ByteString
mac3 = toBytes (hmac mac2 "time < 2015-01-01T00:00" :: HMAC SHA256)

mac4 :: ByteString
mac4 = toBytes (hmac mac3 "email = alice@example.org" :: HMAC SHA256)


checkKey = testCase "Truncated key" $
    key @?= "this is our super secret key; on"

checkMac1 = testCase "HMAC key" $
    "C60B4B3540BB1B2F2EF28D1C895691CC4A5E07A38A9D3B1C3379FB485293372F" @=? hex mac1

checkMac2 = testCase "HMAC key account" $
    "5C933DC9A7D036DFCD1740B4F26D737397A1FF635EAC900F3226973503CAAAA5" @=? hex mac2

checkMac3 = testCase "HMAC key account time" $
    "7A559B20C8B607009EBCE138C200585E9D0DECA6D23B3EAD6C5E0BA6861D3858" @=? hex mac3

checkMac4 = testCase "HMAC key account time email" $
    "E42BBB02A9A5A303483CB6295C497AE51AD1D5CB10003CBE548D907E7E62F5E4" @=? hex mac4
