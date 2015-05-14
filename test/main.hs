module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Sanity
import qualified Crypto.Macaroon.Tests
import qualified Crypto.Macaroon.Serializer.Base64.Tests
import qualified Crypto.Macaroon.Verifier.Tests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ Sanity.tests
                          , Crypto.Macaroon.Tests.tests
                          , Crypto.Macaroon.Serializer.Base64.Tests.tests
                          , Crypto.Macaroon.Verifier.Tests.tests
                          ]

