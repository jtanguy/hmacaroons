module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Crypto.Macaroon.Serializer.Binary.V1.Tests
import qualified Crypto.Macaroon.Serializer.Binary.V2.Tests
import qualified Crypto.Macaroon.Tests
import qualified Crypto.Macaroon.Verifier.Internal.Tests
import qualified Crypto.Macaroon.Verifier.Tests
import qualified Sanity

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ Sanity.tests
                          , Crypto.Macaroon.Tests.tests
                          , Crypto.Macaroon.Serializer.Binary.V1.Tests.tests
                          , Crypto.Macaroon.Serializer.Binary.V2.Tests.tests
                          , Crypto.Macaroon.Verifier.Tests.tests
                          , Crypto.Macaroon.Verifier.Internal.Tests.tests
                          ]

