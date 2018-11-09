module Main where

import           Test.Tasty

import qualified Crypto.Macaroon.Serializer.Base64.Tests
import qualified Crypto.Macaroon.Tests
import qualified Crypto.Macaroon.Verifier.Internal.Tests
import qualified Crypto.Macaroon.Verifier.Tests
import qualified Sanity

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ Sanity.tests
                          , Crypto.Macaroon.Tests.tests
                          , Crypto.Macaroon.Serializer.Base64.Tests.tests
                          , Crypto.Macaroon.Verifier.Tests.tests
                          , Crypto.Macaroon.Verifier.Internal.Tests.tests
                          ]

