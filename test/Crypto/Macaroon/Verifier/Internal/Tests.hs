{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr


This test suite is based on the pymacaroons test suite:
<https://github.com/ecordell/pymacaroons>
-}
module Crypto.Macaroon.Verifier.Internal.Tests where

import           Data.Bool
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Char8             as B8
import           Data.Either
import           Data.Either.Validation
import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck             hiding (Failure, Success)

import           Crypto.Macaroon
import           Crypto.Macaroon.Verifier.Internal

import           Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Verifier.Internal" [ sigs
                                                      , firstParty
                                                      , validationErrorMonoid
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

vtest :: Caveat -> IO VerifierResult
vtest c = return $ if "test" `BS.isPrefixOf` cid c then
    bool (Refused (ValidatorError "Failed")) Verified $ "test = caveat" == cid c
    else Unrelated


m3 :: Macaroon
m3 = addFirstPartyCaveat "value = 42" m2

vval :: Caveat -> IO VerifierResult
vval c = return $ if "value" `BS.isPrefixOf` cid c then
    bool (Refused (ValidatorError "Failed")) Verified $ "value = 42" == cid c
    else Unrelated


{-
 - Tests
 -}

sigs = testProperty "Signatures" $ \sm -> verifySig (secret sm) (macaroon sm) == Right (macaroon sm)


firstParty = testGroup "First party caveats" [
    testCase "Zero caveat" $ do
        res <- verifyCavs [] m :: IO (Either ValidationError Macaroon)
        Right m @=? res
    , testCase "One caveat empty" $ do
        res <- verifyCavs [] m2 :: IO (Either ValidationError Macaroon)
        Left NoVerifier @=? res
    , testCase "One caveat fail" $ do
        res <- verifyCavs [vval] m2 :: IO (Either ValidationError Macaroon)
        Left NoVerifier @=? res
    , testCase "One caveat win" $ do
        res <- verifyCavs [vtest] m2 :: IO (Either ValidationError Macaroon)
        Right m2 @=? res
    , testCase "Two caveat win" $ do
        res <- verifyCavs [vtest, vval] m3 :: IO (Either ValidationError Macaroon)
        Right m3 @=? res
    ]

instance Arbitrary ValidationError where
    arbitrary = oneof
        [ pure SigMismatch
        , pure NoVerifier
        , ParseError <$> arbitrary
        , ValidatorError <$> arbitrary
        ]

validationErrorMonoid = testGroup "Validation Error forms a monoid"
    [ testProperty "Identity (left)" leftId
    , testProperty "Identity (right)" rightId
    , testProperty "Associativity" associativity
    , testProperty "SigMismatch is absorbing (left)" leftAbsorbing
    , testProperty "SigMismatch is absorbing (right)" rightAbsorbing
    ]
  where
    leftId :: ValidationError -> Bool
    leftId ve = mempty `mappend` ve == ve
    rightId :: ValidationError -> Bool
    rightId ve = ve `mappend` mempty == ve
    associativity :: ValidationError -> ValidationError -> ValidationError -> Bool
    associativity a b c = mappend a (mappend b c) == mappend (mappend a b) c
    leftAbsorbing :: ValidationError -> Bool
    leftAbsorbing ve = SigMismatch `mappend` ve == SigMismatch
    rightAbsorbing :: ValidationError -> Bool
    rightAbsorbing ve = ve `mappend` SigMismatch == SigMismatch
