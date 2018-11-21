{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
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
import           Data.List.NonEmpty                (NonEmpty (..))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck             hiding (Failure, Success)

import           Crypto.Macaroon
import           Crypto.Macaroon.Verifier.Internal

import           Crypto.Macaroon.Instances

tests :: TestTree
tests = testGroup "Crypto.Macaroon.Verifier.Internal" [ sigs
                                                      , firstParty
                                                      ]

{-
 - Test fixtures
 -}
sec = B8.pack "this is our super secret key; only we should know it"

type VerifierResult' = VerifierResult String String
type VerifierError' = VerifierError String String
type ValidationError' = ValidationError String String

m :: Macaroon
m = create sec key loc
  where
    key = B8.pack "we used our sec key"
    loc = B8.pack "http://mybank/"

m2 :: Macaroon
m2 = addFirstPartyCaveat "test = caveat" m

vtest :: Caveat -> IO VerifierResult'
vtest c = return $ if "test" `BS.isPrefixOf` cid c then
    bool (Refused (VerifierError "Failed")) Verified $ "test = caveat" == cid c
    else Unrelated


m3 :: Macaroon
m3 = addFirstPartyCaveat "value = 42" m2

vval :: Caveat -> IO VerifierResult'
vval c = return $ if "value" `BS.isPrefixOf` cid c then
    bool (Refused (VerifierError "Failed")) Verified $ "value = 42" == cid c
    else Unrelated


{-
 - Tests
 -}

sigs = testProperty "Signatures" $ \sm -> verifySig @String @String (secret sm) (macaroon sm) == Right (macaroon sm)

getCids :: Either ValidationError' a -> Maybe (NonEmpty (Key, [VerifierError']))
getCids res =
    let errors = either getCaveats (const Nothing)
        getCid (cav, errors) = (cid cav, errors)
        getCaveats (RemainingCaveats es) = Just $ fmap getCid es
        getCaveats _                     = Nothing
    in errors res

type CavResult = IO (Either ValidationError' ())

firstParty = testGroup "First party caveats" [
    testCase "Zero caveat" $ do
        res <- verifyCavs [] (caveats m) :: CavResult
        Right () @=? res
    , testCase "One caveat empty" $ do
        res <- verifyCavs [] (caveats m2) :: CavResult
        Just (("test = caveat",  []) :| [])@=? getCids res
    , testCase "One caveat fail" $ do
        res <- verifyCavs [vval] (caveats m2) :: CavResult
        Just (("test = caveat",  []) :| [])@=? getCids res
    , testCase "One caveat win" $ do
        res <- verifyCavs [vtest] (caveats m2) :: CavResult
        Right () @=? res
    , testCase "Two caveat win" $ do
        res <- verifyCavs [vtest, vval] (caveats m3) :: CavResult
        Right () @=? res
    ]

