{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr


This test suite is based on the pymacaroons test suite:
<https://github.com/ecordell/pymacaroons>
-}
module Crypto.Macaroon.Instances where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import           Data.List
import           Test.Tasty.QuickCheck

import           Crypto.Macaroon

newtype Url = Url { unUrl :: BS.ByteString } deriving (Show)

instance Arbitrary Url where
    arbitrary = do
        protocol <- elements ["http://"]
        name <-  fmap (intercalate ".") <$> listOf1 . listOf1 $ elements ['a'..'z']
        domain <- elements [".com",".net"]
        return . Url . B8.pack $ (protocol ++ name ++ domain)

newtype BSSecret = BSSecret { unSecret :: BS.ByteString } deriving (Show)

instance Arbitrary BSSecret where
    arbitrary = BSSecret . B8.pack <$> scale (*3) arbitrary

newtype Identifier = Identifier { unIdent :: BS.ByteString } deriving (Show)

instance Arbitrary Identifier where
    arbitrary = Identifier . B8.pack <$>(scale (*3) . listOf1 . elements $ ['a'..'z'])

newtype EquationLike = EquationLike { unEqlike :: BS.ByteString } deriving (Show)

instance Arbitrary EquationLike where
    arbitrary = do
        keylen <- choose (3,8)
        key <- B8.pack <$> vectorOf keylen (elements ['a'..'z'])
        val <- B8.pack <$> (scale (*3) . listOf1 . elements $ ['a'..'z'])
        return $ EquationLike (BS.concat [ key, " = ", val])


data SimpleMac = SimpleMac { secret :: BS.ByteString, macaroon :: Macaroon } deriving Show

instance Arbitrary SimpleMac where
    arbitrary = do
        secret <- unSecret <$> arbitrary
        location <- unUrl <$> arbitrary
        ident <- unIdent <$> arbitrary
        fpcavs <- listOf arbitrary
        let mac = foldl (flip addFirstPartyCaveat) (create secret ident location) (map unEqlike fpcavs)
        return $ SimpleMac secret mac


