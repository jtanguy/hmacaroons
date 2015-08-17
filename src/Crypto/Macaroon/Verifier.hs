{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Crypto.Macaroon.Verifier
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Crypto.Macaroon.Verifier (
    verify
  , ValidationError(ValidatorError, ParseError)
  -- , (.<), (.<=), (.==), (.>), (.>=)
  -- , module Data.Attoparsec.ByteString.Char8
) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import           Data.Bool
import qualified Data.ByteString                  as BS
import           Data.Either.Combinators

import           Crypto.Macaroon.Internal
import           Crypto.Macaroon.Verifier.Internal




-- (.<) :: (MonadIO m, Ord a, Parsable a) => Key -> m a -> Caveat -> m (Maybe (Either ValidationError Caveat))
-- (.<) = verifyOpBool "Greater or equal" (<) "<"

-- (.<=) :: (MonadIO m, Ord a, Parsable a) => Key -> m a -> Caveat -> m (Maybe (Either ValidationError Caveat))
-- (.<=) = verifyOpBool "Strictly greater" (<=) "<="

-- (.==) :: (MonadIO m, Eq a, Parsable a) => Key -> m a -> Caveat -> m (Maybe (Either ValidationError Caveat))
-- (.==) = verifyOpBool "Not equal" (==) "="

-- (.>) :: (MonadIO m, Ord a, Parsable a) => Key -> m a -> Caveat -> m (Maybe (Either ValidationError Caveat))
-- (.>) = verifyOpBool "Less or equal" (>) ">"

-- (.>=) :: (MonadIO m, Ord a, Parsable a) => Key -> m a -> Caveat -> m (Maybe (Either ValidationError Caveat))
-- (.>=) = verifyOpBool "Strictly less" (>=) ">="

-- | Verify a Macaroon's signature and caveats, given the corresponding Secret
-- and verifiers.
--
-- A verifier is a function of type
-- @'MonadIO' m => 'Caveat' -> m ('Maybe' ('Either' 'ValidatorError' 'Caveat'))@.
--
-- It should return:
--
-- * 'Nothing' if the caveat is not related to the verifier
-- (for instance a time verifier is given an action caveat);
-- * 'Just' ('Left' ('ParseError' reason)) if the verifier  is related to the
-- caveat, but failed to parse it completely;
-- * 'Just' ('Left' ('ValidatorError' reason)) if the verifier is related to the
-- caveat, parsed it and invalidated it;
-- * 'Just' ('Right' '()') if the verifier has successfully verified the
-- given caveat
verify :: MonadIO m => Secret -> [Caveat -> m (Maybe (Either ValidationError ()))] -> Macaroon -> m (Either ValidationError Macaroon)
verify secret verifiers m = join <$> forM (verifySig secret m) (verifyCavs verifiers)


-- verifyOpBool :: MonadIO m => String -> Parser a -> (a -> a -> Bool) -> BS.ByteString -> Key -> m a -> Caveat -> m (Maybe (Either ValidationError Caveat))
-- verifyOpBool err p f op k val = verifyParser k valueParser $ \s -> do
--     expected <- val
--     return $ bool (Left $ ValidatorError err) (Right Win) =<< f expected <$> mapLeft ParseError (parseOnly p s)
--   where
--     valueParser = string op *> skipSpace *> takeByteString

-- verifyParser :: (MonadIO m) => Key -> Parser a -> (a -> m (Either ValidationError Win)) -> Caveat -> m (Maybe (Either ValidationError Caveat))
-- verifyParser k p f c = case parseOnly keyParser . cid $ c of
--     Left _ -> return Nothing
--     Right bs -> Just <$> case parseOnly p bs of
--       Left err -> return $ Left $ ParseError err
--       Right a -> fmap (const c) <$> f a
--   where
--     keyParser = string k *> skipSpace *> takeByteString

