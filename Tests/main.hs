module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified Test.QuickCheck.Monadic as QCM

import Test.HUnit

import Redis.Tags

import Data.Aeson
import qualified Database.Redis as R

import qualified Control.Exception as E
import System.Exit

import Tests.Wai

main = do
  tryTest waiTests
  tryTest $ defaultMain tests

isExitCodeException e = case E.fromException e of
  Nothing -> Nothing
  Just ExitSuccess -> Just ExitSuccess
  Just (ExitFailure x) -> Just $ ExitFailure x

tryTest t = E.catchJust (isExitCodeException) t (\e -> if (e==ExitSuccess) then return() else E.throw e)

tests = [
        testGroup "JSON Serialization of Tags" [
                testProperty "encode/decode (Pure)" prop_encodedecode,
                testProperty "get/set (Redis)" prop_getset
            ]
    ]

prop_encodedecode t = (decode $ encode t) == Just t 
  where types = (t :: [Tag])
        
prop_getset client tags = QCM.monadicIO $ do
  conn <- QCM.run $ R.connect R.defaultConnectInfo{R.connectHost="127.0.0.1"}
  _ <- QCM.run $ R.runRedis conn $ R.select 1
  _ <- QCM.run $ R.runRedis conn $ setTagsForClient client tags
  r <- QCM.run $ R.runRedis conn $ getTagsForClient client
  QCM.assert $ r == (Right tags)

instance Arbitrary Condition where
  arbitrary = do
    c <- arbitrary
    t <- elements [All, StartsWith c, Contains c, Exact c]
    return t
  
instance Arbitrary Content where
  arbitrary = do
    c <- arbitrary
    t <- elements [Inline c, External c]
    return t

instance Arbitrary Variant where
  arbitrary = do
    nm <- arbitrary
    p <- arbitrary
    cond <- arbitrary
    cont <- arbitrary
    return $ Variant nm p cond cont
    
instance Arbitrary Tag where
  arbitrary = do
    nm <- arbitrary
    vs <- arbitrary
    return $ Tag nm vs

