{-# LANGUAGE OverloadedStrings #-}
module Tests.Wai where

import Network.Wai
import Network.Wai.Test
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()

import qualified Database.Redis as R
import Control.Monad.Reader

import Redis.Tags
import JSServer.App

waiTests :: IO ()
waiTests = do
  conn <- R.connect R.defaultConnectInfo{R.connectHost="127.0.0.1"}
  R.runRedis conn $! R.select 1 --don't crap all over default redis db
  R.runRedis conn $! R.flushdb
  R.runRedis conn $! setTagsForClient "1" sampleTags
  hspecX $ testList conn

  
testApp :: R.Connection -> Application
testApp conn rq = runReaderT (app rq) conn

wApp :: R.Connection -> Session a -> IO a
wApp conn = flip runSession $ testApp conn
  
testList :: R.Connection -> Specs
testList conn = do
  describe "Basic checks of js server" $ do
    it "Get /" $ wApp conn $ do
      rs <- request defaultRequest 
      assertStatus 200 rs
      assertBody "Unable to determine client" rs
    it "Get /1 with no referer" $ wApp conn $ do  
      rs <- request rq1
      assertStatus 200 rs
      assertBody "Unable to parse referer string. We don't know what tag to serve" rs
    it "Get /2" $ wApp conn $ do  
      let rq = setRawPathInfo defaultRequest "2"
      rs <- request rq
      assertStatus 200 rs
      assertBody "Cannot find tags in database" rs
    it "Get /1 with random ref (All tag)" $ wApp conn $ do  
      rs <- request $ addRef "http://www.example.com/dkhgf2gvqcyhyfc" rq1
      assertStatus 200 rs
      assertBody "Default Tag" rs
    it "Check Exact tag" $ wApp conn $ do
      rs <- request $ addRef "http://www.example.com/checkout" rq1
      assertStatus 200 rs
      assertBody "Tag 1" rs
    it "Check StartsWith tag" $ wApp conn $ do
      rs <- request $ addRef "http://www.example.com/checkout/foo" rq1
      assertStatus 200 rs
      assertBody "Tag 2" rs  
    it "Check Contains tag" $ wApp conn $ do
      rs <- request $ addRef "http://www.example.com/foo/checkout/bar" rq1
      assertStatus 200 rs
      assertBody "Tag 3" rs  
    it "Check multi tag" $ wApp conn $ do
      rs <- request $ addRef "http://www.example.com/foo" rq1
      assertStatus 200 rs
      assertBody "Default Tag\r\nTag 2.1" rs

      
addRef ref rq = rq{requestHeaders=(requestHeaders rq)++[("referer",ref)]}
     
rq1 = setRawPathInfo defaultRequest "1"
                    
                    
sampleTags :: [Tag]                
sampleTags = [  
  Tag "Tag number 1" 
      [Variant "Tag 0" 0 All [(Inline "Default Tag")],
       Variant "Tag 1" 3 (Exact "/checkout") [(Inline "Tag 1")],
       Variant "Tag 2" 2 (StartsWith "/checkout") [(Inline "Tag 2")],
       Variant "Tag 3" 1 (Contains "/checkout") [(Inline "Tag 3")]
      ],
  Tag "Tag number 2"
      [Variant "Tag 2.1" 0 (Exact "/foo") [Inline "Tag 2.1"]
       ]
  ]