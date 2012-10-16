{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Redis.Tags (
   Tag(..)
  ,Condition(..)
  ,Content(..) 
  ,Variant(..) 
  ,getTagsForClient 
  ,setTagsForClient 
  ,textToTags 
  ) where

import Prelude
import Data.Aeson
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import qualified Database.Redis as R
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Text.Encoding

data Tag = Tag {
    tagName :: T.Text
  , variants :: [Variant]
  } deriving (Show, Read, Eq)

data Condition = StartsWith T.Text | Contains T.Text | Exact T.Text | All deriving (Show, Read, Eq)

data Content = External T.Text | Inline T.Text deriving (Show, Read, Eq)

data Variant = Variant {
    variantName :: T.Text
  , priority :: Int
  , condition :: Condition
  , content :: [Content]
  } deriving (Show, Read, Eq)

instance ToJSON Condition where
  toJSON (StartsWith x) = object [ 
     "conditionType" .= ("StartsWith"::T.Text)
    ,"conditionValue" .= x
     ]
  toJSON (Contains x) = object [ 
     "conditionType" .= ("Contains"::T.Text)
    ,"conditionValue" .= x
     ]
  toJSON (Exact x) = object [ 
     "conditionType" .= ("Exact"::T.Text)
    ,"conditionValue" .= x
     ]
  toJSON All = object [ 
     "conditionType" .= ("All"::T.Text)
     ]
               
instance ToJSON Content where
  toJSON (External x) = object [
    "contentType" .= ("External"::T.Text)
    ,"contentValue" .= x
     ]
  toJSON (Inline x) = object [
    "contentType" .= ("Inline"::T.Text)
    ,"contentValue" .= x
     ]
  
instance ToJSON Variant where
  toJSON (Variant nm pr cond cont) = object [
                                      "variantName" .= nm
                                    , "priority" .= pr
                                    , "condition" .= cond 
                                    , "content" .= cont 
                                    ]

instance ToJSON Tag where
  toJSON (Tag n vs) = object [
                          "tagName" .= n
                        , "variants" .= vs
                        ]
                                 
instance FromJSON Condition where
  parseJSON (Object v) = do
    t <- v .: "conditionType"
    case t of
      ("StartsWith"::T.Text) -> StartsWith <$> v .: "conditionValue"
      ("Contains"::T.Text) -> Contains <$> v .: "conditionValue"
      ("Exact"::T.Text) -> Exact <$> v .: "conditionValue"
      ("All"::T.Text) -> return All
      _ -> mzero
  parseJSON _ = mzero
  
instance FromJSON Content where
  parseJSON (Object v) = do
    t <- v .: "contentType"
    case t of
      ("External"::T.Text) -> External <$> v .: "contentValue"
      ("Inline"::T.Text) -> Inline <$> v .: "contentValue"
      _ -> mzero
  parseJSON _ = mzero
  
instance FromJSON Variant where
  parseJSON (Object v) = Variant <$>
                           v .: "variantName" <*>
                           v .: "priority" <*>
                           v .: "condition" <*>
                           v .: "content"
  parseJSON _ = mzero
                          
instance FromJSON Tag where                          
  parseJSON (Object t) = Tag <$>
                           t .: "tagName" <*>
                           t .: "variants"
  parseJSON _ = mzero

--Redis stuff
parseToTags :: B.ByteString -> Maybe [Tag]
parseToTags b = decode $ LB.fromChunks [b]

textToTags :: T.Text -> Maybe [Tag]
textToTags = parseToTags . encodeUtf8

getTagsForClient :: T.Text -> R.Redis (Either T.Text [Tag])
getTagsForClient clientid = do
  val <- R.get (encodeUtf8 $ T.append "clients/" clientid)
  case val of
    Left _ -> return $ Left "Error retrieving tags from database"
    Right Nothing -> return $ Left "Cannot find tags in database"
    Right (Just t) -> case parseToTags t of
      Nothing -> return $ Left "Error parsing values returned from database"
      Just tags -> return $ Right tags
  
setTagsForClient :: T.Text -> [Tag] -> R.Redis (Either T.Text ())
setTagsForClient clientid tag = do
  let key = encodeUtf8 $ T.append "clients/" clientid
  k <- R.set key (B.concat $ LB.toChunks $ encode tag)
  case k of
    Left _ -> return $ Left "Error setting key in database"
    Right R.Ok -> return $ Right ()
    Right _ -> return $ Left "Unexpected status returned by database"
  
