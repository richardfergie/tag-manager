{-# LANGUAGE OverloadedStrings #-}
module JSServer.App (app) where

import Network.Wai

import Redis.Tags
import qualified Database.Redis as R

import Control.Monad.Reader

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI

import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import Network.HTTP.Types
import Blaze.ByteString.Builder.ByteString (fromByteString)

import Data.List (sortBy, nubBy)

import Network.URL
import qualified Network.Curl as Curl

import Data.Time.Clock
import Data.Time.Calendar

type RedisConnReader = ReaderT R.Connection

app :: (MonadIO m) => Request -> RedisConnReader m Response
app req 
  | null (pathInfo req) = return $ ResponseBuilder status200 [] $ fromByteString $ encodeUtf8 "Unable to determine client" --no path in request error
  | otherwise = do
     conn <- ask
     t <- liftIO $ R.runRedis conn $ getTagsForClient $ head $ pathInfo req
     case t of
       Left err -> return $ ResponseBuilder status200 [] $ fromByteString $ encodeUtf8 err
       Right alltags -> do
         filteredtags <- returnTags req alltags
         return $ ResponseBuilder status200 [("Content-type", "text/javascript")] $ 
           fromByteString $ encodeUtf8 filteredtags
          
returnTags :: (MonadIO m) => Request -> [Tag] -> RedisConnReader m T.Text
returnTags request tags = case (clientUri request) of
  Nothing -> return "Unable to parse referer string. We don't know what tag to serve"
  Just uri -> do
    let variantlist = (concatMap (tagForUri uri) tags)::[Variant]
    c <- sequence $ map renderContent $ removeDuplicateExternals $ concatMap content variantlist
    return $ T.intercalate "\r\n" c
          
clientUri :: Request -> Maybe T.Text
clientUri req = do
  ref <- lookup (CI.mk "referer") (requestHeaders req)
  url <- importURL $ C.unpack ref
  --need initial "/" added manually
  return $ T.pack $ "/"++(url_path url)++(exportParams $ url_params url)

tagForUri :: T.Text -> Tag -> [Variant]
tagForUri uri tag = case validVariants of
  [] -> []
  vs -> [last $ sortBy (\x y -> compare (priority x) (priority y)) $ vs] {- 
    --returns variant with highest priority
    let var = 
    return var
    tagtext <- sequence $ map renderContent $ content var    
    return $ T.concat $ tagtext -}
  where validVariants = filter (variationMatch uri) $ variants tag

--take a URI and variant
--return if it is a valid match or not
variationMatch :: T.Text -> Variant -> Bool
variationMatch uri v = case (condition v) of
  All -> True
  StartsWith x -> T.isPrefixOf x uri
  Contains x -> T.isInfixOf x uri
  Exact x -> x==uri
  
getExternal :: (MonadIO m) => T.Text -> RedisConnReader m T.Text
getExternal url = do
  conn <- ask
  d <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime 
  let date = T.pack $ (\(year,month,day)->(show year)++(show month)++(show day)) d
  cache <- liftIO $ R.runRedis conn $ R.get $ encodeUtf8 $ T.concat ["cache/", date, "/", url]
  case cache of
    Right (Just val) -> return $ decodeUtf8With lenientDecode val
    _ -> do  
      (resp, res) <- liftIO $ Curl.curlGetString (T.unpack url) []
      case resp of
        Curl.CurlOK -> do
          _ <- liftIO $ R.runRedis conn $ R.set (encodeUtf8 $ T.concat ["cache/", date, "/", url]) (C.pack res)
          return (T.pack res)
        _ -> return $ T.append "Unable to retrieve external dependency: " url

renderContent :: (MonadIO m) => Content -> RedisConnReader m T.Text
renderContent (Inline t) = return t
renderContent (External url) = getExternal url

removeDuplicateExternals :: [Content] -> [Content]
removeDuplicateExternals contentlist = nubBy dedupeExternal contentlist  
   where dedupeExternal (External x) (External y) = (x == y)
         dedupeExternal _ _ = False