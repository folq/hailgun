module Mail.Hailgun.Communication
    ( getRequest
    , postRequest
    , toQueryParams
    , parseResponse
    ) where

import           Control.Arrow                         (second)
import           Control.Monad.Catch                   (MonadThrow (..))
import           Control.Monad.IO.Class                (MonadIO (..))
import           Data.Aeson
import qualified Data.ByteString.Char8                 as BC
import qualified Data.ByteString.Lazy.Char8            as BLC
import qualified Data.Text                             as T
import           Mail.Hailgun.Errors
import           Mail.Hailgun.Internal.Data
import           Network.HTTP.Client                   (Proxy (..),
                                                        Request (..),
                                                        Response (..),
                                                        applyBasicAuth,
                                                        parseUrl, responseBody,
                                                        responseStatus,
                                                        setQueryString)
import           Network.HTTP.Client.Internal          (addProxy)
import           Network.HTTP.Client.MultipartFormData (Part (..), formDataBody,
                                                        partBS)
import qualified Network.HTTP.Types.Method             as NM
import qualified Network.HTTP.Types.Status             as NT

toQueryParams :: [(BC.ByteString, BC.ByteString)] -> [(BC.ByteString, Maybe BC.ByteString)]
toQueryParams = fmap (second Just)

getRequest :: (MonadThrow m) => String -> HailgunContext -> [(BC.ByteString, Maybe BC.ByteString)] -> m Request
getRequest url context queryParams = do
   initRequest <- parseUrl url
   let request = applyHailgunAuth context $ initRequest { method = NM.methodGet, checkStatus = ignoreStatus }
   return $ setQueryString queryParams request

postRequest :: (MonadThrow m, MonadIO m) => String -> HailgunContext -> [(BC.ByteString, BC.ByteString)] -> m Request
postRequest url context formParams = do
   initRequest <- parseUrl url
   let request = initRequest { method = NM.methodPost, checkStatus = ignoreStatus }
   requestWithBody <- encodeFormData formParams request
   return $ applyHailgunAuth context requestWithBody

encodeFormData :: MonadIO m => [(BC.ByteString, BC.ByteString)] -> Request -> m Request
encodeFormData fields = formDataBody (map toPart fields)
   where
      toPart :: (BC.ByteString, BC.ByteString) -> Part
      toPart (name, content) = partBS (T.pack . BC.unpack $ name) content

applyHailgunAuth :: HailgunContext -> Request -> Request
applyHailgunAuth context = addRequestProxy (hailgunProxy context) . authRequest
   where
      addRequestProxy :: Maybe Proxy -> Request -> Request
      addRequestProxy (Just proxy) = addProxy (proxyHost proxy) (proxyPort proxy)
      addRequestProxy _ = id

      authRequest = applyBasicAuth (BC.pack "api") (BC.pack . hailgunApiKey $ context)

parseResponse :: (FromJSON a) => Response BLC.ByteString -> Either HailgunErrorResponse a
parseResponse response = statusToResponse . NT.statusCode . responseStatus $ response
   where
      statusToResponse s
         | s == 200                      = responseDecode response
         | s `elem` [400, 401, 402, 404] = gatherErrors . responseDecode $ response
         | s `elem` [500, 502, 503, 504] = serverError
         | otherwise                     = unexpectedError s

responseDecode :: (FromJSON a) => Response BLC.ByteString -> Either HailgunErrorResponse a
responseDecode = mapError . eitherDecode . responseBody

ignoreStatus :: a -> b -> c -> Maybe d
ignoreStatus _ _ _ = Nothing
