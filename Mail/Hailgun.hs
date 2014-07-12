module Mail.Hailgun 
   ( HailgunMessage
   , HailgunErrorMessage
   , MessageSubject
   , UnverifiedEmailAddress
   , MessageContent(..)
   , MessageRecipients(..)
   , emptyMessageRecipients
   , HailgunContext(..)
   , hailgunMessage
   , sendEmail
   , HailgunSendResponse(..)
   , HailgunErrorResponse(..)
   ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Text.Email.Validate
import Network.HTTP.Client (Request(..), RequestBody(..), parseUrl, httpLbs, withManager, defaultManagerSettings, responseStatus, responseBody, applyBasicAuth)
import Network.HTTP.Client.MultipartFormData (Part(..), formDataBody, partBS)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types.Status as NT
import qualified Network.HTTP.Types.Method as NM
import qualified Network.HTTP.Types.Header as NH

{- 
 - The basic rest API's look like this when used in curl:
 -
 - curl -s --user 'api:key-3ax6xnjp29jd6fds4gc373sgvjxteol0' \
 -     https://api.mailgun.net/v2/samples.mailgun.org/messages \
 -     -F from='Excited User <me@samples.mailgun.org>' \
 -     -F to=baz@example.com \
 -     -F to=bar@example.com \
 -     -F subject='Hello' \
 -     -F text='Testing some Mailgun awesomness!'
 -
 - This is what we need to emulate with this library.
 -}

type UnverifiedEmailAddress = B.ByteString
type MessageSubject = String
data MessageContent
   = TextOnly 
      { textContent :: B.ByteString
      }
   | TextAndHTML
      { textContent :: B.ByteString
      , htmlContent :: B.ByteString
      }

data HailgunMessage = HailgunMessage
   { messageSubject  :: MessageSubject
   , messageContent  :: MessageContent
   , messageFrom     :: EmailAddress
   , messageTo       :: [EmailAddress]
   , messageCC       :: [EmailAddress]
   , messageBCC      :: [EmailAddress]
   }
   -- TODO support sending attachments in the future
   -- TODO inline image support for the future
   -- TODO o:tag support
   -- TODO o:campaign support
   -- messageDKIMSupport :: Bool TODO o:dkim support
   -- TODO o:deliverytime support for up to three days in the future
   -- TODO o:testmode support
   -- TODO o:tracking support
   -- TODO o:tracking-clicks support
   -- TODO o:tracking-opens support
   -- TODO custom mime header support
   -- TODO custome message data support

emptyMessageRecipients :: MessageRecipients
emptyMessageRecipients = MessageRecipients [] [] []

data MessageRecipients = MessageRecipients 
   { recipientsTo    :: [UnverifiedEmailAddress]
   , recipientsCC    :: [UnverifiedEmailAddress]
   , recipientsBCC   :: [UnverifiedEmailAddress]
   }

type HailgunErrorMessage = String

hailgunMessage :: MessageSubject -> MessageContent -> UnverifiedEmailAddress -> MessageRecipients -> Either HailgunErrorMessage HailgunMessage
hailgunMessage subject content sender recipients = do
   from <- validate sender
   to <- mapM validate (recipientsTo recipients)
   cc <- mapM validate (recipientsCC recipients)
   bcc <- mapM validate (recipientsBCC recipients)
   return HailgunMessage 
      { messageSubject = subject
      , messageContent = content
      , messageFrom = from
      , messageTo = to
      , messageCC = cc
      , messageBCC = bcc
      }

toPostVars :: HailgunMessage -> [(BC.ByteString, BC.ByteString)]
toPostVars message = 
   [ (BC.pack "from", BC.pack . show . messageFrom $ message)
   , (BC.pack "subject", BC.pack $ messageSubject message)
   ] ++ to 
   ++ cc 
   ++ bcc 
   ++ fromContent (messageContent message)
   where
      to = convertEmails (BC.pack "to") . messageTo $ message
      cc = convertEmails (BC.pack "cc") . messageCC $ message
      bcc = convertEmails (BC.pack "bcc") . messageBCC $ message

      fromContent :: MessageContent -> [(BC.ByteString, BC.ByteString)]
      fromContent t@(TextOnly _) = [ (BC.pack "text", textContent t) ]
      fromContent th@(TextAndHTML {}) = (BC.pack "html", htmlContent th) : fromContent (TextOnly . textContent $ th)

      convertEmails :: BC.ByteString -> [EmailAddress] -> [(BC.ByteString, BC.ByteString)]
      convertEmails prefix = fmap ((,) prefix . BC.pack . show)

-- Use this method of the HTTP library to convert this into a Request body:
-- https://hackage.haskell.org/package/HTTP-4000.2.17/docs/Network-HTTP-Base.html#v:urlEncodeVars

data HailgunContext = HailgunContext
   { hailgunDomain :: String -- TODO better way to represent a domain
   , hailgunApiKey :: String
   }

data HailgunSendResponse = HailgunSendResponse
   { hsrMessage :: String
   , hsrId      :: String
   }

data HailgunErrorResponse = HailgunErrorResponse
   { herMessage :: String
   }

toHailgunError :: String -> HailgunErrorResponse
toHailgunError = HailgunErrorResponse

instance FromJSON HailgunSendResponse where
   parseJSON (Object v) = HailgunSendResponse
      <$> v .: T.pack "message"
      <*> v .: T.pack "id"
   parseJSON _ = mzero

instance FromJSON HailgunErrorResponse where
   parseJSON (Object v) = HailgunErrorResponse
      <$> v .: T.pack "message"
   parseJSON _ = mzero

encodeFormData :: MonadIO m => [(BC.ByteString, BC.ByteString)] -> Request -> m Request
encodeFormData fields = formDataBody (map toPart fields)
   where
      toPart :: (BC.ByteString, BC.ByteString) -> Part
      toPart (name, content) = partBS (T.pack . BC.unpack $ name) content

sendEmail :: HailgunContext -> HailgunMessage -> IO (Either HailgunErrorResponse HailgunSendResponse)
sendEmail context message = do
   initRequest <- parseUrl url
   let request = initRequest { method = NM.methodPost, checkStatus = \_ _ _ -> Nothing }
   requestWithBody <- encodeFormData (toPostVars message) request
   let authedRequest = applyBasicAuth (BC.pack "api") (BC.pack . hailgunApiKey $ context) requestWithBody
   putStrLn . show $ authedRequest
   response <- withManager tlsManagerSettings (httpLbs authedRequest)
   case responseStatus response of
      (NT.Status { NT.statusCode = 200 }) -> return . convertGood . eitherDecode' . responseBody $ response
      (NT.Status { NT.statusCode = 400 }) -> return . Left . convertBad . eitherDecode' . responseBody $ response
      (NT.Status { NT.statusCode = 401 }) -> return . Left . convertBad . eitherDecode' . responseBody $ response
      (NT.Status { NT.statusCode = 402 }) -> return . Left . convertBad . eitherDecode' . responseBody $ response
      (NT.Status { NT.statusCode = 404 }) -> return . Left . convertBad . eitherDecode' . responseBody $ response
      (NT.Status { NT.statusCode = 500 }) -> serverError
      (NT.Status { NT.statusCode = 502 }) -> serverError
      (NT.Status { NT.statusCode = 503 }) -> serverError
      (NT.Status { NT.statusCode = 504 }) -> serverError
      c         -> retError . unexpectedError $ c
   where
      url = "https://api.mailgun.net/v2/" ++ hailgunDomain context ++ "/messages"
      headers = [ (NH.hContentType, BC.pack contentType) ]
      contentType = "multipart/form-data"
      retError = return . Left . toHailgunError

      serverError = retError "Server Errors - something is wrong on Mailgun’s end"
      unexpectedError x = "Unexpected Non-Standard Mailgun Error: " ++ (show x) 
      toI (x, y, z) = x * 100 + y * 10 + z

convertGood :: Either String HailgunSendResponse -> Either HailgunErrorResponse HailgunSendResponse
convertGood (Left error) = Left . toHailgunError $ error
convertGood (Right response) = Right response

convertBad :: Either String HailgunErrorResponse -> HailgunErrorResponse
convertBad (Left error) = toHailgunError $ error
convertBad (Right e) = e

