{-# LANGUAGE CPP, FlexibleContexts #-}
-- | Hailgun is a Haskell wrapper around the <http://documentation.mailgun.com/api_reference.html Mailgun api's> that use
-- type safety to ensure that you are sending a valid request to the Mailgun API's. Mailgun is a
-- service that lets you send emails. It also contains a number of other email handling API's that
-- will be implimented in the future.
module Mail.Hailgun 
   ( sendEmail
   , hailgunMessage
   , HailgunMessage
   , HailgunContext(..)
   , MessageSubject
   , MessageContent(..)
   , MessageRecipients(..)
   , emptyMessageRecipients
   , UnverifiedEmailAddress
   , HailgunSendResponse(..)
   , HailgunErrorMessage
   , HailgunErrorResponse(..)
   , getDomains
   , Page(..)
   , HailgunDomain(..)
   , HailgunDomainResponse(..)
   , HailgunTime(..)
   , toProxy
   ) where

import Mail.Hailgun.Internal.Data

import            Control.Applicative ((<$>), (<*>), pure)
import            Control.Arrow (second)
import            Control.Monad (mzero)
import            Control.Monad.IO.Class
import            Control.Monad.Catch (MonadThrow(..))
import            Data.Aeson
import qualified  Data.ByteString.Char8 as BC
import qualified  Data.ByteString.Lazy.Char8 as BCL
import qualified  Data.Text as T
import            Data.Time.Clock (UTCTime(..))
import            Data.Time.LocalTime (zonedTimeToUTC)
import            Text.Email.Validate
import            Network.HTTP.Client (Request(..), Response(..), parseUrl, httpLbs, withManager, responseStatus, responseBody, applyBasicAuth, setQueryString, Proxy(..))
import            Network.HTTP.Client.Internal (addProxy)
import            Network.HTTP.Client.MultipartFormData (Part(..), formDataBody, partBS)
import            Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified  Network.HTTP.Types.Status as NT
import qualified  Network.HTTP.Types.Method as NM

import Data.Time.Format (ParseTime(..), parseTime)
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

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

-- | A method to construct a HailgunMessage. You require a subject, content, From address and people
-- to send the email to and it will give you back a valid Hailgun email message. Or it will error
-- out while trying.
hailgunMessage 
   :: MessageSubject -- ^ The purpose of the email surmised.
   -> MessageContent -- ^ The full body of the email.
   -> UnverifiedEmailAddress -- ^ The email account that the recipients should respond to in order to get back to us.
   -> MessageRecipients -- ^ The people that should recieve this email.
   -> Either HailgunErrorMessage HailgunMessage -- ^ Either an error while trying to create a valid message or a valid message.
hailgunMessage subject content sender recipients = do
   from  <- validate sender
   to    <- mapM validate (recipientsTo recipients)
   cc    <- mapM validate (recipientsCC recipients)
   bcc   <- mapM validate (recipientsBCC recipients)
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
   [ (BC.pack "from", toByteString . messageFrom $ message)
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
      convertEmails prefix = fmap ((,) prefix . toByteString)

-- TODO replace with MailgunSendResponse
-- | The response to an email being accepted by the Mailgun API.
data HailgunSendResponse = HailgunSendResponse
   { hsrMessage :: String -- ^ The freeform message from the mailgun API.
   , hsrId      :: String -- ^ The ID of the message that has been accepted by the Mailgun api.
   }

-- TODO make this Hailgun specific and different for the Mailgun api. That way there is the correct
-- separation of concerns.
-- | An error that comes from Mailgun or the Hailgun API.
data HailgunErrorResponse = HailgunErrorResponse
   { herMessage :: String -- ^ A generic message describing the error.
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

-- | Send an email using the Mailgun API's. This method is capable of sending a message over the
-- Mailgun service. All it needs is the appropriate context.
sendEmail 
   :: HailgunContext -- ^ The Mailgun context to operate in.
   -> HailgunMessage -- ^ The Hailgun message to be sent.
   -> IO (Either HailgunErrorResponse HailgunSendResponse) -- ^ The result of the sent email. Either a sent email or a successful send.
sendEmail context message = do
   request <- postRequest url context (toPostVars message)
   response <- withManager tlsManagerSettings (httpLbs request)
   return $ parseResponse response
   where
      url = mailgunApiPrefixContext context ++ "/messages"

parseResponse :: (FromJSON a) => Response BCL.ByteString -> Either HailgunErrorResponse a
parseResponse response = statusToResponse . NT.statusCode . responseStatus $ response
   where
      statusToResponse s
         | s == 200                      = responseDecode response
         | s `elem` [400, 401, 402, 404] = gatherErrors . responseDecode $ response
         | s `elem` [500, 502, 503, 504] = serverError
         | otherwise                     = unexpectedError s

responseDecode :: (FromJSON a) => Response BCL.ByteString -> Either HailgunErrorResponse a
responseDecode = mapError . eitherDecode . responseBody

retError :: String -> Either HailgunErrorResponse a
retError = Left . toHailgunError

serverError :: Either HailgunErrorResponse a
serverError = retError "Server Errors - something is wrong on Mailgun’s end"

unexpectedError :: Int -> Either HailgunErrorResponse a
unexpectedError x = retError $ "Unexpected Non-Standard Mailgun Error: " ++ show x

mapError :: Either String a -> Either HailgunErrorResponse a
mapError = either (Left . toHailgunError) Right

gatherErrors :: Either HailgunErrorResponse HailgunErrorResponse -> Either HailgunErrorResponse a
gatherErrors = either Left Left

mailgunApiPrefix :: String
mailgunApiPrefix = "https://api.mailgun.net/v2" 

mailgunApiPrefixContext :: HailgunContext -> String
mailgunApiPrefixContext context = mailgunApiPrefix ++ "/" ++ hailgunDomain context

ignoreStatus :: a -> b -> c -> Maybe d
ignoreStatus _ _ _ = Nothing

data Page = Page
   { pageStart  :: Integer
   , pageLength :: Integer
   }

pageToParams :: Page -> [(BC.ByteString, BC.ByteString)]
pageToParams page = 
   [ (BC.pack "skip",   BC.pack . show . pageStart $ page)
   , (BC.pack "limit",  BC.pack . show . pageLength $ page)
   ]

toQueryParams :: [(BC.ByteString, BC.ByteString)] -> [(BC.ByteString, Maybe BC.ByteString)]
toQueryParams = fmap (second Just)

getDomains :: HailgunContext -> Page -> IO (Either HailgunErrorResponse HailgunDomainResponse)
getDomains context page = do
   request <- getRequest url context (toQueryParams . pageToParams $ page)
   response <- withManager tlsManagerSettings (httpLbs request)
   return $ parseResponse response
   where
      url = mailgunApiPrefix ++ "/domains"

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

applyHailgunAuth :: HailgunContext -> Request -> Request
applyHailgunAuth context = addRequestProxy (hailgunProxy context) . authRequest
   where 
      addRequestProxy :: Maybe Proxy -> Request -> Request
      addRequestProxy (Just proxy) = addProxy (proxyHost proxy) (proxyPort proxy)
      addRequestProxy _ = id

      authRequest = applyBasicAuth (BC.pack "api") (BC.pack . hailgunApiKey $ context)

data HailgunDomainResponse = HailgunDomainResponse
   { hdrTotalCount :: Integer
   , hdrItems :: [HailgunDomain]
   }

instance FromJSON HailgunDomainResponse where
   parseJSON (Object v) = HailgunDomainResponse
      <$> v .: T.pack "total_count"
      <*> v .: T.pack "items"
   parseJSON _ = mzero

data HailgunDomain = HailgunDomain
   { domainName         :: T.Text
   , domainSmtpLogin    :: String
   , domainSmtpPassword :: String
   , domainCreatedAt    :: HailgunTime
   , domainWildcard     :: Bool
   , domainSpamAction   :: String -- TODO the domain spam action is probably better specified
   }
   deriving(Show)

instance FromJSON HailgunDomain where
   parseJSON (Object v) = HailgunDomain
      <$> v .: T.pack "name"
      <*> v .: T.pack "smtp_login"
      <*> v .: T.pack "smtp_password"
      <*> v .: T.pack "created_at"
      <*> v .: T.pack "wildcard"
      <*> v .: T.pack "spam_action"
   parseJSON _ = mzero

newtype HailgunTime = HailgunTime UTCTime
   deriving (Eq, Ord, Show)

-- Example Input: 'Thu, 13 Oct 2011 18:02:00 GMT'
instance FromJSON HailgunTime where
   parseJSON = withText "HailgunTime" $ \t ->
      case parseTime defaultTimeLocale "%a, %d %b %Y %T %Z" (T.unpack t) of
         Just d -> pure d
         _      -> fail "could not parse Mailgun Style date"

instance ParseTime HailgunTime where
   buildTime l = HailgunTime . zonedTimeToUTC . buildTime l

toProxy :: String -> Int -> Proxy
toProxy host port = Proxy (BC.pack host) port
