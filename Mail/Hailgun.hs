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
   ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Text.Email.Validate
import Network.HTTP.Client (Request(..), parseUrl, httpLbs, withManager, responseStatus, responseBody, applyBasicAuth)
import Network.HTTP.Client.MultipartFormData (Part(..), formDataBody, partBS)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types.Status as NT
import qualified Network.HTTP.Types.Method as NM

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

type UnverifiedEmailAddress = B.ByteString -- ^ Represents an email address that is not yet verified.
type MessageSubject = String -- ^ Represents a message subject.

-- | Any email content that you wish to send should be encoded into these types before it is sent.
-- Currently, according to the API, you should always send a Text Only part in the email and you can
-- optionally add a nicely formatted HTML version of that email to the sent message. 
-- 
-- @
-- It is best to send multi-part emails using both text and HTML or text only. Sending HTML only
-- email is not well received by ESPs.
-- @
-- (<http://documentation.mailgun.com/best_practices.html#email-content Source>)
--
-- This API mirrors that advice so that you can always get it right.
data MessageContent
   -- | The Text only version of the message content.
   = TextOnly
      { textContent :: B.ByteString 
      }
   -- | A message that contains both a Text version of the email content and a HTML version of the email content.
   | TextAndHTML
      { textContent :: B.ByteString -- ^ The text content that you wish to send (please note that many clients will take the HTML version first if it is present but that the text version is a great fallback).
      , htmlContent :: B.ByteString -- ^ The HTML content that you wish to send.
      }

-- | A Hailgun Email message that may be sent. It contains important information such as the address
-- that the email is from, the addresses that it should be going to, the subject of the message and
-- the content of the message. Any email that you wish to send via this api must be converted into
-- this structure first. To create a message then please use the hailgunMessage interface.
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

-- | No recipients for your email. Useful singleton instance to avoid boilerplate in your code. For
-- example: 
--
-- @
-- toBob = emptyMessageRecipients { recipientsTo = [\"bob\@bob.test\"] }
-- @
emptyMessageRecipients :: MessageRecipients
emptyMessageRecipients = MessageRecipients [] [] []

-- | A collection of unverified email recipients separated into the To, CC and BCC groupings that
-- email supports.
data MessageRecipients = MessageRecipients 
   { recipientsTo    :: [UnverifiedEmailAddress] -- ^ The people to email directly.
   , recipientsCC    :: [UnverifiedEmailAddress] -- ^ The people to \"Carbon Copy\" into the email. Honestly, why is that term not deprecated yet?
   , recipientsBCC   :: [UnverifiedEmailAddress] -- ^ The people to \"Blind Carbon Copy\" into the email. There really needs to be a better name for this too.
   }

-- | A generic error message that is returned by the Hailgun library.
type HailgunErrorMessage = String

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

-- | When comunnicating to the Mailgun service you need to have some common pieces of information to
-- authenticate successfully. This context encapsulates that required information.
data HailgunContext = HailgunContext
   -- TODO better way to represent a domain
   { hailgunDomain :: String -- ^ The domain of the mailgun account that you wish to send the emails through.
   , hailgunApiKey :: String -- ^ The API key for the mailgun account so that you can successfully make requests. Please note that it should include the 'key' prefix.
   }

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
   initRequest <- parseUrl url
   let request = initRequest { method = NM.methodPost, checkStatus = \_ _ _ -> Nothing }
   requestWithBody <- encodeFormData (toPostVars message) request
   let authedRequest = applyBasicAuth (BC.pack "api") (BC.pack . hailgunApiKey $ context) requestWithBody
   response <- withManager tlsManagerSettings (httpLbs authedRequest)
   case NT.statusCode . responseStatus $ response of
      200 -> return . convertGood . eitherDecode' . responseBody $ response
      400 -> return . Left . convertBad . eitherDecode' . responseBody $ response
      401 -> return . Left . convertBad . eitherDecode' . responseBody $ response
      402 -> return . Left . convertBad . eitherDecode' . responseBody $ response
      404 -> return . Left . convertBad . eitherDecode' . responseBody $ response
      500 -> serverError
      502 -> serverError
      503 -> serverError
      504 -> serverError
      c   -> retError . unexpectedError $ c
   where
      url = "https://api.mailgun.net/v2/" ++ hailgunDomain context ++ "/messages"
      retError = return . Left . toHailgunError

      serverError = retError "Server Errors - something is wrong on Mailgun’s end"
      unexpectedError x = "Unexpected Non-Standard Mailgun Error: " ++ show x

convertGood :: Either String HailgunSendResponse -> Either HailgunErrorResponse HailgunSendResponse
convertGood (Left error) = Left . toHailgunError $ error
convertGood (Right response) = Right response

convertBad :: Either String HailgunErrorResponse -> HailgunErrorResponse
convertBad (Left error) = toHailgunError error
convertBad (Right e)    = e
