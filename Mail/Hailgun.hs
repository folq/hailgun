module Mail.Hailgun 
   ( HailgunMessage
   , MessageSubject
   , MessageContent(..)
   , MessageRecipients(..)
   ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Text.Email.Validate
import Network.HTTP (simpleHTTP, postRequestWithBody, urlEncodeVars, rspCode)

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

data MessageRecipients = MessageRepipients 
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

toPostVars :: HailgunMessage -> [(String, String)]
toPostVars message = 
   [ ("from", show . messageFrom $ message)
   , ("subject", messageSubject message)
   ] ++ to 
   ++ cc 
   ++ bcc 
   ++ fromContent (messageContent message)
   where
      to = convertEmails "to" . messageTo $ message
      cc = convertEmails "cc" . messageCC $ message
      bcc = convertEmails "bcc" . messageBCC $ message

      fromContent :: MessageContent -> [(String, String)]
      fromContent t@(TextOnly _) = [ ("text", BC.unpack $ textContent t) ]
      fromContent th@(TextAndHTML {}) = ("html", BC.unpack $ htmlContent th) : fromContent (TextOnly . textContent $ th)

      convertEmails :: String -> [EmailAddress] -> [(String, String)]
      convertEmails prefix = fmap ((,) prefix . show)

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

instance FromJSON HailgunSendResponse where
   parseJSON (Object v) = HailgunSendResponse
      <$> v .: T.pack "message"
      <*> v .: T.pack "id"
   parseJSON _ = mzero

sendEmail :: HailgunContext -> HailgunMessage -> IO (Either HailgunErrorMessage HailgunSendResponse)
sendEmail context message = do
   result <- simpleHTTP (postRequestWithBody url contentType body)
   case result of
      Left connectionError -> error "The connection failed"
      Right response -> case rspCode response of
         (2, 0, 0) -> error "Request successful but no response gathered"
         (4, 0, 0) -> error "Bad Request - Often missing a required parameter"
         (4, 0, 1) -> error "Unauthorized - No valid API key provided"
         (4, 0, 2) -> error "Request Failed - Parameters were valid but request failed"
         (4, 0, 4) -> error "Not Found - The requested item doesn’t exist"
         (5, 0, x) -> if x `elem` (0 : [2..4]) 
                        then error "Server Errors - something is wrong on Mailgun’s end"
                        else error "NonStandard Error"
         (x, y, z) -> error "NonStandard Error"
   where
      url = "https://api.mailgun.net/v2/" ++ hailgunDomain context ++ "/messages"
      contentType = undefined
      body = urlEncodeVars . toPostVars $ message

