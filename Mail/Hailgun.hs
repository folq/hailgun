module Mail.Hailgun where

import Data.ByteString
import Text.Email.Validate

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

type UnverifiedEmailAddress = ByteString
type MessageSubject = String
data MessageContent
   = TextOnly 
      { textContent :: String
      }
   | TextAndHTML
      { textContent :: String
      , htmlContent :: String
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

-- Use this method of the HTTP library to convert this into a Request body:
-- https://hackage.haskell.org/package/HTTP-4000.2.17/docs/Network-HTTP-Base.html#v:urlEncodeVars
