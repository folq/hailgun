{-# LANGUAGE FlexibleContexts #-}
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

import           Mail.Hailgun.Domains
import           Mail.Hailgun.Errors
import           Mail.Hailgun.Internal.Data
import           Mail.Hailgun.Pagination
import           Mail.Hailgun.SendEmail

import qualified Data.ByteString.Char8      as BC
import           Network.HTTP.Client        (Proxy (..))
import           Text.Email.Validate

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

toProxy :: String -> Int -> Proxy
toProxy host = Proxy (BC.pack host)
