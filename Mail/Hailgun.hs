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
   , addAttachment
   ) where

import           Mail.Hailgun.Attachment
import           Mail.Hailgun.Domains
import           Mail.Hailgun.Errors
import           Mail.Hailgun.Internal.Data
import           Mail.Hailgun.Message
import           Mail.Hailgun.Pagination
import           Mail.Hailgun.SendEmail

import qualified Data.ByteString.Char8      as BC
import           Network.HTTP.Client        (Proxy (..))

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

toProxy :: String -> Int -> Proxy
toProxy host = Proxy (BC.pack host)
