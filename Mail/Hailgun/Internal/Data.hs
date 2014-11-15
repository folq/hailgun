{-# LANGUAGE CPP #-}
module Mail.Hailgun.Internal.Data
    ( HailgunContext(..)
    , HailgunMessage(..)
    , MessageSubject
    , MessageContent(..)
    , UnverifiedEmailAddress
    , MessageRecipients(..)
    , emptyMessageRecipients
    , HailgunErrorMessage
    , HailgunTime(..)
    ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString     as B
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime (..))
import           Data.Time.Format    (ParseTime (..), parseTime)
import           Data.Time.LocalTime (zonedTimeToUTC)
import qualified Network.HTTP.Client as NHC
import qualified Text.Email.Validate as TEV

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format    (defaultTimeLocale)
#else
import           System.Locale       (defaultTimeLocale)
#endif

type UnverifiedEmailAddress = B.ByteString -- ^ Represents an email address that is not yet verified.
type MessageSubject = String -- ^ Represents a message subject.

-- | A generic error message that is returned by the Hailgun library.
type HailgunErrorMessage = String

-- | When comunnicating to the Mailgun service you need to have some common pieces of information to
-- authenticate successfully. This context encapsulates that required information.
data HailgunContext = HailgunContext
   -- TODO better way to represent a domain
   { hailgunDomain :: String -- ^ The domain of the mailgun account that you wish to send the emails through.
   , hailgunApiKey :: String -- ^ The API key for the mailgun account so that you can successfully make requests. Please note that it should include the 'key' prefix.
   , hailgunProxy  :: Maybe NHC.Proxy
   }

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
   { messageSubject :: MessageSubject
   , messageContent :: MessageContent
   , messageFrom    :: TEV.EmailAddress
   , messageTo      :: [TEV.EmailAddress]
   , messageCC      :: [TEV.EmailAddress]
   , messageBCC     :: [TEV.EmailAddress]
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
   -- TODO custom message data support

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
   { recipientsTo  :: [UnverifiedEmailAddress] -- ^ The people to email directly.
   , recipientsCC  :: [UnverifiedEmailAddress] -- ^ The people to \"Carbon Copy\" into the email. Honestly, why is that term not deprecated yet?
   , recipientsBCC :: [UnverifiedEmailAddress] -- ^ The people to \"Blind Carbon Copy\" into the email. There really needs to be a better name for this too.
   }

-- The user should just give us a list of attachments and we should automatically make them inline or not
-- We should consider sending the HTML message as a quoted-string: http://hackage.haskell.org/package/dataenc-0.14.0.5/docs/Codec-Binary-QuotedPrintable.html
-- We should use TagSoup to parse the constructed HTML message so that we can see if any inline images are expected:

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
