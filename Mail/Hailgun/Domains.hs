module Mail.Hailgun.Domains
    ( getDomains
    , HailgunDomainResponse(..)
    , HailgunDomain(..)
    ) where

import           Control.Applicative
import           Control.Monad              (mzero)
import           Data.Aeson
import qualified Data.Text                  as T
import           Mail.Hailgun.Communication
import           Mail.Hailgun.Errors
import           Mail.Hailgun.Internal.Data
import           Mail.Hailgun.MailgunApi
import           Mail.Hailgun.Pagination
import           Network.HTTP.Client        (httpLbs, withManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)

getDomains :: HailgunContext -> Page -> IO (Either HailgunErrorResponse HailgunDomainResponse)
getDomains context page = do
   request <- getRequest url context (toQueryParams . pageToParams $ page)
   response <- withManager tlsManagerSettings (httpLbs request)
   return $ parseResponse response
   where
      url = mailgunApiPrefix ++ "/domains"

data HailgunDomainResponse = HailgunDomainResponse
   { hdrTotalCount :: Integer
   , hdrItems      :: [HailgunDomain]
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
