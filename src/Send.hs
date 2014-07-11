module Main where

import Mail.Hailgun
import Data.Configurator (load, require, Worth(..))
import Data.Text (pack)

-- The purpose of this module is to provide a way to send emails to the Mailgun service using the
-- command line. This is mainly for the purposes of testing initially. Using this executable should
-- be the basis for our integration tests. It should also be the starting point for everybody that
-- wishes to test this library out.

hailgunConfFile :: FilePath
hailgunConfFile = "hailgun.send.conf"

mailgunDomainLabel = pack "mailgun-domain"
mailgunApiKeyLabel = pack "mailgun-api-key"

loadHailgunContext :: FilePath -> IO HailgunContext
loadHailgunContext configFile = do
   hailgunConf <- load [Required configFile]
   domain <- require hailgunConf mailgunDomainLabel
   apiKey <- require hailgunConf mailgunApiKeyLabel
   return $ HailgunContext 
      { hailgunDomain = domain
      , hailgunApiKey = apiKey
      }

main = do
   hailgunContext <- loadHailgunContext hailgunConfFile
   putStrLn "Sending emails has not been implimented yet."
