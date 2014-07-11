module Main where

import Mail.Hailgun

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Configurator (load, require, Worth(..))
import qualified Data.Text as T
import System.Console.GetOpt (getOpt, OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo)
import System.Environment (getArgs)

-- The purpose of this module is to provide a way to send emails to the Mailgun service using the
-- command line. This is mainly for the purposes of testing initially. Using this executable should
-- be the basis for our integration tests. It should also be the starting point for everybody that
-- wishes to test this library out.

data Flag 
   = Help
   | From { email :: UnverifiedEmailAddress }
   | To { email :: UnverifiedEmailAddress }
   | Subject { subject :: MessageSubject }
   deriving (Eq, Show)
   
options :: [OptDescr Flag]
options =
   [ Option ['h'] ["help"]    (NoArg Help)                     "displays this help message"
   , Option ['f'] ["from"]    (ReqArg fromP "me@test.test")    "You are required to provide sender of this email."
   , Option ['t'] ["to"]      (ReqArg toP "them@test.test")    "You will need to provide atleast one person that you wish to send the email to."
   -- TODO Confirm that this is required.
   , Option ['s'] ["subject"] (ReqArg Subject "subject")      "You need to send an email subject."
   ]
   where
      fromP = From . BC.pack
      toP = To . BC.pack

hailgunConfFile :: FilePath
hailgunConfFile = "hailgun.send.conf"

mailgunDomainLabel = T.pack "mailgun-domain"
mailgunApiKeyLabel = T.pack "mailgun-api-key"

loadHailgunContext :: FilePath -> IO HailgunContext
loadHailgunContext configFile = do
   hailgunConf <- load [Required configFile]
   domain <- require hailgunConf mailgunDomainLabel
   apiKey <- require hailgunConf mailgunApiKeyLabel
   return $ HailgunContext 
      { hailgunDomain = domain
      , hailgunApiKey = apiKey
      }

handleSend :: [Flag] -> Either HailgunErrorMessage HailgunMessage
handleSend flags = do
   case (unverifiedFrom, subjects) of
      ([from], [subject]) -> hailgunMessage subject dummyContent from simpleRecipients
      ([], []) -> fail "You need to provide both a from address and a subject to send an email."
      (xs, []) -> fail "You have more than one from address and only one is allowed"
      ([], xs) -> fail "You have more than one subject and only one is allowed"
      _        -> fail "You have too many from adresses and subjects, you should only have one of each."
   where
      unverifiedTo = fmap email . filter isTo $ flags
      unverifiedFrom = fmap email . filter isFrom $ flags
      subjects = fmap subject . filter isSubject $ flags

      simpleRecipients = emptyMessageRecipients { recipientsTo = unverifiedTo }

      dummyContent = TextOnly . BC.pack $ "TODO let this be supplied in the command line arguments."

isSubject :: Flag -> Bool
isSubject (Subject _) = True
isSubject _ = False

isTo :: Flag -> Bool
isTo (To _) = True
isTo _ = False

isFrom :: Flag -> Bool
isFrom (From _) = True
isFrom _ = False

sendMessage :: HailgunMessage -> IO ()
sendMessage message = do
   hailgunContext <- loadHailgunContext hailgunConfFile
   response <- sendEmail hailgunContext message
   case response of
      Left error -> putStrLn $ "Failed to send email: " ++ error
      Right result -> do
         putStrLn "Sent Email!"
         putStrLn $ "Id: " ++ hsrId result
         putStrLn $ "Message: " ++ hsrMessage result

usageMessage = "Send emails using the Mailgun api."

main = do
   arguments <- getArgs
   case getOpt Permute options arguments of
      (flags, _, []) -> if Help `elem` flags
         then putStrLn $ usageInfo usageMessage options
         else case handleSend flags of
            (Right message) -> sendMessage message
            (Left error) -> putStrLn $ "Error generating mail: " ++ error
      (_, _, xs) -> error "Got some errors when parsing the command line options. TODO show nicer"
