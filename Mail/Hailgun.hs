module Mail.Hailgun where

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

type EmailAddress = String
type MessageSubject = String
data MessageBody 
   = TextBody String
   | HTMLBody String

data HailgunMessage = HailgunMessage
   { messageSubject  :: MessageSubject
   , messageBody     :: MessageBody
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
