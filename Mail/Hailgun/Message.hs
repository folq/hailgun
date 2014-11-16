module Mail.Hailgun.Message
    ( hailgunMessage
    ) where

import           Control.Arrow
import           Control.Monad                  (guard)
import qualified Data.ByteString.Char8          as BC
import           Data.List                      (partition)
import           Data.Monoid
import           Mail.Hailgun.Attachment
import           Mail.Hailgun.AttachmentsSearch
import           Mail.Hailgun.Internal.Data
import           Text.Email.Validate

-- | A method to construct a HailgunMessage. You require a subject, content, From address and people
-- to send the email to and it will give you back a valid Hailgun email message. Or it will error
-- out while trying.
hailgunMessage
   :: MessageSubject -- ^ The purpose of the email surmised.
   -> MessageContent -- ^ The full body of the email.
   -> UnverifiedEmailAddress -- ^ The email account that the recipients should respond to in order to get back to us.
   -> MessageRecipients -- ^ The people that should recieve this email.
   -> [Attachment]
   -> Either HailgunErrorMessage HailgunMessage -- ^ Either an error while trying to create a valid message or a valid message.
hailgunMessage subject content sender recipients simpleAttachments = do
   from  <- validate sender
   to    <- mapM validate (recipientsTo recipients)
   cc    <- mapM validate (recipientsCC recipients)
   bcc   <- mapM validate (recipientsBCC recipients)
   attachments <- attachmentsInferredFromMessage content simpleAttachments
   return HailgunMessage
      { messageSubject = subject
      , messageContent = content
      , messageFrom = from
      , messageTo = to
      , messageCC = cc
      , messageBCC = bcc
      , messageAttachments = attachments
      }

attachmentsInferredFromMessage :: MessageContent -> [Attachment] -> Either String [SpecificAttachment]
attachmentsInferredFromMessage mContent simpleAttachments =
   case mContent of
      (TextOnly _) -> return $ fmap toStandardAttachment simpleAttachments
      th@(TextAndHTML {}) -> do
         let inlineImageNames = findInlineImagesInHtmlEmail . htmlContent $ th
         convertAttachments simpleAttachments inlineImageNames

convertAttachments :: [Attachment] -> [InlineImage] -> Either String [SpecificAttachment]
convertAttachments attachments images = do
   let splitAttachments = partition (isAttachmentInlineImage images) attachments
   guard (length images == (length . fst $ splitAttachments))
   return . appendPair $ (fmap toInlineAttachment *** fmap toStandardAttachment) splitAttachments

appendPair :: Monoid a => (a, a) -> a
appendPair = uncurry mappend

isAttachmentInlineImage :: [InlineImage] -> Attachment -> Bool
isAttachmentInlineImage images attachment = or . fmap (attachmentForInlineImage attachment) $ images

attachmentForInlineImage :: Attachment -> InlineImage -> Bool
attachmentForInlineImage attachment image = (BC.pack . attachmentFilePath $ attachment) == imageSrc image

