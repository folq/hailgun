module Mail.Hailgun.Attachment.Internal
    ( toStandardAttachment
    , toInlineAttachment
    , cleanAttachmentFilePath
    ) where

import           Mail.Hailgun.Internal.Data
import           System.FilePath            (takeFileName)

toStandardAttachment :: Attachment -> SpecificAttachment
toStandardAttachment (Attachment filepath body) = SpecificAttachment Attached filepath body

toInlineAttachment :: Attachment -> SpecificAttachment
toInlineAttachment (Attachment filepath body) = SpecificAttachment Inline filepath body

cleanAttachmentFilePath :: Attachment -> Attachment
cleanAttachmentFilePath (Attachment filepath body) = Attachment (takeFileName filepath) body
