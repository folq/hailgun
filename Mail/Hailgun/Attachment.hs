module Mail.Hailgun.Attachment
    ( createAttachment
    , createAttachmentLazy
    , addAttachment
    , toStandardAttachment
    , toInlineAttachment
    )
    where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Monoid
import           Mail.Hailgun.Internal.Data

createAttachment :: FilePath -> B.ByteString -> Attachment
createAttachment filename body = Attachment filename (AttachmentBS body)

createAttachmentLazy :: FilePath -> BL.ByteString -> Attachment
createAttachmentLazy filename body = Attachment filename (AttachmentLBS body)

toStandardAttachment :: Attachment -> SpecificAttachment
toStandardAttachment (Attachment filepath body) = SpecificAttachment Attached filepath body

toInlineAttachment :: Attachment -> SpecificAttachment
toInlineAttachment (Attachment filepath body) = SpecificAttachment Inline filepath body

addAttachment :: SpecificAttachment -> Endo HailgunMessage
addAttachment attachment = Endo $ \message -> message
    { messageAttachments = attachment : messageAttachments message
    }

