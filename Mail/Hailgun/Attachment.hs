module Mail.Hailgun.Attachment
    ( createAttachment
    , createAttachmentLazy
    , addAttachment
    )
    where

import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Monoid
import           Mail.Hailgun.Attachment.Internal
import           Mail.Hailgun.Internal.Data

-- | Creates an attachment from a filepath and strict ByteString.
createAttachment :: FilePath -> B.ByteString -> Attachment
createAttachment filename body = Attachment filename (AttachmentBS body)

-- | Creates an attachment from a filepath and lazy ByteString.
createAttachmentLazy :: FilePath -> BL.ByteString -> Attachment
createAttachmentLazy filename body = Attachment filename (AttachmentLBS body)

-- | Allows you to add an attachment to an already created HailgunMessage. But please note that it will only
-- add your attachment as a standard attachment. Inline attachments must be added at the time that the
-- HailgunMessage was created.
addAttachment :: Attachment -> Endo HailgunMessage
addAttachment attachment = Endo $ \message -> message
    { messageAttachments = (toStandardAttachment . cleanAttachmentFilePath $ attachment) : messageAttachments message
    }

