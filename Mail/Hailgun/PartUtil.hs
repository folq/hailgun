module Mail.Hailgun.PartUtil
    ( paramsToPart
    , attachmentToPart
    ) where

import qualified Data.ByteString.Char8                 as BC
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (decodeUtf8)
import           Mail.Hailgun.Internal.Data
import qualified Network.HTTP.Client                   as NC
import qualified Network.HTTP.Client.MultipartFormData as NCM

paramsToPart :: (BC.ByteString, BC.ByteString) -> NCM.Part
paramsToPart (name, content) = NCM.partBS (decodeUtf8 name) content

attachmentToPart :: SpecificAttachment -> NCM.Part
attachmentToPart (SpecificAttachment attachmentType filename body) =
    NCM.partFileRequestBody partName filename requestBody
    where
        partName = attachmentTypeToName attachmentType
        requestBody = attachmentBodyToRequestBody body

attachmentBodyToRequestBody :: AttachmentBody -> NC.RequestBody
attachmentBodyToRequestBody (AttachmentBS body) = NC.RequestBodyBS body
attachmentBodyToRequestBody (AttachmentLBS body) = NC.RequestBodyLBS body

attachmentTypeToName :: AttachmentType -> T.Text
attachmentTypeToName Attached = T.pack "attachment"
attachmentTypeToName Inline = T.pack "inline"

