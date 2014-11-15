module Mail.Hailgun.AttachmentsSearch where

import           Control.Applicative
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.List             (find)
import           Data.Maybe            (catMaybes)
import qualified Text.HTML.TagSoup     as TS

findInlineImagesInHtmlEmail :: B.ByteString -> [InlineImage]
findInlineImagesInHtmlEmail = catMaybes . fmap tagToInlineImage . TS.parseTags

data InlineImage = InlineImage
   { imageSrc :: B.ByteString
   } deriving (Eq, Show)

imgTagName :: B.ByteString
imgTagName = BC.pack "img"

srcTagName :: B.ByteString
srcTagName = BC.pack "src"

tagToInlineImage :: TS.Tag B.ByteString -> Maybe InlineImage
tagToInlineImage (TS.TagOpen name attrs) =
   if name == imgTagName
      then InlineImage . snd <$> find ((==) srcTagName . fst) attrs
      else Nothing
tagToInlineImage _ = Nothing
