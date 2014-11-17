module Mail.Hailgun.AttachmentsSearch
   ( findInlineImagesInHtmlEmail
   , InlineImage(..)
   ) where

import           Control.Monad         (guard)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.List             (find, nub)
import           Data.Maybe            (catMaybes)
import qualified Text.HTML.TagSoup     as TS

-- Finds the unique inline images in the email
findInlineImagesInHtmlEmail :: B.ByteString -> [InlineImage]
findInlineImagesInHtmlEmail = nub . catMaybes . fmap tagToInlineImage . TS.parseTags

data InlineImage = InlineImage
   { imageSrc :: B.ByteString
   } deriving (Eq, Show)

imgTagName :: B.ByteString
imgTagName = BC.pack "img"

srcTagName :: B.ByteString
srcTagName = BC.pack "src"

inlineImageUrlPrefix :: B.ByteString
inlineImageUrlPrefix = BC.pack "cid:"

tagToInlineImage :: TS.Tag B.ByteString -> Maybe InlineImage
tagToInlineImage (TS.TagOpen name attrs) =
   if name == imgTagName
      then do
         srcAttr <- find ((==) srcTagName . fst) attrs
         guard (inlineImageUrlPrefix `B.isPrefixOf` snd srcAttr)
         return . InlineImage . B.drop (B.length inlineImageUrlPrefix) . snd $ srcAttr
      else Nothing
tagToInlineImage _ = Nothing
