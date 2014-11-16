module Mail.Hailgun.Pagination
    ( Page(..)
    , pageToParams
    ) where

import qualified Data.ByteString.Char8 as BC

-- | Represents a single page of results. You specify a page of results when you wish to to make a request to a
-- paginated resource.
data Page = Page
   { pageStart  :: Integer
   , pageLength :: Integer
   }

pageToParams :: Page -> [(BC.ByteString, BC.ByteString)]
pageToParams page =
   [ (BC.pack "skip",   BC.pack . show . pageStart $ page)
   , (BC.pack "limit",  BC.pack . show . pageLength $ page)
   ]

