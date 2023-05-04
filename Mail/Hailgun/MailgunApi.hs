module Mail.Hailgun.MailgunApi
    ( mailgunApiPrefixContext
    ) where

import           Mail.Hailgun.Internal.Data


mailgunApiPrefixContext :: HailgunContext -> String
mailgunApiPrefixContext context = hailgunApiPrefix context ++ "/" ++ hailgunDomain context
