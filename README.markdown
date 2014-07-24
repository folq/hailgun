# Hailgun

The hailgun library is a Haskell library that integrates with Mailgun. Mailgun is a service that
allows controlled sending of emails. The intial focus of the API is on sending emails rather than 
recieving them.

This library was implimented based upon the information provided on the [Mailgun API Reference][1].
The goal of this library is match this API 1-1.

## Building this library

This library just uses [cabal][2] to build itself. To do so for development then follow the following
steps:

    cabal sandbox init
    cabal install --only-dependencies
    cabal build

At that point in time you should have a working version of the library. If you wish to see the
library in action then you could look at the [hailgun-send library][3].

 [1]: http://documentation.mailgun.com/api_reference.html
 [2]: http://www.haskell.org/cabal/
 [3]: https://hackage.haskell.org/package/hailgun-send
