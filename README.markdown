# Hailgun

The hailgun library is a Haskell library that integrates with Mailgun. Mailgun is a service that
allows controlled sending of emails. The intial focus of the API is on sending emails rather than 
recieving them.

This library was implimented based upon the information provided on the [Mailgun API Reference][1].
The goal of this library is match this API 1-1.

## Building this library

This library just uses [cabal][5] to build itself. To do so for development then follow the following
steps:

    cabal sandbox init
    cabal install --only-dependencies
    cabal build

At that point in time you should have a working version of the library so you could run the example
hailgun-send command by:

    cabal run hailgun-send -- --help

And you can continue trying this libary from here.

## Trying this library

You probably want to know that this library works to prove that it would be a good choice for your
project. Follow these steps to get started with the library and Mailgun and send a test email:

 1. [Sign up for Mailgun][2].
 1. Mailgun will give you options to use [Curl][3] to send emails to yourself right after signup.
    You should do so to test that your account works. (Optional)
 1. Grab the sandbox domain that was generated and the API key by visiting the [Mailgun control panel][4].
 1. Create a file caled hailgun.send.conf in the current directory and put the API Key and the
    mailgun sandbox domain in the file. You can copy the hailgun.send.conf.default file to see the
    correct format.
 1. Now you may use hailgun-send to send emails by passing in the correct command line arguments.
    You can see the command line argument options with `cabal run hailgun-send -- --help`

If you are lucky this has only taken you a few minutes to get working and you are now sending emails
through the Mailgun API from the terminal.

 [1]: http://documentation.mailgun.com/api_reference.html
 [2]: https://mailgun.com/signup
 [3]: http://man.cx/curl
 [4]: https://mailgun.com/cp
 [5]: http://www.haskell.org/cabal/
