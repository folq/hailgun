# Hailgun

The hailgun library is a Haskell library that integrates with Mailgun. Mailgun is a service that
allows controlled sending of emails. The intial focus of the API is on sending emails rather than 
recieving them.

This library was implimented based upon the information provided on the [Mailgun API Reference][1].
The goal of this library is match this API 1-1.

## Trying this library

You probably want to know that this library works to prove that it would be a good choice for your
project. Follow these steps to get started with the library and Mailgun and send a test email:

 1. [Sign up for Mailgun][2].
 1. Mailgun will give you options to use [Curl][3] to send emails to yourself right after signup.
    You should do so to test that your account works. (Optional)
 1. Grab the sandbox domain that was generated and the API key 

 [1]: http://documentation.mailgun.com/api_reference.html
