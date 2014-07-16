# The Hailgun library

Very recently I was writing a shared service in Haskell and we realised that we would need to
integrate with an email service provider. After a little bit of research I concluded that Mailgun
was a great service for developers to send their emails so I looked for Mailgun integration
libraries that looked like they were being groomed to be "the" Mailgun library for Haskell and I was
disappointed to find that the [existing][1] libraries did not seem to be very comprehensive or did
not make use of type safety.

So I [wrote my own library and I called it "Hailgun"][3]. Then I [uploaded it to Hackage][2].

I believe this library to be better because:

 - It supports simple email sending just like the existing libraries.
 - The type system is stricter, leaving less room for incorrect usage of the API. Giving you the
   assurance that you will be sending emails correctly.
 - There is [a backlog of issues][4] that has been roadmapped such that the library will have full
   Mailgun API support. It just needs time to develop it.
 - The support for the various parts of the API is being implimented and released incrementally so that people
   can get the benefits of the hailgun library now.
 - It comes with the **hailgun-send** executable out of the box.
   This executable can be used on the command line to send emails through the Mailgun API.
 - This library has been written in pure Haskell and does not use any FFI wrappers around another
   Mailgun library: the goal of this library is to work on any platform.

And this is just for version 0.1.0.0. To people that read this in the future, you should check out
the library on hackage to see what it impiliments now.

## Sending a test email

I'm going to explain here how to send a test email using **hailgun-send** so that you can quickly
see that the library works and can be used to send your emails in pure Haskell code.

If you don't have a Mailgun account and a sandbox for it then [go sign up for one][5]. Once you have 
created your account you should have been given an API key and have a sandbox domain name. Please 
create a file called 'hailgun.send.conf' in the current directory and make the contents of the file:

    mailgun-domain    = "sandbox-mailgun-domain.com"
    mailgun-api-key   = "key-thatmailgungaveme9234uoah234"

Once you have that file in place you should then be able to use **hailgun-send**. For example, this
is one invocation that I used:

    hailgun-send \
      --from 'postmaster@sandbox17bd032a44ea44f5b540470a8ab4787f.mailgun.org' \
      --to 'robertmassaioli@massaioli.com' \
      --subject 'Hailgun v0.1.0 test email' \
      -x data/email.text \
      -m data/email.html

You will notice that I provided a from and to address, an email subject and the content of the email
in two files. You should create the email.text and email.html files yourself or you could simply use
[the ones that are avaliable in the repository][6]. Please note that the HTML version of the email
is optional but the text version is always required.

## Concluding Words

In short you should now be able to send emails, via Mailgun, with Haskell and the library should be
improved to have more API support as time goes by. I hope this is useful to some peoplee and, if a
Mailgun developer should happen to swing by this post then please [feel free to review my code][7].

 [1]: http://hackage.haskell.org/package/mailgun
 [2]: http://hackage.haskell.org/package/hailgun
 [3]: https://bitbucket.org/robertmassaioli/hailgun
 [4]: https://bitbucket.org/robertmassaioli/hailgun/issues?status=new&status=open&sort=version
 [5]: https://mailgun.com/signup
 [6]: https://bitbucket.org/robertmassaioli/hailgun/src/c3ac04b1fdff971f9ecf2cf3553174e09f72fd68/data/
 [7]: https://bitbucket.org/robertmassaioli/hailgun/issue/17/get-feedback-from-the-mailgun-team
