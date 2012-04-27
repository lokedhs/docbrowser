# Docbrowser - Web-based Common Lisp documentation browser

## Author information

Elias Mårtenson
- Email: lokedhs@gmail.com
- Google+ profile: http://profiles.google.com/lokedhs

## Source code repository location

The latest version of Docbrowser can be found at Google Code project
hosting site:

http://code.google.com/p/docbrowser/

## Compatibility

The application has been tested on SBCL, CCL and CLISP. It should work
on other 

## Starting the server

After loading the package, run the following command from the REPL:

(docbrowser:start-docserver)

This will start the docserver on port 8080. A different port number
can also be given as an optional argument.
