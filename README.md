# Docbrowser - Web-based Common Lisp documentation browser

## Author information

Elias Mårtenson
- Email: lokedhs@gmail.com
- Google+ profile: http://profiles.google.com/lokedhs

## Source code repository location

The latest version of Docbrowser can be found at Github:

https://github.com/lokedhs/docbrowser

## Compatibility

The application has been tested on SBCL, CCL and CLISP. Please let me
know of any issues on other Common Lisp implementations.

## Starting the server

After loading the package, run the following command from the REPL:

```lisp
(docbrowser:start-docserver)
```

This will start the docserver on port 8080. A different port number
can also be given with an optional `:port` argument.

To access the docbrowser, point a web browser to
http://localhost:8080/

The main page presents a list of all the available packages. You can
click on one to see its functions and variables, with their
docstrings. You can also go to a function's source.
