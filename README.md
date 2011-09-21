

DE.SETF.WILBUR: a maintenance fork of net.sourceforge.wilbur
-------

Introduction
------------
`de.setf.wilbur` extends and updates the 2010-02-14 version of the "Wilbur Semantic Web Toolkit for CLOS" to maintain
compatibility with current Common Lisp runtimes.

 - MCL : 5.2
 - SBCL : 1.0.36 (linux)

## Downloading

[github](http://github.com/mon-key/de.setf.wilbur)

git://github.com/mon-key/de.setf.wilbur.git

## Installing

:NOTE We use "wilbur-FORK" instead of "de.setf.wilbur" as the pathname for our clone 

 shell> cd  /path/to/your/git-repos

 shell> git clone git://github.com/mon-key/de.setf.wilbur.git wilbur-FORK


In a running lisp tell asdf where to find the .asd files in "wilbur-FORK" repository:

 cl-user> (push #P"/path/to/your/git-repos/wilbur-FORK/src/" asdf:*central-registry*)

 cl-user> (quicklisp:quickload 'wilbur)

:NOTE When loading/building the wilbur.asd will set some
CL:LOGICAL-PATHNAME-TRANSLATIONS for the host "wilbur" according to the value of
CL:*LOAD-PATHNAME*. The default behaviour _should_ suffice and I've not
encountered problems with its default return values for
CL:TRANSLATE-LOGICAL-PATHNAME however, should you encounter build/install
failures errors around the CL:LOGICAL-PATHNAME-TRANSLATIONS for the host
"wilbur" are likely the _first_ point of failure. 
For additional discussion see the comments in src/wilbur.asd

## Licensing

The library is governed by the original LLGPL license

Note however, that we've taken the liberties with the original source of
separating LLGPL license headers from all <FOO>.lisp files beneath the following
directories:

 /src/*
 /src/goodies/*
 /src/nox/*

and consolodating the boilerplate to the single file: src/LICENSE
This may (or not) be entirely kosher w/r/t to the terms of LGPL, LLGPL but IANAL

So, in the off chance that your project choosed to distribute the source of
_this_ FORK you may wish to revert the LLGPL licence headers to their original
form per the canonical source distrubuted at the location below:

- [net.sourceforge.wilbur](wilbur-rdf.sourceforge.net/)
  - 2010 [Ora Lassila](ora.lassila@nokia.com)

