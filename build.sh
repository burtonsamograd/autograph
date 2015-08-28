#!/bin/bash

# Disable ASDF compiler cache
#export ASDF_OUTPUT_TRANSLATIONS=/:
#rm -rf quicklisp

# Check for sbcl
which sbcl || { echo "Please install SBCL to build autograph."; exit 1; }

# Pull down quicklisp
[ ! -e quicklisp.lisp ] && wget https://beta.quicklisp.org/quicklisp.lisp

sbcl --load quicklisp.lisp --eval \
     "$([[ ! -e quicklisp ]] && echo -n '(quicklisp-quickstart:install :path "./quicklisp")' || echo -n '(load "quicklisp/setup.lisp")')" --eval '(ql:quickload :cl-ppcre)' --eval '(ql:quickload :anaphora)' \
     --load autograph.lisp --eval '(sb-ext:save-lisp-and-die "autograph" :executable t
                                   :toplevel (lambda ()  (main sb-ext:*posix-argv*)))'
