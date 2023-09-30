;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; From trivial-gray-streams
#+:abcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))
#+(or cmu genera)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'excl:stream-write-string)
    (require "streamc.fasl")))
#+(or ecl clasp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gray::redefine-cl-functions))

(uiop:define-package :graven-image
  (:use :common-lisp)
  (:nicknames :gimage)
  (:export
   #:y-or-n-p* #:yes-or-no-p* #:*yes-or-no-options*
   #:apropos-list* #:apropos* #:apropod*
   ;; #:ed*
   #:fields* #:description*
   #:$ #:definterface :*interface-lines*
   #:describe* #:inspect*
   #:function-lambda-expression* #:function-lambda-list* #:function-name* #:function-type*
   #:lambda-expression* #:function-arglist* #:lambda-list* #:arglist*
   #:time* #:benchmark* #:with-time*
   #:room* #:with-room*
   #:dribble*
   #:documentation* #:doc*
   ;; #:step* #:trace* #:untrace*
   )
  (:import-from
   #+sbcl      #:sb-ext
   #+clozure       #:ccl
   #+ecl       #:ext
   #+abcl      #:ext
   #+clasp     #:ext
   #+lispworks #:hcl
   #+allegro   #:excl
   #:package-local-nicknames)
  (:import-from
   #+sbcl :sb-gray
   #+allegro :excl
   #+cmu :ext
   #+(or clisp ecl mkcl mocl clasp) :gray
   #+openmcl :ccl
   #+lispworks :stream
   #+(or abcl genera) :gray-streams
   #+mezzano :mezzano.gray
   #-(or sbcl allegro cmu clisp openmcl lispworks ecl clasp mkcl abcl mocl genera mezzano) ...
   #:fundamental-character-output-stream
   #:stream-line-column
   #:stream-write-char)
  (:import-from
   #+abcl      #:mop
   #+allegro   #:mop
   #+clisp     #:clos
   #+clozure   #:ccl
   #+cmu       #:clos-mop
   #+ecl       #:clos
   #+clasp     #:clos
   #+lispworks #:clos
   #+mcl       #:ccl
   #+sbcl      #:sb-mop
   #+scl       #:clos
   #+mezzano   #:mezzano.clos
   #+sicl      #:sicl-clos
   #:class-direct-superclasses
   #:class-slots
   #:generic-function-lambda-list
   #:generic-function-method-combination
   #:generic-function-methods
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #:slot-definition-name)
  (:documentation "CL standard debugging utilities improved:
- `y-or-n-p' -> `y-or-n-p*'.
- `yes-or-no-p' -> `yes-or-no-p*'.
  - See `*yes-or-no-options*' for `y-or-n-p*'/`yes-or-no-p*'
    configuration.
- `apropos-list' -> `apropos-list*'.
- `apropos' -> `apropos*'.
  - Also see `apropod*' for name+documentation search.
- `describe' -> `describe*'.
- `inspect' -> `inspect*'.
  - Also see `fields*' and `description*' for the underlying
    inspection utils.
- `function-lambda-expression' -> `function-lambda-expression*'
  - Also see `function-lambda-list*', `function-name*'
    `function-type*'.
- `time' -> `time*'.
  - There's benchmarking-ready `benchmark*' too.
  - Also see `with-time*' with the exposed `time'-internal APIs.
- `room' -> `room*'
  - Also see `with-room*' exposing room APIs.
- `dribble' -> `dribble*'."))
