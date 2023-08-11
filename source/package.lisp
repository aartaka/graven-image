;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package :graven-image
  (:use :common-lisp)
  (:nicknames :gimage)
  (:export
   #:y-or-n-p* #:yes-or-no-p* #:*yes-or-no-options*
   #:apropos-list* #:apropos* #:apropod*
   ;; #:ed*
   #:fields* #:description*
   #:$ #:definterface
   #:describe* #:inspect*
   #:function-lambda-expression* #:function-lambda-list* #:function-name* #:function-type*
   #:lambda-expression* #:function-arlist* #:lambda-list* #:arlist*
   #:time* #:with-time*
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
  - Also see `with-time*' with the exposed `time'-internal APIs.
- `dribble' -> `dribble*'."))
