;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package :graven-image
  (:use :common-lisp)
  (:export
   #:y-or-n-p* #:yes-or-no-p* #:*yes-or-no-options*
   #:apropos-list* #:apropos* #:apropod*
   ;; #:ed*
   #:describe* #:fields*
   #:$ #:definterface
   #:inspect* #:description*
   #:function-lambda-expression* #:function-lambda-list* #:function-name* #:function-type*
   #:time* #:with-time*
   #:dribble*
   ;; #:step* #:trace* #:untrace*
   )
  (:import-from
   #+sbcl      #:sb-ext
   #+ccl       #:ccl
   #+ecl       #:ext
   #+abcl      #:ext
   #+clasp     #:ext
   #+lispworks #:hcl
   #+allegro   #:excl
   #:package-local-nicknames)
  (:documentation "CL standard debugging utilities improved."))
