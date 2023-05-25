;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package :graven-image
  (:use :common-lisp)
  (:export
   #:y-or-n-p* #:yes-or-no-p* #:*yes-or-no-options*
   #:apropos-list* #:apropos* #:apropod*
   ;; #:ed* #:inspect* #:inspect-object*
   ;; #:describe* #:describe-object*
   #:function-lambda-expression* #:function-lambda-list* #:function-name* #:function-type*
   #:time*
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
  (:import-from
   #+sbcl      #:sb-ext
   #+ecl       #:ext
   #:package-locked-p
   #:package-lock
   #:package-unlock)
  (:documentation "CL standard debugging utilities improved."))
