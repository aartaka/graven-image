;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package :graven-image
  (:use :common-lisp)
  (:export
   #:y-or-n-p* #:yes-or-no-p* #:*yes-or-no-options*
   #:apropos-list* #:apropos*
   ;; #:ed* #:inspect*
   ;; #:describe* #:describe-object*
   #:function-lambda-expression*
   #:time*
   ;; #:step* #:trace* #:untrace*
   )
  (:documentation "CL standard debugging utilities improved."))
