;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image/tests)

(define-test bound-features ()
  (assert-true (fboundp 'y-or-n-p*))
  (assert-true (fboundp 'yes-or-no-p*))
  (assert-true (boundp '*yes-or-no-options*))
  (assert-true (fboundp 'apropos-list*))
  (assert-true (fboundp 'apropos*))
  (assert-true (fboundp 'apropod*))
  ;; #:ed*
  ;; #:inspect*
  (assert-true (fboundp 'describe*))
  (assert-true (fboundp 'description*))
  (assert-true (fboundp 'properties*))
  (assert-true (fboundp 'function-lambda-expression*))
  (assert-true (fboundp 'function-lambda-list*))
  (assert-true (fboundp 'function-name*))
  (assert-true (fboundp 'function-type*))
  (assert-true (macro-function 'time*))
  (assert-true (macro-function 'with-time*))
  ;; #:step* #:trace* #:untrace*
  )
