;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :asdf)

(defsystem "graven-image"
  :description "Common Lisp standard debugging utilities made more extensible and useful."
  :author "Artyom Bologov"
  :homepage "https://github.com/aartaka/graven-image"
  :license  "BSD-3 Clause"
  :version "0.0.1"
  :serial t
  :pathname "source/"
  :depends-on ("closer-mop")
  :components ((:file "package")
               (:file "graven-image")
               (:file "function-lambda-expression")
               (:file "yes-or-no")
               (:file "apropos")
               (:file "time")
               (:file "inspect"))
  ;; :in-order-to ((test-op "graven-image/tests"))
  )

(defsystem "graven-image/cl"
  :depends-on ("graven-image")
  :description "Binding CL functions/macros to Graven Image counterparts."
  :components ((:file "source/cl")))
