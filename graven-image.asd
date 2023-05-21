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
  :components ((:file "package")
               (:file "graven-image")
               (:file "yes-or-no")
               (:file "apropos")
               (:file "function-lambda-expression")
               (:file "time"))
  :in-order-to ((test-op "graven-image/tests")))

(defsystem "graven-image/import"
  :depends-on ("graven-image")
  :description "Importing Graven Image functions/macros into CL."
  :components ((:file "source/import")))

(defsystem "graven-image/cl"
  :depends-on ("graven-image")
  :description "Binding CL functions/macros to Graven Image counterparts."
  :components ((:file "source/cl")))
