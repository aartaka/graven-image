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
  :depends-on ("closer-mop" "trivial-gray-streams" #+sbcl "sb-introspect")
  :components ((:file "package")
               (:file "graven-image")
               (:file "function-lambda-expression")
               (:file "yes-or-no")
               (:file "apropos")
               (:file "time")
               (:file "room")
               (:file "fields")
               (:file "description")
               (:file "interface")
               (:file "describe")
               (:file "inspect")
               (:file "dribble")
               (:file "documentation")))
