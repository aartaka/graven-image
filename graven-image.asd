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
  :depends-on ("closer-mop" "dissect")
  :components ((:file "package")
               (:file "graven-image")
               (:file "function-lambda-expression")
               (:file "yes-or-no")
               (:file "apropos")
               (:file "time")
               (:file "fields")
               (:file "interface")
               (:file "describe")
               (:file "inspect")
               (:file "dribble")
               (:file "debugger"))
  :in-order-to ((test-op (test-op "graven-image/tests"))))

(defsystem "graven-image/cl"
  :depends-on ("graven-image")
  :description "Binding CL functions/macros to Graven Image counterparts."
  :components ((:file "source/cl")))

(defsystem "graven-image/tests"
  :depends-on ("graven-image" "lisp-unit2")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (o c)
                    (if *debugger-hook*
                        (symbol-call :lisp-unit2 :run-tests
                                     :package :graven-image/tests
                                     :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))
                        (let* ((*debugger-hook* nil)
                               (test-results (symbol-call :lisp-unit2 :run-tests
                                                          :package :graven-image/tests
                                                          :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
                          (when (or
                                 (uiop:symbol-call :lisp-unit2 :failed test-results)
                                 (uiop:symbol-call :lisp-unit2 :errors test-results))
                            ;; Arbitrary but hopefully recognizable exit code.
                            (quit 18))))))
