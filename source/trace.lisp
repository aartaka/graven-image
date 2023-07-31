;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defun trace-function (function
                       &key report
                         if-all if-before if-after ;; condition/cond
                         ;; CLISP: pre-break-if/post-break-if
                         break-all break-before break-after
                         ;; CLISP: pre-print, post-print, print
                         print-all print-before print-after
                         backtrace-all backtrace-after backtrace-before
                         eval-all eval-before eval-after ;; CLISP: pre/post
                         ;; wherein/inside? encapsulate? methods?
                         ;; report? define-if-not? step/step-if?
                         ;; suppress-if?  max-depth? bindings?
                       &allow-other-keys)
  nil)

(defun trace-list ()
  nil)

(defmacro trace* (&rest args)
  (if args
      (multiple-value-bind (specs args)
          (process-trace-args args)
        `(progn
           ,@(loop for spec in specs
                   collect `(trace-function (quote ,spec) ,@args))))
      `(trace-list)))

(defun process-trace-args (args)
  (loop with skip = nil
        for (arg . rest) on args
        until (and (not arg) (not skip))
        if skip
          do (setf skip nil)
        else if (eq :function arg)
               do (identity nil)
        else if (keywordp arg)
               append (let ((val (first rest)))
                        (case arg
                          ((:if :cond :condition :print :break :backtrace)
                           (warn "Ambiguous use of ~a keyword (CCL and SBCL have different interpretations.)
Ignoring it.
Use ~a-BEFORE or ~a-ALL instead." arg arg arg)
                           nil)
                          ((:cond-before :condition-before :before-if) (list :if-before val))
                          ((:cond-after :condition-after :after-if) (list :if-after val))
                          (:supress-if (list :if-all `(not ,val)))
                          (:pre (list :eval-before val))
                          (:post (list :eval-after val))
                          (:pre-print (list :print-before val))
                          (:post-print (list :print-after val))
                          (:pre-break-if (list :break-before val))
                          (:post-break-if (list :break-after val))
                          (t (list arg val))))
                 into options
                 and do (setf skip t)
        else
          collect arg into specs
        finally (return (values specs options))))
