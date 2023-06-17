;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defun restarts (condition)
  (mapcar (lambda (restart)
            (list (restart-name restart) restart))
          (compute-restarts condition)))

(defun print-restart (stream index key value &rest args)
  (declare (ignore args))
  (format stream "~&[~d~@[: ~s~]] ~a" index (or key value) value))

(defun backtrace ()
  "Inspect condition and its backtrace frames."
  (inspect* *object*))

(defun continue-restart (&optional restart)
  (if restart
      (invoke-restart-interactively restart)
      (prog1
          (continue)
        (format *stream* "No CONTINUE restart, doing nothing."))))

(definterface debugger* *debug-io* (condition)
  ((*print-lines* (or *print-lines* 20))
   (*summary-fn* #'description*)
   (*fields-fn* #'restarts)
   (*print-field-fn* #'print-restart)
   (*action-fn* #'invoke-restart-interactively))
  "Open the CONDITION in the debugger to handle it.

Debugger lists the available restarts and allows to act on the
condition and its environment."
  (:inspect #'backtrace)
  (:backtrace #'backtrace)
  (:continue #'continue-restart)
  (:restart #'continue-restart)
  (:go #'continue-restart))
