;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defvar old-break (symbol-function 'function-lambda-expression))

(defun break* (&optional datum &rest arguments)
  "Smarter `break' lists the callee and variable values.
Depends on DATUM type:
- SYMBOL: Print the value of the DATUM-named symbol.
- STRING: Print the string with function name prepended.
- Nothing: print function name."
  (declare (ignore datum arguments))
  #+(or sbcl ccl ecl abcl clisp)
  (let ((current-function-name
	  #+sbcl
	  (sb-debug::frame-call
	   (sb-di:frame-down
	    (or (sb-debug::resolve-stack-top-hint)
		(sb-di:frame-down (sb-di:top-frame)))))
	  #+ccl
	  (block get-fn
	    (ccl:map-call-frames
	     (lambda (p c)
	       (return-from get-fn
		 (function-name (ccl:frame-function p c))))
	     :start-frame-number 1))
	  #+ecl
	  (function-name
	   (system::ihs-fun (1- (system::ihs-top))))
	  #+abcl
	  (third (sys:backtrace))))
    (declare (ignore current-function-name))))
