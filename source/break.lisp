;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defvar old-break (symbol-function 'break))

(define-generic break* (&optional datum &rest arguments)
  "Smarter `break' lists the callee and variable values.
Depends on DATUM type:
- SYMBOL: Print the value of the DATUM-named symbol.
- STRING: Print the string with function name prepended.
- Nothing: print function name."
  (declare (ignorable datum arguments))
  #+(or sbcl ccl ecl abcl)
  (let* ((current-function-name
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
	   (third (sys:backtrace)))
	 (format-string
	   (with-output-to-string (s)
	     (when current-function-name
	       (format s "~s: " current-function-name))
	     (if (null datum)
		 (format s "Break")
		 (let* ((all-args (cons datum arguments))
			(symbols (loop for a in all-args
				       while (symbolp a)
				       collect a))
			(rest (ignore-errors
			       (subseq all-args (length symbols)))))
		   (when symbols
		     (format s "~:{~s=~s~^, ~} "
			     (mapcar (lambda (s)
				       (list s (symbol-value s)))
				     symbols)))
		   (when rest
		     (apply #'format s rest))))
	     (fresh-line s))))
    (funcall old-break format-string))
  #-(or sbcl ccl ecl abcl)
  (apply old-break
	 (when datum
	   (cons datum arguments))))
