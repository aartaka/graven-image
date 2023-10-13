;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)


(-> %break (t list (cons string *)))
(defun %break (current-fn-name variables args)
  (let ((max-var-length (reduce #'max
                                variables
                                :initial-value 0
                                :key (lambda (var)
                                       (length (format nil "~s" (first var)))))))
    (with-output-to-string (s)
      (when current-fn-name
        (format s "In ~s: " current-fn-name))
      (when variables
        (loop for (var val) in variables
              do (format s "~& ~s~vt= ~s~%" var (+ 2 max-var-length) val)))
      (apply #'format s args))))

(defmacro current-frame-name ()
  (quote
   #+sbcl
   (sb-debug::frame-call
    (or (sb-debug::resolve-stack-top-hint)
        (sb-di:frame-down (sb-di:top-frame))))
   #+ccl
   (block get-fn
     (ccl:map-call-frames
      (lambda (p c)
        (return-from get-fn
          (ignore-errors
           (function-name (ccl:frame-function p c)))))
      :start-frame-number 0))
   #+ecl
   (ignore-errors
    (function-name
     (system::ihs-fun (system::ihs-top))))
   #+abcl
   (first (sys:frame-to-list (second (sys:backtrace))))
   #-(or sbcl ccl ecl abcl)
   nil))

(defmacro break* (&rest arguments)
  "A more useful wrapper around `break'.
- Lists the name for the function/block `break*' is called from.
- Lists symbol values when the quoted symbols are provided before the
  actual `break' arguments.
- And all the rest is regular `break' arguments.

Affected by:
- `break' implementation.
- Backtrace fetching support.

Examples:

;; Compatible with the old BREAK:
\(break*)
;; = (break)
\(break* \"hello\")
;; = (break \"hello\")
\(break* \"Format string with ~a value\" \"string\")
;; = (break \"Format string with ~a value\" \"string\")

;; Often lists the function it's called inside of:
\(defun bar () (break*))
\(bar)
;; In BAR: Break

;; And allows listing symbol values:
\(defun foo (a b c)
  (break* 'a 'b 'c \"Testing arguments\"))
\(foo 1 2 3)
;; In FOO: A=1 B=2 C=3 Testing arguments"
  (let* ((symbols (loop for a in arguments
			            while (and (consp a)
				                   (eq 'quote (car a)))
			            collect (second a)))
	     (rest (or (ignore-errors
		            (subseq arguments (length symbols)))
		           (list "Break"))))
    `(break
      (%break
       (current-frame-name)
       (list ,@(mapcar (lambda (s)
                         `(list (quote ,s) ,s))
                       symbols))
       (list ,@rest)))))
