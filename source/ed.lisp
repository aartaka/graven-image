;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defvar %ed-mode :line
  "The current editing mode `ed*' is in.
:LINES is line editing, `%^' is a list of string.
:FORMS is s-expressions editing, and `%^' is a list of expressions.")
(defvar %^ nil)
(defvar %^-index nil)

(defvar ^ nil
  "Currently `ed*'-ited object.")
(defvar *ed-lines* 20
  "Number of expressions/lines to zoom through.")
(defvar *ed-commands* 20
  "Commands for editing.")

(defun ed-print-forms ()
  (dotimes (i (min *ed-lines* (length %^)))
    (format *query-io* (if (eq %ed-mode :line)
                           "~&~vd: ~a"
                         "~&~vd: ~s")
            (floor (log (+ *ed-lines* %^-index) 10))
            (+ i %^-index)
            (elt %^ (+ %^-index i)))))

(defun ed-zoom ()
  (ed-print-forms)
  (incf %^-index *ed-lines*))

(defun ed-nothing ()
  "Command that does nothing."
  nil)

(defun ed-help ()
  "Does nothing at the moment")

(defun ed-exit ()
  "Exit the interface."
  (throw 'toplevel (values)))

(defun %%ed ()
  (let ((*ed-lines* (or *ed-lines*
                        (parse-integer (uiop:getenv "LINES") :junk-allowed t)
                        10))
        (*ed-commands*
          `((:? ,#'ed-help)
            (:help ,#'ed-help)
            (:quit ,#'ed-exit)
            (:exit ,#'ed-exit)
            (:scroll ,#'ed-zoom)
            (:zoom ,#'ed-zoom))))
    (ed-print-forms)
    (loop
      (format *query-io* "~&~a> " 'ed*)
      (finish-output *query-io*)
      (let* ((forms (read-maybe-spaced *query-io*))
             (head (first forms)))
        (typecase head
          (keyword (let ((matching-commands
                           (remove-if (complement (lambda (c-key)
                                                    (uiop:string-prefix-p head c-key)))
                                      *ed-commands* :key #'first)))
                     (if matching-commands
                         (apply (cadar matching-commands)
                                (rest forms))
                         (print head))))
          (integer (setf %^-index head))
          (t (dolist (val (multiple-value-list (eval head)))
               (print val *query-io*))))))))

(defgeneric %ed (object)
  (:method ((object string))
    (let* ((%^ (uiop:split-string object :separator '(#\Newline)))
           (%^-index 0)
           (%ed-mode :line))
      (%%ed)))
  (:method ((object cons))
    (let* ((%^ object)
           (%^-index 0)
           (%ed-mode :forms))
      (%%ed))))

(defmacro call-with-ed-toplevel (&rest object)
  `(let ((^ ,@object))
     (catch 'toplevel
       (loop
         (%ed ^)
         (when (yes-or-no-p* "Quit editing of ~s?" ^)
           (throw 'toplevel (values)))))))

(defgeneric ed* (object)
  (:documentation "Edit the OBJECT interactively.

OBJECT can be either of:
- String.
- Pathname.
- List.
- Function.
- Symbol.

If the OBJECT is a string or pathname to non-Lisp file, edit in the
line mode akin to UNIX ed. Also activated when TYPE is :STRING.

If the OBJECT is a list, function object, function name, or Lisp file
pathname, edit in s-expr-aware mode.")
  (:method ((object string))
    (call-with-ed-toplevel object))
  (:method ((object pathname))
    (call-with-ed-toplevel
        (if (member (pathname-type object)
                    '("lisp" "lsp" "cl" "l") ;; Scheme, Racket etc. too?
                    :test #'string-equal)
            (uiop:read-file-forms object)
            (uiop:read-file-string))))
  (:method ((object cons))
    (call-with-ed-toplevel object))
  (:method ((object function))
    (if (function-lambda-expression* object)
        (call-with-ed-toplevel
            (function-lambda-expression* object))
        (warn "Cannot find the source for ~a" object)))
  (:method ((object symbol))
    (cond
      ((null object)
       (warn "Cannot edit NIL"))
      ((null (function-lambda-expression* object))
       (warn "Cannot find the source for ~a" object))
      (t (call-with-ed-toplevel
             (function-lambda-expression* object))))))