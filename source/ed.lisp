;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defvar %ed-mode :lines
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

(defun ed-print-forms (&optional (to-scroll *ed-lines*))
  (let ((to-scroll (min to-scroll (- (length %^) %^-index))))
    (if (<= to-scroll 1)
        (warn "Nowhere to scroll, already at the last form.")
        (dotimes (i to-scroll)
          (format *query-io* (if (eq %ed-mode :lines)
                                 "~&~vd: ~a"
                                 "~&~vd: ~s")
                  (floor (log (+ *ed-lines* %^-index) 10))
                  (+ i %^-index)
                  (elt %^ (+ %^-index i)))))))

(defun ed-zoom ()
  "Scroll the editor window, printing the forms."
  (ed-print-forms)
  (setf %^-index
        (min (1- (length %^))
             (+ %^-index *ed-lines*))))

(defun ed-nothing ()
  "Command that does nothing."
  nil)

(defun ed-help ()
  "Show the instructions for using the editor."
  (format *query-io*
        "~&This is ED*~%~
~&Available commands are:
~:{~&~:[~s~*~;~s~{ ~a~}~]~30t~@[~a~]~}

Possible inputs are:
- Blank line: scroll down.
- Mere symbols: run one of the commands above, matching the symbol.
  - If no command matches, evaluate the symbol.
- Evaluate the provided code otherwise.~%"
        (mapcar (lambda (command)
                  (destructuring-bind (name function)
                      command
                    (list (function-lambda-list* function)
                          name (function-lambda-list* function)
                          (or (documentation function t)
                              (ignore-errors
                               (documentation (function-name function) 'function))))))
                *ed-commands*)))

(defun ed-exit ()
  "Exit the editor."
  (throw 'toplevel (values)))

(defun ed-print ()
  "Print the current form."
  (funcall
   (if (eq %ed-mode :forms)
       #'print
       #'princ)
   (elt %^ %^-index) *query-io*))

(defun ed-next ()
  "Go to the next form."
  (when (< %^-index (1- (length %^)))
    (incf %^-index))
  (ed-print))

(defun ed-previous ()
  "Go to the next form."
  (unless (zerop %^-index)
    (decf %^-index))
  (ed-print))

(defun ed-into ()
  "Edit the current form."
  (if (and (eq %ed-mode :forms)
           (listp (elt %^ %^-index)))
      (%ed (elt %^ %^-index))
      (warn "Cannot edit into ~s" (elt %^ %^-index))))

(defun ed-out ()
  "Leave the current editor level."
  (throw 'inner t))

(defun ed-append ()
  "Add more forms/lines after the current one.
In case of :FORMS, reads one s-expression and appends it.
In case of :LINES, reads several lines until the line with a dot on
its own. Appends the lines read to the buffer."
  (let ((to-append
          (ecase %ed-mode
            (:forms (list (read *query-io*)))
            (:lines (loop for line = (read-line *query-io*)
                          until (equal line ".")
                          collect line)))))
    (setf (cdr (nthcdr %^-index %^))
          (append to-append (cdr (nthcdr %^-index %^))))))

(defun ed-change ()
  "Add more forms/lines, replacing the current ones, if present."
  (let ((to-add
          (ecase %ed-mode
            (:forms (list (read *query-io*)))
            (:lines (loop for line = (read-line *query-io*)
                          until (equal line ".")
                          collect line)))))
    (setf (subseq %^ %^-index)
          (append to-add (subseq %^ (+ (length to-add) %^-index))))))

(defun ed-delete ()
  "Remove the current form/line."
  (setf %^
        (append (subseq %^ 0 %^-index)
                (cdr (nthcdr %^-index %^)))))

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
            (:zoom ,#'ed-zoom)
            (:print ,#'ed-print)
            (:next ,#'ed-next)
            (:forward ,#'ed-next)
            (:previous ,#'ed-previous)
            (:back ,#'ed-previous)
            (:into ,#'ed-into)
            (:down ,#'ed-into)
            (:edit ,#'ed-into)
            (:descend ,#'ed-into)
            (:out ,#'ed-out)
            (:pop ,#'ed-out)
            (:up ,#'ed-out)
            (:ascend ,#'ed-out)
            (:append ,#'ed-append)
            (:suffix ,#'ed-append)
            (:annex ,#'ed-append)
            (:change ,#'ed-change)
            (:replace ,#'ed-change)
            (:modify ,#'ed-change)
            (:rewrite ,#'ed-change)
            ;; Kill/cut/copy?
            (:delete ,#'ed-delete)
            (:kill ,#'ed-delete)
            (:remove ,#'ed-delete))))
    (ed-print-forms)
    (catch 'inner
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
            (integer
             (setf %^-index (min head (1- (length %^))))
             (ed-print))
            (t (dolist (val (multiple-value-list (eval head)))
                 (print val *query-io*)))))))))

(defgeneric %ed (object)
  (:method ((object string))
    (let* ((%^ (uiop:split-string object :separator '(#\Newline)))
           (%^-index 0)
           (%ed-mode :lines))
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
