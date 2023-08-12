;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(-> field-indices (list) list)
(defun field-indices (fields)
  "Map integer indices to every property in FIELDS.
Implies that FIELDS have a (KEY VALUE . ARGS) structure
Non-trivial, because some of the FIELDS have integer keys."
  (loop with taken = (remove-if-not #'integerp (mapcar #'first fields))
        for (name) in fields
        for index from 0
        when (integerp name)
          collect name
        else
          collect (loop for i from index
                        while (member i taken)
                        finally (return (prog1
                                            i
                                          (setf index i))))))

(defvar *object* nil
  "The object currently inspected.")
(defvar *stream* nil
  "The bidirectional stream to read/write to.")
(defvar *summary-fn* nil
  "The (OBJECT STREAM) function to print OBJECT summary to STREAM.")
(defvar *fields-fn* nil
  "The function to return OBJECT fields printable into interface.")
(defvar *print-field-fn*
  "The (STREAM INDEX KEY VALUE &REST ARGS) function to print a singular field of the `*object*'.")
;; TODO: Allow action to be marking or cursor movement.
(defvar *action-fn*
  "The (VALUE) function that does whatever action is suitable for the property value in the current context.
For inspector, that's a recursive inspection.")
(defvar *length* nil
  "Total length of the object fields.")
(defvar *offset* 0
  "The current offset into the object fields.")

(defun print-fields ()
  "Print the current page of fields."
  (loop with fields = (funcall *fields-fn* *object*)
        with real-page-len = (min *length* (+ *offset* *print-lines*))
        for index from *offset* below real-page-Len
        for (key value . args) in (subseq fields *offset*)
        do (apply *print-field-fn* *stream* index key value args)
        finally (unless (= real-page-len *length*)
                  (format *stream* "~&[Showing fields ~d-~d out of ~d]"
                          *offset* (1- real-page-len) (1- *length*)))))

(defun summarize ()
  (funcall *summary-fn* *object* *stream*))

(defun exit ()
  "Exit the interface."
  (throw 'toplevel (values)))

(defun up ()
  "Go up to the previous level of the interface."
  (throw 'internal (values)))

(defun next-page ()
  "Show the next page of fields (if any)."
  (if (>= (+ *offset* *print-lines*) *length*)
      (format *stream* "~&Nowhere to scroll, already at the last page.")
      (setf *offset* (+ *offset* *print-lines*)))
  (print-fields))

(defun previous-page ()
  "Show the previous page of fields (if any)."
  (if (zerop *offset*)
      (format *stream* "~&Nowhere to scroll, already at the first page.")
      (setf *offset* (max 0 (- *offset* *print-lines*))))
  (print-fields))

(defun home ()
  "Scroll back to the first page of fields."
  (if (zerop *offset*)
      (format *stream* "~&Nowhere to scroll, already at the first page.")
      (setf *offset* 0))
  (summarize)
  (print-fields))

(defun width (new)
  "Change the page size."
  (setf *print-lines* new)
  (print-fields))

(defun self ()
  "Show the currently inspected object."
  (summarize)
  (print-fields))

(defun evaluate (expression)
  "Evaluate the EXPRESSION."
  (dolist (val (multiple-value-list (eval expression)))
    (print val *stream*)))

(defvar *commands*
  `((:quit ,#'exit)
    (:exit ,#'exit)
    (:length ,#'width)
    (:width ,#'width)
    (:widen ,#'width)
    (:next ,#'next-page)
    (:previous ,#'previous-page)
    (:print ,#'print-fields)
    (:page ,#'print-fields)
    (:home ,#'home)
    (:reset ,#'home)
    (:top ,#'home)
    (:this ,#'self)
    (:self ,#'self)
    (:redisplay ,#'self)
    (:show ,#'self)
    (:current ,#'self)
    (:again ,#'self)
    (:eval ,#'evaluate)
    (:up ,#'up)
    (:pop ,#'up)
    (:back ,#'up))
  "Alist of commands accessible to the current interface.")

(defun help ()
  "Show the instructions for using this interface."
  (format *stream*
          "~&This is an interactive interface for ~a~%~
~&Available commands are:
~:{~&~:[~s~*~;(~s~{ ~a~})~]~30t~@[~a~]~}

Possible inputs are:
- Blank line: scroll the listing down.
- Mere symbols: run one of the commands above, matching the symbol.
  - If there's no matching command, then match against fields.
    - If nothing matches, evaluate the symbol.
- Integer: act on the field indexed by this integer.
  - If there are none, evaluate the integer.
- Any other atom: find the field with this atom as a key.
  - Evaluate it otherwise.
- S-expression: match the list head against commands and fields,
  as above.
  - If the list head does not match anything, evaluate the
    s-expression.
  - Inside this s-expression, you can use the `$' function to fetch
    the list of values under provided keys.~%"
          *object* (mapcar (lambda (command)
                             (destructuring-bind (name function)
                                 command
                               (list (function-lambda-list* function)
                                     name (function-lambda-list* function)
                                     (or (documentation function t)
                                         (ignore-errors
                                          (documentation (function-name function) 'function))))))
                           *commands*)))

(unless (find :help *commands* :key #'first)
  (setf *commands*
        (append
         `((:? ,#'help)
           (:help ,#'help))
         *commands*)))

(defun find-command-or-prop (key commands fields)
  "Find the KEY in COMMANDS/FIELDS by its prefix/value.

Returns two values:
- The field/command matching the KEY, as a list.
- Whether the found thing is a command (= member of COMMANDS).

Search is different for different KEY types:
- Integer: only search FIELDS by their indices.
- SYMBOL: search both commands and fields, but only by symbol
  names.
- Anything else: search literal object."
  (typecase key
    (integer (values (elt fields (position key (field-indices fields))) nil))
    (symbol
     (loop for match in (append commands fields)
           for (match-key) = match
           when (and (symbolp match-key)
                     (uiop:string-prefix-p (symbol-name key) (symbol-name match-key)))
             do (return (values match (member match commands)))))
    (t (find key fields :key #'first :test #'equal))))

(defun $ (&rest keys)
  "Return a list of values for fields under KEYS.
Useful inside an interface to query the values of the object one's
interacting with."
  (let ((fields (funcall *fields-fn* *object*)))
    (mapcar (lambda (key)
              (second (find-command-or-prop key nil fields)))
            keys)))

(defun read-maybe-spaced (stream)
  (let ((first (read-line stream nil "")))
    (if (equal "" first)
        `(:next)
        (loop with string = first
              for (forms error)
                = (multiple-value-list
                   (ignore-errors
                    (with-input-from-string (str-stream string)
                      (uiop:slurp-stream-forms str-stream))))
              while error
              do (setf string (uiop:strcat string #\Newline (read-line stream)))
              finally (return
                        (with-input-from-string (str-stream string)
                          (uiop:slurp-stream-forms str-stream)))))))

(defmacro definterface (name stream (object)
                        ((var val) &rest vars+vals)
                        documentation
                        &body key+commands)
  "Create an interactive interface for NAME function.
The interface is centered around the OBJECT-named argument.

Generates the internal function named %NAME, which does most of the
book-keeping, like reading from STREAM and dispatching `*commands*'.

The body of the DEFINTERFACE is the list of (KEY COMMAND) pairs to add

Provide `*summary-fn*', `*fields-fn*', and `*print-field-fn*' to list
in the interface. Good examples for `*summary-fn*' and `*fields-fn*'
are `description*' and `fields*' (respectively) for the
inspector."
  (let ((internal-name (intern (uiop:strcat "%" (symbol-name name)) (symbol-package name)))
        (vars-vals (cons (list var val) vars+vals)))
    `(progn
       (define-generic ,internal-name (,object)
         ,(format nil "Internal function for ~a." name)
         (catch 'internal
           (let* ((*object* ,object)
                  (*stream* ,stream)
                  (*commands*
                    (append
                     ;; Override old commands, if necessary.
                     (remove-if (lambda (name) (member name (list ,@(mapcar #'first key+commands))))
                                *commands* :key #'first)
                     (list ,@(loop for (key command) in key+commands
                                   collect `(list ,key ,command)))))
                  ,@(loop for (name initvalue) in vars-vals
                          collect `(,name ,initvalue))
                  (fields (funcall *fields-fn* *object*))
                  (*length* (length fields)))
             (summarize)
             (print-fields)
             (loop
               (format *stream* "~&~a> " (quote ,name))
               (finish-output *stream*)
               (let ((forms (read-maybe-spaced *stream*)))
                 (multiple-value-bind (result command-p)
                     (find-command-or-prop (first (uiop:ensure-list (first forms)))
                                           *commands* (when (not (listp (first forms)))
                                                        fields))
                   (format t "~&Results of find-command-or-prop are ~s and ~s" result (not (not command-p)))
                   (restart-case
                       (cond
                         ((and result command-p)
                          (apply (second result)
                                 (mapcar #'eval (rest forms))))
                         ((and result (not command-p))
                          (funcall *action-fn* (second result))
                          (summarize)
                          (print-fields))
                         (t (dolist (val (multiple-value-list (eval (first forms))))
                              (print val *query-io*))))
                     (back-to-interface ()
                       :report (lambda (s)
                                 (format s "~&Get back to ~s interface" (quote ,name)))
                       nil))))))))
       (define-generic ,name (,object)
         ,documentation
         (catch 'toplevel
           (loop
             (,internal-name ,object)
             (when (yes-or-no-p* "Exit the ~a for ~s?" (quote ,name) ,object)
               (throw 'toplevel (values)))))))))
