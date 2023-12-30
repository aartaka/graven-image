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

(defvar *commands* '()
  "All the commands accessible in the inspector.")
(defvar *inspect-lines* 20
  "Number of fields displayed in one screen.")
(defvar *offset* 0
  "The current offset into the object fields.")

(defun nothing ()
  "Command that does nothing."
  nil)

(defun exit ()
  "Exit the interface."
  (throw 'toplevel (values)))

(defun up ()
  "Go up to the previous level of the interface."
  (throw 'internal (values)))

(defun next-page ()
  "Show the next page of fields (if any)."
  (if (>= (+ *offset* *inspect-lines*) (length (fields* *)))
      (format *query-io* "~&Nowhere to scroll, already at the last page.")
      (progn
        (setf *offset* (+ *offset* *inspect-lines*))
        (print-fields))))

(defun previous-page ()
  "Show the previous page of fields (if any)."
  (if (zerop *offset*)
      (format *query-io* "~&Nowhere to scroll, already at the first page.")
      (progn
        (setf *offset* (max 0 (- *offset* *inspect-lines*)))
        (print-fields))))

(defun home ()
  "Scroll back to the first page of fields."
  (if (zerop *offset*)
      (format *query-io* "~&Nowhere to scroll, already at the first page.")
      (progn
        (setf *offset* 0)
        (description* * *query-io*)
        (print-fields))))

(defun width (new)
  "Change the page size."
  (setf *inspect-lines* new)
  (print-fields))

(defun self ()
  "Show the currently inspected object."
  (description* * *query-io*)
  (print-fields))

(defun evaluate (expression)
  "Evaluate the EXPRESSION."
  (dolist (val (multiple-value-list (eval expression)))
    (print val *query-io*)))

(defun help ()
  "Show the instructions for using this interface."
  (format *query-io*
          "~&This is an interactive interface for ~a~%~
~&Available commands are:
~:{~&~:[~s~*~;~s~{ ~a~}~]~30t~@[~a~]~}

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
          * (mapcar (lambda (command)
                      (destructuring-bind (name function)
                          command
                        (list (function-lambda-list* function)
                              name (function-lambda-list* function)
                              (or (documentation function t)
                                  (ignore-errors
                                   (documentation (function-name function) 'function))))))
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
    (integer (values (if (find key (field-indices fields))
                         (elt fields (position key (field-indices fields)))
                         (warn "No field with key ~s" key))
                     nil))
    (symbol
     (flet ((filter (pred list)
              (remove-if (complement pred) list :key #'first)))
       (let* ((command-matches (filter (lambda (c-key)
                                         (uiop:string-prefix-p key c-key))
                                       commands))
              (symbol-fields (filter #'symbolp fields))
              (field-matches (filter
                              (lambda (f-key)
                                (uiop:string-prefix-p key f-key))
                              symbol-fields)))
         (cond
           ((= 1 (+ (length command-matches) (length field-matches)))
            (values (or (first command-matches)
                        (first field-matches))
                    (null field-matches)))
           ((plusp (+ (length command-matches) (length field-matches)))
            ;; Inspired by SBCL.
            (warn "Several matches found with prefix ~s:~@[~&Commands: ~{~A~^, ~}~]~@[~&Fields: ~{~A~^, ~}~]"
                  key (mapcar #'first command-matches) (mapcar #'first field-matches))
            (values `(:nothing ,#'nothing) t))
           (t
            (warn "No property with name ~s" key)
            (values `(:nothing ,#'nothing) t))))))
    (t (find key fields :key #'first :test #'equal))))

(defun $ (&rest keys)
  "Return a list of values for fields under KEYS.
Useful inside an interface to query the values of the object one's
interacting with."
  (mapcar (lambda (key)
            (second (find-command-or-prop key nil (fields* *))))
          keys))

(defun @@ (&rest keys)
  "Alias for `$'"
  (apply #'$ keys))

(defun read-maybe-spaced (stream)
  (let ((first (read-line stream nil "")))
    (if (equal "" first)
        `(:next)
        (loop with string = first
              for error
                = (nth-value 1 (ignore-errors
                                (with-input-from-string (str-stream string)
                                  (uiop:slurp-stream-forms str-stream))))
              while error
              do (setf string (uiop:strcat string #\Newline (read-line stream)))
              finally (return
                        (with-input-from-string (str-stream string)
                          (uiop:slurp-stream-forms str-stream)))))))

(defun set-field (key value)
  "Set the KEY-ed field to VALUE."
  (let ((prop (find-command-or-prop key nil (fields* *))))
    (cond
      ((and prop (third prop))
       (print (funcall (third prop) value (second prop))))
      (prop
       (format *query-io* "~&Cannot modify this field."))
      (t
       (format *query-io* "~&No such field found.")))))

(defun istep (key)
  "Inspect the object under KEY."
  (%inspect
   (second (find-command-or-prop key nil (fields* *)))))

(defun print-fields ()
  (loop with fields = (fields* *)
        with length = (length fields)
        with max-field-length
          = (reduce #'max fields :key (lambda (f) (length (princ-to-string (first f)))))
        with real-page-len = (min length (+ *offset* *inspect-lines*))
        for index from *offset* below real-page-len
        for (key value . args) in (subseq fields *offset*)
        do (format *query-io* "~&[~d]~:[ ~:[~s~;~a~]~;~2*~]~vt =~:[=~;!~]= ~s"
                   index (integerp key) (symbolp key) key
                   (+ 4 (floor (log length 10)) max-field-length)
                   (first args) value)
        finally (if (= real-page-len length)
                    (format *query-io* "~&[Last page, fields ~d-~d out of ~d]"
                            *offset* (1- length) (1- length))
                    (format *query-io* "~&[Showing fields ~d-~d out of ~d]"
                            *offset* (1- real-page-len) (1- length)))))

(define-generic %inspect (object)
  "Internal function for `inspect*'."
  (catch 'internal
    (let* ((* object)
           (@ object)
           #+ccl (ccl:@ object)
           (fields (fields* @))
           (*offset* 0)
           (*inspect-lines* (or *inspect-lines*
                                (parse-integer (uiop:getenv "LINES") :junk-allowed t)
                                20))
           (*commands*
             `((:? ,#'help)
               (:help ,#'help)
               (:quit ,#'exit)
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
               (:back ,#'up)
               (:set ,#'set-field)
               (:modify ,#'set-field)
               (:istep ,#'istep)
               (:inspect ,#'istep)
               (:standard ,#'standard-print)
               (:aesthetic ,#'aesthetic-print))))
      (description* object *query-io*)
      (print-fields)
      (loop
        (format *query-io* "~&~a> " 'inspect*)
        (finish-output *query-io*)
        (let ((forms (read-maybe-spaced *query-io*)))
          (multiple-value-bind (result command-p)
              (find-command-or-prop (first forms) *commands* fields)
            (restart-case
                (cond
                  ((and result command-p)
                   (apply (second result)
                          (mapcar #'eval (rest forms))))
                  ((and result (not command-p))
                   (%inspect (second result))
                   (description* object *query-io*)
                   (print-fields))
                  (t (dolist (val (multiple-value-list (eval (first forms))))
                       (print val *query-io*))))
              (back-to-inspect ()
                :report (lambda (s)
                          (format s "Get back to inspection of ~a" object))
                nil))))))))

(define-generic inspect* (object)
  "Interactively query the OBJECT.

OBJECT summary and fields are printed to and
expressions/commands/indices/field names are read from
`*query-io*'.

Fields are paginated, with commands available to scroll.

Influenced by:
- `*query-io*'.
- `*inspect-lines*' for page size.
- Other printer variables for the display of the field values."
  (catch 'toplevel
    (loop
      (%inspect object)
      (when (yes-or-no-p* "Quit inspection of ~s?"  object)
        (throw 'toplevel (values))))))

(defun standard-print ()
  "Print the inspected object readably."
  (format *query-io* "~&~s" *))

(defun aesthetic-print ()
  "Print the inspected object aesthetically."
  (format *query-io* "~&~a" *))
