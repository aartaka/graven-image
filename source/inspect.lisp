;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defun set-field (key value)
  "Set the KEY-ed field to VALUE."
  (let ((prop (find-command-or-prop key nil (funcall *fields-fn* *object*))))
    (cond
      ((and prop (third prop))
       (print (funcall (third prop) value (second prop))))
      (prop
       (format *query-io* "~&Cannot modify this field."))
      (t
       (format *query-io* "~&No such field found.")))))

(defun istep (key)
  "Inspect the object under KEY."
  (uiop:symbol-call :graven-image :%inspect*
                    (second (find-command-or-prop key nil (funcall *fields-fn* *object*)))))

;; Dummy vars.
(defvar *max-field-length*)
(defvar *max-field-index*)

(definterface inspect* *query-io* (object)
  ((*print-lines* (or *print-lines* 20))
   (*summary-fn* #'description*)
   (*fields-fn* #'fields*)
   (*max-field-length*
    (reduce #'max (fields* *object*)
            :key (lambda (f) (length (princ-to-string (first f))))))
   (*max-field-index* (length (fields* *object*)))
   (*print-field-fn* #'(lambda (stream index key value &rest other-args)
                         (format stream "~&[~d]~:[ ~:[~s~;~a~]~;~2*~]~vt =~:[=~;!~]= ~s"
                                 index (integerp key) (symbolp key) key
                                 (+ 4 (floor (log *max-field-index* 10)) *max-field-length*)
                                 (first other-args) value)))
   (*action-fn* #'%inspect*))
  "Interactively query the OBJECT.

OBJECT summary and fields are printed to and
expressions/commands/indices/field names are read from
`*query-io*'.

Fields are paginated, with commands available to scroll.

Influenced by:
- `*query-io*'.
- `*print-lines*' for page size.
- Other printer variables for the display of the field values."
  (:set #'set-field)
  (:modify #'set-field)
  (:istep #'istep)
  (:inspect #'istep)
  (:standard #'standard-print)
  (:aesthetic #'aesthetic-print))

(defun standard-print ()
  "Print the inspected object readably."
  (format *stream* "~&~s" *object*))

(defun aesthetic-print ()
  "Print the inspected object aesthetically."
  (format *stream* "~&~a" *object*))
