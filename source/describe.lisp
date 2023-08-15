;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(define-generic describe* (object &optional (stream t) ignore-methods)
  "Display OBJECT information to a STREAM.

Shows a summary of OBJECT features and then lists all the properties
OBJECT has.

STREAM could be:
- T --- information is printed to *STANDARD-OUTPUT*.
- NIL --- information is printed to a string and this string is
  returned from DESCRIBE*.
- Any stream --- information is printed there.

DESCRIBE-OBJECT methods are honored and used, unless IGNORE-METHODS is
true. If IGNORE-METHODS, a regular summary+properties structure is
used for OBJECT info.

Influenced by:
- `*standard-output*'.
- `print-object' method for OBJECT.
- Printer variables for the display of the field values.
- `fields*' and `description*' methods on OBJECT.
- `describe-object' methods (unless IGNORE-METHODS.)"
  (let* ((stream (etypecase stream
                   (null (make-string-output-stream))
                   ((eql t) *standard-output*)
                   (stream object)))
         (describe-object-method (find-method #'describe-object '()
                                              (list (class-of object) (class-of stream)) nil)))
    (if (and describe-object-method
             (not ignore-methods))
        (funcall #'describe-object object stream)
        (progn
          (fresh-line stream)
          (description* object stream)
          (fresh-line stream)
          (loop with fields = (fields* object)
                with max-tab = (1+ (reduce #'max fields
                                           :key #'(lambda (f) (length (princ-to-string (first f))))))
                for (name value) in fields
                do (if (symbolp name)
                       (format stream "~&~a~vt = ~s~%" name max-tab value)
                       (format stream "~&~s~vt = ~s~%" name max-tab value)))))
    (if (typep stream 'string-stream)
        (get-output-stream-string stream)
        (values))))
