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
used for OBJECT info."
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
          (loop for (name value) in (fields* object)
                do (if (symbolp name)
                       (format stream "~&~a = ~s~%" name value)
                       (format stream "~&~s = ~s~%" name value)))))
    (if (typep stream 'string-stream)
        (get-output-stream-string stream)
        (values))))
