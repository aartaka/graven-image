;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(define-generic describe* (object &optional (stream t) respect-methods)
  "Display OBJECT information to a STREAM.

Shows a summary of OBJECT features and then lists all the properties
OBJECT has.

STREAM could be:
- T --- information is printed to *STANDARD-OUTPUT*.
- NIL --- information is printed to a string and this string is
  returned from DESCRIBE*.
- Any stream --- information is printed there.

DESCRIBE-OBJECT methods are honored and used when RESPECT-METHODS is
true. Otherwise (NIL), a regular summary+properties structure is used
for OBJECT info.

Affected by:
- `*standard-output*'.
- `fields*' and `description*' methods on OBJECT.
- `describe-object' methods (when RESPECT-METHODS.)
- Printer variables for the display of the field values.

Example from the spec:
\(defclass spaceship ()
  ((captain :initarg :captain :accessor spaceship-captain)
   (serial# :initarg :serial-number :accessor spaceship-serial-number)))

\(defclass federation-starship (spaceship) ())

\(defmethod describe-object ((s spaceship) stream)
  (with-slots (captain serial#) s
    (format stream \"~&~S is a spaceship of type ~S,~
                     ~%with ~A at the helm ~
                       and with serial number ~D.~%\"
            s (type-of s) captain serial#)))

\(defvar ship (make-instance 'federation-starship
                             :captain \"Rachel Garrett\"
                             :serial-number \"NCC-1701-C\"))

\(describe ship)
;; #<FEDERATION-STARSHIP {100456B353}> is a spaceship of type FEDERATION-STARSHIP,
;; with Rachel Garrett at the helm and with serial number NCC-1701-C.

;; Respect `describe-object' methods.
\(describe* ship t t)
;; #<FEDERATION-STARSHIP {100456B353}> is a spaceship of type FEDERATION-STARSHIP,
;; with Rachel Garrett at the helm and with serial number NCC-1701-C.

;; Ignore methods.
\(describe* ship)
;; Federation-starship #<FEDERATION-STARSHIP {100456B353}>
;; ID                = 68792267603
;; CLASS             = #<STANDARD-CLASS COMMON-LISP-USER::FEDERATION-STARSHIP>
;; SLOT-DEFINITIONS  = (#<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION COMMON-LISP-USER::CAPTAIN>
;;                      #<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION COMMON-LISP-USER::|SERIAL#|>)
;; TYPE              = FEDERATION-STARSHIP
;; SERIAL#           = \"NCC-1701-C\"
;; CAPTAIN           = \"Rachel Garrett\""
  (let* ((stream (etypecase stream
                   (null (make-string-output-stream))
                   ((eql t) *standard-output*)
                   (stream object)))
         (method-p (find-if
                    (lambda (m)
                      (subtypep (class-name (class-of object))
                                (class-name (first (method-specializers m)))))
                    (generic-function-methods #'describe-object))))
    (if (and method-p respect-methods)
        (funcall #'describe-object object stream)
        (progn
          (fresh-line stream)
          (description* object stream)
          (fresh-line stream)
          (loop with fields = (remove :self (fields* object) :key #'first)
                with max-tab = (1+ (reduce #'max fields
                                           :key #'(lambda (f) (length (princ-to-string (first f))))))
                for (name value) in fields
                do (if (symbolp name)
                       (format stream "~&~a~vt = ~s~%" name max-tab value)
                       (format stream "~&~s~vt = ~s~%" name max-tab value)))))
    (if (typep stream 'string-stream)
        (get-output-stream-string stream)
        (values))))
