;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defmethod documentation ((x symbol) (doc-type (eql t)))
  (macrolet ((doc (type)
               `(ignore-errors (documentation x (quote ,type)))))
    (when x
      (or (ignore-errors
           (documentation (macro-function x) t))
          (doc function)
          (ignore-errors
           (documentation (fdefinition x) t))
          (ignore-errors
           (documentation (symbol-function x) t))
          (doc variable)
          (doc type)
          (ignore-errors
           (documentation (find-class x) t))
          (doc structure)
          (ignore-errors
           (documentation (find-package x) t))
          (doc compiler-macro)
          (doc setf)
          (doc method-combination)))))

(defmacro safe-doc (val &optional (type t))
  `(ignore-errors (documentation ,val ,type)))

(defmethod (setf documentation) (value (x symbol) (doc-type (eql t)))
  (macrolet ((doc (type)
               `(ignore-errors (documentation x (quote ,type))))
             (set-doc (type)
               `(setf (documentation x (quote ,type))
                      value)))
    (cond
      ((null x) nil)
      ((or (doc function)
           (safe-doc (fdefinition x))
           (safe-doc (symbol-function x)))
       (set-doc function)
       (setf (documentation (fdefinition x) t)
             value)
       (setf (documentation (symbol-function x) t)
             value)
       (when (macro-function x)
         (setf (documentation (macro-function x) t)
               value)))
      ((doc variable) (set-doc type))
      ((or (doc type)
           (safe-doc (find-class x)))
       (set-doc type)
       (ignore-errors
        (setf (documentation (find-class x) t)
              value)))
      ((doc structure)
       (set-doc structure))
      ((safe-doc (find-package x))
       (setf (documentation (find-package x) t)
             value))
      ((doc compiler-macro) (set-doc compiler-macro))
      ((doc setf) (set-doc setf))
      ((doc method-combination) (set-doc method-combination)))))

(define-generic documentation* (object &optional (doc-type t))
  "Get the documentation string of OBJECT.
Like `documentation', but DOC-TYPE is optional and defaults to T.

Influenced by:
- The current state of the image (defined functions, classes etc.)
- `documentation' methods."
  (documentation object doc-type))

(define-generic (setf documentation*) (value object &optional (doc-type t))
  "Set the `documentation' string of OBJECT (of type DOC-TYPE) to VALUE."
  (setf (documentation object doc-type)
        value))

(defalias doc* documentation*)
(define-generic (setf doc*) (value object &optional (doc-type t))
  "Set the `documentation' string of OBJECT (of type DOC-TYPE) to VALUE."
  (setf (documentation* object doc-type) value))
