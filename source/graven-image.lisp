;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

;; Stolen and improved from Nyxt.
(defmacro without-package-locks (&body body)
  #+(and sbcl sb-package-locks)
  `(sb-ext:without-package-locks
     ,@body)
  #+(and ecl package-locks)
  `(ext:without-package-locks
     ,@body)
  #-(or (and sbcl sb-package-locks)
        (and ecl package-locks))
  `(progn ,@body))

(defun read-nolocks (stream)
  "Same as read, but without package lock."
  (without-package-locks
    (read stream nil nil)))

;; Stolen from Serapeum.
(defmacro -> (name (&rest arg-types) &optional return-type)
  "Shorter ftype declaration for NAME.
Idea stolen from Serapeum, but the implementation is much simpler and
unsafer."
  `(declaim (ftype (function (,@arg-types) ,@(when return-type
                                               (list return-type)))
                   ,name)))

;; Stolen from Serapeum.
(defun fmt (control &rest arguments)
  "Shortened FORMAT."
  (apply #'format nil control arguments))

;; Stolen from Nyxt with slight alternations.
(defun funcall* (maybe-fn args)
  "Only funcall MAYBE-FN on ARGS when it's actually a function."
  (when (functionp maybe-fn)
    (apply maybe-fn args)))

(defmacro define-generic (name (&rest method-args) &body (documentation . body))
  `(defgeneric ,name (,@(mapcar #'first (mapcar #'uiop:ensure-list method-args)))
     (:method (,@method-args)
       ,@body)
     (:documentation ,documentation)))
