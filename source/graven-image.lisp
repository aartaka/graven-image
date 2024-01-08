;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

;; Stolen and improved from Nyxt.
(defmacro without-package-locks (&body body)
  "Ignore package locks, where necessary.
Also muffle redefinition warnings."
  `(handler-bind ((warning #'muffle-warning))
     #+(and sbcl sb-package-locks)
     (sb-ext:without-package-locks
       ,@body)
     #+(and ecl package-locks)
     (ext:without-package-locks
       ,@body)
     #+clisp
     ;; WHAT, CLISP HAS PACKAGE LOCKS!!!!
     (ext:without-package-lock ()
       ,@body)
     #-(or (and sbcl sb-package-locks)
           (and ecl package-locks)
           clisp)
     (progn ,@body)))

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
  `(let ((generic (defgeneric ,name (,@(mapcar #'first (mapcar #'uiop:ensure-list method-args)))
                    (:method (,@method-args)
                      ,@body)
                    (:documentation ,documentation))))
     (setf (documentation (fdefinition ',name) t) ,documentation)
     (ignore-errors
      (setf (documentation ',name 'function) ,documentation))
     generic))

(defmacro defalias (new-name old-name)
  `(ignore-errors
    (setf (fdefinition ',new-name) (fdefinition ',old-name)
          (documentation (fdefinition ',new-name) t) (documentation (fdefinition ',old-name) t))
    ;; For (setf function) fdefinitions.
    (ignore-errors
     (setf (documentation ',new-name 'function) (documentation ',old-name 'function)))))

(defmacro load-time-warn (&rest args)
  "A macro to warn about ARGS only once: while loading Graven Image.
Otherwise every call to an unimplemented function spams warnings all
over the REPL."
  (apply #'warn args)
  nil)

(defun make-keyword (string)
  "Convert STRING (a valid string designator) into a keyword symbol."
  (let ((*package* (find-package :keyword)))
    (read-from-string (string string))))
