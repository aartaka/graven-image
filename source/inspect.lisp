;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defgeneric inspect-object* (object &key strip-null &allow-other-keys)
  (:method :around (object &key (strip-null t) &allow-other-keys)
    (delete
     nil
     (mapcar (lambda (prop)
               (destructuring-bind (name value &optional setter)
                   prop
                 (cond
                   ;; If the value is setf-able, then allow to set
                   ;; it, even if it's NIL.
                   (setter (list name value setter))
                   (value (list name value))
                   (strip-null nil)
                   (t prop))))
             (append
              `((self ,object) ; From CCL.
                (id
                 #+sbcl ,(sb-kernel::get-lisp-obj-address object)
                 #+ccl ,(if (integerp object)
                            object
                            (ccl::%address-of object))
                 ;; Not perfect, but ECL doesn't leave us much to do here.
                 #+ecl ,(parse-integer
                         (subseq (with-output-to-string (s)
                                   (si:print-unreadable-object-function object s nil t (lambda ())))
                                 5) ; To strip "#< 0x" off and read hex directly.
                         :junk-allowed t
                         :radix 16)
                 #+abcl ,(system::identity-hash-code object)
                 #-(or sbcl ccl ecl abcl) ,(sxhash object))
                (class ,(class-of object)
                       ,(lambda (new-value)
                          (change-class object (find-class new-value))))
                (type ,(type-of object))
                #+ccl
                (wrapper ,(ccl::%class-own-wrapper (class-of object))))
              (call-next-method)))))
  (:documentation "Return a list of OBJECT properties to inspect.
Every property is a list of (NAME VALUE &optional SETTER) lists, where

- NAME is a thing (preferably symbol) naming the property.

- SETTER is a function of one argument to set the property to a new
value. For slots, this setter will likely be setting the `slot-value',
or using a setf-accessor."))

(defun symbol-visibility (symbol)
  (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol))))

(defun cons-to-list (cons)
  (list (car cons) (cdr cons)))

(defmethod inspect-object* ((object symbol) &key &allow-other-keys)
  `((name ,(symbol-name object))
    (package ,(symbol-package object))
    (visibility ,(symbol-visibility object)
                ,(unless (or (null (symbol-visibility object))
                             (eq :inherited (symbol-visibility object)))
                   (lambda (new-value)
                     (cond
                       ((eq new-value :external)
                        (export object (symbol-package object)))
                       ((eq new-value :internal)
                        (unexport object (symbol-package object)))))))
    ,@(when (fboundp object)
        `((function-binding
           ,(symbol-value object)
           ,(lambda (new-value)
            (setf (symbol-value object) new-value)))))
    ,@(when (boundp object)
        `((value-binding
           ,(symbol-value object)
           ,(lambda (new-value)
              (setf (symbol-value object) new-value)))))
    ,@(when (ignore-errors (find-class object nil))
        `((class-binding ,(ignore-errors (find-class object nil)))))
    ,@(when (ignore-errors (find-package object))
        `((package-binding ,(ignore-errors (find-package object)))))
    (plist ,(symbol-plist object))))

(defmethod inspect-object* ((object cons) &key &allow-other-keys)
  (let ((dotted-list (not (null (cdr (last object))))))
    (if dotted-list
        `((car ,(car object)
               ,(lambda (new-value)
                  (rplaca object new-value)))
          (cdr ,(cdr object)
               ,(lambda (new-value)
                  (rplacd object new-value))))
        (append
         `((length ,(length object)))
         (loop for i from 0
               for elem in object
               collect (let ((i i)
                             (elem elem))
                         (list i elem (lambda (new-value)
                                        (setf (nth i object) new-value)))))))))


(defmethod inspect-object* ((object complex) &key &allow-other-keys)
  `((imagpart ,(imagpart object))
    (realpart ,(realpart object))))

(defmethod inspect-object* ((object number) &key &allow-other-keys)
  `(,@(when (typep object 'ratio)
        `((numerator ,(numerator object))
          (denominator ,(denominator object))))
    ,@(when (floatp object)
        (multiple-value-bind (significand exponent sign)
            (integer-decode-float object)
          `((exponent ,exponent)
            (mantissa ,significand)
            (sign ,sign)
            (radix ,(float-radix object))
            (precision ,(float-precision object)))))
    ,@(when (or (floatp object)
                (typep object 'ratio))
        `((nearest-integer ,(round object))))
    ,(typecase object
       (short-float
        `(most-positive-short-float ,most-positive-short-float))
       (single-float
        `(most-positive-single-float ,most-positive-single-float))
       (double-float
        `(most-positive-double-float ,most-positive-double-float))
       (long-float
        `(most-positive-long-float ,most-positive-long-float))
       (fixnum
        `(most-positive-fixnum ,most-positive-fixnum)))))

#+sbcl
(defun remove-sbcl-props-from (object &rest names-to-remove)
  (mapcar #'cons-to-list
          (set-difference
           (nth-value 2 (sb-impl::inspected-parts object))
           names-to-remove
           :key (lambda (x)
                  (typecase x
                    (cons (car x))
                    (symbol x))))))

#+ccl
(defun get-ccl-props (object &rest props)
  (mapcar
   (lambda (prop)
     (list prop (ccl:uvref object (symbol-value prop))))
   props))

(defmethod inspect-object* ((object package) &key &allow-other-keys)
  `((name ,(package-name object))
    (description ,(documentation object 'package))
    (nicknames ,(package-nicknames object))
    (external-symbols
     ,(loop for sym being the external-symbol in object
            collect sym))
    (internal-symbols
     ,(loop for sym being the present-symbol in object
            when (eql (symbol-visibility sym) :internal)
              collect sym))
    (inherited-symbols
     ,(loop for sym being the present-symbol in object
            when (eql (symbol-visibility sym) :inherited)
              collect sym))
    (used-by ,(package-used-by-list object))
    (uses ,(package-use-list object))
    #+(or sb-package-locks package-locks)
    (locked ,(package-locked-p object)
            ,(lambda (new-value)
               (if new-value
                   (lock-package object)
                   (unlock-package object))))
    #+package-local-nicknames
    (local-nicknames ,(package-local-nicknames object))
    #+ccl
    ,@(get-ccl-props
       object 'ccl::pkg.itab 'ccl::pkg.etab 'ccl::pkg.shadowed 'ccl::pkg.lock 'ccl::pkg.intern-hook)
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-impl::%name 'sb-impl::%used-by
       'sb-impl::internal-symbols 'sb-impl::external-symbols
       'sb-impl::doc-string 'sb-impl::%local-nicknames)))

(defmethod inspect-object* ((object readtable) &key &allow-other-keys)
  `((case ,(readtable-case object)
      ,(lambda (new-value)
         (setf (readtable-case object) new-value)))
    #+sbcl
    (normalization ,(readtable-normalization object)
                   ,(lambda (new-value)
                      (setf (readtable-normalization object) new-value)))
    #+sbcl
    (symbol-preference ,(sb-impl::%readtable-symbol-preference object))
    #+sbcl
    (string-preference ,(sb-impl::%readtable-string-preference object))
    #+ccl
    ,@(get-ccl-props object 'ccl::rdtab.ttab 'ccl::rdtab.macros)
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-impl::%readtable-normalization 'sb-impl::%readtable-case)))

(defmethod inspect-object* ((object random-state) &key &allow-other-keys)
  `(#+ccl
    ,@(get-ccl-props object 'random.mrg31k3p-state)
    #+sbcl
    ,@(remove-sbcl-props-from object)))

(defmethod inspect-object* ((object character) &key &allow-other-keys)
  `((code ,(char-code object))
    (name ,(char-name object))
    (digit-char-p ,(digit-char-p object))
    (alpha-char-p ,(alpha-char-p object))
    (graphic-char-p ,(graphic-char-p object))
    (alphanumericp ,(alphanumericp object))))

(defmethod inspect-object* ((object array) &key &allow-other-keys)
  `(,@(if (= 1 (length (array-dimensions object)))
          `((length ,(array-dimension object 0)
                    (lambda (new-value)
                      (adjust-array object (list new-value)))))
          `((dimensions ,(array-dimensions object)
                        (lambda (new-value)
                          (adjust-array object new-value)))))
    ,@(unless (stringp object)
        `((rank ,(array-rank object))
          (element-type ,(array-element-type object))
          (upgraded-element-type ,(upgraded-array-element-type object))))
    ,@(when (displaced-array-p object)
        `((displacement ,(array-displacement object))))
    ,@(when (array-has-fill-pointer-p object)
        `((fill-pointer ,(fill-pointer object)
                        (lambda (new-value)
                          (setf (fill-pointer object) new-value)))))
    ,@(loop for elt across object
            for i from 0
            collect (list i elt
                          (lambda (new-value)
                            (setf (elt object i) new-value))))))

(defmethod inspect-object* ((object pathname) &key &allow-other-keys)
  `(,@(when (uiop:logical-pathname-p object)
        `((translation ,(translate-logical-pathname object))))
    (wild-p ,(wild-pathname-p object))
    (namestring ,(namestring object))
    (native-namestring ,(uiop:native-namestring object))
    (truename ,(truename object))
    (host ,(pathname-host object))
    (device ,(pathname-device object))
    (directory ,(pathname-directory object))
    (name ,(pathname-name object))
    (type ,(pathname-type object))
    (version (pathname-version object))
    ,@(when (uiop:file-pathname-p object)
        `((author ,(file-author object))
          (write-date ,(file-write-date object))))
    ,@(when (member (pathname-type object)
                    '("lsp" "lisp")
                    :test #'string-equal)
        `((compile-pathname ,(compile-file-pathname object))))
    ,@(when (directory-pathname-p object)
        `((files ,(uiop:directory-files object))
          (subdirectories ,(uiop:subdirectories object))))
    ,@(remove-sbcl-props-from
       object
       'sb-impl::device 'sb-impl::name 'sb-impl::version 'type 'namestring)))
