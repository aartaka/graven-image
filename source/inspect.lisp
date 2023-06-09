;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

;; Stolen from Nyxt:
(-> scalar-p (t) boolean)
(defun scalar-p (object)
  "Return true if OBJECT is of one of the following types:
- symbol,
- character,
- string,
- non-complex number."
  (typep object '(or symbol character string real)))

(-> id (t) integer)
(defun id (object)
  #+sbcl (sb-kernel:get-lisp-obj-address object)
  #+ccl (ccl:%address-of object)
  #+ecl (si:pointer object)
  #+abcl (system::identity-hash-code object)
  #+clisp (system::address-of object)
  #+gcl (system:address object)
  #+allegro (excl:lispval-to-address object)
  #-(or sbcl ccl ecl abcl clisp gcl allegro) (sxhash object))

(defgeneric properties* (object &key strip-null &allow-other-keys)
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
              `((:self ,object)  ; From CCL.
                (:id ,(id object))
                (:class ,(class-of object)
                        ,(lambda (new-value _)
                           (declare (ignorable _))
                           (change-class object (find-class new-value))))
                (:type ,(type-of object))
                #+ccl
                (:wrapper ,(ccl::%class-own-wrapper (class-of object))))
              (call-next-method)))))
  (:documentation "Return a list of OBJECT properties to inspect.
Every property is a list of (NAME VALUE &optional SETTER) lists, where

- NAME is a thing (preferably symbol) naming the property.

- VALUE is the contents of the property.

- And SETTER is a function of two arguments (new-value old-value) to
modify the property. For slots, this setter will likely be setting the
`slot-value'.

When STRIP-NULL, properties with null VALUE and SETTER are filtered
out."))

(-> symbol-visibility (symbol) (or null (member :inherited :external :internal :uninterned)))
(defun symbol-visibility (symbol)
  (if (symbol-package symbol)
      (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol)))
      :uninterned))

(defmethod properties* ((object symbol) &key &allow-other-keys)
  `((:name ,(symbol-name object))
    (:package ,(symbol-package object))
    (:visibility ,(symbol-visibility object)
                 ,(unless (member (symbol-visibility object) '(nil :uninterned :inherited))
                    (lambda (new-value _)
                      (declare (ignorable _))
                      (cond
                        ((eq new-value :external)
                         (export object (symbol-package object)))
                        ((eq new-value :internal)
                         (unexport object (symbol-package object)))))))
    ,@(when (fboundp object)
        `((:function-binding
           ,(symbol-value object)
           ,(lambda (new-value _)
              (declare (ignorable _))
              (setf (symbol-value object) new-value)))))
    ,@(when (boundp object)
        `((:value-binding
           ,(symbol-value object)
           ,(lambda (new-value _)
              (declare (ignorable _))
              (setf (symbol-value object) new-value)))))
    ,@(when (ignore-errors (find-class object nil))
        `((:class-binding ,(ignore-errors (find-class object nil)))))
    ,@(when (ignore-errors (find-package object))
        `((:package-binding ,(ignore-errors (find-package object)))))
    (:plist ,(symbol-plist object))))

(-> dotted-p (list) boolean)
(defun dotted-p (cons)
  (not (null (cdr (last cons)))))

(defmethod properties* ((object cons) &key &allow-other-keys)
  (if (dotted-p object)
      `((:car ,(car object)
              ,(lambda (new-value _)
                 (declare (ignorable _))
                 (rplaca object new-value)))
        (:cdr ,(cdr object)
              ,(lambda (new-value _)
                 (declare (ignorable _))
                 (rplacd object new-value))))
      (append
       `((:length ,(length object)))
       (loop for i from 0
             for elem in object
             collect (let ((i i)
                           (elem elem))
                       (list i elem (lambda (new-value _)
                                      (declare (ignorable _))
                                      (setf (nth i object) new-value))))))))


(defmethod properties* ((object complex) &key &allow-other-keys)
  `((:imagpart ,(imagpart object))
    (:realpart ,(realpart object))))

(defmethod properties* ((object number) &key &allow-other-keys)
  `(,@(when (typep object 'ratio)
        `((:numerator ,(numerator object))
          (:denominator ,(denominator object))))
    ,@(when (floatp object)
        (multiple-value-bind (significand exponent sign)
            (integer-decode-float object)
          `((:exponent ,exponent)
            (:mantissa ,significand)
            (:sign ,sign)
            (:radix ,(float-radix object))
            (:precision ,(float-precision object)))))
    ,@(when (or (floatp object)
                (typep object 'ratio))
        `((:nearest-integer ,(round object))))
    ,(typecase object
       (short-float
        `(:most-positive-short-float ,most-positive-short-float))
       (single-float
        `(:most-positive-single-float ,most-positive-single-float))
       (double-float
        `(:most-positive-double-float ,most-positive-double-float))
       (long-float
        `(:most-positive-long-float ,most-positive-long-float))
       (fixnum
        `(:most-positive-fixnum ,most-positive-fixnum)))))

#+sbcl
(defun remove-sbcl-props-from (object &rest names-to-remove)
  (mapcar #'(lambda (cons)
              (list (car cons) (cdr cons)))
          (set-difference
           (nth-value 2 (sb-impl::inspected-parts object))
           names-to-remove
           :key (lambda (x)
                  (typecase x
                    (cons (car x))
                    (symbol x)
                    (string x)))
           :test #'equal)))

#+ccl
(defun get-ccl-props (object &rest props)
  (mapcar
   (lambda (prop)
     (list prop
           (typecase object
             (generic-function
              (ccl::nth-immediate object (symbol-value prop)))
             (t (ccl:uvref object (symbol-value prop))))))
   props))

(-> all-symbols ((or package symbol)) list)
(defun all-symbols (package)
  (loop for sym being the present-symbol in package
        collect sym))

(-> external-symbols ((or package symbol)) list)
(defun external-symbols (package)
  (loop for sym being the external-symbol in package
        collect sym))

(-> internal-symbols ((or package symbol)) list)
(defun internal-symbols (package)
  (loop for sym being the present-symbol in package
        when (eql (symbol-visibility sym) :internal)
          collect sym))

(-> inherited-symbols ((or package symbol)) list)
(defun inherited-symbols (package)
  (loop for sym being the present-symbol in package
        when (eql (symbol-visibility sym) :inherited)
          collect sym))

(defmethod properties* ((object package) &key &allow-other-keys)
  `((:name ,(package-name object))
    (:description ,(documentation object 'package))
    (:nicknames ,(package-nicknames object))
    (:external-symbols ,(external-symbols object))
    (:internal-symbols ,(internal-symbols object))
    (:inherited-symbols ,(inherited-symbols object))
    (:used-by ,(package-used-by-list object))
    (:uses ,(package-use-list object))
    #+(or sb-package-locks package-locks)
    (locked #+sbcl ,(sb-ext:package-locked-p object)
            #+ecl ,(ext:package-locked-p object)
            ,(lambda (new-value _)
               (declare (ignorable _))
               (if new-value
                   #+sbcl (sb-ext:lock-package object)
                   #+ecl (ext:lock-package object)
                   #+sbcl (sb-ext:unlock-package object)
                   #+ecl (ext:unlock-package object))))
    #+(or sb-ext ccl ext ext ext hcl excl)
    (:local-nicknames ,(package-local-nicknames object))
    #+ccl
    ,@(get-ccl-props
       object 'ccl::pkg.itab 'ccl::pkg.etab 'ccl::pkg.shadowed 'ccl::pkg.lock 'ccl::pkg.intern-hook)
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-impl::%name 'sb-impl::%used-by
       'sb-impl::internal-symbols 'sb-impl::external-symbols
       'sb-impl::doc-string 'sb-impl::%local-nicknames)))

(defmethod properties* ((object readtable) &key &allow-other-keys)
  `((:case ,(readtable-case object)
      ,(lambda (new-value _)
         (declare (ignorable _))
         (setf (readtable-case object) new-value)))
    #+sbcl
    (:normalization ,(sb-ext::readtable-normalization object)
                    ,(lambda (new-value _)
                       (declare (ignorable _))
                       (setf (sb-ext::readtable-normalization object) new-value)))
    #+sbcl
    (:symbol-preference ,(sb-impl::%readtable-symbol-preference object))
    #+sbcl
    (:string-preference ,(sb-impl::%readtable-string-preference object))
    #+ccl
    ,@(get-ccl-props object 'ccl::rdtab.ttab 'ccl::rdtab.macros)
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-impl::%readtable-normalization 'sb-impl::%readtable-case)))

(defmethod properties* ((object random-state) &key &allow-other-keys)
  `(#+ccl
    ,@(get-ccl-props object 'ccl::random.mrg31k3p-state)
    #+sbcl
    ,@(remove-sbcl-props-from object)))

(defmethod properties* ((object character) &key &allow-other-keys)
  `((:code ,(char-code object))
    (:name ,(char-name object))
    (:digit-char-p ,(digit-char-p object))
    (:alpha-char-p ,(alpha-char-p object))
    (:graphic-char-p ,(graphic-char-p object))
    (:alphanumericp ,(alphanumericp object))
    (:char-code-limit ,char-code-limit)))

(defmethod properties* ((object array) &key &allow-other-keys)
  `((:dimensions ,(array-dimensions object)
                 ,(lambda (new-value _)
                    (declare (ignorable _))
                    (adjust-array object new-value)))
    ,@(unless (stringp object)
        `((:rank ,(array-rank object))
          (:element-type ,(array-element-type object))
          (:upgraded-element-type ,(upgraded-array-element-type (type-of object)))))
    ,@(when (array-displacement object)
        (multiple-value-bind (displaced-to offset)
            (array-displacement object)
          `((:displaced-to ,displaced-to)
            (:offset ,offset))))
    ,@(when (array-has-fill-pointer-p object)
        `((:fill-pointer ,(fill-pointer object)
                         (lambda (new-value _)
                           (declare (ignorable _))
                           (setf (fill-pointer object) new-value)))))
    ,@(loop for elt across object
            for i from 0
            collect (list i elt
                          (lambda (new-value _)
                            (declare (ignorable _))
                            (setf (elt object i) new-value))))))

(defmethod properties* ((object pathname) &key &allow-other-keys)
  `(,@(when (uiop:logical-pathname-p object)
        `((:translation ,(translate-logical-pathname object))))
    (:wild-p ,(wild-pathname-p object))
    (:namestring ,(namestring object))
    (:native-namestring ,(uiop:native-namestring object))
    (:truename ,(truename object))
    (:host ,(pathname-host object))
    (:device ,(pathname-device object))
    (:directory ,(pathname-directory object))
    (:name ,(pathname-name object))
    (:type ,(pathname-type object))
    (:version (pathname-version object))
    ,@(when (uiop:file-pathname-p object)
        `((:author ,(file-author object))
          (:write-date ,(file-write-date object))))
    ,@(when (member (pathname-type object)
                    '("lsp" "lisp")
                    :test #'string-equal)
        `((:compile-pathname ,(compile-file-pathname object))))
    ,@(when (uiop:directory-pathname-p object)
        `((:files ,(uiop:directory-files object))
          (:subdirectories ,(uiop:subdirectories object))))
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-impl::device 'sb-impl::name 'sb-impl::version 'type 'namestring)))

(defmethod properties* ((object hash-table) &key &allow-other-keys)
  `((:test ,(hash-table-test object))
    (:size ,(hash-table-size object))
    (:count ,(hash-table-count object))
    (:rehash-size ,(hash-table-rehash-size object))
    (:rehash-threshold ,(hash-table-rehash-threshold object))
    #+(or sbcl ecl ccl)
    (:weakness
     #+ecl ,(si:hash-table-weakness object)
     #+sbcl ,(sb-impl::hash-table-weakness object)
     #+ccl ,(ccl:hash-table-weak-p object))
    ,@(loop for key being the hash-key in object
              using (hash-value val)
            when (scalar-p key)
              collect (list key val
                            (lambda (new-value _)
                              (declare (ignorable _))
                              (setf (gethash key object)
                                    new-value)))
                into inline-props
            else
              collect key into complex-props
              and collect val into complex-props
            finally (return (append inline-props
                                    (list (list 'other-pairs complex-props)))))
    #+ccl
    ,@(get-ccl-props
       object
       'ccl::nhash.keytransF 'ccl::nhash.compareF 'ccl::nhash.rehash-bits 'ccl::nhash.vector
       'ccl::nhash.lock 'ccl::nhash.owner 'ccl::nhash.grow-threshold 'ccl::nhash.puthash-count
       'ccl::nhash.exclusion-lock 'ccl::nhash.find 'ccl::nhash.find-new 'ccl::nhash.read-only
       'ccl::nhash.min-size)
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-impl::test 'sb-impl::rehash-size 'sb-impl::rehash-threshold 'sb-impl::%count)))

(defmethod properties* ((object stream) &key &allow-other-keys)
  `((:direction ,(cond
                   ((typep object 'two-way-stream) :io)
                   ((input-stream-p object) :input)
                   ((output-stream-p object) :output)))
    (:open ,(open-stream-p object)
           ,(lambda (new-value old-value)
              (when old-value
                (case new-value
                  ((nil) (close object))
                  (:abort (close object :abort t))))))
    (:element-type ,(stream-element-type object))
    (:format ,(stream-external-format object))
    ,@(typecase object
        ;; On SBCL, echo-stream is an instance of two-way-stream...
        (echo-stream
         `((:in-echo ,(echo-stream-input-stream object))
           (:out-echo ,(echo-stream-output-stream object))))
        (two-way-stream
         `((:input ,(two-way-stream-input-stream object))
           (:output ,(two-way-stream-output-stream object))))
        (concatenated-stream
         `((:concatenates ,(concatenated-stream-streams object))))
        (broadcast-stream
         `((:broadcasts ,(broadcast-stream-streams object))))
        (synonym-stream
         `((:synonym ,(synonym-stream-symbol object))))
        (file-stream
         `((:pathname ,(pathname object))
           (:position ,(file-position object))
           (:length (file-length object))
           (:probe ,(probe-file object)
                   ,(lambda (new-value old-value)
                      (let* ((file (pathname object))
                             (exists-p old-value))
                        (cond
                          ((and exists-p (null new-value))
                           (delete-file file)
                           (close object))
                          ((and new-value (not exists-p))
                           (open file
                                 :direction :probe
                                 :if-does-not-exist :create))))))
           #+ccl
           ,@(get-ccl-props object 'ccl::basic-file-stream.actual-filename))))
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-impl::file 'sb-impl::element-type 'sb-impl::dual-channel-p 'sb-impl::pathname)))

(-> object-slots ((or standard-object structure-object)) list)
(defun object-slots (object)
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(-> inspect-slots ((or standard-object structure-object)) list)
(defun inspect-slots (object)
  (append
   (mapcar (lambda (name)
             (list name (if (slot-boundp object name)
                            (slot-value object name)
                            :unbound)
                   (lambda (new-value _)
                     (declare (ignorable _))
                     (setf (slot-value object name) new-value))))
           (object-slots object))
   #+ccl
   (get-ccl-props
    object
    'ccl::instance.hash 'ccl::instance.slots)
   #+sbcl
   (apply #'remove-sbcl-props-from object
          (object-slots object))))

(defmethod properties* ((object standard-object) &key &allow-other-keys)
  (inspect-slots object))

(defmethod properties* ((object structure-object) &key &allow-other-keys)
  (inspect-slots object))

(defmethod properties* ((object function) &key &allow-other-keys)
  `((:name ,(function-name* object)
           ,(lambda (new-name old-name)
              (compile new-name (fdefinition old-name))))
    (:arguments ,(function-lambda-list* object))
    (:ftype ,(function-type* object))
    (:expression ,(function-lambda-expression* object)
                 ,(lambda (new-value _)
                    (declare (ignorable _))
                    (compile (function-name* object)
                             new-value)))
    ,@(when (typep object 'generic-function)
        `((:methods ,(closer-mop:generic-function-methods object))
          (:method-combination ,(closer-mop:generic-function-method-combination object))
          #+ccl
          ,@(get-ccl-props
             object
             'ccl::gf.code-vector 'ccl::gf.slots 'ccl::gf.dispatch-table 'ccl::gf.dcode 'ccl::gf.hash 'ccl::gf.bits)
          #+ccl
          ,@(when (typep object 'standard-generic-function)
              (get-ccl-props object 'ccl::sgf.method-class 'ccl::sgf.decls 'ccl::sgf.dependents))))
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-pcl::name 'sb-pcl::methods 'sb-pcl::%method-combination "Lambda-list" "Ftype")))

(-> restart-interactive (restart))
(defun restart-interactive (restart)
  (declare (ignorable restart))
  #+ccl (ccl::%restart-interactive restart)
  #+sbcl (sb-kernel::restart-interactive-function restart)
  #+ecl (si::restart-interactive-function restart)
  #-(or ccl sbcl ecl) nil)

(defmethod properties* ((object restart) &key &allow-other-keys)
  `((:name ,(restart-name object))
    (:interactive ,(restart-interactive object))
    (:test
     #+ccl ,(ccl::%restart-test object)
     #+sbcl ,(sb-kernel::restart-test-function object)
     #+ecl ,(si::restart-test-function object)
     #-(or ccl sbcl ecl) nil)
    (:action
     #+ccl ,(ccl::%restart-action object)
     #+sbcl ,(sb-kernel::restart-function object)
     #+ecl ,(si::restart-function object)
     #-(or ccl sbcl ecl) nil)
    (:report
     #+ccl ,(ccl::%restart-report object)
     #+sbcl ,(sb-kernel::restart-report-function object)
     #+ecl ,(si::restart-report-function object)
     #-(or ccl sbcl ecl) nil)))

(defgeneric description* (object &optional stream)
  (:method :around (object &optional stream)
    (let* ((type (first (uiop:ensure-list (type-of object)))))
      (format stream "~&~@(~a~) " type)
      (call-next-method)))
  (:method (object &optional stream)
    (format stream "~s" object))
  (:documentation "Print human-readable description of OBJECT to STREAM.

Methods should include the most useful information and things that are
not suitable for the `properties*' key-value format."))

(defmethod description* ((object symbol) &optional stream)
  (if (keywordp object)
      (format stream "~a" object)
      (format stream
              "~a (~a~@[ to ~a~]~@[, ~{~a: ~s~^, ~}~])~@[~* [bound]~]~@[~* [fbound]~]~@[~* [class]~]"
              object
              (symbol-visibility object) (ignore-errors (package-name (symbol-package object)))
              (symbol-plist object)
              (boundp object) (fboundp object) (ignore-errors (find-class object nil)))))

;; TODO: integer binary layout (two's complement?).
(defmethod description* ((object integer) &optional stream)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time object)
    (format stream
            "~s (~a bits, #b~b, #o~o, #x~x, ~2,'0d:~2,'0d:~2,'0d ~
~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
~a~[th~;st~;nd~;rd~:;th~], year ~a)"
            object (ceiling (log object 2)) object object object
            hour minute second month date (mod date 10) year)))

;; TODO: float/double etc. binary layout
(defmethod description* ((object float) &optional stream)
  (format stream "~s (~e)" object object))

(defmethod description* ((object ratio) &optional stream)
  (format stream "~s (~e)~:[~*~; ~f%~]"
          object object (< object 100) (coerce object 'float)))

(defmethod description* ((object complex) &optional stream)
  (format stream "~s (~a+~ai)" object (realpart object) (imagpart object)))

(defmethod description* ((object character) &optional stream)
  (if (not (graphic-char-p object))
      (format stream "~s (~d/#x~x)" object (char-code object) (char-code object))
      (format stream "~a (~d/#x~x/~a, ~:[punctuation~;~:[alphabetic~;numeric~]~])"
              object
              (char-code object) (char-code object) (char-name object)
              (alphanumericp object)
              (digit-char-p object))))

(defmethod description* ((object cons) &optional stream)
  (if (not (consp (cdr object)))
      (format stream "(~s . ~s)" (car object) (cdr object))
      (call-next-method)))

;; TODO: ECL lists shadowed symbols and used-by list
(defmethod description* ((object package) &optional stream)
  (format stream "~a~@[/~{~a~^/~}~] [exports ~a/~a~:[~*~;, uses ~{~a~^, ~}~]]~@[: ~a~]"
          (package-name object)
          (package-nicknames object)
          (length (external-symbols object))
          (length (all-symbols object))
          (package-use-list object)
          (mapcar #'package-name (package-use-list object))
          (documentation object t)))

(defmethod description* ((object restart) &optional stream)
  (format stream "~s~@[~* (interactive)~]~@[:
~a~]"
          (restart-name object) (restart-interactive object)
          object))

(defmethod description* ((object hash-table) &optional stream)
  (format stream "[~a, ~d/~d]~:[
 ~s~;~*~]"
          (hash-table-test object)
          (hash-table-count object) (hash-table-size object)
          (zerop (hash-table-count object))
          (loop for key being the hash-key in object
                  using (hash-value val)
                collect (list key val))))

(defmethod description* ((object array) &optional stream) ; string too
  (format stream "~{~a~^ ~}[~d~@[/~d~]]~@[ ~s~]"
          (uiop:ensure-list (array-element-type object))
          (length object) (ignore-errors (fill-pointer object))
          object))

(defmethod description* ((object stream) &optional stream)
  (labels ((directions (object)
             (uiop:ensure-list
              (cond
                ((typep object 'echo-stream) :echo)
                ((typep object 'broadcast-stream)
                 (mapcar (constantly :out)
                         (broadcast-stream-streams object)))
                ((typep object 'concatenated-stream)
                 (mapcar (constantly :in)
                         (concatenated-stream-streams object)))
                ((typep object 'synonym-stream)
                 (cons :synonym
                       (reduce #'append (mapcar #'directions
                                                (symbol-value (synonym-stream-symbol object))))))
                ((typep object 'two-way-stream) (list :in :out))
                ((input-stream-p object) :in)
                ((output-stream-p object) :out)))))
    (format stream "~{~a~^+~}~@[~a~]~:[~3*~;~@[ ~a~]~@[#L~d~]~@[-~d~]~]"
            (directions object)
            (uiop:ensure-list (ignore-errors (stream-external-format object)))
            (uiop:file-stream-p object)
            (ignore-errors (pathname object))
            (ignore-errors (file-position object))
            (ignore-errors (file-length object)))))

(defmethod description* ((object pathname) &optional stream)
  (format stream "~a~@[ -> ~a~]"
          object
          (cond
            ((uiop:logical-pathname-p object)
             (translate-logical-pathname object))
            ((and (ignore-errors (uiop:native-namestring object))
                  (not (equal (namestring object)
                              (uiop:native-namestring object))))
             (uiop:native-namestring object))
            (t (ignore-errors
                (unless (equal (truename object) object)
                  (truename object)))))))

(defmethod description* ((object function) &optional stream)
  (format stream "~:[λ~*~;~a ~](~:[?~*~;~{~a~^ ~}~])~@[
 ↑ ~{~a~^ ~}~]~:[~2*~;
 : ~a -> ~a~]~@[
~a~]"
          (and (function-name* object)
               (symbolp (function-name* object)))
          (function-name* object)
          (function-lambda-expression* object)
          (function-lambda-list* object)
          (let ((closure (nth-value 1 (function-lambda-expression* object))))
            (when closure
              (typecase closure
                (list (mapcar (lambda (pair) (list (car pair) (cdr pair))) closure))
                (t (list "?")))))
          (function-type* object)
          (second (function-type* object))
          (third (function-type* object))
          (documentation object t)))
(-> object-description ((or standard-object structure-object) (or stream boolean)))
(defun object-description (object stream)
  (format stream "~s~@[
~a~]"
          object (or (documentation (class-name (class-of object)) 'type)
                     (documentation (class-name (class-of object)) 'structure))))

(defmethod description* ((object standard-object) &optional stream)
  (object-description object stream))

(defmethod description* ((object structure-object) &optional stream)
  (object-description object stream))

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
          (loop for (name value) in (properties* object)
                do (if (symbolp name)
                       (format stream "~&~a = ~s~%" name value)
                       (format stream "~&~s = ~s~%" name value)))))
    (if (typep stream 'string-stream)
        (get-output-stream-string stream)
        (values))))

(-> property-indices (list) list)
(defun property-indices (properties)
  "Map integer indices to every property in PROPERTIES.
Non-trivial, because some of the PROPERTIES have integer keys."
  (loop with taken = (remove-if-not #'integerp (mapcar #'first properties))
        for (name) in properties
        for index from 0
        when (integerp name)
          collect name
        else
          collect (loop for i from index
                        while (member i taken)
                        finally (return (prog1
                                            i
                                          (setf index i))))))

(defun internal-inspect* (object strip-null)
  (let* ((properties (properties* object :strip-null strip-null))
         (prop-length (length properties))
         ;; VT-100-ish pages, unless rebound.
         (page-length (or *print-length* 20))
         (indices (property-indices properties))
         (offset 0)
         (next-offset (min (+ offset page-length) prop-length))
         commands)
    (labels ((print-props ()
               (loop for i from offset below next-offset
                     for index = (elt indices i)
                     for (key value setter) = (elt properties i)
                     do (let ((*package* (if (symbolp key)
                                             (symbol-package key)
                                             *package*)))
                          (format *query-io* "~&~@[[~d]~]~:[ ~:[~s~;~a~]~;~2*~] =~@[setfable=~*~] ~s"
                                  index (integerp key) (symbolp key) key setter value)))
               (when (< next-offset prop-length)
                 (format *query-io* "~&[Showing properties ~d-~d out of ~d]"
                         offset (1- next-offset) prop-length)))
             (find-by-key (key commands properties)
               "Find the KEY in COMMANDS/PROPERTIES by its prefix/value.
Returns two values:
- The property/command matching the KEY, as a list.
- Whether the found thing is a command (= member of COMMANDS).

Search is different for different KEY types:
- Integer: only search PROPERTIES by their indices.
- SYMBOL: search both commands and properties, but only by symbol
  names.
- Anything else: search literal object."
               (typecase key
                 (integer (values (elt properties (position key indices)) nil))
                 (symbol
                  (loop for match in (append commands properties)
                        for (match-key) = match
                        when (and (symbolp match-key)
                                  (uiop:string-prefix-p (symbol-name key) (symbol-name match-key)))
                          do (return (values match (member match commands)))))
                 (t (find key properties :key #'first :test #'equal))))
             (next-page ()
               (if (= next-offset prop-length)
                   (format *query-io* "~&Nowhere to scroll, already at the last page.")
                   (setf offset next-offset
                         next-offset (min (+ offset page-length) prop-length)))
               (print-props))
             (previous-page ()
               (if (zerop offset)
                   (format *query-io* "~&Nowhere to scroll, already at the first page.")
                   (setf next-offset offset
                         offset (max 0 (- offset page-length))))
               (print-props))
             (home ()
               (if (zerop offset)
                   (format *query-io* "~&Nowhere to scroll, already at the first page.")
                   (setf offset 0
                         next-offset (min (+ offset page-length) prop-length)))
               (description* object *query-io*)
               (print-props))
             (width (new)
               (setf page-length new))
             (set-property (key &optional new-value)
               (let ((prop (find-by-key key nil properties)))
                 (cond
                   ((and prop
                         (third prop))
                    (print (funcall (third prop) new-value (second prop))))
                   (prop
                    (format *query-io* "~&Cannot modify this property."))
                   (t
                    (format *query-io* "~&No such property found.")))
                 (when (third prop)
                   (funcall (third prop) new-value (second prop)))))
             (self ()
               (description* object *query-io*)
               (print-props))
             (exit ()
               (throw 'topmost-inspect* (values)))
             (up ()
               (return-from internal-inspect* (values)))
             (istep (key)
               (internal-inspect*
                (second (find-by-key key nil properties))
                strip-null))
             (eval-form (form)
               (print (eval form) *query-io*))
             (help ()
               (format *query-io*
                       "~&This is an interactive inspector for ~a~%~
~&Available commands are:
~:{~&~a ~15t~*~a~}

Possible inputs are:
- Mere symbols: run one of the commands above, matching the symbol.
  - If there's no matching command, then match against properties.
    - If nothing matches, evaluate the symbol.
- Integer: find the property indexed by this integer.
  - If there are none, evaluate the integer.
- Any other atom: find the property with this atom as a key.
  - Evaluate it otherwise.
- S-expression: match the list head against commands and properties,
  as above.
  - If the list head does not match anything, evaluate the
    s-expression.~%"
                       object commands)))
      ;; Rebinding the local variable to have proper references to functions.
      (setf commands
            `((:? ,#'help "Print a list of commands")
              (:help ,#'help "Print a list of commands")
              (:length ,#'width "(:LENGTH NEW-LENGTH) Change the page size")
              (:width ,#'width "(:WIDTH NEW-LENGTH) Change the page size")
              (:widen ,#'width "(:WIDEN NEW-LENGTH) Change the page size")
              (:next-page ,#'next-page "Show the next page of properties (if any)")
              (:previous-page ,#'previous-page "Show the previous page of properties (if any)")
              (:home ,#'home "Scroll back to the first page of properties")
              (:reset ,#'home "Scroll back to the first page of properties")
              (:top ,#'home "Scroll back to the first page of properties")
              ;; (:history history "Show the current inspector path")
              ;; (:path history "Show the current inspector path")
              (:set ,#'set-property "(:SET PROPERTY VALUE) Set the PROPERTY to VALUE.")
              (:modify ,#'set-property "(:MODIFY PROPERTY VALUE) Set the PROPERTY to VALUE.")
              (:this ,#'self "Show the currently inspected object")
              (:self ,#'self "Show the currently inspected object")
              (:redisplay ,#'self "Show the currently inspected object")
              (:show ,#'self "Show the currently inspected object")
              (:current ,#'self "Show the currently inspected object")
              (:again ,#'self "Show the currently inspected object")
              (:quit ,#'exit "Exit the inspector")
              (:exit ,#'exit "Exit the inspector")
              (:up ,#'up "Move to the previous/upper inspected object or exit the inspector")
              (:pop ,#'up "Move to the previous/upper inspected object or exit the inspector")
              (:back ,#'up "Move to the previous/upper inspected object or exit the inspector")
              (:inspect ,#'istep "(:INSPECT KEY) Inspect the object under KEY")
              (:istep ,#'istep "(:ISTEP KEY) Inspect the object under KEY")
              (:evaluate ,#'eval-form "(:EVALUATE FORM) Evaluate the FORM")))
      (loop
        (description* object *query-io*)
        (print-props)
        (format *query-io* "~&i> ")
        (finish-output *query-io*)
        (let ((input (read *query-io*)))
          (multiple-value-bind (result command-p)
              (find-by-key (first (uiop:ensure-list input)) commands properties)
            (cond
              ((and result command-p)
               (apply (second result)
                      (mapcar #'eval (rest (uiop:ensure-list input)))))
              ((and result (not command-p))
               (internal-inspect* (second result) strip-null))
              (t (dolist (val (multiple-value-list (eval input)))
                   (print val *query-io*))))))))))

(define-generic inspect* (object &optional (strip-null t))
  "Interactively query the OBJECT.

OBJECT summary and properties are printed to and
expressions/commands/indices/property names are read from
`*query-io*'.

Properties are paginated, with commands available to scroll,

Influenced by:
- `*query-io*'.
- `*print-length*' for page size."
  (catch 'topmost-inspect*
    (loop
      (internal-inspect* object strip-null)
      ;; This is to disallow :POP exiting the inspector.
      (format *query-io* "~&Cannot pop from the toplevel inspector, use :Q instead."))))
