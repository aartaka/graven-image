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
  #+clozure (ccl:%address-of object)
  #+ecl (si:pointer object)
  #+abcl (system::identity-hash-code object)
  #+clisp (system::address-of object)
  #+gcl (system:address object)
  #+allegro (excl:lispval-to-address object)
  #-(or sbcl clozure ecl abcl clisp gcl allegro) (sxhash object))

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

#+clozure
(defun get-ccl-props (object &rest props)
  (mapcar
   (lambda (prop)
     (list prop
           (typecase object
             (function
              (ccl::nth-immediate object (symbol-value prop)))
             (t (ccl:uvref object (symbol-value prop))))))
   props))

#+abcl
(defun abcl-props-except (object &rest except)
  (loop for (name . value) in (system:inspected-parts object)
        unless (member name except :test #'string=)
          collect (list (intern name :keyword) value)))

(defun reverse-append (&rest lists)
  (remove-duplicates (reduce #'append (nreverse lists))
                     :key #'first
                     :from-end t))

(define-method-combination reverse-append
  :identity-with-one-argument t)

(defgeneric fields* (object &key &allow-other-keys)
  (:method-combination reverse-append)
  (:documentation "Return a list of OBJECT fields to inspect.
Every property is a list of (NAME VALUE &optional SETTER) lists, where

- NAME is a thing (preferably symbol) naming the property.

- VALUE is the contents of the property.

- And SETTER is a function of two arguments (new-value old-value) to
modify the property. For slots, this setter will likely be setting the
`slot-value'."))

(-> symbol-visibility (symbol) (or null (member :inherited :external :internal :uninterned)))
(defun symbol-visibility (symbol)
  (if (symbol-package symbol)
      (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol)))
      :uninterned))

(defmacro deffields ((name specifier) &body fields)
  `(defmethod fields* reverse-append ((,name ,specifier) &key &allow-other-keys)
     ,@fields))

(defmacro defproperty (specifier name function)
  `(defmethod fields* reverse-append ((object ,specifier) &key &allow-other-keys)
     (list (list ,name (,function object)))))

(deffields (object symbol)
  `((:name ,(symbol-name object))
    (:package ,(symbol-package object))
    (:visibility ,(symbol-visibility object)
                 ,(unless (member (symbol-visibility object) '(nil :uninterned :inherited))
                    (lambda (new-value _)
                      (declare (ignorable _))
                      (typecase new-value
                        ((or (eql :external)
                             (eql t))
                         (export object (symbol-package object)))
                        ((or (eql :internal)
                             null)
                         (unexport object (symbol-package object)))
                        ((eql :uninterned)
                         (uiop:unintern* object (symbol-package object) nil))))))
    ,@(when (fboundp object)
        (cond
          ((special-operator-p object)
           `((:special-operator t)))
          ((macro-function object)
           `((:macro-binding ,(macro-function object))))
          ((fboundp object)
           `((:function-binding
              ,(symbol-function object)
              ,(lambda (new-value _)
                 (declare (ignorable _))
                 ;; `fdefinition'? `compile'?
                 (setf (symbol-function object) new-value)))
             ,@(when (compiler-macro-function object)
                 `((:compiler-macro-binding ,(compiler-macro-function object))))))))
    ,@(when (boundp object)
        `((:value-binding
           ,(symbol-value object)
           ,(lambda (new-value _)
              (declare (ignorable _))
              (setf (symbol-value object) new-value)))))
    ,@(when (ignore-errors (find-class object nil))
        `((:class-binding ,(ignore-errors (find-class object nil)))))
    ,@(when (uiop:find-package* object nil)
        `((:package-binding ,(uiop:find-package* object nil))))
    (:plist ,(symbol-plist object))))

(-> dotted-p (list) boolean)
(defun dotted-p (cons)
  (not (null (cdr (last cons)))))

(deffields (object null)
  (call-next-method))

(deffields (object list)
  `((:length ,(length object))
    ,@(loop for i from 0
            for elem in object
            collect (let ((i i)
                          (elem elem))
                      (list i elem (lambda (new-value _)
                                     (declare (ignorable _))
                                     (setf (nth i object) new-value)))))))

(deffields (object cons)
  (when (dotted-p object)
    `((:car ,(car object)
            ,(lambda (new-value _)
               (declare (ignorable _))
               (rplaca object new-value)))
      (:cdr ,(cdr object)
            ,(lambda (new-value _)
               (declare (ignorable _))
               (rplacd object new-value))))))


(deffields (object complex)
  `((:imagpart ,(imagpart object))
    (:realpart ,(realpart object))))

(deffields (object ratio)
  `((:numerator ,(numerator object))
    (:denominator ,(denominator object))
    (:nearest-integer ,(round object))))

(deffields (object float)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float object)
    `((:exponent ,exponent)
      (:mantissa ,significand)
      (:sign ,sign)
      (:radix ,(float-radix object))
      (:precision ,(float-precision object))
      ,@(when (typep object 'short-float)
          `((:most-positive-short-float ,most-positive-short-float)
            (:most-negative-short-float ,most-negative-short-float)))
      ,@(when (typep object 'single-float)
          `((:most-positive-single-float ,most-positive-single-float)
            (:most-negative-single-float ,most-negative-single-float)))
      ,@(when (typep object 'double-float)
          `((:most-positive-double-float ,most-positive-double-float)
           (:most-negative-double-float ,most-negative-double-float)))
      ,@(when (typep object 'long-float)
          `((:most-positive-long-float ,most-positive-long-float)
            (:most-negative-long-float ,most-negative-long-float)))
      (:nearest-integer ,(round object)))))

(deffields (object integer)
  (append
   `((:integer-length (integer-length object)))
   (when (typep object 'fixnum)
     `((:most-positive-fixnum ,most-positive-fixnum)
       (:most-negative-fixnum ,most-negative-fixnum)))))

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

(deffields (object package)
  `((:name ,(package-name object))
    (:description ,(documentation object t))
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
    #+(or sb-ext clozure ext ext ext hcl excl)
    (:local-nicknames ,(package-local-nicknames object))
    #+clozure
    ,@(get-ccl-props
       object 'ccl::pkg.itab 'ccl::pkg.etab 'ccl::pkg.shadowed 'ccl::pkg.lock 'ccl::pkg.intern-hook)
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-impl::%name 'sb-impl::%used-by
       'sb-impl::internal-symbols 'sb-impl::external-symbols
       'sb-impl::doc-string 'sb-impl::%local-nicknames)))

(deffields (object readtable)
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
    #+clozure
    ,@(get-ccl-props object 'ccl::rdtab.ttab 'ccl::rdtab.macros)
    #+sbcl
    ,@(remove-sbcl-props-from
       object
       'sb-impl::%readtable-normalization 'sb-impl::%readtable-case)))

(deffields (object random-state)
  `(#+clozure
    ,@(get-ccl-props object 'ccl::random.mrg31k3p-state)
    #+sbcl
    ,@(remove-sbcl-props-from object)))

(deffields (object character)
  `((:code ,(char-code object))
    (:name ,(char-name object))
    (:digit-char-p ,(digit-char-p object))
    (:alpha-char-p ,(alpha-char-p object))
    (:graphic-char-p ,(graphic-char-p object))
    (:alphanumericp ,(alphanumericp object))
    (:char-code-limit ,char-code-limit)))

(deffields (object string)
  `((:uppercase ,(every #'upper-case-p object))
    (:lowercase ,(every #'lower-case-p object))
    (:graphic ,(every #'graphic-char-p object))))

(deffields (object array)
  `((:dimensions ,(array-dimensions object)
                 ,(lambda (new-value _)
                    (declare (ignorable _))
                    (adjust-array object new-value)))
    (:rank ,(array-rank object))
    (:element-type ,(array-element-type object))
    (:upgraded-element-type ,(upgraded-array-element-type (type-of object)))
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

(defproperty logical-pathname
  :translation translate-logical-pathname)

(deffields (object pathname)
  (let ((logical-p (uiop:logical-pathname-p object))
        (link-p (not (equal (truename object) object))))
    `((:wild-p ,(wild-pathname-p object))
      (:namestring ,(namestring object))
      ,@(unless (or logical-p
                    (string= (namestring object)
                             (uiop:native-namestring object)))
          `((:native-namestring ,(uiop:native-namestring object))))
      ,@(when link-p
          `((:truename ,(truename object))))
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
      (:home ,(user-homedir-pathname))
      (:cwd ,(uiop:getcwd))
      #+sbcl
      ,@(remove-sbcl-props-from
         object
         'sb-impl::host 'sb-impl::device 'sb-impl::name 'sb-impl::version 'type 'namestring))))

(deffields (object hash-table)
  `((:test ,(hash-table-test object))
    (:size ,(hash-table-size object))
    (:count ,(hash-table-count object))
    (:rehash-size ,(hash-table-rehash-size object))
    (:rehash-threshold ,(hash-table-rehash-threshold object))
    #+(or sbcl ecl clozure abcl)
    (:weakness
     #+ecl ,(si:hash-table-weakness object)
     #+sbcl ,(sb-impl::hash-table-weakness object)
     #+clozure ,(ccl:hash-table-weak-p object)
     #+abcl ,(system:hash-table-weakness object))
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
    #+clozure
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

(deffields (object two-way-stream)
  `((:input ,(two-way-stream-input-stream object))
    (:output ,(two-way-stream-output-stream object))))

;; On SBCL, echo-stream is an instance of two-way-stream...
(deffields (object echo-stream)
  `((:echo-input ,(echo-stream-input-stream object))
    (:echo-output ,(echo-stream-output-stream object))))

(defproperty concatenated-stream
  :concatenates concatenated-stream-streams)

(defproperty broadcast-stream
  :broadcasts broadcast-stream-streams)

(defproperty synonym-stream
  :synonym synonym-stream-symbol)

(deffields (object file-stream)
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
    #+clozure
    ,@(get-ccl-props object 'ccl::basic-file-stream.actual-filename)))

(deffields (object stream)
  `((:direction ,(cond
                   ((typep object 'two-way-stream) :io)
                   ((input-stream-p object) :input)
                   ((output-stream-p object) :output)))
    (:interactive ,(interactive-stream-p object))
    #+abcl
    ,@`((:offset ,(system::stream-offset object))
        (:line-number ,(system::stream-line-number object))
        (:system ,(system::system-stream-p object))
        (:url ,(typep object 'system:url-stream))
        (:jar ,(typep object 'system:jar-stream))
        ,@(when (output-stream-p object)
            `((:charpos ,(system::stream-charpos object)))))
    (:open ,(open-stream-p object)
           ,(lambda (new-value old-value)
              (when old-value
                (case new-value
                  ((nil) (close object))
                  (:abort (close object :abort t))))))
    (:element-type ,(stream-element-type object))
    (:format ,(stream-external-format object))
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
   #+clozure
   (get-ccl-props
    object
    'ccl::instance.hash 'ccl::instance.slots)
   #+sbcl
   (apply #'remove-sbcl-props-from object
          (object-slots object))
   #+abcl
   (abcl-props-except object "DOCUMENTATION" "DIRECT-SLOTS" "SLOTS")))

(deffields (object standard-object)
  (inspect-slots object))

(deffields (object structure-object)
  (inspect-slots object))

(deffields (object function)
  `((:name ,(function-name* object)
           ,(lambda (new-name old-name)
              (compile new-name (fdefinition old-name))))
    (:arguments ,(function-lambda-list* object))
    ,@(when (function-type* object)
        `((:ftype ,(function-type* object))))
    (:expression ,(function-lambda-expression* object)
                 ,(lambda (new-value _)
                    (declare (ignorable _))
                    (compile (function-name* object)
                             new-value)))
    ,@(when (typep object 'generic-function)
        `((:methods ,(closer-mop:generic-function-methods object))))
    (:lambda-list-keywords ,lambda-list-keywords)))

(deffields (object generic-function)
  `((:methods ,(closer-mop:generic-function-methods object))
    (:method-combination ,(closer-mop:generic-function-method-combination object))
    #+clozure
    ,@(get-ccl-props
       object
       'ccl::gf.code-vector 'ccl::gf.slots 'ccl::gf.dispatch-table 'ccl::gf.dcode 'ccl::gf.hash 'ccl::gf.bits)
    #+clozure
    ,@(when (typep object 'standard-generic-function)
        (get-ccl-props object 'ccl::sgf.method-class 'ccl::sgf.decls 'ccl::sgf.dependents))))

(-> restart-interactive (restart))
(defun restart-interactive (restart)
  (declare (ignorable restart))
  #+clozure (ccl::%restart-interactive restart)
  #+sbcl (sb-kernel::restart-interactive-function restart)
  #+ecl (si::restart-interactive-function restart)
  #-(or clozure sbcl ecl) nil)

(deffields (object restart)
  `((:name ,(restart-name object))
    (:interactive ,(restart-interactive object))
    (:test
     #+clozure ,(ccl::%restart-test object)
     #+sbcl ,(sb-kernel::restart-test-function object)
     #+ecl ,(si::restart-test-function object)
     #-(or clozure sbcl ecl) nil)
    (:action
     #+clozure ,(ccl::%restart-action object)
     #+sbcl ,(sb-kernel::restart-function object)
     #+ecl ,(si::restart-function object)
     #-(or clozure sbcl ecl) nil)
    (:report
     #+clozure ,(ccl::%restart-report object)
     #+sbcl ,(sb-kernel::restart-report-function object)
     #+ecl ,(si::restart-report-function object)
     #-(or clozure sbcl ecl) nil)))

(deffields (object condition)
  `((:restarts ,(compute-restarts object))
    (:continuable ,(find 'continue (compute-restarts object) :key #'restart-name))))

(deffields (object simple-condition)
  `((:format-control ,(simple-condition-format-control object))
    (:format-arguments ,(simple-condition-format-arguments object))))

(deffields (object arithmetic-error)
  `((:operation ,(arithmetic-error-operation object))
    (:operands ,(arithmetic-error-operands object))))

(defproperty cell-error :name cell-error-name)
(defproperty package-error :package package-error-package)
(defproperty stream-error :stream stream-error-stream)
(defproperty print-not-readable :object print-not-readable-object)

(deffields (object type-error)
  `((:datum ,(type-error-datum object))
    (:expected ,(type-error-expected-type object))))

(defproperty unbound-slot :instance unbound-slot-instance)
(defproperty file-error :pathname file-error-pathname)

(defmethod fields* reverse-append (object &key &allow-other-keys)
  (let ((slot-defs (ignore-errors (closer-mop:class-slots (class-of object)))))
    `((:self ,object) ;; Inspired by CCL.
      (:id ,(id object))
      (:class ,(class-of object)
              ,(lambda (new-value _)
                 (declare (ignorable _))
                 (change-class object (find-class new-value))))
      ,@(when slot-defs
          (list (list :slot-definitions slot-defs)))
      (:type ,(type-of object))
      #+clozure
      (:wrapper ,(ccl::%class-own-wrapper (class-of object))))))
