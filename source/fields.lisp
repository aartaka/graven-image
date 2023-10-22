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
(defvar sbcl-props-to-ignore
  (list
   ;; Package
   'sb-impl::%name 'sb-impl::%used-by 'sb-impl::internal-symbols
   'sb-impl::external-symbols 'sb-impl::doc-string 'sb-impl::%local-nicknames
   ;; Readtable
   'sb-impl::%readtable-normalization 'sb-impl::%readtable-case
   'sb-impl::%readtable-string-preference 'sb-impl::%readtable-symbol-preference
   ;; Pathname
   'sb-impl::host 'sb-impl::device 'sb-impl::name 'sb-impl::version 'type 'namestring
   ;; Hash-table
   'sb-impl::test 'sb-impl::rehash-size 'sb-impl::rehash-threshold 'sb-impl::%count
   ;; Stream
   'sb-impl::file 'sb-impl::element-type 'sb-impl::dual-channel-p 'sb-impl::pathname))

#+sbcl
(defun except-sbcl-props (object)
  (mapcar #'(lambda (cons)
              (list (car cons) (cdr cons)))
          (set-difference
           (nth-value 2 (sb-impl::inspected-parts object))
           sbcl-props-to-ignore
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

#+allegro
(defun value (def object)
  (let ((type (inspect::field-def-type def))
        (name (inspect::field-def-name def))
        (access (inspect::field-def-access def)))
    (ecase type
      ((:unsigned-word :unsigned-byte :unsigned-natural
                       :unsigned-long :unsigned-half-long
                       :unsigned-3byte :unsigned-long32)
       (list name (inspect::component-ref-v object access type)))
      ((:lisp :value :func)
       (list name (inspect::component-ref object access)))
      (:indirect
       (destructuring-bind (prefix count ref set) access
         (declare (ignore set prefix))
         (loop for i below (funcall count object)
               append (list (format nil "~A-~D" name i)
                            (funcall ref object i))))))))

#+allegro
(defun all-allegro-fields (o)
  (ignore-errors
   (loop for (d dd) on (inspect::inspect-ctl o)
         for (name value) = (value d o)
         for keyword = (make-keyword name)
         until (eq d dd)
         collect (list keyword value))))

#+allegro
(defun allegro-fields (o &rest fields)
  (remove-if-not
   (lambda (field)
     (member (first field) fields))
   (all-allegro-fields o)))

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

(defmacro deffield (specifier name function)
  `(defmethod fields* reverse-append ((object ,specifier) &key &allow-other-keys)
     (list (list ,name (,function object)))))

(deffields (object symbol)
  `((symbol-name ,(symbol-name object))
    (symbol-package ,(symbol-package object))
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
           `((special-operator-p t)))
          ((macro-function object)
           `((macro-function ,(macro-function object))))
          ((fboundp object)
           `((symbol-function
              ,(symbol-function object)
              ,(lambda (new-value _)
                 (declare (ignorable _))
                 ;; `fdefinition'? `compile'?
                 (setf (symbol-function object) new-value)))
             ,@(when (compiler-macro-function object)
                 `((:compiler-macro-binding ,(compiler-macro-function object))))))))
    ,@(when (boundp object)
        `((symbol-value
           ,(symbol-value object)
           ,(lambda (new-value _)
              (declare (ignorable _))
              (setf (symbol-value object) new-value)))))
    ,@(when (ignore-errors (find-class object nil))
        `((class ,(ignore-errors (find-class object nil)))))
    ,@(when (uiop:find-package* object nil)
        `((package ,(uiop:find-package* object nil))))
    (symbol-plist ,(symbol-plist object))))

(-> dotted-p (list) boolean)
(defun dotted-p (cons)
  (not (null (cdr (last cons)))))

(deffields (object null)
  (call-next-method))

;; TODO: Extensible sequences' features?
(deffields (object sequence)
  (unless (and (consp object)
               (dotted-p object))
    `((length ,(length object)))))

(deffields (object cons)
  (append
   (loop for i from 0
         for elem in (butlast object)
         collect (let ((i i)
                       (elem elem))
                   (list i elem (lambda (new-value _)
                                  (declare (ignorable _))
                                  (setf (nth i object) new-value)))))
   (let ((last-index (length (butlast object))))
     `((,last-index ,(car (last object))
                    ,(lambda (new-value _)
                       (declare (ignorable _))
                       (setf (car (last object)) new-value)))))
   (when (dotted-p object)
     `((cdr ,(cdr (last object))
            ,(lambda (new-value _)
               (declare (ignorable _))
               (setf (cdr (last object)) new-value)))))))


(deffields (object complex)
  `((imagpart ,(imagpart object))
    (realpart ,(realpart object))))

(deffields (object ratio)
  `((numerator ,(numerator object))
    (denominator ,(denominator object))
    (round ,(round object))))

(deffields (object float)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float object)
    `((:exponent ,exponent)
      (:mantissa ,significand)
      (:sign ,sign)
      (float-radix ,(float-radix object))
      (float-precision ,(float-precision object))
      ,@(when (typep object 'short-float)
          `((most-positive-short-float ,most-positive-short-float)
            (most-negative-short-float ,most-negative-short-float)))
      ,@(when (typep object 'single-float)
          `((most-positive-single-float ,most-positive-single-float)
            (most-negative-single-float ,most-negative-single-float)))
      ,@(when (typep object 'double-float)
          `((most-positive-double-float ,most-positive-double-float)
            (most-negative-double-float ,most-negative-double-float)))
      ,@(when (typep object 'long-float)
          `((most-positive-long-float ,most-positive-long-float)
            (most-negative-long-float ,most-negative-long-float)))
      (:nearest-integer ,(round object)))))

(deffields (object integer)
  (append
   `((integer-length (integer-length object)))
   (when (typep object 'fixnum)
     `((most-positive-fixnum ,most-positive-fixnum)
       (most-negative-fixnum ,most-negative-fixnum)))))

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
  `((package-name ,(package-name object))
    (:description ,(documentation object t))
    (package-nicknames ,(package-nicknames object))
    (:external-symbols ,(external-symbols object))
    (:internal-symbols ,(internal-symbols object))
    (:inherited-symbols ,(inherited-symbols object))
    (package-used-by-list ,(package-used-by-list object))
    (package-use-list ,(package-use-list object))
    #+(or sb-package-locks package-locks allegro)
    (locked #+sbcl ,(sb-ext:package-locked-p object)
            #+ecl ,(ext:package-locked-p object)
            #+allegro ,(cadadr (allegro-fields object :lock))
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
    ,@(except-sbcl-props object)
    #+allegro
    ,@(allegro-fields
       object :tables :mode :direct-parent :direct-children :foreign-protocol :flat)))

(deffields (object readtable)
  `((readtable-case
     ,(readtable-case object)
     ,(lambda (new-value _)
        (declare (ignorable _))
        (setf (readtable-case object) new-value)))
    #+sbcl
    (:normalization
     ,(sb-ext::readtable-normalization object)
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
    ,@(except-sbcl-props object)
    #+allegro
    ,@(allegro-fields object :attr :macros :dispatch)))

(deffields (object random-state)
  `(#+clozure
    ,@(get-ccl-props object 'ccl::random.mrg31k3p-state)
    #+sbcl
    ,@(except-sbcl-props object)
    #+allegro
    ,@(allegro-fields object :smplocker :mti :fixseed)))

(deffields (object character)
  `((char-code ,(char-code object))
    (char-name ,(char-name object))
    (digit-char-p ,(digit-char-p object))
    (alpha-char-p ,(alpha-char-p object))
    (graphic-char-p ,(graphic-char-p object))
    (alphanumericp ,(alphanumericp object))
    (char-code-limit ,char-code-limit)))

(deffields (object string)
  `((:uppercase ,(every #'upper-case-p object))
    (:lowercase ,(every #'lower-case-p object))
    (:graphic ,(every #'graphic-char-p object))))

(deffields (object array)
  `((array-dimensions
     ,(array-dimensions object)
     ,(when (adjustable-array-p object)
        (lambda (new-value _)
          (declare (ignorable _))
          (adjust-array object new-value))))
    (array-rank ,(array-rank object))
    (array-element-type ,(array-element-type object))
    (upgraded-array-element-type ,(upgraded-array-element-type (type-of object)))
    ,@(when (array-displacement object)
        (multiple-value-bind (displaced-to offset)
            (array-displacement object)
          `((:displaced-to ,displaced-to)
            (:offset ,offset))))
    ,@(when (array-has-fill-pointer-p object)
        `((fill-pointer
           ,(fill-pointer object)
           (lambda (new-value _)
             (declare (ignorable _))
             (setf (fill-pointer object) new-value)))))
    ,@(loop for elt across object
            for i from 0
            collect (list i elt
                          (lambda (new-value _)
                            (declare (ignorable _))
                            (setf (elt object i) new-value))))))

(deffield logical-pathname
  :translation translate-logical-pathname)

(deffields (object pathname)
  (let ((logical-p (uiop:logical-pathname-p object))
        (link-p (not (equal (truename object) object))))
    `((wild-pathname-p ,(wild-pathname-p object))
      (namestring ,(namestring object))
      ,@(unless (or logical-p
                    (string= (namestring object)
                             (uiop:native-namestring object)))
          `((:native-namestring ,(uiop:native-namestring object))))
      ,@(when link-p
          `((truename ,(truename object))))
      (pathname-host ,(pathname-host object))
      (pathname-device ,(pathname-device object))
      (pathname-directory ,(pathname-directory object))
      (pathname-name ,(pathname-name object))
      (pathname-type ,(pathname-type object))
      (pathname-version ,(pathname-version object))
      (directory-namestring ,(directory-namestring object))
      ,@(when (uiop:file-pathname-p object)
          `((file-author ,(file-author object))
            (file-write-date ,(file-write-date object))))
      ,@(when (member (pathname-type object)
                      '("lsp" "lisp")
                      :test #'string-equal)
          `((compile-file-pathname ,(compile-file-pathname object))))
      ,@(when (uiop:directory-pathname-p object)
          `((:files ,(uiop:directory-files object))
            (:subdirectories ,(uiop:subdirectories object))))
      (user-homedir-pathname ,(user-homedir-pathname))
      (:cwd ,(uiop:getcwd))
      #+sbcl
      ,@(except-sbcl-props object)
      #+allegro
      ,@(allegro-fields object :dir-namestring))))

(deffields (object hash-table)
  `((hash-table-test ,(hash-table-test object))
    (hash-table-size ,(hash-table-size object))
    (hash-table-count ,(hash-table-count object))
    (hash-table-rehash-size ,(hash-table-rehash-size object))
    (hash-table-rehash-threshold ,(hash-table-rehash-threshold object))
    #+(or sbcl ecl clozure abcl)
    (:weakness
     #+ecl ,(si:hash-table-weakness object)
     #+sbcl ,(sb-impl::hash-table-weakness object)
     #+clozure ,(ccl:hash-table-weak-p object)
     #+abcl ,(system:hash-table-weakness object))
    #+clozure
    ,@(get-ccl-props
       object
       'ccl::nhash.keytransF 'ccl::nhash.compareF 'ccl::nhash.rehash-bits 'ccl::nhash.vector
       'ccl::nhash.lock 'ccl::nhash.owner 'ccl::nhash.grow-threshold 'ccl::nhash.puthash-count
       'ccl::nhash.exclusion-lock 'ccl::nhash.find 'ccl::nhash.find-new 'ccl::nhash.read-only
       'ccl::nhash.min-size)
    #+sbcl
    ,@(except-sbcl-props object)
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
                                    (list (list 'other-pairs complex-props)))))))

(deffields (object two-way-stream)
  `((:input ,(two-way-stream-input-stream object))
    (:output ,(two-way-stream-output-stream object))))

;; On SBCL, echo-stream is an instance of two-way-stream...
(deffields (object echo-stream)
  `((:echo-input ,(echo-stream-input-stream object))
    (:echo-output ,(echo-stream-output-stream object))))

(deffield concatenated-stream :concatenates concatenated-stream-streams)
(deffield broadcast-stream :broadcasts broadcast-stream-streams)
(deffield synonym-stream :synonym synonym-stream-symbol)
;; TODO: string-stream. Somehow `get-output-stream-string` without
;; clearing the stream. Maybe get the string and then re-output it to
;; the stream.

(deffields (object file-stream)
  `((pathname ,(pathname object))
    (file-position ,(file-position object))
    (file-length (file-length object))
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
    (interactive-stream-p ,(interactive-stream-p object))
    #+abcl
    ,@`((:offset ,(system::stream-offset object))
        (:line-number ,(system::stream-line-number object))
        (:system ,(system::system-stream-p object))
        (:url ,(typep object 'system:url-stream))
        (:jar ,(typep object 'system:jar-stream))
        ,@(when (output-stream-p object)
            `((:charpos ,(system::stream-charpos object)))))
    (open-stream-p
     ,(open-stream-p object)
     ,(lambda (new-value old-value)
        (when old-value
          (case new-value
            ((nil) (close object))
            (:abort (close object :abort t))))))
    (stream-element-type ,(stream-element-type object))
    (stream-external-format ,(stream-external-format object))
    #+sbcl
    ,@(except-sbcl-props object)))

(-> object-slots ((or standard-object structure-object)) list)
(defun object-slots (object)
  (mapcar #'slot-definition-name
          (class-slots (class-of object))))

(-> inspect-slots ((or standard-object structure-object)) list)
(defun inspect-slots (object)
  (append
   #+clozure
   (get-ccl-props
    object
    'ccl::instance.hash 'ccl::instance.slots)
   (mapcar (lambda (name)
             (list name (if (slot-boundp object name)
                            (slot-value object name)
                            :unbound)
                   (lambda (new-value _)
                     (declare (ignorable _))
                     (setf (slot-value object name) new-value))))
           (set-difference (object-slots object)
                           #+sbcl sbcl-props-to-ignore
                           #+allegro '(excl::plist excl::flags)
                           #-(or sbcl allegro) nil))))

(deffields (object standard-object)
  (inspect-slots object))

(deffields (object structure-object)
  (inspect-slots object))

(deffields (object function)
  `((:name
     ,(function-name* object)
     ,(lambda (new-name old-name)
        (compile new-name (fdefinition old-name))))
    (:arguments ,(function-lambda-list* object))
    (compiled-function-p ,(compiled-function-p object))
    ,@(when (function-type* object)
        `((:ftype ,(function-type* object))))
    (:expression
     ,(function-lambda-expression* object)
     ,(lambda (new-value _)
        (declare (ignorable _))
        (compile (function-name* object)
                 new-value)))
    #+allegro
    ,@(allegro-fields object :start :code :gc-info :immed-args :locals)
    (lambda-list-keywords ,lambda-list-keywords)
    (call-arguments-limit ,call-arguments-limit)
    (lambda-parameters-limit ,lambda-parameters-limit)))

(-> restart-interactive (restart))
(defun restart-interactive (restart)
  (declare (ignorable restart))
  #+clozure (ccl::%restart-interactive restart)
  #+sbcl (sb-kernel::restart-interactive-function restart)
  #+ecl (si::restart-interactive-function restart)
  #-(or clozure sbcl ecl) nil)

(deffields (object restart)
  `((restart-name ,(restart-name object))
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
  `((compute-restarts ,(compute-restarts object))
    (continue ,(find 'continue (compute-restarts object) :key #'restart-name))))

(deffields (object simple-condition)
  `((:format-control ,(simple-condition-format-control object))
    (:format-arguments ,(simple-condition-format-arguments object))))

(deffields (object arithmetic-error)
  `((:operation ,(arithmetic-error-operation object))
    (:operands ,(arithmetic-error-operands object))))

(deffield cell-error :name cell-error-name)
(deffield package-error :package package-error-package)
(deffield stream-error :stream stream-error-stream)
(deffield print-not-readable :object print-not-readable-object)
(deffield unbound-slot :instance unbound-slot-instance)
(deffield file-error :pathname file-error-pathname)

(deffields (object type-error)
  `((:datum ,(type-error-datum object))
    (:expected ,(type-error-expected-type object))))

(defmethod fields* reverse-append (object &key &allow-other-keys)
  (let ((slot-defs (ignore-errors (class-slots (class-of object)))))
    `((:self ,object) ;; Inspired by CCL.
      (:id ,(id object))
      (class-of
       ,(class-of object)
       ,(unless (typep (class-of object) 'built-in-class)
          (lambda (new-value _)
            (declare (ignorable _))
            (change-class object (find-class new-value)))))
      ,@(when slot-defs
          (list (list :slot-definitions slot-defs)))
      (type-of ,(type-of object))
      #+clozure
      (:wrapper ,(ccl::%class-own-wrapper (class-of object)))
      #+allegro
      ,@(allegro-fields :lock-index :hash :type :flags :xflags :excl-type :plist))))
