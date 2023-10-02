;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(deftype function-designator ()
  '(or function generic-function standard-method))

(defvar old-function-lambda-expression (symbol-function 'function-lambda-expression))

(-> function-closure-p (function-designator) (or boolean list))
(defun function-closure-p (function)
  ;; TODO: ECL returns closures somehow, but the implementation is
  ;; terribly obscure...
  (let ((function (if (typep function 'standard-method)
                      (method-generic-function function)
                      function)))
    (declare (ignorable function))
    #+clozure
    (and (typep function 'ccl:compiled-lexical-closure)
         ;; Convert to alist.
         (loop for (name value) in (ccl::closure-closed-over-values function)
               collect (cons name value)))
    #+(or cmucl scl)
    (and (= (kernel:get-type function) vm:closure-header-type)
         (loop for i below (- (kernel:get-closure-length function)
                              #+cmucl 1
                              #+scl (1- vm:closure-info-offset))
               collect (cons i (kernel:%closure-index-ref function i))))
    #+sbcl
    (and (sb-kernel:closurep function)
         ;; Is that the right one?
         (loop for i below (1- (sb-kernel:get-closure-length function))
               collect (cons i (sb-kernel:%closure-index-ref function i))))
    #+abcl
    (let ((environment (nth-value 1 (funcall old-function-lambda-expression function))))
      (cond
        ((and environment
              (typep environment 'system::environment))
         (system:environment-variables environment))
        (environment environment)
        (t nil)))
    #+allegro
    (let* ((closure (nth-value 1 (funcall old-function-lambda-expression function))))
      (cond
        ((typep closure 'sys::augmentable-environment)
         (let ((ht (sys::ha$h-table-ht
                    (slot-value (sys::augmentable-environment-base closure)
                                'system::variable-hashtable))))
           (typecase ht
             (cons
              (cons (car ht) (caadr (cadadr ht))))
             (hash-table
              (loop for key being the hash-key in ht
                      using (hash-value val)
                    collect (cons key (caar (cdadar val))))))))
        ;; FIXME: There should be a way to crack this one!
        ((typep closure 'excl::closure)
         t)))
    #+clisp
    ;; TODO: venv fenv benv genv denv
    (let ((closure (nth-value 1 (funcall old-function-lambda-expression function))))
      (when (arrayp closure)
        (loop for (name value)
                on (coerce (elt closure 0) 'list)
                  by #'cddr
              while name
              collect (cons name value))))
    #-(or clozure cmucl scl sbcl abcl allegro clisp)
    (prog1
        t
      (load-time-warn "closure inspection is not implemented for this CL, help in implementing it!"))))

;; FIXME: Phew, that's a long one... Maybe use Slynk after all? Graven
;;  Image won't be dependency-free and will have dangerous recursive
;;  references to `function-lambda-expression' on some implementations, though.
(-> function-name (function-designator) t)
(defun function-name (function)
  (let ((function (if (typep function 'standard-method)
                      (method-generic-function function)
                      function)))
    (declare (ignorable function))
    #+allegro
    (cross-reference::object-to-function-name function)
    #+clozure
    (ccl:function-name function)
    #+clasp
    (if (typep function 'generic-function)
        (clos::generic-function-name function)
        (ext:compiled-function-name function))
    #+(or cmucl scl)
    (cond ((eval:interpreted-function-p function)
           (eval:interpreted-function-name function))
          #+cmucl
          ((pcl::generic-function-p function)
           (pcl::generic-function-name function))
          #+scl
          ((typep function 'generic-function)
           (clos:generic-function-name function))
          ((c::byte-function-or-closure-p function)
           (c::byte-function-name function))
          (t (kernel:%function-name (kernel:%function-self function))))
    #+cormanlisp
    (ignore-errors (getf (cl::function-info-list function) 'cl::function-name))
    #+ecl
    (if (typep function 'generic-function)
        (clos:generic-function-name function)
        (si:compiled-function-name function))
    #+mkcl
    (si:compiled-function-name function)
    #+sbcl
    (sb-impl::%fun-name function)
    #-(or abcl allegro clozure clasp cmucl cormanlisp ecl lispworks mkcl sbcl scl)
    (load-time-warn "function name fetching is not implemented for this CL, help in implementing it!")))

(-> function-name-symbol (function-designator))
(defun function-name-symbol (function)
  (let ((name (function-name function)))
    (typecase name
      (symbol name)
      ((cons (eql macro-function) t)
       (second name)))))

(-> transform-definition-to-lambda (list boolean) list)
(defun transform-definition-to-lambda (definition force)
  (when definition
    (case (first definition)
      (lambda definition)
      ((defmacro defun)
       `(lambda ,@(cddr definition)))
      (defmethod
          (when force
            `(lambda ,@(member-if #'listp definition))))
      (defgeneric
          (when (and force
                     (= 1 (count :method (cdddr definition)
                                 :key #'first)))
            (let ((single-method (find :method (cdddr definition)
                                       :key #'first)))
              (when (equal (second single-method)
                           (third definition))
                `(lambda ,(third definition)
                   ,@(when (documentation (second definition) 'function)
                       (list (documentation (second definition) 'function)))
                   ,@(cddr single-method))))))
      (t nil))))

(-> transform-definition-to-name (list) symbol)
(defun transform-definition-to-name (definition)
  (when definition
    (case (first definition)
      (lambda (if (second definition)
                  `(lambda ,(second definition))
                  'lambda))
      ((defmacro defun defgeneric defmethod) (second definition))
      (t nil))))

(-> function-arglist (function-designator symbol))
(defun function-arglist (function name)
  ;; Slynk hooks into `function-lambda-expression', but that's a
  ;; dangerous recursive reference for us, thus
  ;; `old-function-lambda-expression'.
  (declare (ignorable function name))
  ;; Slynk often hooks into `function-lambda-expression', but that's a
  ;; dangerous recursive reference for us, thus
  ;; `old-function-lambda-expression'.
  (or (when (typep function 'generic-function)
        (generic-function-lambda-list function))
      (when (typep function 'standard-method)
        (method-lambda-list function))
      (ignore-errors (second (funcall old-function-lambda-expression function)))
      (macrolet ((try-arglist (&rest vars)
                   `(or ,@(loop for var in vars
                                collect `(ignore-errors
                                          (#+abcl
                                           sys::arglist
                                           #+allegro
                                           excl:arglist
                                           #+clozure
                                           ccl:arglist
                                           #+clisp
                                           ext:arglist
                                           #+sbcl
                                           sb-introspect:function-lambda-list
                                           ,var))))))
        (ignore-errors
         #+abcl
         (try-arglist function name)
         #+allegro
         (try-arglist function name)
         #+clozure
         ;; Why `*break-on-signals*' NIL in Slynk?
         (let ((*break-on-signals* nil))
           (try-arglist function name))
         #+clasp
         (sys:function-lambda-list name)
         #+clisp
         (ignore-errors (or (ext:arglist function)
                            (ext:arglist name)))
         #+cmucl
         ;; Copied from Slynk
         (cond ((eval:interpreted-function-p fun)
                (eval:interpreted-function-arglist fun))
               ((pcl::generic-function-p fun)
                (pcl:generic-function-lambda-list fun))
               ((c::byte-function-or-closure-p fun)
                (byte-code-function-arglist fun))
               ((kernel:%function-arglist (kernel:%function-self fun))
                (handler-case (read-arglist fun)
                  (error () :not-available)))
               (t
                (ignore-errors (debug-function-arglist (di::function-debug-function fun)))))
         #+cormanlisp
         (cond
           ((macro-function name)
            (ccl::macro-lambda-list function))
           ((eq (class-of name) cl::the-class-standard-gf)
            (generic-function-lambda-list name))
           (ccl:function-lambda-list name))
         #+ecl
         (ext:function-lambda-list name)
         #+lispworks
         (let ((arglist (lw:function-lambda-list function)))
           (unless (eq arglist :dont-know)
             (labels ((to-symbols (thing)
                        "A primitive rewrite of Slynk's replace-strings-with-symbols."
                        (typecase thing
                          (list (mapcar #'to-symbols thing))
                          (string (intern thing))
                          (t thing)))))
             (to-symbols arglist)))
         #+sbcl
         (try-arglist function name)
         #+scl
         (ext:function-arglist name)))
      #-(or abcl allegro clozure clasp clisp cmucl cormanlisp ecl ecl lispworks sbcl scl)
      (load-time-warn "arglist fetching is not implemented for this CL, help in implementing it!")))

(-> function-source-expression-fallback (function-designator) list)
(defun function-source-expression-fallback (function)
  (let* ((name (function-name-symbol function))
         (arglist (function-arglist function name)))
    `(lambda (,@arglist)
       ,@(let ((doc (or (documentation function t)
                        (when (typep function 'standard-method)
                          (documentation (method-generic-function function) t))
                        (documentation name 'function))))
           (when doc
             (list doc))))))

(-> function-source-expression (function-designator boolean))
(defun function-source-expression (function force)
  (declare (ignorable function force))
  (or
   (handler-case
       (let ((*package* (symbol-package (function-name function))))
         (labels (;; Only used on supported implementations
                  #+(or clozure ecl sbcl abcl)
                  (maybe-unsafe-read (stream)
                    (when stream
                      (handler-case
                          (let ((*read-eval* nil))
                            (read-nolocks stream))
                        (reader-error ()
                          (when force
                            (ignore-errors
                             (read-nolocks stream)))))))
                  ;; Not used on SBCL and unsupported implementations.
                  #+(or clozure ecl abcl)
                  (read-from-position (file position)
                    (when (and position file)
                      (ignore-errors
                       (with-open-file (f file)
			 (file-position f position)
                         (maybe-unsafe-read f))))))
           #+clozure
           (let* ((note (or (ccl:function-source-note function)
                            (find-if #'ccl:source-note-p (first (ccl:find-definition-sources function)))))
                  (text (when note
                          (or (ccl:source-note-text note)
                              (ccl:ensure-source-note-text note))))
                  (position (when (and note (not text))
                              (ccl:source-note-start-pos note)))
                  (file (when (and note (not text))
                          (ignore-errors (translate-logical-pathname (ccl:source-note-filename note))))))
             (cond
               (text
                (read-from-string text nil nil))
               ((and file position)
                (read-from-position file position))))
           #+ecl
           (multiple-value-bind (file position)
               (ext:compiled-function-file function)
             (when (and file position)
               (read-from-position (translate-logical-pathname file) position)))
           #+sbcl
           (let* ((sources
                    (ignore-errors
                     (or (sb-introspect:find-definition-sources-by-name (function-name-symbol function) :function)
                         (sb-introspect:find-definition-sources-by-name (function-name-symbol function) :generic-function)
                         (sb-introspect:find-definition-sources-by-name (function-name-symbol function) :macro))))
                  (file (when sources
                          (sb-introspect:definition-source-pathname (first sources))))
                  (form-path (when sources
                               (sb-introspect:definition-source-form-path (first sources))))
                  (char-offset (when sources
                                 (sb-introspect:definition-source-character-offset (first sources)))))
             ;; FIXME: Not using form number there, because it's too involved
             ;; and likely means some macro magic which will bork the lambda
             ;; expression anyway.
             (cond
               ((and char-offset file)
                (with-open-file (f (translate-logical-pathname file))
                  (loop repeat char-offset
                        do (read-char f nil nil))
                  (maybe-unsafe-read f)))
               ((and form-path file)
                (with-open-file (f (translate-logical-pathname file))
                  (loop repeat (first (uiop:ensure-list form-path))
                        do (maybe-unsafe-read f))
                  (maybe-unsafe-read f)))))
           #+abcl
           (let* ((name (function-name function))
                  (sources (get name 'sys::source nil))
                  (source-triplet (find-if #'(lambda (triplet)
                                               (typecase (first triplet)
                                                 ((eql :macro)
                                                  triplet)
                                                 ((eql :compiler-macro)
                                                  triplet)
                                                 ((cons (eql :function) (cons symbol))
                                                  triplet)
                                                 (t nil)))
                                           sources)))
             (when source-triplet
               (destructuring-bind (_ file position)
                   source-triplet
                 (declare (ignore _))
                 (read-from-position (translate-logical-pathname file) position))))
           #-(or clozure ecl sbcl abcl)
           (load-time-warn "source fetching is not implemented for this CL implementation, help in implementing it!")))
     (error () nil))
   (when force
     (function-source-expression-fallback function))))

(-> ensure-function ((or symbol function-designator)) (or null function-designator))
(defun ensure-function (function)
  (typecase function
    ((and symbol
	  (satisfies special-operator-p))
     nil)
    (symbol
     (or (macro-function function)
         (symbol-function function)))
    ((or function standard-method) function)))

(define-generic function-lambda-expression* (function &optional force)
  "Returns information about FUNCTION:
- The defining lambda, suitable for `compile' (or the best guess at
  getting one, if the FUNCTION is not a regular one and FORCE is on).
- Whether the FUNCTION is closed over some values and what these
  values are (when possible). Returns an alist of closed-over
  variables on most supported implementations.
- The name of the function, whenever applicable.
- The type of the function, whenever found.

When FORCE, return the lambda even if it's not suitable for `compile'
or is otherwise not representing the FUNCTION truthfully. Might be
useful to fetch the arglist (`function-lambda-list*' might work
better) or body, though. Use at your own risk!

Influenced by:
- MOP implementation.
- Implementation support for name/closure/arglist/type inspection.
- Ability to find and parse sources for functions."
  (let* ((function (ensure-function function))
         (definition (ignore-errors (function-source-expression function force))))
    (unless function
      (return-from function-lambda-expression* (values nil nil nil nil)))
    (multiple-value-bind (expression closure-p name)
        (ignore-errors (funcall old-function-lambda-expression function))
      (values
       (or expression
           (when (listp definition)
             (transform-definition-to-lambda definition force)))
       (cond
         ;; Error from `old-function-lambda-expression'.
         ((typep closure-p 'error) nil)
         ;; T is suspicious.
         ((eq closure-p t) (function-closure-p function))
         ;; Allegro, ABCL, and CLISP return opaque env objects.
         #+(or allegro abcl clisp)
         (closure-p (function-closure-p function))
         #-(or allegro abcl clisp)
         (closure-p closure-p)
         (t nil))
       (or name
           (function-name function)
           (transform-definition-to-name definition))
       (or
        #+sbcl
        (sb-introspect:function-type function)
        #+(or cmucl scl)
        (kernel:%function-type function)
        #+(or ecl gcl)
        (let ((fname (function-name-symbol function)))
          (when fname
            (multiple-value-bind (arg-types arg-types-p)
                (compiler::get-arg-types fname)
              (multiple-value-bind (return-type return-type-p)
                  (compiler::get-return-type fname)
                `(function (,@(if arg-types-p
                                  arg-types
                                  (mapcar (lambda (arg)
                                            (if (and (symbolp arg)
                                                     (member arg lambda-list-keywords))
                                                arg
                                                t))
                                          (function-arglist function fname))))
                           ,(if return-type-p
                                return-type
                                t))))))
	#+clozure
	(ccl::find-ftype-decl (function-name-symbol function) ccl::*nx-lexical-environment*)
	#+abcl
	(sys::proclaimed-ftype (function-name-symbol function))
	#+allegro
	(compiler::declared-ftype-p (function-name-symbol function))
        #-(or cmucl scl sbcl ecl gcl clozure abcl allegro)
        nil)))))

;;; Helpers

(define-generic function-lambda-list* (function)
  "Return lambda list of the FUNCTION.
Depends on `function-lambda-expression*', but has an
implementation-dependent fallback."
  (let ((expression (function-lambda-expression* function)))
    (if expression
        (second expression)
        ;; INTERNAL APIS!
        (let ((fn (ensure-function function)))
          (function-arglist fn (function-name-symbol fn))))))

(define-generic function-name* (function)
  "Get the name of the FUNCTION.
It's not guaranteed that the returned value is a symbol.
Depends on `function-lambda-expression*'."
  (nth-value 2 (function-lambda-expression* function)))

(define-generic function-type* (function)
  "Get the ftype of the FUNCTION.
The return value is non-nil only on some implementations.
Depends on `function-lambda-expression*'."
  (nth-value 3 (function-lambda-expression* function)))

(defalias lambda-expression* function-lambda-expression*)
(defalias function-arglist* function-lambda-list*)
(defalias lambda-list* function-lambda-list*)
(defalias arglist* function-lambda-list*)
