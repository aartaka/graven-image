;;; SBCL:
(function-lambda-expression #'(lambda (x) x))
;; (LAMBDA (X) X)
;; T
;; (LAMBDA (X))
(function-lambda-expression
 (funcall #'(lambda () #'(lambda (x) x))))
;; (LAMBDA (X) X)
;; T
;; (LAMBDA (X))
(function-lambda-expression
 (funcall #'(lambda (x) #'(lambda () x)) nil))
;; (LAMBDA () X)
;; T
;; (LAMBDA ())
(flet ((foo (x) x))
  (setf (symbol-function 'bar) #'foo)
  (function-lambda-expression #'bar))
;; ; in: FLET ((FOO (X) X))
;; ;     #'BAR
;; ;
;; ; caught STYLE-WARNING:
;; ;   undefined function: COMMON-LISP-USER::BAR
;; ;
;; ; compilation unit finished
;; ;   Undefined function:
;; ;     BAR
;; ;   caught 1 STYLE-WARNING condition
;; (LAMBDA (X) (BLOCK FOO X))
;; T
;; (FLET FOO)
(progn
  (defun foo ()
    (flet ((bar (x) x))
      #'bar))
  (function-lambda-expression (foo)))
;; (LAMBDA (X) (BLOCK BAR X))
;; T
;; (FLET BAR :IN FOO)
(function-lambda-expression #'identity)
;; NIL
;; T
;; IDENTITY
(progn
  (defun bar (x) x)
  (function-lambda-expression #'bar))
;; (LAMBDA (X) (BLOCK BAR X))
;; T
;; BAR


;;; CCL:
(function-lambda-expression #'(lambda (x) x))
;; NIL
;; NIL
;; NIL
(function-lambda-expression
 (funcall #'(lambda () #'(lambda (x) x))))
;; NIL
;; NIL
;; NIL
(function-lambda-expression
 (funcall #'(lambda (x) #'(lambda () x)) nil))
;; NIL
;; T
;; NIL
(flet ((foo (x) x))
  (setf (symbol-function 'bar) #'foo)
  (function-lambda-expression #'bar))
;; ;Compiler warnings :
;; ;   In an anonymous lambda form at position 90: Undefined function BAR
;; NIL
;; T
;; FOO
(progn
  (defun foo ()
    (flet ((bar (x) x))
      #'bar))
  (function-lambda-expression (foo)))
(function-lambda-expression (foo))
;; NIL
;; T
;; (:INTERNAL BAR FOO)
(function-lambda-expression #'identity)
;; NIL
;; NIL
;; IDENTITY
(progn
  (defun bar (x) x)
  (function-lambda-expression #'bar))
;; NIL
;; NIL
;; BAR


;; ECL:
(function-lambda-expression #'(lambda (x) x))
;; (LAMBDA (X) X)
;; NIL
;; NIL
(function-lambda-expression
 (funcall #'(lambda () #'(lambda (x) x))))
;; (LAMBDA (X) X)
;; NIL
;; NIL
(function-lambda-expression
 (funcall #'(lambda (x) #'(lambda () x)) nil))
;; (LAMBDA NIL X)
;; ((X))
;; NIL
(flet ((foo (x) x))
  (setf (symbol-function 'bar) #'foo)
  (function-lambda-expression #'bar))
;; (EXT:LAMBDA-BLOCK FOO (X) X)
;; NIL
;; FOO
(progn
  (defun foo ()
    (flet ((bar (x) x))
      #'bar))
  (function-lambda-expression (foo)))
;; (EXT:LAMBDA-BLOCK BAR (X) X)
;; NIL
;; BAR
(function-lambda-expression #'identity)
;; NIL
;; NIL
;; IDENTITY
(progn
  (defun bar (x) x)
  (function-lambda-expression #'bar))
;; (EXT:LAMBDA-BLOCK BAR (X) (DECLARE (SI::C-GLOBAL)) X)
;; NIL
;; BAR

;; ABCL:
(function-lambda-expression #'(lambda (x) x))
;; (LAMBDA (X) X)
;; NIL
;; NIL
(function-lambda-expression
 (funcall #'(lambda () #'(lambda (x) x))))
;; (LAMBDA (X) X)
;; NIL
;; NIL
(function-lambda-expression
 (funcall #'(lambda (x) #'(lambda () x)) nil))
;; (LAMBDA NIL X)
;; #<ENVIRONMENT {1F7D2A4E}>
;; NIL
(flet ((foo (x) x))
  (setf (symbol-function 'bar) #'foo)
  (function-lambda-expression #'bar))
;; (LAMBDA (X) (BLOCK FOO X))
;; NIL
;; (FLET FOO)
(progn
  (defun foo ()
    (flet ((bar (x) x))
      #'bar))
  (function-lambda-expression (foo)))
;; (LAMBDA (X) (BLOCK BAR X))
;; NIL
;; (FLET BAR)
(function-lambda-expression #'identity)
;; NIL
;; T
;; IDENTITY
(progn
  (defun bar (x) x)
  (function-lambda-expression #'bar))
;; (LAMBDA (X) (BLOCK BAR X))
;; NIL
;; BAR

;;; CLISP:
(function-lambda-expression #'(lambda (x) x))
;; (LAMBDA (X) X)
;; #(NIL NIL NIL NIL ((DECLARATION OPTIMIZE DECLARATION DYNAMICALLY-MODIFIABLE)))
;; :LAMBDA
(function-lambda-expression
 (funcall #'(lambda () #'(lambda (x) x))))
;; (LAMBDA (X) X)
;; #(NIL NIL NIL NIL ((DECLARATION OPTIMIZE DECLARATION DYNAMICALLY-MODIFIABLE)))
;; :LAMBDA
(function-lambda-expression
 (funcall #'(lambda (x) #'(lambda () x)) nil))
;; (LAMBDA NIL X)
;; #(#(X NIL NIL) NIL NIL NIL
;;   ((DECLARATION OPTIMIZE DECLARATION DYNAMICALLY-MODIFIABLE)))
;; :LAMBDA
(flet ((foo (x) x))
  (setf (symbol-function 'bar) #'foo)
  (function-lambda-expression #'bar))
;; (LAMBDA (X) (BLOCK FOO X))
;; #(NIL NIL NIL NIL ((DECLARATION OPTIMIZE DECLARATION DYNAMICALLY-MODIFIABLE)))
;; FOO
(progn
  (defun foo ()
    (flet ((bar (x) x))
      #'bar))
  (function-lambda-expression (foo)))
;; (LAMBDA (X) (BLOCK BAR X))
;; #(NIL NIL NIL NIL ((DECLARATION OPTIMIZE DECLARATION DYNAMICALLY-MODIFIABLE)))
;; BAR
(function-lambda-expression #'identity)
;; NIL
;; NIL
;; IDENTITY
(progn
  (defun bar (x) x)
  (function-lambda-expression #'bar))
;; (LAMBDA (X) (DECLARE (SYSTEM::IN-DEFUN BAR)) (BLOCK BAR X))
;; #(NIL NIL NIL NIL ((DECLARATION OPTIMIZE DECLARATION DYNAMICALLY-MODIFIABLE)))
;; BAR

;;; Allegro
(function-lambda-expression #'(lambda (x) x))
;; (LAMBDA (X) X)
;; NIL
;; NIL
(function-lambda-expression
 (funcall #'(lambda () #'(lambda (x) x))))
;; (LAMBDA (X) X)
;; NIL
;; NIL
(function-lambda-expression
 (funcall #'(lambda (x) #'(lambda () x)) nil))
;; (LAMBDA NIL X)
;; #<Augmentable INTERPRETER environment 1>
;; NIL
(flet ((foo (x) x))
  (setf (symbol-function 'bar) #'foo)
  (function-lambda-expression #'bar))
;; (LAMBDA (X) (BLOCK FOO X))
;; NIL
;; (FLET () FOO)
(progn
  (defun foo ()
    (flet ((bar (x) x))
      #'bar))
  (function-lambda-expression (foo)))
;; (LAMBDA (X) (BLOCK BAR X))
;; #<Augmentable INTERPRETER environment 1>
;; (FLET FOO BAR)
(function-lambda-expression #'identity)
;; ; Autoloading for EXCL::LDB-LISP-SOURCE:
;; ; Fast loading
;; ;    /gnu/store/6xyvxabjlyap4p8myw2xk8xkjh6dxrlj-allegro-cl-10.1/share/allegro-cl/code/lldb.004
;; ;;; Installing lldb patch, version 4.
;; NIL
;; NIL
;; IDENTITY
(progn
  (defun bar (x) x)
  (function-lambda-expression #'bar))
;; (LAMBDA (X) (BLOCK BAR X))
;; NIL
;; BAR
