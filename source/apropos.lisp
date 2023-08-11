;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defvar old-apropos-list (symbol-function 'apropos-list)
  "Good old CL `apropos-list' so we can reuse it in `apropos-list*'.")

(-> reduce-old-apropos (string (or package symbol list) &rest list))
(defun reduce-old-apropos (string packages &rest args)
  (cond
    ((null packages)
     (apply old-apropos-list string nil args))
    ((or (packagep packages)
         (symbolp packages)
         (and (listp packages)
              (= 1 (length packages))))
     (apply old-apropos-list
            string (first (uiop:ensure-list packages)) args))
    (t
     (reduce #'append (uiop:ensure-list packages)
             :key (lambda (p) (apply old-apropos-list string p args))))))

(-> all-docs (symbol))
(defun all-docs (sym)
  ;; Not including compiler-macro, setf, and method-combination,
  ;; because yes.
  (uiop:strcat (ignore-errors (documentation sym 'variable))
               #\Newline
               (ignore-errors (documentation sym 'function))
               #\Newline
               (ignore-errors (documentation sym 'type))
               #\Newline
               (ignore-errors (documentation sym 'structure))))

(-> %apropos-list (string (or package symbol list) boolean boolean) list)
(defun %apropos-list (string packages external-only docs-too)
  (loop for package in packages
        with symbols = (unless external-only
                         (reduce-old-apropos string package))
        when external-only
          nconc (loop for sym being the external-symbol in package
                      when (or (search string (symbol-name sym) :test #'string-equal)
                               (and docs-too
                                    (search string (all-docs sym) :test #'string-equal)))
                        do (push sym symbols))
        else
          do (loop for sym being the present-symbol in package
                   when (and
                         (eq (symbol-package sym) (find-package package))
                         (and docs-too
                              (search string (all-docs sym) :test #'string-equal)))
                     do (pushnew sym symbols))))

(define-generic apropos-list* (string &optional (packages (list-all-packages)) external-only docs-too)
  "Search for symbols in PACKAGES with names (+docs when DOCS-TOO) containing STRING.
Sorts these by the frequency of STRING appearance in the respective
name/documentation.

When EXTERNAL-ONLY, only search the external symbols of
PACKAGES (ported from SBCL/Allegro).

PACKAGES can be:
- A package.
- A symbol denoting a package.
- A list of packages/symbols.
- NIL (all the accessible packages are implied).

Influenced by:
- Current list of packages and their contents."
  (declare (ignorable external-only docs-too))
  (let ((string (string string)))
    (cond
      ;; NOTE: Reusing built-in external-only functionality of SBCL/Allegro.
      ;; FIXME: Is their listing reliable enough? Seems te be so.
      #+(or sbcl allegro)
      ((not docs-too)
       (reduce-old-apropos string packages external-only))
      ;; FIXME: Test on more impls (help needed with this one!)
      ;; FIXME: The case-sensitivity of standard `apropos/-list' is
      ;; not specified, so this might bring some inconsistencies. All
      ;; the implementations I've tested list things in the
      ;; case-insensitive fashion, though.
      #+(or clozure ecl gcl abcl clisp)
      ((and (not external-only) (not docs-too))
       (reduce-old-apropos string packages))
      #+(or sbcl clozure ecl gcl abcl clisp allegro)
      (t (%apropos-list string packages external-only docs-too))
      #-(or sbcl clozure ecl gcl abcl clisp allegro)
      (t
       (warn "apropos-list* is not implemented for this CL, help in implementing it!")
       (reduce-old-apropos string packages)))))

(define-generic apropos* (string &optional package external-only docs-too)
  "Print a list of PACKAGE symbols with names (+docs when DOCS-TOO) containing STRING.

In case the symbol is naming several things (variable, macro,
function, class), these interpretations will be listed
separately. Lists documentation for documented things and values for
bound variables.

When EXTERNAL-ONLY, only search the external symbols of
PACKAGE (ported from SBCL).

PACKAGE can be:
- A package.
- A symbol denoting a package.
- A list of packages/symbols.
- NIL (all the accessible packages are implied).

Influenced by:
- Current list of packages and their contents.
- `*standard-output*'.

Note that you can influence the printout by let-bindings:
- `*standard-output*',
- `*print-case*'
- `*print-level*'
- `*print-length*'
- and many other printer variables."
  ;; This is because function-* APIs often throw warnings for
  ;; non-implemented methods.
  (handler-bind ((warning #'muffle-warning))
    (dolist (symbol (apropos-list* string package external-only docs-too))
      (flet ((crop-docs (docs)
               (let ((line (when docs
                             (first (uiop:split-string docs :separator '(#\newline))))))
                 (cond
                   ((equal line docs)
                    line)
                   ;; If it's a finished sentence, then return.
                   ((uiop:string-suffix-p line ".")
                    line)
                   ;; If unfinished, add ellipsis.
                   (t
                    (uiop:strcat line "..."))))))
        (fresh-line)
        (format t "~s" symbol)
        (when (boundp symbol)
          (cond
            ((or (eq t symbol)
                 (null symbol)
                 (keywordp symbol))
             (format t " [~a]" 'self-evaluating))
            ((constantp symbol)
             (format t " [~a = ~s~@[ : ~a~]]"
                     'constant (symbol-value symbol) (crop-docs (documentation symbol 'variable))))
            (t (format t " [~a = ~s~@[ : ~a~]]"
                       'variable (symbol-value symbol) (crop-docs (documentation symbol 'variable))))))
        (when (fboundp symbol)
          (format t " [~a~@[ ~s~]~@[ : ~a~]]"
                  (cond
                    ((special-operator-p symbol)
                     'special-operator)
                    ((macro-function symbol)
                     'macro)
                    ((typep (symbol-function symbol) 'generic-function)
                     'generic-function)
                    (t 'function))
                  (unless (special-operator-p symbol)
                    (function-lambda-list* (or (macro-function symbol)
                                               (symbol-function symbol))))
                  (unless (special-operator-p symbol)
                    (crop-docs (or (documentation symbol 'function)
                                   (ignore-errors (documentation (macro-function symbol) t))
                                   (ignore-errors (documentation (symbol-function symbol) t)))))))
        (when (ignore-errors (find-class symbol nil))
          (format t " [~a~@[ ~s~]~@[ : ~a~]]"
                  'class
                  (mapcar #'class-name
                          (closer-mop:class-direct-superclasses (find-class symbol nil)))
                  (crop-docs
                   (or (documentation symbol 'type)
                       (documentation symbol 'structure))))))))
  (values))

;;; Helpers

(define-generic apropod* (string &optional package external-only)
  "A version of `apropos*', but with documentation search by default."
  (apropos* string package external-only t))
