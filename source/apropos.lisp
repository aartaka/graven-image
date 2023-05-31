;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defvar old-apropos-list (symbol-function 'apropos-list)
  "Good old CL `apropos-list' so we can reuse it in `apropos-list*'.")

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

(-> count-appearances (string symbol) number)
(defun count-appearances (string symbol)
  "Quite a strange heuristic.
Two scores: symbol name and symbol documentation ones.
- Symbol score is how much of a SYMBOL name STRING occupies.
- Documentation score is how much of docs do STRING mentions occupy."
  (flet ((count-substrings (substring string)
           (let ((count 0))
             (uiop:frob-substrings
              string (list substring)
              (lambda (match frob)
                (incf count)
                (funcall frob match)))
             count)))
    (let ((formatted-sym (format nil "~s" symbol)))
      (+ (/ (count-substrings string formatted-sym)
            (length formatted-sym))
         (let ((docs (all-docs symbol)))
           (if (uiop:emptyp docs)
               0
               (/ (count-substrings string docs)
                  (length docs))))))))

(-> %apropos-list (string (or package symbol list) boolean boolean) list)
(defun %apropos-list (string packages external-only docs-too)
  (loop for package in packages
        when external-only
          nconc (loop for sym being the external-symbol in package
                      when (or (search string (format nil "~s" sym) :test #'string-equal)
                               (and docs-too
                                    (search string (all-docs sym) :test #'string-equal)))
                        collect sym)
        else
          nconc (loop for sym being the present-symbol in package
                      when (and
                            (eq (symbol-package sym) (find-package package))
                            (or (search string (format nil "~s" sym) :test #'string-equal)
                                (and docs-too
                                     (search string (all-docs sym) :test #'string-equal))))
                        collect sym)))

(-> apropos-list* ((or string symbol) &optional (or package symbol list) boolean boolean) list)
(defun apropos-list* (string &optional package external-only docs-too)
  "Search for symbols in PACKAGE with names (+docs when DOCS-TOO) containing STRING.
Sorts these by the frequency of STRING appearance in the respective
name/documentation.

When EXTERNAL-ONLY, only search the external symbols of
PACKAGE (ported from SBCL).

PACKAGE can be:
- A package.
- A symbol denoting a package.
- A list of packages/symbols.
- NIL (all the accessible packages are implied).

Influenced by:
- Current list of packages and their contents."
  (declare (ignorable external-only docs-too))
  (let ((packages (mapcar #'find-package (uiop:ensure-list (or package (list-all-packages)))))
        (string (string string)))
    (sort
     (remove-duplicates
      (cond
        ;; NOTE: Reusing built-in external-only functionality of SBCL.
        ;; FIXME: Is SBCL listing reliable enough? Seems te be so.
        #+sbcl
        ((not docs-too)
         (reduce #'append packages
                 :key (lambda (p) (funcall old-apropos-list string p external-only))))
        ;; FIXME: Test on more impls (help needed with this one!)
        ;; FIXME: The case-sensitivity of standard `apropos/-list' is
        ;; not specified, so this might bring some inconsistencies. All
        ;; the implementations I've tested list things in the
        ;; case-insensitive fashion, though.
        #+(or ccl ecl gcl abcl clisp)
        ((and (not external-only) (not docs-too))
         (reduce #'append packages
                 :key (lambda (p) (funcall old-apropos-list string p))))
        #+(or sbcl ccl ecl gcl abcl clisp)
        (t (%apropos-list string packages external-only docs-too))
        #-(or sbcl ccl ecl gcl abcl clisp)
        (t
         (warn "apropos-list* is not implemented for this CL, help in implementing it!")
         (reduce #'append packages
                 :key (lambda (p) (funcall old-apropos-list string p))))))
     #'> :key (lambda (sym)
                (count-appearances (string string) sym)))))

(-> apropos* ((or string symbol) &optional (or package symbol list) boolean boolean))
(defun apropos* (string &optional package external-only docs-too)
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
- `*standard-output*'."
  ;; This is because function-* APIs often throw warnings for
  ;; non-implemented methods.
  (handler-bind ((warning #'muffle-warning))
    (dolist (symbol (apropos-list* string package external-only docs-too))
      (flet ((crop-docs (docs)
               (when docs
                 (first (uiop:split-string docs :separator '(#\newline))))))
        (fresh-line)
        (format t "~s" symbol)
        (when (boundp symbol)
          (cond
            ((or (eq t symbol)
                 (null symbol)
                 (keywordp symbol))
             (format t " [self-evaluating]"))
            ((constantp symbol)
             (format t " [constant: ~s~@[ (~a)~]]"
                     (symbol-value symbol) (crop-docs (documentation symbol 'variable))))
            (t (format t " [variable: ~s~@[ (~a)~]]"
                       (symbol-value symbol) (crop-docs (documentation symbol 'variable))))))
        (when (fboundp symbol)
          (format t " [~:[function~;macro~] (~{~s~^ ~})~@[ (~a)~]]"
                  (macro-function symbol)
                  (function-lambda-list* (or (macro-function symbol)
                                             (symbol-function symbol)))
                  (crop-docs (or (documentation symbol 'function)
                                 (documentation (macro-function symbol) t)
                                 (ignore-errors (documentation (symbol-function symbol) t))))))
        (when (ignore-errors (find-class symbol nil))
          (format t " [class~@[ (~a)~]"
                  (crop-docs
                   (or (documentation symbol 'type)
                       (documentation symbol 'structure))))))))
  (values))

;;; Helpers

(-> apropod* ((or string symbol) &optional (or package symbol list) boolean))
(defun apropod* (string &optional package external-only)
  "A version of `apropos*', but with documentation search by default."
  (apropos* string package external-only t))
