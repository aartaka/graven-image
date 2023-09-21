;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell --container -D -f guix.scm
;;
;; Replace --container by --pure if you still want ASDF to see external
;; libraries in ~/common-lisp, etc.
;;
;;; Code:

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix git-download)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz))

(define-public sbcl-graven-image
  (package
   (name "sbcl-graven-image")
   (version "0.0.1")
   (source
    (local-file (dirname (current-filename)) #:recursive? #t)
    ;;;; Or this, in case of contributing to Guix.
    ;; (origin
    ;;   (method git-fetch)
    ;;   (uri (git-reference
    ;;         (url "https://github.com/aartaka/graven-image")
    ;;         (commit version)))
    ;;   (file-name (git-file-name "cl-graven-image" version))
    ;;   (sha256
    ;;    (base32
    ;;     "SPECIFY-HASH")))
    )
   (build-system asdf-build-system/sbcl)
   (native-inputs
    (list sbcl-lisp-unit2 sbcl))
   (inputs
    (list sbcl-closer-mop))
   (synopsis "Common Lisp standard debugging utilities made more extensible and useful.")
   (home-page "https://github.com/atlas-engineer/graven-image")
   (description
    "Graven Image is a library enhancing Common Lisp standard debugging tools.
The functions that Graven image adds:
@itemize
@item @code{yes-or-no-p*/y-or-n-p*} for user querying.
@item @code{apropos*} and @code{apropos-list*} for symbol search.
@item @code{function-lambda-expression*} for function inspection.
@item Helpers around @code{function-lambda-expression*}.
@item @code{documentation*} for faster object documentation.
@item @code{inspect*} and @code{describe*} for object inspection.
@item @code{dribble*} for interactive REPL recording.
@item @code{time*}, @code{benchmark*} and @code{with-time*} for code
timing stats.
@item @code{room*} and @code{with-room*} for memory consumption stats.
@end itemize")
   (license license:bsd-3)))

(define-public cl-graven-image
  (sbcl-package->cl-source-package sbcl-graven-image))

(define-public ecl-graven-image
  (sbcl-package->ecl-package sbcl-graven-image))

cl-graven-image
