;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

;; (sb-debug::trace-1 function-or-name info &optional definition)
;; (ccl::trace-function spec &rest args &key ...)
;; (si::trace* specs)

(defmacro trace* (&rest trace-specs)
  (let* ((options (loop for (spec . rest) on trace-specs
                        when (keywordp spec)
                          collect spec
                          and collect (first rest)))
         (fspecs (loop for (spec . rest) on trace-specs
                       unless (member spec options :test #'equal)
                         collect spec)))
    (list options fspecs)))
