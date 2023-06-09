;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :cl)

(graven-image::without-package-locks
  (fmakunbound 'apropos)
  (fmakunbound 'apropos-list)
  (fmakunbound 'y-or-n-p)
  (fmakunbound 'yes-or-no-p)
  (fmakunbound 'function-lambda-expression)
  (fmakunbound 'time)
  (fmakunbound 'describe)
  (fmakunbound 'inspect)
  (fmakunbound 'dribble)
  (fmakunbound 'invoke-debugger)
  (setf (fdefinition 'apropos) (fdefinition 'graven-image:apropos*)
        (fdefinition 'apropos-list) (fdefinition 'graven-image:apropos-list*)
        (fdefinition 'y-or-n-p) (fdefinition 'graven-image:y-or-n-p*)
        (fdefinition 'yes-or-no-p) (fdefinition 'graven-image:yes-or-no-p*)
        (fdefinition 'function-lambda-expression) (fdefinition 'graven-image:function-lambda-expression*)
        (fdefinition 'describe) (fdefinition 'graven-image:describe*)
        (fdefinition 'inspect) (fdefinition 'graven-image:inspect*)
        (macro-function 'time) (macro-function 'graven-image:time*)
        (fdefinition 'dribble) (fdefinition 'graven-image:dribble*)
        (fdefinition 'invoke-debugger) (fdefinition 'graven-image:debugger*)))
