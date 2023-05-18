;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

;; There should be at least one cons in this alist.
(declaim (type (cons (cons string boolean) *) *yes-or-no-options*))
(defvar *yes-or-no-options*
  (copy-tree                       ; Don't modify a literal structure!
   '(("y" . t)
     ("ye" . t)
     ("yes" . t)
     ("yep" . t)
     ("yeah" . t)
     ("ay" . t)
     ("n" . nil)
     ("no" . nil)
     ("nah" . nil)
     ("nope" . nil)))
  "Alist of all the yes/no options and their respective boolean values.
Append new value conses to support new `y-or-n-p'/`yes-or-no-p'
answers.")

(-> y-or-n-p*  (&optional string &rest list) boolean)
(-> yes-or-no-p*  (&optional string &rest list) boolean)
(defun y-or-n-p* (&optional control &rest arguments)
  "Return a boolean for whether the user input is affirmative/negative.
Prompt for input again if the answer is neither.

Influenced by:
- `*query-io*' for the input/output.
- `*yes-or-no-options*' for the possible answer values.

Both `y-or-n-p*' and `yes-or-no-p*' refer to the same function,
because their answer list is the same—`*yes-or-no-options*'. They also
are the same in that there's no annoying beeps anymore.

The rationale for no-beep policy is: all the prompts that user
responds to are equally important (because these won't be prompts if
they weren't important). Some of these prompts are urgent, but
attracting user attention is not guaranteed to solve the urgency
anyway—one might ignore the beeps altogether."
  ;; Maybe a recursive design for simplicity and better stack traces?
  (flet ((print-prompt ()
           (when control
             (apply #'format *query-io* (uiop:strcat control " ") arguments))
           (format *query-io* "(y[es] or n[o]) ")
           (finish-output *query-io*)))
    (print-prompt)
    (loop for answer = (read-line *query-io* nil nil)
          for (answer-string . value) = (assoc answer *yes-or-no-options* :test #'string-equal)
          until answer-string
          do (print-prompt)
          finally (return value))))

(setf (fdefinition 'yes-or-no-p*) (fdefinition 'y-or-n-p*))
