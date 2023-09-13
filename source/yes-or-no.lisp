;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

;; There should be at least one cons in this alist.
(declaim (type (cons (cons string boolean) *) *yes-or-no-options*))
(defvar *yes-or-no-options*
  (copy-tree                       ; Don't modify a literal structure!
   '(("yes" . t)
     ("yep" . t)
     ("yeah" . t)
     ("ay" . t)
     ("no" . nil)
     ("nah" . nil)
     ("nope" . nil)))
  "Alist of all the yes/no options and their respective boolean values.
Append new value conses to support new `y-or-n-p'/`yes-or-no-p'
answers.")

(defun %y-or-n-p (&optional control &rest arguments)
  (flet ((print-prompt ()
           (when control
             (apply #'format *query-io* (uiop:strcat control " ") arguments))
           (format *query-io* "(y[es] or n[o]) ")
           (finish-output *query-io*)))
    (print-prompt)
    (loop for answer = (read-line *query-io* nil nil)
          for matches = (remove-if #'(lambda (option)
                                       ;; Maybe case-sensitive?
                                       (uiop:string-prefix-p (string-upcase answer)
                                                             (string-upcase (car option))))
                                   *yes-or-no-options*)
          until (or (= 1 (length matches))
                    (= 1 (length (remove-duplicates (mapcar #'cdr matches)))))
          do (print-prompt)
          finally (return (cdr (first matches))))))

(define-generic y-or-n-p* (&optional control &rest arguments)
  "Return a boolean for whether the user input is affirmative/negative.
Prompt for input again if the answer is neither.

Influenced by:
- `*query-io*' for the input/output.
- `*yes-or-no-options*' for the possible answer values."
  (apply #'%y-or-n-p control arguments))

(define-generic yes-or-no-p* (&optional control &rest arguments)
  "Return a boolean for whether the user input is affirmative/negative.
Prompt for input again if the answer is neither.

Does not beep anymore. The rationale for no-beep policy is: all the
prompts that user responds to are equally important (because these
won't be prompts if they weren't important). Some of these prompts are
urgent, but attracting user attention is not guaranteed to solve the
urgency anyway—one might ignore the beeps altogether.

If you want beeps, though, you can always define a :before/:around
method that beeps as much as you want it to :)

Influenced by:
- `*query-io*' for the input/output.
- `*yes-or-no-options*' for the possible answer values."
  (apply #'%y-or-n-p control arguments))
