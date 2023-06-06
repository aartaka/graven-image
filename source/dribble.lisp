;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defvar *dribble-stream* nil)
(defvar *dribble-pathname* nil
  "The file we're dribbling to.")

(defgeneric dribble* (&optional pathname if-exists)
  (:method (&optional (pathname (merge-pathnames #p".dribble.lisp" (user-homedir-pathname)))
              (if-exists :append))
    (let ((old-stdout *standard-output*))
      (labels ((format-both (&rest args)
                 (apply #'format *dribble-stream* args)
                 (force-output *dribble-stream*)
                 (apply #'format old-stdout args)
                 (force-output old-stdout))
               (print-dribble (action)
                 (multiple-value-bind (second minute hour date month year)
                     (decode-universal-time (get-universal-time))
                   (format-both "~&;;; ~a dribbling to ~A on ~2,'0d:~2,'0d:~2,'0d ~
~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
~a~[th~;st~;nd~;rd~:;th~] ~a (on ~a ~a).~%"
                                action
                                (uiop:native-namestring (translate-logical-pathname *dribble-pathname*))
                                hour minute second month date (mod date 10) year
                                (lisp-implementation-type) (lisp-implementation-version))))
               (lines (string)
                 (unless (uiop:emptyp string)
                   (uiop:split-string string :separator (string #\Newline))))
               (print-output (output formatter)
                 (dolist (line (lines output))
                   (unless (every (lambda (c)
                                    ;; Part of Serapeum whitespace chars.
                                    (member c '(#\Space #\Tab #\Linefeed #\Return #\Newline)))
                                  line)
                     (format-both formatter line)))))
        (setf *dribble-pathname* pathname
              *dribble-stream* (open *dribble-pathname*
                                     :direction :output
                                     :if-exists if-exists
                                     :if-does-not-exist :create))
        (print-dribble "Started")
        (unwind-protect
             (block dribble-repl
               (loop
                 (format t "~&d> ")
                 (finish-output)
                 (let* ((eof-gensym (gensym "EOF"))
                        (input (read *standard-input* nil eof-gensym)))
                   (cond
                     ((or (equal '(dribble*) input)
                          (equal '(dribble) input))
                      (return-from dribble-repl))
                     ((not (eq eof-gensym input))
                      (with-output-to-string (*standard-output*)
                        (with-output-to-string (*error-output*)
                          (let ((results (multiple-value-list (eval input)))
                                (output (get-output-stream-string *standard-output*))
                                (error-output (get-output-stream-string *error-output*)))
                            (format *dribble-stream* "~&~s" input)
                            (force-output *dribble-stream*)
                            (print-output output "~&;;~a")
                            (print-output error-output "~&;;!~a")
                            (dolist (result results)
                              (let ((lines (lines (format nil "~s" result))))
                                (format-both "~&;;=> ~a" (first lines))
                                (dolist (l (rest lines))
                                  (format-both "~&;;  ~a" l))))))))))))
          (print-dribble "Finished")
          (close *dribble-stream*)
          (setf *dribble-pathname* nil
                *dribble-stream* nil)
          (values)))))
  (:documentation "Start a new REPL recording the inputted forms, their result, and outputs.

Save the recording at PATHNAME (defaults to ~/.dribble.lisp).  If the
PATHNAME already exists, do IF-EXISTS (as per `cl:open').

The file resulting from the dribbling has:
- Opening and closing commentaries showing the time boundaries of
  dribbling.
- Inputted forms verbatim.
- Regular output of the forms with \";;\" prepended.
- Error output with \";;!\" prepended.
- Listing of values with \";;=>\" prepended to each value.

With such a format, dribble file can be loaded into the running Lisp
image to reproduce the recorded session."))
