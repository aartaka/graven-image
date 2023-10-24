;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defclass dribble-stream (fundamental-character-output-stream)
  ((actual-stream
    :initform nil
    :initarg :actual-stream)))
(defclass dribble-out-stream (dribble-stream) ())
(defclass dribble-in-stream (dribble-stream) ())
(defun dribble-stream-p (stream)
  (typep stream 'dribble-stream))

(defvar *dribble-stream* nil "The outer dribble stream that the output is dispatched to.")
(defvar *dribble-pathname* nil "The file we're dribbling to.")

(defvar starting-p nil)
(defvar output-p nil)
(defmethod stream-line-column ((stream dribble-stream))
  (when starting-p
    0))
(defmethod stream-write-char ((stream dribble-out-stream) character)
  (setf output-p t)
  (cond
    ((and (not starting-p)
          (eql #\Newline character))
     (let ((starting-p t))
       (format (slot-value stream 'actual-stream) "~%;; ")))
    (t
     (write-char character (slot-value stream 'actual-stream)))))

(defmethod stream-write-char ((stream dribble-in-stream) character)
  (cond
    (output-p
     (force-output (slot-value stream 'actual-stream))
     (setf output-p nil)
     (write-char #\Newline (slot-value stream 'actual-stream))
     (write-char character (slot-value stream 'actual-stream)))
    (t
     (write-char character (slot-value stream 'actual-stream)))))

(define-generic dribble* (&optional (pathname (merge-pathnames #p".dribble.lisp" (user-homedir-pathname)) pathname-p)
                          (if-exists :append) (if-does-not-exist :create))
  "Save the recording of the current REPL session at PATHNAME (defaults to ~/.dribble.lisp).

If PATHNAME already exists, do IF-EXISTS (as per `cl:open').

The file resulting from DRIBBLE* has:
- Opening and closing commentaries showing the time boundaries of
  dribbling.
- Inputted forms verbatim.
- Values and outputs prefixed by semicolons.

With such a format, dribble file can be loaded into the running Lisp
image to reproduce the recorded session.

Affected by:
- `*standard-output*'.
- `*standard-input*'.
- `*error-output*'.
- `*terminal-io*'.
- Gray Streams implementation (broken on ABCL, in particular)."
  (declare (ignorable pathname pathname-p if-exists if-does-not-exist))
  (labels ((print-dribble (started?)
             (multiple-value-bind (second minute hour date month year)
                 (decode-universal-time (get-universal-time))
               (format *standard-output*
                       "~%~a dribbling to ~A on ~2,'0d:~2,'0d:~2,'0d ~
~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
~a~[th~;st~;nd~;rd~:;th~] ~a (on ~a ~a).~%"
                       (if started?
                           "Started"
                           "Finished")
                       (uiop:native-namestring (translate-logical-pathname *dribble-pathname*))
                       hour minute second month date (mod date 10) year
                       (lisp-implementation-type) (lisp-implementation-version))
               (when started?
                 (print `(in-package ,(package-name *package*))
                        (slot-value *dribble-stream* 'actual-stream))
                 (print `(dribble* ,@(when pathname-p
                                       (list pathname if-exists if-does-not-exist)))
                        (slot-value *dribble-stream* 'actual-stream))))))
    (cond
      ((and *dribble-pathname* (not pathname-p))
       (print-dribble nil)
       (force-output (slot-value *dribble-stream* 'actual-stream))
       (setf *dribble-pathname* nil
             *dribble-stream* nil)
       #-clozure
       (setf *standard-output* (first (remove-if #'dribble-stream-p (broadcast-stream-streams *standard-output*)))
             *error-output* (first (remove-if #'dribble-stream-p (broadcast-stream-streams *error-output*)))
             *standard-input* (echo-stream-input-stream *standard-input*))
       #+clozure
       (setf *terminal-io* (echo-stream-input-stream (two-way-stream-input-stream *terminal-io*)))
       nil)
      (*dribble-pathname*
       (cerror "Do nothing" "~%Already dribbling to ~a~%" *dribble-pathname*))
      (pathname
       (let* ((actual-stream (open pathname
                                   :direction :io
                                   :if-exists if-exists
                                   :if-does-not-exist if-does-not-exist)))
         (setf *dribble-pathname* pathname
               *dribble-stream* (make-instance 'dribble-out-stream :actual-stream actual-stream))
         #-clozure
         (setf *error-output* (make-broadcast-stream *error-output* *dribble-stream*)
               *standard-output* (make-broadcast-stream *standard-output* *dribble-stream*)
               *standard-input* (make-echo-stream
                                 *standard-input*
                                 (make-instance 'dribble-in-stream :actual-stream actual-stream)))
         #+clozure
         (setf *terminal-io* (make-two-way-stream
                              (make-echo-stream
                               *terminal-io*
                               (make-instance 'dribble-in-stream :actual-stream actual-stream))
                              (make-broadcast-stream *terminal-io* *dribble-stream*))))
       (print-dribble t)
       nil))))
