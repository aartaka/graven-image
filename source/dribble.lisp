;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)


(defclass dribble-stream (trivial-gray-streams:fundamental-character-output-stream)
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
(defmethod trivial-gray-streams:stream-line-column ((stream dribble-stream))
  (when starting-p
    0))
(defmethod trivial-gray-streams:stream-write-char ((stream dribble-out-stream) character)
  (setf output-p t)
  (cond
    ((and (not starting-p)
          (eql #\Newline character))
     (let ((starting-p t))
       (format (slot-value stream 'actual-stream) "~%;; ")))
    (t
     (write-char character (slot-value stream 'actual-stream)))))

(defmethod trivial-gray-streams:stream-write-char ((stream dribble-in-stream) character)
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
  "Start a new REPL recording the inputted forms, their result, and outputs.

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
image to reproduce the recorded session."
  (labels ((print-dribble (action)
             (multiple-value-bind (second minute hour date month year)
                 (decode-universal-time (get-universal-time))
               (format *standard-output*
                       "~%~a dribbling to ~A on ~2,'0d:~2,'0d:~2,'0d ~
~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
~a~[th~;st~;nd~;rd~:;th~] ~a (on ~a ~a).~%"
                       action
                       (uiop:native-namestring (translate-logical-pathname *dribble-pathname*))
                       hour minute second month date (mod date 10) year
                       (lisp-implementation-type) (lisp-implementation-version)))))
    (cond
      ((and *dribble-pathname* (not pathname-p))
       (print-dribble "Finished")
       (force-output (slot-value *dribble-stream* 'actual-stream))
       (setf *dribble-pathname* nil
             *standard-output* (first (remove-if #'dribble-stream-p (broadcast-stream-streams *standard-output*)))
             ;; *error-output* (first (remove-if #'dribble-stream-p (broadcast-stream-streams *error-output*)))
             *standard-input* (echo-stream-input-stream *standard-input*)
             *dribble-stream* nil))
      (*dribble-pathname*
       (cerror "Do nothing" "~%Already dribbling to ~a~%" *dribble-pathname*))
      (pathname
       (setf *dribble-pathname* pathname
             actual-stream (open *dribble-pathname*
                                 :direction :io
                                 :if-exists if-exists
                                 :if-does-not-exist if-does-not-exist)
             *dribble-stream* (make-instance 'dribble-out-stream :actual-stream actual-stream)
             ;; *error-output* (make-broadcast-stream *error-output* *dribble-stream*)
             *standard-output* (make-broadcast-stream *standard-output* *dribble-stream*)
             *standard-input* (make-echo-stream
                               *standard-input*
                               (make-instance 'dribble-in-stream :actual-stream actual-stream)))
       (print-dribble "Started")))))
