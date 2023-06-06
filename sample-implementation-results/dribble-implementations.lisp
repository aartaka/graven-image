;;; SBCL:
;; (LAMBDA (&OPTIONAL PATHNAME &KEY (IF-EXISTS :APPEND))
;;   "With a file name as an argument, dribble opens the file and sends a
;;   record of further I/O to that file. Without an argument, it closes
;;   the dribble file, and quits logging."
;;   (FLET ((INSTALL-STREAMS (DRIBBLE INPUT OUTPUT ERROR)
;;            (SETF *DRIBBLE-STREAM* DRIBBLE
;;                  *STANDARD-INPUT* INPUT
;;                  *STANDARD-OUTPUT* OUTPUT
;;                  *ERROR-OUTPUT* ERROR)))
;;     (COND
;;      (PATHNAME
;;       (PUSH
;;        (LIST *DRIBBLE-STREAM* *STANDARD-INPUT* *STANDARD-OUTPUT*
;;              *ERROR-OUTPUT*)
;;        *PREVIOUS-DRIBBLE-STREAMS*)
;;       (LET ((NEW-DRIBBLE
;;              (OPEN PATHNAME :DIRECTION :OUTPUT :IF-EXISTS IF-EXISTS
;;                    :IF-DOES-NOT-EXIST :CREATE)))
;;         (INSTALL-STREAMS NEW-DRIBBLE
;;          (MAKE-ECHO-STREAM *STANDARD-INPUT* NEW-DRIBBLE)
;;          (MAKE-BROADCAST-STREAM *STANDARD-OUTPUT* NEW-DRIBBLE)
;;          (MAKE-BROADCAST-STREAM *ERROR-OUTPUT* NEW-DRIBBLE))))
;;      ((NULL *DRIBBLE-STREAM*)
;;       (ERROR "not currently dribbling"))
;;      (T (CLOSE *DRIBBLE-STREAM*)
;;       (APPLY #'INSTALL-STREAMS (POP *PREVIOUS-DRIBBLE-STREAMS*)))))
;;   (VALUES))

;;; CCL:
;; (defun dribble (&optional filename)
;;   "With a file name as an argument, dribble opens the file and sends a
;;      record of further I/O to that file. Without an argument, it closes
;;      the dribble file, and quits logging."
;;   (process-dribble *current-process* filename))
;; (defmethod process-stop-dribbling ((p process))
;;   (with-slots (dribble-stream dribble-saved-terminal-io) p
;;     (when dribble-stream
;;       (close dribble-stream)
;;       (setq dribble-stream nil))
;;     (when dribble-saved-terminal-io
;;       (setq *terminal-io* dribble-saved-terminal-io
;;             dribble-saved-terminal-io nil))))
;; (defmethod process-dribble ((p process) path)
;;   (with-slots (dribble-stream dribble-saved-terminal-io) p
;;     (process-stop-dribbling p)
;;     (when path
;;       (let* ((in (two-way-stream-input-stream *terminal-io*))
;;              (out (two-way-stream-output-stream *terminal-io*))
;;              (f (open path :direction :output :if-exists :append
;;                       :if-does-not-exist :create)))
;;         (without-interrupts
;;          (setq dribble-stream f
;;                dribble-saved-terminal-io *terminal-io*
;;                *terminal-io* (make-echoing-two-way-stream
;;                               (make-echo-stream in f)
;;                               (make-broadcast-stream out f)))))
;;       path)))

;;; ECL:
;; (defun dribble (&optional (pathname "DRIBBLE.LOG" psp))
;;   "Args: (&optional filespec)
;; If FILESPEC is given, starts recording the interaction to the specified file.
;; FILESPEC may be a symbol, a string, a pathname, or a file stream.  If FILESPEC
;; is not given, ends the recording."
;;   (cond (*dribble-closure*
;;          (funcall *dribble-closure* psp))
;;         ((null psp)
;;          (error "Not in dribble."))
;;         (t
;;          (let* ((namestring (namestring pathname))
;;                 (stream (open pathname :direction :output
;;                               :if-exists :supersede
;;                               :if-does-not-exist :create))
;;                 (dribble-stream (make-two-way-stream
;;                                  (make-echo-stream *terminal-io* stream)
;;                                  (make-broadcast-stream *terminal-io* stream)))
;;                 (standard-input *standard-input*)
;;                 (standard-output *standard-output*)
;;                 (closure #'(lambda (pathname-p)
;;                              (when pathname-p
;;                                (error "Already in dribble (to ~A)" namestring))
;;                              (unless (and (eq dribble-stream *standard-input*)
;;                                           (eq dribble-stream *standard-output*))
;;                                (warn "Stream variables rebound while DRIBBLE is on.~%Some output may be lost."))
;;                              (format stream "~&Finished dribbling to ~A." namestring)
;;                              (close stream)
;;                              (setq *standard-input* standard-input
;;                                    *standard-output* standard-output
;;                                    *dribble-closure* nil))))
;;            (multiple-value-bind (sec min hour day month year)
;;                (get-decoded-time)
;;              (format dribble-stream "~&Starts dribbling to ~A (~d/~d/~d, ~2,'0d:~2,'0d:~2,'0d)."
;;                      namestring year month day hour min sec)
;;              (setq *standard-input* dribble-stream
;;                    *standard-output* dribble-stream
;;                    *dribble-closure* closure)))))
;;   (values))

;;; CLISP: too complex, will revisit later.

;;; ABCL:
;; (defvar *previous-dribble-streams* nil)
;; (defvar *dribble-stream* nil)
;; (defun dribble (&optional pathname &key (if-exists :append))
;;   "With a file name as an argument, dribble opens the file and sends a
;;   record of further I/O to that file. Without an argument, it closes
;;   the dribble file, and quits logging."
;;   (cond (pathname
;;          (let* ((new-dribble-stream
;;                  (open pathname
;;                        :direction :output
;;                        :if-exists if-exists
;;                        :if-does-not-exist :create))
;;                 (new-standard-output
;;                  (make-broadcast-stream *standard-output* new-dribble-stream))
;;                 (new-error-output
;;                  (make-broadcast-stream *error-output* new-dribble-stream))
;;                 (new-standard-input
;;                  (make-echo-stream *standard-input* new-dribble-stream)))
;;            (push (list *dribble-stream* *standard-input* *standard-output*
;;                        *error-output*)
;;                  *previous-dribble-streams*)
;;            (setf *dribble-stream* new-dribble-stream)
;;            (setf *standard-input* new-standard-input)
;;            (setf *standard-output* new-standard-output)
;;            (setf *error-output* new-error-output)
;;            ;; Starting a new internal REPL for dribbling
;;            (loop do
;;              (format t "~a> " (package-name *package*))
;;              (with-simple-restart (abort "Error detected in dribbling")
;;                (handler-case
;;                  (let ((input (read *standard-input*)))
;;                    (print (eval input) *standard-output*)
;;                    (terpri)
;;                    (when (equal input '(dribble))
;;                      (return)))
;;                  (error (c)
;;                    (format *error-output* "~a~%" c)
;;                    (error c)))))))
;;         ((null *dribble-stream*)
;;          (error "Not currently dribbling."))
;;         (t
;;          (let ((old-streams (pop *previous-dribble-streams*)))
;;            (close *dribble-stream*)
;;            (setf *dribble-stream* (first old-streams))
;;            (setf *standard-input* (second old-streams))
;;            (setf *standard-output* (third old-streams))
;;            (setf *error-output* (fourth old-streams)))))
;;   (values))
