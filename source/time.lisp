;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defun return-time-props (kind props form)
  (flet ((print-props (stream)
           (format stream
                   "~&Time spent ~:[evaluating~;aborting~] ~s:
Real time: ~f seconds.
Run time (system): ~f seconds.
Run time (user): ~f seconds.
GC time: ~f seconds.
Bytes allocated: ~a.
Page faults: ~a.~%"
                   (cdr (assoc :aborted props))
                   form (cdr (assoc :real props))
                   (cdr (assoc :system props))
                   (cdr (assoc :user props))
                   (cdr (assoc :gc props))
                   (cdr (assoc :allocated props))
                   (or (cdr (assoc :faults props)) 0))))
    (typecase kind
      ((eql :data) props)
      (stream (print-props kind))
      ((eql t) (print-props *trace-output*))
      (null (with-output-to-string (s)
              (print-props s))))))

(defmacro time* (form &optional (return-kind t))
  "Execute FORM and get timing information for it.
The way information is returned depends on RETURN-KIND:
- T --- print to *TRACE-OUTPUT*.
- NIL --- print to string and return it.
- Any output stream --- write to it.
- :DATA --- return an alist of timing data (after the last value
  returned by FORM):
  - (:real . seconds) for real time.
  - (:user . seconds) for user run time.
  - (:system . seconds) for user run time.
  - (:cores . number) for CPU cores utilized.
  - (:gc . seconds) for time spend on GC.
  - (:cpu . percentage) for CPU load in percent.
  - (:allocated . bytes).
  - (:aborted . boolean)."
  (let ((props (gensym "PROPS")))
    `(let ((,props (list)))
       #+sbcl
       (sb-ext::call-with-timing
        (lambda (&key real-time-ms user-run-time-us system-run-time-us
                   gc-run-time-ms processor-cycles eval-calls
                   lambdas-converted (page-faults 0) bytes-consed
                   aborted)
          (declare (ignorable processor-cycles eval-calls lambdas-converted))
          (push (cons :aborted aborted) ,props)
          (when real-time-ms
            (push (cons :real (/ real-time-ms 1000)) ,props))
          (when user-run-time-us
            (push (cons :user (/ user-run-time-us 1000000)) ,props))
          (when system-run-time-us
            (push (cons :system (/ system-run-time-us 1000000)) ,props))
          (when gc-run-time-ms
            (push (cons :gc (/ gc-run-time-ms 1000)) ,props))
          (when bytes-consed
            (push (cons :allocated bytes-consed) ,props))
          (push (cons :faults page-faults) ,props))
        (lambda () ,form))
       #+ccl
       (let ((ccl::*report-time-function*
               (lambda (&key form results elapsed-time user-time
                          system-time gc-time bytes-allocated
                          minor-page-faults major-page-faults
                          swaps)
                 (declare (ignorable swaps form))
                 (let ((page-faults (+ minor-page-faults major-page-faults)))
                   (unless (zerop page-faults)
                     (push (cons :faults page-faults) ,props)))
                 (push (cons :allocated bytes-allocated) ,props)
                 (push (cons :real (/ elapsed-time internal-time-units-per-second)) ,props)
                 (push (cons :system (/ system-time internal-time-units-per-second)) ,props)
                 (push (cons :user (/ user-time internal-time-units-per-second)) ,props)
                 (push (cons :gc (/ gc-time internal-time-units-per-second)) ,props)
                 (push (cons :count (ccl::cpu-count)) ,props)(ccl::cpu-count)
                 (values-list (append results (list (return-time-props ,return-kind ,props ',form)))))))
         (ccl::report-time ',form (lambda () ,form))))))
