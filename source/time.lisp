;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defun return-time-props (kind props form)
  (flet ((print-props (stream)
           (format stream
                   "~&Time spent ~@[un~*~]successfully evaluating ~s:~
~@[~&Real time: ~f seconds.~]~
~@[~&Run time (system): ~f seconds.~]~
~@[~&Run time (user): ~f seconds.~]~
~@[~&CPU cycles: ~d.~]~
~@[~&CPU cores: ~d.~]~
~@[~&GC: ~d times.~]~
~@[~&GC time: ~f seconds.~]~
~@[~&Bytes allocated: ~a.~]~
~@[~&Page faults: ~a.~]"
                   (cdr (assoc :aborted props))
                   form (cdr (assoc :real props))
                   (cdr (assoc :system props))
                   (cdr (assoc :user props))
                   (cdr (assoc :cpu props))
                   (cdr (assoc :cores props))
                   (cdr (assoc :gc-count props))
                   (cdr (assoc :gc props))
                   (cdr (assoc :allocated props))
                   (cdr (assoc :faults props)))))
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
  - (:system . seconds) for system run time.
  - (:cores . number) for CPU cores utilized.
  - (:gc-count . number) for the times GC was invoked.
  - (:gc . seconds) for time spend on GC.
  - (:cpu . cycles) for CPU cycles spent.
  - (:allocated . bytes).
  - (:aborted . boolean) for whether the evaluation ended up with
    non-local transfer of control.
  - (:faults . number) for page faults."
  (let ((props (gensym "PROPS")))
    `(let ((,props (list)))
       (values-list
        (append
         (multiple-value-list
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
             (when processor-cycles
               (push (cons :cpu processor-cycles) ,props))
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
                    (push (cons :cores (ccl::cpu-count)) ,props)
                    (values-list results))))
            (ccl::report-time ',form (lambda () ,form)))
          #+clisp
          (multiple-value-bind (old-real1 old-real2 old-run1 old-run2 old-gc1 old-gc2 old-space1 old-space2 old-gccount)
              (system::%%time)
            (let ((aborted t))
              (unwind-protect
                   (multiple-value-prog1
                       ,form
                     (setf aborted nil))
                (multiple-value-bind (new-real1 new-real2 new-run1 new-run2 new-gc1 new-gc2 new-space1 new-space2 new-gccount)
                    (system::%%time)
                  (flet ((diff4 (newval1 newval2 oldval1 oldval2)
                           (+ (* (- newval1 oldval1) internal-time-units-per-second)
                              (- newval2 oldval2))))
                    (push (cons :aborted aborted) ,props)
                    (push (cons :real (/ (diff4 new-real1 new-real2 old-real1 old-real2)
                                         internal-time-units-per-second))
                          ,props)
                    (push (cons :user (/ (diff4 new-run1 new-run2 old-run1 old-run2)
                                         internal-time-units-per-second))
                          ,props)
                    (push (cons :allocated (system::delta4 new-space1 new-space2 old-space1 old-space2 24))
                          ,props)
                    (let ((gc-time (diff4 new-gc1 new-gc2 old-gc1 old-gc2))
                          (gc-count (- new-gccount old-gccount)))
                      (unless (zerop gc-time)
                        (push (cons :gc (/ gc-time internal-time-units-per-second))
                              ,props))
                      (unless (zerop gc-count)
                        (push (cons :gc-count gc-count) ,props))))))))
          #-(or sbcl ccl clisp)
          (let ((old-real-time (get-internal-real-time))
                (old-run-time (get-internal-run-time))
                #+ecl
                (ecl-force-gc (si::gc t))
                (old-gc-time
                  #+(and ecl (not boehm-gc))
                  (si::gc-time))
                (old-bytes-allocated
                  #+(and ecl boehm-gc)
                  (si::gc-stats t))
                (old-gc-count
                  #+(and ecl boehm-gc)
                  (nth-value 1 (si::gc-stats t)))
                (aborted t))
            (declare (ignorable
                      #+ecl ecl-force-gc
                      old-gc-time old-gc-count old-bytes-allocated))
            (unwind-protect
                 (multiple-value-prog1
                     ,form
                   (setf aborted nil))
              (push (cons :aborted aborted) ,props)
              (push (cons :real (/ (- (get-internal-real-time)
                                      old-real-time)
                                   internal-time-units-per-second))
                    ,props)
              (push (cons :user (/ (- (get-internal-run-time)
                                      old-run-time)
                                   internal-time-units-per-second))
                    ,props)
              #+(and ecl (not boehm-gc))
              (push (cons :gc (/ (- (si::gc-time) old-gc-time)
                                 internal-time-units-per-second))
                    ,props)
              #+(and ecl boehm-gc)
              (push (cons :allocated (- (si::gc-stats t) old-bytes-allocated))
                    ,props)
              #+(and ecl boehm-gc)
              (push (cons :gc-count (- (nth-value 1 (si::gc-stats t)) old-gc-count))
                    ,props))))
         (list (return-time-props ,return-kind ,props ',form)))))))
