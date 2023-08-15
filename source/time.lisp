;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defmacro with-time* ((&rest time-keywords)
                      (&rest multiple-value-args) form
                      &body body)
  "Measure the timing properties for FORM and bind them to TIME-KEYWORDS in BODY.
The values of FORM are bound to MULTIPLE-VALUE-ARGS.

Both TIME-KEYWORDS and MULTIPLE-VALUE-ARGS are destructuring lists,
allowing for &REST, &KEY etc. in them.

TIME-KEYWORDS is a &KEY destructuring list, but one may omit &KEY and
&ALLOW-OTHER-KEYS in it.

TIME-KEYWORDS are destructuring a keyword-indexed property list with:
- :REAL --- for real time (in seconds).
- :USER --- for user run time (in seconds).
- :SYSTEM --- for system run time (in seconds).
- :CYCLES --- for CPU cycles spent.
- :LOAD --- for CPU load (percentage).
- :GC-COUNT --- for the times GC was invoked.
- :GC --- for time spend on GC (in seconds).
- :ALLOCATED --- for the amount of bytes consed.
- :ABORTED --- for whether the evaluation errored out.
- :FAULTS --- for the number of page faults (both major and minor).

Not all of properties are guaranteed to be there. More so: it's almost
always the case that some are missing."
  (let ((props (gensym "PROPS"))
        (values (gensym "VALUES")))
    `(let* ((,props (list))
            (,values (multiple-value-list
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
                           (push (cons :cycles processor-cycles) ,props))
                         (when bytes-consed
                           (push (cons :allocated bytes-consed) ,props))
                         (when (and page-faults (plusp page-faults))
                           (push (cons :faults page-faults) ,props)))
                       (lambda () ,form))
                      #+clozure
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
                                (values-list results))))
                        (ccl::report-time
                         ',form
                         (lambda ()
                           (handler-case
                               ,form
                             (serious-condition ()
                               (push (cons :aborted t) ,props)
                               nil)))))
                      #+clisp
                      (multiple-value-bind (old-real1 old-real2 old-run1 old-run2 old-gc1 old-gc2 old-space1 old-space2 old-gccount)
                          (system::%%time)
                        (multiple-value-prog1
                            (handler-case
                                ,form
                              (error ()
                                (push (cons :aborted t) ,props)
                                nil))
                          (multiple-value-bind (new-real1 new-real2 new-run1 new-run2 new-gc1 new-gc2 new-space1 new-space2 new-gccount)
                              (system::%%time)
                            (flet ((diff4 (newval1 newval2 oldval1 oldval2)
                                     (+ (* (- newval1 oldval1) internal-time-units-per-second)
                                        (- newval2 oldval2))))
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
                                  (push (cons :gc-count gc-count) ,props)))))))
                      #+allegro
                      (excl::time-a-funcall
                       (lambda (s tgcu tgcs tu ts tr scons sother static
                                &optional pfmajor pfminor gcpfmajor gcpfminor threadu threads)
                         (declare (ignorable s threadu threads))
                         (push (cons :system (/ ts 1000000)) ,props)
                         (push (cons :user (/ tu 1000000)) ,props)
                         (push (cons :real (/ tr 1000000)) ,props)
                         ;; FIXME: How do they calculate it? Is it byte count?
                         ;; For 500501 cons cells the number is 740496. 1.5 byte
                         ;; per cons cell???
                         (push (cons :allocated (+ scons sother static)) ,props)
                         (let ((faults (+ (or pfmajor 0)
                                          (or pfminor 0)
                                          (or gcpfmajor 0)
                                          (or gcpfminor 0))))
                           (unless (zerop faults)
                             (push (cons :faults (+ (or pfmajor 0)
                                                    (or pfminor 0)
                                                    (or gcpfmajor 0)
                                                    (or gcpfminor 0)))
                                   ,props)))
                         (push (cons :gc (/ (+ tgcu tgcs) 1000000)) ,props))
                       *trace-output*
                       (lambda ()
                         (handler-case
                             ,form
                           (error ()
                             (push (cons :aborted t) ,props)
                             (values)))))
                      #-(or sbcl clozure clisp allegro)
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
                              (nth-value 1 (si::gc-stats t))))
                        (declare (ignorable
                                  #+ecl ecl-force-gc
                                  old-gc-time old-gc-count old-bytes-allocated))
                        (multiple-value-prog1
                            (handler-case
                                ,form
                              (error ()
                                (push (cons :aborted t) ,props)
                                nil))
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
                                ,props))))))
       (destructuring-bind (,@(unless (eq (car time-keywords) '&key)
                                (list '&key))
                            ,@time-keywords
                            ,@(unless (eq (car (last time-keywords)) '&allow-other-keys)
                                (list '&allow-other-keys)))
           (reduce #'append (mapcar (lambda (p) (list (car p) (cdr p))) ,props))
         (destructuring-bind (,@multiple-value-args)
             ,values
           ,@body)))))

(defmacro time* (&rest forms)
  "Execute FORMS and print timing information for them.
The values of FORMS evaluation are returned unaltered.

Influenced by:
- `with-time*' implementation support.
- `*trace-output*' for printing."
  (let ((form (if (= 1 (length forms))
                  (first forms)
                  (cons 'progn forms))))
    `(with-time* (&key aborted real system user cycles load cores gc-count gc allocated faults)
         (&rest values)
         ,form
       (format *trace-output*
               "~&Time spent ~@[un~*~]successfully evaluating:~
~&~s~
~@[~&Real time:         ~f seconds~]~
~@[~&Run time (system): ~f seconds~]~
~@[~&Run time (user):   ~f seconds~]~
~@[~&CPU cycles:        ~:d~]~
~@[~&CPU load:          ~d~]~
~@[~&GC:                ~d times~]~
~@[~&GC time:           ~f seconds~]~
~@[~&Bytes allocated:   ~:d~]~
~@[~&Page faults:       ~:d~]"
               aborted ',form
               real system user
               cycles load
               gc-count gc
               allocated
               faults)
       (values-list values))))
