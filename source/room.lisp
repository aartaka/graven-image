;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :graven-image)

(defvar old-room (symbol-function 'room))

;; TODO: GCL lists useful things like:
;; word size:            64 bits
;; page size:            4096 bytes
;; heap start:           0x810000
;; heap max :            0x40101D000
;; shared library start: 0x0
;; cstack start:         0x0
;; cstack mark offset:   78 bytes
;; cstack direction:     downward
;; cstack alignment:     16 bytes
;; cstack max:           16079 bytes
;; immfix start:         0x8000000000000000
;; immfix size:          4611686018427387904 fixnums
;; physical memory:      4063824 pages

#+clozure
(defun room-stats ()
  (multiple-value-bind (heap-used static-used staticlib-used frozen-space-size)
      (ccl::%usedbytes)
    (multiple-value-bind (stack stack-used)
        (ccl::%stack-space)
      (let* ((heap-free (ccl::%freebytes))
             (heap (+ heap-used heap-free))
             (static (+ static-used staticlib-used frozen-space-size))
             (stack-used-by-thread (ccl::%stack-space-by-lisp-thread)))
        ;; TODO: frozen/reserved
        (append (list :heap heap)
                (list :heap-used heap-used)
                (list :stack stack)
                (list :stack-used stack-used)
                (list :static static)
                (list :types
                      ;; FIXME: NIL for "logical size". What
                      ;; is logical size?
                      (loop for (type count nil physical-size)
                              in (ccl:collect-heap-utilization)
                            collect (list type
                                          :bytes physical-size
                                          :instances count)
                              into type-data
                            sum physical-size into total-bytes
                            sum count into total-instances
                            finally (return (append type-data
                                                    (list (list t :bytes total-bytes
                                                                  :instances total-instances))))))
                (list :threads
                      (map 'list
                           (lambda (ti)
                             (destructuring-bind
                                 (thread sp-free sp-used . rest)
                                 ti
                               (declare (ignorable rest))
                               ;; TODO: vsp, tsp stack memory.
                               (let ((process (find-if (lambda (p) (eq (ccl::process-thread p) thread))
                                                       (ccl:all-processes))))
                                 (list thread
                                       :name (ccl:process-name process)
                                       :control-stack (+ sp-free sp-used)
                                       :control-stack-used sp-used))))
                           stack-used-by-thread)))))))

#+ecl
(defun room-stats ()
  #+boehm-gc
  (list
   :heap (ext:get-limit 'ext:heap-size)
   :stack (ext:get-limit 'ext:lisp-stack))
  #-boehm-gc
  (multiple-value-bind
        (maxpage leftpage ncbpage maxcbpage ncb cbgbccount holepage l)
      (sys::room-report)
    ;; maxpage---number of pages ECL can use.
    ;; leftpage---number of available pages.
    ;; ncbpage---number of pages actually allocated for
    ;; contiguous blocks.
    ;; maxcbpage---maximum number of pages for contiguous blocks.
    ;; ncb---???
    ;; cbgbccount---number of times the garbage collector.
    ;; has been called to collect contiguous blocks.
    ;; holepage---number of pages in the hole.
    ;; l
    (declare (ignore ncbpage maxcbpage ncb holepage l))
    (append
     ;; REVIEW: Which one of `ext:c-stack',
     ;; `ext:binding-stack', `ext:frame-stack', and
     ;; `ext:lisp-stack' is THE stack?  Other
     ;; implementations have a clear notion of control
     ;; stack...
     (list :stack (ext:get-limit 'ext:frame-stack))
     (list :heap (* 4096 maxpage))
     (list :heap-used (* 4096 (- maxpage leftpage)))
     (list :gc-count cbgbccount)
     (list
      :types
      (loop for (nused nfree npage maxpage gbccount) on l
              by (lambda (list) (nthcdr 5 list))
            for type in '(cons
                          ;; fixnum Beppe
                          fixnum char
                          bignum ratio short-float long-float complex
                          symbol package hash-table
                          array vector string bit-vector
                          stream random-state readtable pathname
                          bytecodes cfun cclosure
                          #-clos structure #+clos instance #+clos generic-function
                          #+threads mp::process #+threads mp::lock
                          si::foreign)
            collect (list type :bytes (* 4096 npage))
              into type-data
            sum (* 4096 npage)
              into total-bytes
            finally (return (append type-data
                                    (list (list t :bytes total-bytes)))))))))

#+clisp
(defun room-stats ()
  (multiple-value-bind (used room static gc-count gc-space gc-time)
      (sys::%room)
    (declare (ignorable gc-time))
    (append
     (list :heap (+ used room))
     (list :heap-used used)
     (list :static static)
     (list :gc-count gc-count)
     (list :gc gc-space)
     (list
      :types (let ((total-bytes 0)
                   (total-instances 0))
               (loop for (type instances . bytes)
                       across (sort (sys::heap-statistics) #'> :key #'cddr)
                     collect (list type
                                   :bytes bytes
                                   :instances instances)
                       into types
                     sum bytes into total-bytes
                     sum instances into total-instances
                     finally (return (append (remove t types :key #'first)
                                             (list (list
                                                    t
                                                    :bytes total-bytes
                                                    :instances total-instances))))))))))

#+sbcl
(defun room-stats ()
  ;; TODO: binding stack and thread-specific memory.
  (labels ((find-symbol-value (symbol package)
             (ignore-errors (symbol-value (uiop:find-symbol* symbol package nil))))
           (get-type-memory (type)
             (funcall (third (find type (or (ignore-errors
                                             (find-symbol-value :+all-spaces+ :sb-vm))
                                            (append (find-symbol-value :+heap-spaces+ :sb-vm)
                                                    (find-symbol-value :+stack-spaces+ :sb-vm)))
                                   :key #'first)))))
    (append
     (list :static (get-type-memory :static))
     (list :heap (sb-ext::dynamic-space-size))
     (list :heap-used (get-type-memory :dynamic))
     (list :stack (- sb-vm::*control-stack-end* sb-vm::*control-stack-start*))
     (list :stack-used (get-type-memory :control-stack))
     (list :read-only (get-type-memory :read-only))
     (list
      :types (let ((type-instances (make-hash-table))
                   (type-total-bytes (make-hash-table)))
               (sb-vm:map-allocated-objects
                (lambda (obj type size)
                  (declare (ignorable type))
                  (incf (gethash (class-name (class-of obj)) type-instances 0))
                  (incf (gethash (class-name (class-of obj)) type-total-bytes 0) size))
                :dynamic)
               (loop for type being the hash-key of type-instances
                     for bytes = (gethash type type-total-bytes 0)
                     for instances = (gethash type type-instances 0)
                     sum bytes into total-bytes
                     sum instances into total-instances
                     collect (list type
                                   :bytes bytes
                                   :instances instances)
                       into types
                     finally (return
                               (append
                                types
                                (list (list t
                                            :bytes total-bytes
                                            :instances total-instances))))))))))

#+abcl
(defun room-stats ()
  ;; FIXME: Find more data!!!!!
  (let* ((runtime (java:jstatic "getRuntime"
                                (java:jclass "java.lang.Runtime")))
         ;; TODO: maxMemory? What does this method mean?
         (total-memory (java:jcall "totalMemory" runtime))
         (free-memory (java:jcall "freeMemory" runtime)))
    (list :heap total-memory
          :heap-used (- total-memory free-memory))))

(defmacro with-room* ((&rest room-keywords)
                      &body body)
  "Binds ROOM-KEYWORDS to memory statistics for the duration of BODY.

ROOM-KEYWORDS is a keyword destructuring list, but &KEY and
&ALLOW-OTHER-KEYS can both be omitted.

ROOM-KEYWORDS are destructuring a keyword-indexed property list with:
- :HEAP --- for total heap size.
- :HEAP-USED --- for used heap size.
- :STACK --- for (control) stack size.
- :STACK-USED --- for space used on the stack.
- :STATIC --- for the static memory size.
- :READ-ONLY --- SBCL-specific read-only memory use.
- :GC --- total GC-collected space.
- :GC-COUNT --- total number of garbage collections.
- :THREADS --- per-thread memory consumption. Is a list of:
(THREAD &KEY NAME CONTROL-STACK CONTROL-STACK-USED).
- :TYPES -- per-type allocation. Is a list of
(TYPE-NAME &KEY BYTES INSTANCES)

For :THREADS and :TYPES, the last element can be a
(T &KEY ...) list which collects the total statistics for all
threads/types.

Not all of properties are guaranteed to be there. More so: it's almost
always the case that some are missing."
  `(destructuring-bind (,@(unless (member (car room-keywords) '(&key &rest))
                            (list '&key))
                        ,@room-keywords
                        ,@(unless (eq (car (last room-keywords)) '&allow-other-keys)
                            (list '&allow-other-keys)))
       #+(or clozure sbcl ecl clisp abcl)
     (room-stats)
     #-(or clozure sbcl ecl clisp abcl)
     (load-time-warn "Cannot fetch room statistics for this implementation. Help in fixing it!")
     ,@body))

(defun room* (&optional (verbose :default) (destination t))
  "Print memory usage statistics to DESTINATION.
Output size/details depend on VERBOSE:
- T --- all the information.
- :DEFAULT --- moderate amount of information
- NIL --- minimum necessary information.

Affected by:
- `with-room*' implementation support.
- `*standard-output*' for printing (when DESTINATION is T).
- Printer variables for float format and form printing."
  (declare (ignorable verbose))
  (with-room* (&rest params
               &key heap heap-used
               stack stack-used
               static
               read-only
               gc gc-count
               threads types)
    (declare (ignorable heap heap-used stack stack-used static gc gc-count threads types read-only))
    (flet ((minimal ()
             (format destination "~&~:[~&~3*~;~&Heap: ~:[?~*~;~:d~]/~:d bytes~@[ (~f%)~]~]~
~:[~&~3*~;~&Stack: ~:[?~*~;~:d~]/~:d bytes~@[ (~f%)~]~]~
~@[~&Static: ~:d bytes~]~
~@[~&Read-only: ~:d bytes~]"
                     heap heap-used heap-used heap (when heap-used (* 100 (/ heap-used heap)))
                     stack stack-used stack-used stack (when stack-used (* 100  (/ stack-used stack)))
                     static
                     read-only))
           (print-types (count)
             "Print types and their memory consumption.
COUNT can be:
- Integer --- to print the first COUNT types.
- NIL --- to print all the types.
Unconditionally prints the T type clause in the end."
             (let* ((t-type (find t types :key #'first))
                    (types (remove t types :key #'first))
                    (types (sort (copy-list types) #'>
                                 :key (lambda (type)
                                        (or (second (member :bytes type))
                                            0))))
                    (max-name-length
                      (1+ (reduce #'max types
                                  :key (lambda (type) (length (string (first type))))
                                  :initial-value 0)))
                    (types-to-print (cond
                                      ((and types count)
                                       (subseq
                                        types 0 (min (1- (length types))
                                                     count)))
                                      (types types))))
               ;; TODO: print percentage of how much total memory a
               ;; type occupies and the size of every type.
               (when types-to-print
                 (format destination "~&~%Printing memory stats for ~:[all~;the ~d biggest~] types"
                         types-to-print (length types-to-print))
                 (dolist (type types-to-print)
                   (destructuring-bind (name &key (bytes 0) (instances 0))
                       type
                     (format destination "~&~s ~vt~d bytes, ~d objects" name max-name-length bytes instances)))
                 (destructuring-bind (&key (bytes 0) (instances 0))
                     (rest t-type)
                   (format destination "~&Total ~vt~d bytes, ~d objects" max-name-length bytes instances))))))
      (unless params
        (format destination "~&No memory stats available, falling back to implementation-specific ROOM.~%")
        (return-from room* (values-list (multiple-value-list (funcall old-room)))))
      (minimal)
      (when verbose
        (print-types (when (eq :default verbose)
                       10))))))
