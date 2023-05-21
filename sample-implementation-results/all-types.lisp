'(t
  atom
  readtable
  package
  symbol
  restart
  random-state
  hash-table
  structure-object
  standard-object
  method
  method-combination
  character
  sequence
  string
  stream
  function
  pathname
  condition
  .

  ;; These are based on what SLYNK inspects, so it might not be an
  ;; exhaustive list of direct T subtypes.

  #+abcl
  (java:java-exception mop::slot-definition java:java-object)
  #+ccl
  (uvector-inspector)
  #+clasp
  (sys:cxx-object sys:vaslist)
  #+(or cmucl scl)
  (kernel:code-component
   kernel:fdefn
   #+cmucl kernel:funcallable-instance)
  #+cormanlisp
  (pathnames::pathname-internal)
  #+sbcl
  (sb-kernel:code-component sb-ext:weak-pointer sb-kernel:fdefn))
