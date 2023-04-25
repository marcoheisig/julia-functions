(defsystem "julia-functions"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :description "A quick hack to get the Julia compilation mode into CLOS."
  :license "MIT"
  :depends-on
  ("alexandria"
   "closer-mop"
   "trivial-macroexpand-all")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "lambda-lists")
   (:file "effective-method")
   (:file "julia-method")
   (:file "julia-function")
   (:file "optimization")))
