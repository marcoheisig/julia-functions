(cl:in-package #:cl-user)

(defpackage #:julia-functions
  (:use
   #:closer-common-lisp)

  (:export
   #:julia-function
   #:julia-method))
