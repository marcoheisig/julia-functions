(in-package #:julia-functions)

(defclass julia-function (standard-generic-function)
  ((%emf-defuns
    :initform (make-hash-table :test #'equal)
    :accessor julia-function-emf-defuns))
  (:default-initargs :method-class (find-class 'julia-method))
  (:metaclass funcallable-standard-class))
