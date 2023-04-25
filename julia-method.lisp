(in-package #:julia-functions)

(defclass julia-method (standard-method)
  ((%lambda
    :initarg .lambda.
    :reader julia-method-lambda
    :initform (alexandria:required-argument '.lambda.))))
