(in-package #:julia-functions)

(defgeneric lisp-sum (vector scalar))

(defgeneric julia-sum (vector scalar)
  (:generic-function-class julia-function))

(progn
  (defmethod lisp-sum ((vector vector) (real real))
    #1=
    (let ((sum real))
      (declare (real sum))
      (loop for index below (length vector) do
        (incf sum (elt vector index)))
      sum))

  (defmethod julia-sum ((vector vector) (real real))
    #1#))

(defmethod lisp-sum :before ((vector vector) (real real))
  (print "Calling LISP-SUM."))

(defmethod julia-sum :before ((vector vector) (real real))
  (print "Calling JULIA-SUM."))

(defun benchmark (fn)
  (let* ((n (expt 10 7))
         (vector (make-array n :element-type 'double-float
                               :initial-element 1d0)))
    (time
     (loop repeat 10 do
       (assert (= (* 1d0 n) (funcall fn vector 0d0)))))))







;;; Wait, it gets better.... :)

(defun lisp-benchmark ()
  (let* ((n (expt 10 7))
         (vector (make-array n :element-type 'double-float
                               :initial-element 1d0)))
    (time
     (loop repeat 10 do
       (assert (= (* 1d0 n) (lisp-sum vector 0d0)))))))

(defun julia-benchmark ()
  (let* ((n (expt 10 7))
         (vector (make-array n :element-type 'double-float
                               :initial-element 1d0)))
    (time
     (loop repeat 10 do
       (assert (= (* 1d0 n) (julia-sum vector 0d0)))))))
