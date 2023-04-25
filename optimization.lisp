(in-package #:julia-functions)

(defun specializer-lists (methods)
  (let ((gf (method-generic-function (first methods))))
    (loop for position below (length (generic-function-argument-precedence-order gf))
          collect
          (let ((leaf-classes '()))
            (loop for method in methods do
              (let ((specializer (elt (method-specializers method) position)))
                (loop for lc in (class-leaf-subclasses specializer) do
                  (pushnew lc leaf-classes))))
            leaf-classes))))

(defmethod compute-effective-method-function
    ((julia-function julia-function) effective-method options)
  (let* ((name (generic-function-name julia-function))
         (methods (generic-function-methods julia-function))
         (lambda-list
           (anonymize-ordinary-lambda-list
            ;; Unfortunately, we don't know the list of applicable methods
            ;; anymore at this stage.  So instead, we consider all methods
            ;; applicable.
            (compute-effective-method-lambda-list julia-function methods))))
    (compile
     nil
     `(lambda ,lambda-list
        (declare (ignorable ,@lambda-list))
        ,(labels ((expand (position specializer-lists specializers)
                    (if (null specializer-lists)
                        `(funcall (ensure-julia-emf-defun #',name ',(mapcar #'class-name (reverse specializers)))
                                  ;; TODO
                                  ,@lambda-list)
                        `(etypecase ,(nth position lambda-list)
                           ,@(loop for specializer in (first specializer-lists)
                                   collect
                                   `(,(class-name specializer)
                                     ,(expand (1+ position)
                                              (rest specializer-lists)
                                              (cons specializer specializers))))))))
           (expand 0 (specializer-lists methods) '()))))))

(defvar *last-emf-defun*)

(defun compute-julia-emf-defun (julia-function class-names)
  (let* ((name (gensym "JULIA-EMF-"))
         (signature (mapcar #'find-class class-names))
         (prototypes (mapcar #'class-prototype signature))
         (applicable-methods (compute-applicable-methods julia-function prototypes))
         (lambda-list
           (anonymize-ordinary-lambda-list
            (compute-effective-method-lambda-list julia-function applicable-methods))))
    (eval
     (setf *last-emf-defun*
           `(progn
              (declaim (inline ,name))
              (defun ,name ,lambda-list
                (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
                ,@(loop for specializer in signature
                        for argument in lambda-list
                        collect `(declare (type ,(class-name specializer) ,argument)))
                ,(expand-effective-method-body
                  (compute-effective-method
                   julia-function
                   (generic-function-method-combination julia-function)
                   applicable-methods)
                  julia-function lambda-list)))))))

(defun ensure-julia-emf-defun (julia-function signature)
  (alexandria:ensure-gethash
   signature
   (julia-function-emf-defuns julia-function)
   (compute-julia-emf-defun julia-function signature)))

(defmethod shared-initialize :after
    ((julia-function julia-function) (slot-names t)  &key &allow-other-keys)
  (declare (ignore slot-names))
  (clrhash (julia-function-emf-defuns julia-function))
  (let ((name (generic-function-name julia-function)))
    ;; Ensure that the function is known to SBCL.
    (unless (sb-c::info :function :info name)
      (eval `(sb-c:defknown ,name * * ())))))

(defmethod add-method :after
    ((julia-function julia-function)
     (julia-method julia-method))
  (mapc
   (lambda (specializers)
     (let ((name (generic-function-name julia-function))
           (types (mapcar #'class-name specializers)))
       (handler-case
           (eval
            `(sb-c:deftransform ,name ((&rest args) (,@types))
               (optimize-julia-function-call #',name ',types)))
         (sb-kernel:redefinition-with-deftransform () (values)))))
   (apply
    #'alexandria:map-product
    #'list
    (specializer-lists (generic-function-methods julia-function)))))

(defun optimize-julia-function-call (julia-function signature)
  `(lambda (&rest args)
     (apply #',(ensure-julia-emf-defun julia-function signature) args)))

(defmethod make-method-lambda :around
    ((gf julia-function)
     (julia-method julia-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list*
      '.lambda.
      (make-julia-method-lambda gf julia-method lambda environment)
      initargs))))

(defun make-julia-method-lambda
    (generic-function method lambda environment)
  (declare (ignore method))
  (destructuring-bind (lambda-symbol lambda-list &rest body) lambda
    (assert (eql lambda-symbol 'lambda))
    (multiple-value-bind (required optional rest-var keyword allow-other-keys-p auxiliary)
        (parse-ordinary-lambda-list lambda-list)
      (multiple-value-bind (forms declarations)
          (alexandria:parse-body body)
        (let ((partially-flattened-lambda-list
                `(,@(lambda-list-variables
                     (unparse-ordinary-lambda-list
                      required optional rest-var keyword allow-other-keys-p '()))
                  ,@(unparse-ordinary-lambda-list '() '() nil '() nil auxiliary))))
          (trivial-macroexpand-all:macroexpand-all
           `(lambda ,partially-flattened-lambda-list
              (declare (ignorable ,@(mapcar #'required-info-variable required)))
              ,@declarations
              (block ,(block-name (generic-function-name generic-function))
                ,@forms))
           environment))))))
