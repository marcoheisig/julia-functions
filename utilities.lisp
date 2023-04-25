(in-package #:julia-functions)

(defun block-name (function-name)
  (etypecase function-name
    ((and symbol (not null)) function-name)
    ((cons (eql setf) (cons symbol null)) (second function-name))))

(defun null-lexical-environement-p (environment)
  (declare (ignorable environment))
  (or (null environment)
      #+sbcl (sb-c::null-lexenv-p environment)))

(defgeneric gensymify (object)
  (:method ((string string))
    (gensym (string-upcase (concatenate 'string string "-"))))
  (:method ((symbol symbol))
    (if (null (symbol-package symbol))
        ;; If we are dealing with uninterned symbols, we strip any
        ;; non-alphanumeric characters.  This has the effect that
        ;; gensymification of gensyms doesn't just add more and more
        ;; digits and hypens.
        (let ((name (symbol-name symbol)))
          (gensymify (subseq name 0 (1+ (position-if #'alpha-char-p name :from-end t)))))
        (gensymify (symbol-name symbol))))
  (:method ((object t))
    (gensymify (princ-to-string object))))

(defun class-subclasses (class)
  (let ((table (make-hash-table)))
    (labels ((rec (class)
               (unless (gethash class table)
                 (setf (gethash class table) t)
                 (mapc #'rec (class-direct-subclasses class)))))
      (rec class))
    (loop for c being the hash-keys of table collect c)))

(defun class-leaf-subclasses (class)
  (remove-if #'class-direct-subclasses (class-subclasses class)))

