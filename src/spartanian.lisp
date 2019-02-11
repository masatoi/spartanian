(defpackage spartanian
  (:use :cl))
(in-package :spartanian)

;;; Hashed Format

(defstruct (hash-tensor (:constructor %make-hash-tensor)
                        (:print-object %print-hash-tensor))
  shape indices-value-table default-value)

(defun %print-hash-tensor (obj stream)
  (format stream "#S(HASH-TENSOR~T:SHAPE ~A)"
          (hash-tensor-shape obj)))

(defun make-hash-tensor (shape-list &key (default-value 0d0))
  (assert (and (listp shape-list)
               (every #'integerp shape-list)))
  (%make-hash-tensor :shape shape-list
                     :indices-value-table (make-hash-table :test 'equal)
                     :default-value default-value))

(defun htref (hash-tensor &rest subscripts)
  (assert (= (length (hash-tensor-shape hash-tensor)) (length subscripts)))
  (assert (every #'integerp subscripts))
  (assert (every (lambda (shape-dim subscript)
                   (<= 0 subscript (1- shape-dim)))
                 (hash-tensor-shape hash-tensor)
                 subscripts))
  (gethash subscripts
           (hash-tensor-indices-value-table hash-tensor)
           (hash-tensor-default-value hash-tensor)))

(defun (setf htref) (new-value hash-tensor &rest subscripts)
  (assert (= (length (hash-tensor-shape hash-tensor)) (length subscripts)))
  (assert (every #'integerp subscripts))
  (assert (every (lambda (shape-dim subscript)
                   (<= 0 subscript (1- shape-dim)))
                 (hash-tensor-shape hash-tensor)
                 subscripts))
  (setf
   (gethash subscripts
            (hash-tensor-indices-value-table hash-tensor)
            (hash-tensor-default-value hash-tensor))
   new-value)
  new-value)

;;; Coordinate Format

(defstruct (coo (:constructor %make-coo)
                (:print-object %print-coo))
  shape indices-matrix value-vector default-value)

(defun %print-coo (obj stream)
  (format stream "#S(COO~T:SHAPE ~A)"
          (coo-shape obj)))

(defun make-coo (shape-list non-zero-size
                 &key (element-type 'double-float) (default-value 0d0))
  (assert (and (listp shape-list)
               (every #'integerp shape-list)))
  (%make-coo :shape shape-list
             :indices-matrix (make-array (list non-zero-size (length shape-list))
                                         :element-type 'fixnum)
             :value-vector (make-array non-zero-size
                                       :element-type element-type)
             :default-value default-value))

(defun coo-index (coo &rest subscripts)
  (let* ((indice-matrix (coo-indices-matrix coo))
         (non-zero-size (array-dimension indice-matrix 0))
         (dim (array-dimension indice-matrix 1)))
    (loop for i from 0 below non-zero-size do
      (let ((cnt 0))
        (loop for j from 0 below dim
              for subscript in subscripts
              do (if (= (aref indice-matrix i j) subscript)
                     (incf cnt)
                     (return)))
        (when (= dim cnt)
          (return-from coo-index i))))
    nil))

(defun cooref (coo &rest subscripts)
  (assert (= (length (coo-shape coo)) (length subscripts)))
  (assert (every (lambda (shape-dim subscript)
                   (<= 0 subscript (1- shape-dim)))
                 (coo-shape coo)
                 subscripts))
  (if (apply #'coo-index coo subscripts)
      (aref (coo-value-vector coo)
            (apply #'coo-index coo subscripts))
      (coo-default-value coo)))

(defun (setf cooref) (new-value coo &rest subscripts)
  (assert (= (length (coo-shape coo)) (1- (length subscripts))))
  (assert (every #'integerp subscripts))
  (assert (every (lambda (shape-dim subscript)
                   (<= 0 subscript (1- shape-dim)))
                 (coo-shape coo)
                 (cdr subscripts)))
  (assert (<= 0 (car subscripts) (1- (array-dimension (coo-indices-matrix coo) 0))))

  (let ((i (car subscripts))
        (subscripts (cdr subscripts))
        (dim (array-dimension (coo-indices-matrix coo) 1)))
    (loop for j from 0 below dim
          for subscript in subscripts
          do (setf (aref (coo-indices-matrix coo) i j) subscript))
    (setf (aref (coo-value-vector coo) i) new-value))
  new-value)

;; (defparameter coo1 (make-coo '(3 3 3) 10))
;; (cooref coo1 1 0 1)
;; (setf (cooref coo1 0 1 0 1) 2d0)
;; (coo-indices-matrix coo1)
;; ;; #2A((1 0 1)
;; ;;     (0 0 0)
;; ;;     (0 0 0)
;; ;;     (0 0 0)
;; ;;     (0 0 0)
;; ;;     (0 0 0)
;; ;;     (0 0 0)
;; ;;     (0 0 0)
;; ;;     (0 0 0)
;; ;;     (0 0 0))
;; (coo-value-vector coo1)
;; ;; #(2.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)

;;; Compressed Sparse Fiber Format (TODO)
