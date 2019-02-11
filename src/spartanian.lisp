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
  shape indices-value-matrix)

(defun %print-coo (obj stream)
  (format stream "#S(COO~T:SHAPE ~A)"
          (coo-shape obj)))

(defun make-coo (shape-list non-zero-size &key (element-type 'double-float))
  (assert (and (listp shape-list)
               (every #'integerp shape-list)))
  (%make-coo :shape shape-list
             :indices-value-matrix (make-array (list non-zero-size (1+ (length shape-list)))
                                               :element-type element-type)))

(defun cooref (coo &rest subscripts)
  (assert (= (length (coo-shape coo)) (length subscripts)))
  (assert (every (lambda (shape-dim subscript)
                   (<= 0 subscript (1- shape-dim)))
                 (coo-shape coo)
                 subscripts))
  (loop for i from 0 below (array-dimension (coo-indices-value-matrix) 0) do
    (every (lambda (j)
             (aref (coo-indices-value-matrix) 

;;; Compressed Sparse Fiber Format

(defstruct csf)
