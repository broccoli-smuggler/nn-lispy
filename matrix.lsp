;;; Matrix operations

;; Random matrix creation functions
(defun random-vector-zero-mean (len)
	(let ((acc nil))
		(dotimes (i len)
			(push (- (* (random 1.0) 2) 1) acc))
		acc))
		
(defun make-rand-matrix (n m)
	(let ((acc nil))
		(dotimes (i n)
			(push (random-vector-zero-mean m) acc))
		acc))

(defun rand-matrix-zero-mean (n m)
	(make-array (list n m)
		:initial-contents (make-rand-matrix n m)))
		
;; Rossetta code for transpose
(defun transpose (A)
  (let* ((m (array-dimension A 0))
         (n (array-dimension A 1))
         (B (make-array `(,n ,m) :initial-element 0)))
    (loop for i from 0 below m do
          (loop for j from 0 below n do
                (setf (aref B j i)
                      (aref A i j))))
    B))

(defun dot (v w)
	(reduce #'+ (map 'vector #'(lambda (x y) (* x y)) v w)))
		
(defun dot-product (m1 m2)
	(let* ((r1 (array-dimensions m1))
		   (r2 (array-dimensions m2))
		   (i (nth 0 r1))
		   (j (nth 1 r2))
		   (n (nth 1 r1))
		   (acc (make-array (list i j) :initial-element 0)))
	    (dotimes (x i)
			(dotimes (y j)
				(setf (aref acc x y) (dot (make-array n :displaced-to m1 :displaced-index-offset (* x n))
										  (make-array n :displaced-to (transpose m2) :displaced-index-offset (* y n))))))
		acc))

;; Apply a function to every element of a Matrix
(defun func-M (func M)
	(let* ((dims (array-dimensions M))
		   (out-M (make-array dims :initial-element 0)))
		(dotimes (i (nth 0 dims))
			(dotimes (j (nth 1 dims))
				(setf (aref out-M i j) (funcall func (aref M i j)))))
		out-M))
		
;; Apply a function to two same sized matrix (cast)
(defun M-func-M (func M1 M2)
	(let* ((dims (array-dimensions M1))
		   (out-M (make-array dims :initial-element 0)))
	    (dotimes (i (nth 0 dims))
			(dotimes (j (nth 1 dims))
				(setf (aref out-M i j) (funcall func (aref M1 i j) (aref M2 i j)))))
		out-M))
