(load "C:/Dev/LISP/nn-lispy/activation-functions.lsp")
								
;;; Basic neural network in lisp

(defvar *Y*)
(defvar *x*)
(defvar *syn-0*)

(setq *Y* (make-array '(4 1)
		    :initial-contents '((0) 
			 				    (0) 
								(1) 
			 					(1))))
 
(setq *x* (make-array '(4 4)
		     :initial-contents '((1 0 1 1) 
			 					 (1 1 1 0) 
			 					 (0 0 1 0) 
			 					 (1 1 1 1))))
								 
(setq *syn-0* (rand-matrix-zero-mean 4 1))

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
		   (acc (make-array (list i j) :initial-element 0)))
	    (dotimes (x i)
			(dotimes (y j)
				(setf (aref acc x y) (dot (make-array i :displaced-to m1 :displaced-index-offset (* x i))
										  (make-array i :displaced-to (transpose m2) :displaced-index-offset (* y i))))))
		acc))
		
(defun forward-prop-layer (layer syn active-fn)
	(let* ((dotted (dot-product layer syn))
		   (dims (array-dimensions dotted))
		   (out-layer (make-array dims :initial-element 0)))
		(dotimes (x (nth 0 dims))
			(dotimes (y (nth 1 dims))
				(setf (aref out-layer x y) (funcall active-fn (aref dotted x y)))))
		out-layer))
		

(print (forward-prop-layer *x* *syn-0* 'ReLu))








