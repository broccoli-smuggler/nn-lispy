;;; Activation functions

(defun sigmoid (x)
	(/ 1 (+ 1 (exp (* -1 x)))))
	
(defun dev-sigmoid (x)
	(* x (- 1 x)))

(defun ReLu (x)
	(max 0 x))
	
(defun dev-ReLu (x)
	(if (< x 0)
		0
		1))

(defun ELU (x a)
	(if (listp a)
		(setf a (nth 0 a)))
	(if (>= x 0)
		x
		(* a (- (exp x) 1))))

(defun dev-ELU (x a)
	(if (listp a)
		(setf a (nth 0 a)))
	(if (< x 0)
		(+ (ELU x a) a)
		1))