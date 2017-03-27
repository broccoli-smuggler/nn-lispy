;;; Activation functions

(defun ReLu (x)
	(max 0 x))
	
(defun dev-ReLu (x)
	(if (< x 0)
		0
		1))

(defun ELU (x)
	(if (>= x 0)
		x
		(* 0.5 (- (exp x) 1))))

(defun dev-ELU (x)
	(if (< x 0)
		(+ (ELU x) 0.5)
		1))