;;; ReLu functions

(defun ReLu (x)
	(max 0 x))
	
(defun dev-ReLu (x)
	(if (<= x 0)
		0
		1))

(defun ELU (a x)
	(if (>= x 0)
		x
		(* a (- (exp x) 1))))

(defun dev-ELU (a x)
	(if (<= x 0)
		(* a (log x) (exp x))
		1))