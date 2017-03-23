(load "C:/Dev/LISP/nn-lispy/activation-functions.lsp")

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
								
;;; Basic neural network in lisp
(defun random-list-zero-mean (len)
	(let ((acc nil))
		(dotimes (i len)
			(push (- (* (random 1.0) 2) 1) acc))
		acc))
		
(defun make-n-m-rand-lists (n m)
	(let ((acc nil))
		(dotimes (i n)
			(push (random-list-zero-mean m) acc))
		acc))
	

(defun rand-array-zero-mean (n m)
	(make-array (list n m)
		:initial-contents (make-n-m-rand-lists n m)))

(setq *syn0* (rand-array-zero-mean 4 1))
	

(print *syn0*)








