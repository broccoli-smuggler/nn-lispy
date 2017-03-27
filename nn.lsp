(load "C:/Dev/LISP/nn-lispy/activation-functions.lsp")
(load "C:/Dev/LISP/nn-lispy/matrix.lsp")
								
;;; Basic neural network in lisp

(defvar *Y*)
(defvar *x*)
(defvar *syn-0*)
(defvar layer1)
(defvar deltaOL)

(setq *Y* (make-array '(4 1)
		    :initial-contents '((0) 
			 				    (0) 
								(1) 
			 					(1))))
 
(setq *x* (make-array '(4 3)
		     :initial-contents '((0 0 1) 
			 					 (0 0 1) 
			 					 (1 1 1) 
			 					 (1 1 1))))
								 
(setq *syn-0* (rand-matrix-zero-mean 3 1))


; Apply the activation function to the composition of two layers
(defun forward-prop-layer (activate-func layer0 syn0)
   (func-M activate-func (dot-product layer0 syn0)))  

(defun clamp (x)
	(if (>= x 1)
		1
		x))
   
;; We clamp the output layer, otherwise we do silly things
(defun out-error (y layer-O)
	(M-func-M '- y (func-M 'clamp layer-O)))  ;Error is just y - last layer

(defun delta-of-error (layer-E layer)
	(M-func-M '* layer-E (func-M 'dev-ReLu layer)))

;; Backpropogate the error change to the previous synapse (weights)
(defun update-syn (syn0 layer0 deltaL1)
	(M-func-M '+ syn0 (dot-product (transpose layer0) deltaL1)))


(dotimes (i 6)
	(setf layer1 (forward-prop-layer 'ReLu *x* *syn-0*))
	(setf deltaOL (delta-of-error (out-error *y* layer1) (func-M 'clamp layer1)))
	(setf *syn-0* (update-syn *syn-0* *x* deltaOL)))
	
(print (func-M 'clamp (forward-prop-layer 'ReLu *x* *syn-0*)))











