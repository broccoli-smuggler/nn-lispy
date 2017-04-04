(load "C:/Dev/LISP/nn-lispy/activation-functions.lsp")
(load "C:/Dev/LISP/nn-lispy/matrix.lsp")
                                
;;; Basic neural network in lisp

(defvar *Y*)
(defvar *x*)
(defvar *syn-0*)
(defvar *syn-1*)
(defvar layer1)
(defvar layer2)
(defvar delta0L)
(defvar delta1L)
(defvar err0)
(defvar err1)

(setq *Y* (make-array '(7 1)
            :initial-contents '((0) 
                                (1) 
                                (1) 
                                (0)
                                (0)
                                (1)
                                (1))))
 
(setq *x* (make-array '(7 4)
             :initial-contents '((0 0 1 1) 
                                 (0 1 1 0) 
                                 (1 0 1 0) 
                                 (1 1 1 0)
                                 (0 0 0 1)
                                 (1 1 0 1)
                                 (0 1 0 0))))
                                 
(setq *syn-0* (rand-matrix-zero-mean 4 7))
(setq *syn-1* (rand-matrix-zero-mean 7 1))
   
; Apply the activation function to the composition of two layers
(defun forward-prop-layer (activate-func layer syn activate-args)
    (if (null activate-args)
        (func-M activate-func (dot-product layer syn))
        (func-M activate-func (dot-product layer syn) activate-args)))  
   
(defun error-function (y layer-O)
	(M-func-M '- y layer-O))
	
(defun softmax-function (M)
	(let* ((dims (array-dimensions M))
		    (out-M (make-array dims :initial-element 0))
			(i (nth 0 dims))
			(j (nth 1 dims)))
		    (dotimes (y j)
				(let* ((v (make-array i :displaced-to M :displaced-index-offset (* y i)))
					   (v-exp (map 'vector #'exp v))
					   (the-sum (reduce #'+ v-exp)))
					(dotimes (x i)
						(setf (aref out-M x y) (/ (svref v-exp x) the-sum)))))
			out-M))
	

(defun delta-of-error (layer-E layer dev-func activate-args)
    (if (null activate-args)
        (M-func-M '* layer-E (func-M dev-func layer))
        (M-func-M '* layer-E (func-M dev-func layer activate-args))))

;; Backpropogate the error change to the previous synapse (weights)
(defun update-syn (syn layer delta)
    (M-func-M '+ syn (dot-product (transpose layer) delta)))


(defvar activate-args)
(setf activate-args 0.4)  ; ELU works with alpha < 0.5 for y - out-layer
(defvar *activate-func*)
(setf *activate-func* 'ELU)
(defvar *dev-activate-func*)
(setf *dev-activate-func* 'dev-ELU)


(dotimes (i 300)
    ; Forward
    (setf layer1 (forward-prop-layer *activate-func* *x* *syn-0* activate-args))
    (setf layer2 (forward-prop-layer *activate-func* layer1 *syn-1* activate-args))
    
    ; Back
	(print (softmax-function layer2))
    (setf err1 (error-function *y* (func-M 'top-clamp layer2 1)))
    (setf delta1L (delta-of-error err1 layer2 *dev-activate-func* activate-args))
    (setf err0 (dot-product delta1L (transpose *syn-1*)))
    (setf delta0L (delta-of-error err0 layer1 *dev-activate-func* activate-args))
    
    ; Update
    (setf *syn-1* (update-syn *syn-1* layer1 delta1L))
    (setf *syn-0* (update-syn *syn-0* *x* delta0L)))

; Result
(print (func-M 'top-clamp (forward-prop-layer *activate-func* layer1 *syn-1* activate-args) 1))











