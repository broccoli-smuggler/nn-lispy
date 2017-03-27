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

(setq *Y* (make-array '(4 1)
            :initial-contents '((0) 
                                (1) 
                                (1) 
                                (0))))
 
(setq *x* (make-array '(4 3)
             :initial-contents '((0 0 1) 
                                 (0 1 1) 
                                 (1 0 1) 
                                 (1 1 1))))
                                 
(setq *syn-0* (rand-matrix-zero-mean 3 4))
(setq *syn-1* (rand-matrix-zero-mean 4 1))


(defun clamp (x)
    (if (>= x 1)
        1
        x))
   
; Apply the activation function to the composition of two layers
(defun forward-prop-layer (activate-func layer syn)
   (func-M activate-func (dot-product layer syn)))  
   
;; We clamp the output layer, otherwise we do silly things
(defun out-error (y layer-O &optional (clp t))
    (if (not clp)
        (M-func-M '- y layer-O)
        (M-func-M '- y (func-M 'clamp layer-O))))  ;Error is just y - last layer

(defun delta-of-error (layer-E layer)
    (M-func-M '* layer-E (func-M 'dev-sigmoid layer)))

;; Backpropogate the error change to the previous synapse (weights)
(defun update-syn (syn layer delta)
    (M-func-M '+ syn (dot-product (transpose layer) delta)))

(dotimes (i 5000)
    ; Forward
    (setf layer1 (forward-prop-layer 'sigmoid *x* *syn-0*))
    (setf layer2 (forward-prop-layer 'sigmoid layer1 *syn-1*))
    
    ; Back
    (setf err1 (out-error *y* layer2 t))
    (setf delta1L (delta-of-error err1 (func-M 'clamp layer2)))
    
    (setf err0 (dot-product delta1L (transpose *syn-1*)))
    (setf delta0L (delta-of-error err0 layer1))

    
    ; Update
    (setf *syn-1* (update-syn *syn-1* layer1 delta1L))
    (setf *syn-0* (update-syn *syn-0* *x* delta0L)))

(print (forward-prop-layer 'sigmoid layer1 *syn-1*))











