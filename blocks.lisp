(defparameter *blocks* '((b1 shape brick)
                         (b1 colour green)                                                                         
                         (b1 size small)                                                                           
                         (b1 supported-by b2)                                                                      
                         (b1 supported-by b3)                                                                      
                         (b2 shape brick)                                                                          
                         (b2 colour red)                                                                           
                         (b2 size small)                                                                           
                         (b2 supports b1)                                                                          
                         (b2 left-of b3)                                                                           
                         (b3 shape brick)                                                                          
                         (b3 colour red)                                                                           
                         (b3 size small)                                                                           
                         (b3 supports b1)                                                                          
                         (b3 right-of b2)                                                                          
                         (b4 shape pyramid)                                                                        
                         (b4 colour blue)                                                                          
                         (b4 size large)                                                                           
                         (b4 supported-by b5)                                                                      
                         (b5 shape cube)                                                                           
                         (b5 colour green)                                                                         
                         (b5 size large)                                                                           
                         (b5 supports b4)                                                                          
                         (b6 shape brick)                                                                          
                         (b6 colour purple)                                                                        
                         (b6 size large)))

(defun match-element (e-1 e-2)
  "Does E-1 match E-2?"
  (or (eq e-1 e-2)
      (eq e-2 '?)))

(defun match-triple (assertion pattern)
  "Does ASSERTION match PATTERN?"
  (every #'match-element assertion pattern))

(defun fetch (pattern)
  "Return all assertions in the global database *BLOCKS* that match PATTERN."
  (remove-if-not #'(lambda (assertion)
		     (match-triple assertion pattern))
		 *blocks*))

(defun colour-pattern (block)
  "The pattern that will match the colour of BLOCK."
  `(,block colour ?))

(defun supporters-pattern (block)
  "The pattern that will match the supporters of BLOCK."
  `(? supports ,block))

(defun supporters (block)
  "The supporters of BLOCK."
  (mapcar #'car
	  (fetch (supporters-pattern block))))

(defun supported-by-cube (block)
  "Is BLOCK supported by a cube?"
  (some #'(lambda (supporter)
	    (fetch `(,supporter shape cube)))
	(supporters block)))

(defun description (block)
  "A description of BLOCK."
  (apply #'append
	 (mapcar #'cdr
		 (fetch `(,block ? ?)))))
