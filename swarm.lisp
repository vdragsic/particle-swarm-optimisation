
;; ---------------------------------------------------------
;; init constants

(defparameter *l-constraint* -10.0
  "Low constraint")

(defparameter *h-constraint* 10.0
  "High constraint")

(defparameter *dimensions* 4
  "Number of dimensions")

(defparameter *max-velocity* 5.0
  "Maximum velocity for particles")

(defparameter *num-particles* 50
  "Number of particles")

;; learning factors
(defparameter *c1* 2.)
(defparameter *c2* 2.)


;; list of particles

(defvar *particles* nil
  "List of particles: ((position) (velocity) (local best) fitness")



;; test functions

(defun rastrigin-subfunc (x)
  "x^2 - 10 cos (2 PI x)"

  (- (* x x) (* 10.0 (cos (* 2.0 PI x)))))

(defun get-rastrigin (args)
  "Generalized Rastigin function --> for testing"

  (+ 
   (* 10 (length args))
   (reduce #'+ (mapcar #'(lambda (x) (rastrigin-subfunc x)) args))))


;; ---------------------------------------------------------
;; functions

;; init functions

(defun get-random ()
  "Returns random number between low and high constraint"

  (+ (random (- *h-constraint* *l-constraint*)) *l-constraint*))


(defun gen-random-position (&optional (n *dimensions*))
  "Returns list of n random numbers"

  (if (<= n 0)
      nil
      (cons (get-random) (get-random-position (- n 1)))))
      

(defun gen-start-velocity (&optional (n *dimensions*))
  "Returns list of n zeros (0)"

  (if (<= n 0)
      nil
      (cons 0 (gen-start-velocity (- n 1)))))


(defun init-particle ()
  "Creates one particle list --> ((position) (velocity) fitness)"

  (let*
      ((position (gen-random-position)) ;; generate random start position
       (velocity (gen-start-velocity))  ;; generate start velocity (0)
       (fitness (get-rastrigin position)))
    (list position velocity position fitness)))


(defun init-population (&optional (n *num-particles*))
  "Initializes population of n particles, 
   evaluates their fitnesses and puts the best one in *global-best*"

  (setf *particles* nil)
  (dotimes (i n)
    (push (init-particle) *particles*)))


;; eval functions

;; ps ... a vjerovatno se moglo i pametnije ovo napisat ;)
(defun get-best-neighbour (index)
  "Get better neighbour's p-best position, by distance = 1"

  ;; get indexes
  (let* ((prev (if (eq index 0) (1- *num-particles*) (1- index)))
	(next (if (eq index (1- *num-particles*)) 0 (1+ index))))
    
    ;; choose better
    (if (< (fourth (nth prev *particles*))
	   (fourth (nth next *particles*)))
	(third (nth prev *particles*))
	(third (nth next *particles*)))))


(defun eval-velocity-subfunc (position velocity pbest gbest)
  "Velocity calculation, for one dimension"

  (let ((new-velocity
	(+ velocity
	  (* *c1* (random 1.0) (- pbest position))
	  (* *c2* (random 1.0) (- gbest position)))))

    (cond
      ((< new-velocity (- *max-velocity*))
       (- *max-velocity*))
      ((> new-velocity *max-velocity*)
       *max-velocity*)
      (t
       new-velocity))))
       

(defun eval-position (position velocity)
  "Position calculation, for one dimension"

  (let ((new-position (+ position velocity)))
    	 
    (cond
      ((< new-position *l-constraint*) *l-constraint*)
      ((> new-position *h-constraint*) *h-constraint*)
      (t new-position))))


;; glavni buckuris, isto vjerovatno moze pametnije
(defun eval-particle (index)
  "Evaluates velocity for particle identified by index"
  
  (let*
      ((position (first (nth index *particles*)))
       (velocity (second (nth index *particles*)))
       (p-best (third (nth index *particles*)))
       (fitness (fourth (nth index *particles*)))
       (g-best (get-best-neighbour index)))

    ;; update velocity
    (setf 
     (second (nth index *particles*))
     (mapcar #'(lambda (p v pb gb) 
		 (eval-velocity-subfunc p v pb gb)) 
	     position velocity p-best g-best))

    ;; update position
    (setf
     (first (nth index *particles*))
     (mapcar #'(lambda (p v)
		 (eval-position p v))
	     position velocity))

    ;; update pbest and fitness
    (let ((new-fitness 
	   (get-rastrigin (first (nth index *particles*)))))
      (if (< new-fitness fitness)
	  (progn
	    (setf (third (nth index *particles*)) (first (nth index *particles*)))
	    (setf (fourth (nth index *particles*)) new-fitness))))))


(defun print-best-particle ()
  "Prints index of particles with best fitness"

  (let* 
      ((best-index 0)
       (best-fitness (fourth (nth best-index *particles*))))

    (dotimes (i *num-particles*)
      (if (< (fourth (nth i *particles*)) best-fitness)
	  (progn
	    (setf best-index i)
	    (setf best-fitness (fourth (nth best-index *particles*))))))

    (format t "~a: ~a~%" best-index best-fitness)))
          

(defun eval-population (&optional (n 100000))
  "Evaluates all particles in population"

  (dotimes (i n)
    (dotimes (j *num-particles*)
      (eval-particle j))))

