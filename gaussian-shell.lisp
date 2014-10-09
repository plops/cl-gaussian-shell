(defun factorial-d (n &optional (acc 1d0))
  (declare (type double-float n acc))
  (if (<= n 1)
      acc
      (factorial-d (- n 1) (* acc n))))

(defun nchoosek-d (n k)
  (/ (factorial-d n)
     (* (factorial-d k) (factorial-d (- n k)))))

(nchoosek-d 40d0 20d0)

(defun factorial (n &optional (acc 1))
  (if (<= n 1)
      acc
      (factorial (- n 1) (* acc n))))

(defun nchoosek (n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

(- (nchoosek 60 20)
   (nchoosek-d 60d0 20d0))

(nchoosek 3 1)

(defun shell-binomials (mm)
  (let ((mm-over-m (nchoosek mm 1)))
    (loop for m from 1 upto mm collect
	 (prog1 
	     mm-over-m
	   ;; update to m+1
	   (setf mm-over-m (* (/ (- mm m)
				 (+ m 1))
			      mm-over-m))))))

(defun a2 (m delta sigma)
  (* (expt sigma 2) 
     (/ (+ (* 2 m (expt delta 2)) (* 4 (expt sigma 2)))
	(+ (*   m (expt delta 2)) (* 4 (expt sigma 2))))))

(defun b2 (m delta sigma)
  (+ (/ 2 (* m (expt delta 2)))
     (/ (expt sigma 2))))

(defun c (m delta sigma k)
  (/ (expt (* k sigma m delta) 2)
     (+ (* m (expt delta 2))
	(* 4 (expt sigma 2)))))

(defun d (m delta sigma k)
  (/ (* 2 (expt k 2) (expt sigma 4))
     (+ (* m (expt delta 2))
	(* 4 (expt sigma 2)))))


(defun w-sorted (s1x s2x mm delta sigma k)
 (let ((l (sort (loop for b in (shell-binomials mm) and m from 1 collect 
		     (list m b))
		#'< :key #'second)))
   (labels ((loc (m b)
	      (* b (expt -1 (- m 1)) (sqrt (/ (a2 m delta sigma) 
					      (* m (b2 m delta sigma))))
		 (exp (+ (* -1 (c m delta sigma k) (+ (expt s1x 2)
						      (expt s2x 2)))
			 (* -1 (d m delta sigma k) (expt (- s1x s2x) 2)))))))
     (reduce #'+ (append (loop for (m b) in (list (first l)) collect
		   (loc m b))
	      (let ((ll (loop for (m b) in (rest l) collect
			     (loc m b))))
		(loop for i below (- (length ll) 1) by 2 collect
		     (+ (elt ll i)
			(elt ll (+ i 1))))))))))

(w-sorted .1 .1 6 .7 .4 .5)

(defun w (s1x s2x mm delta sigma k)
  (let ((binomials (shell-binomials mm)))
    (loop for m from 1 upto mm and b in binomials sum
	  (* b 
	     (expt -1 (- m 1))
	     (rationalize (sqrt (/ (a2 m delta sigma) 
				(* m (b2 m delta sigma)))))
	     (rationalize (exp (- (* -1 (c m delta sigma k) (+ (expt s1x 2)
							    (expt s2x 2)))
			       (* (d m delta sigma k) (expt (- s1x s2x) 2)))))))))
#+nil
(w (/ 3 10) (/ 1 10) 30 (/ 7 10) (/ 4 10) (/ 2))
#+nil
(* 1d0 (reduce #'+
	 (mapcar (lambda (x) (reduce #'* x))
		 (w (/ 3 10) (/ 1 10) 30 (/ 7 10) (/ 4 10) (/ 2)))))

;;(ql:quickload "oct")


(defun sqrt-babylon (s &key (itermax 12) (digits 100) (debug nil))
  (let ((x 2/1))
    (loop for i below itermax while (< (- digits)
				       (log (abs (- s (* x x))) 10)) do
	  
	  (setf x (* 1/2 (+ x (/ s x))))
	  (when debug
	   (format t "~12,12f ~12,12f~%" (* 1d0 (abs (- s (* x x)))) (* 1d0 (log (abs (- s (* x x))) 10)))))
    x))

#+nil
(sqrt-babylon 2 :debug t :digits 2000)

#+nil
(with-open-file (s "/dev/shm/o.dat" :direction :output :if-exists :supersede)
  (let ((mm 30)); loop for mm from 30 upto 30 by 2 do
	(loop for i from -2500 upto 2500 by 1 do
	      (let ((v (w (/ i 100) (/ 10) mm (/ 7 10) (/ 4 10) (/ 2))))
	       (format s "~12,8f ~12,8f~%" (* .01 i) (* 1d0 v))))
	(terpri s)))

#+nil
(list
 (mapcar #'second (sort (loop for i from 0 and e in (mapcar #'abs (w .1 .1 30 .7 .4 .5))
			      and f in (w .2 .1 30 .7 .4 .5)
			      collect (list e f)) #'< :key #'first))
 (mapcar #'second (sort (loop for i from 0 and e in (mapcar #'abs (w .1 .1 30 .7 .4 .5))
			    and f in (w .1 .1 30 .7 .4 .5)
			    collect (list e f)) #'< :key #'first)))
#+nil
(sort (w .1 .1 30 .7 .4 .5) #'<)
