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
	     (sqrt-babylon (/ (a2 m delta sigma) 
			      (* m (b2 m delta sigma))))
	     (rationalize (exp (- (* -1 (c m delta sigma k) (+ (expt s1x 2)
						   (expt s2x 2)))
		      (* (d m delta sigma k) (expt (- s1x s2x) 2)))
		   ))))))
#+nil
(w (/ 3 10) (/ 1 10) 20 (/ 7 10) (/ 4 10) (/ 2))
#+nil
(* 1d0 (reduce #'+
	 (mapcar (lambda (x) (reduce #'* x))
		 (w (/ 3 10) (/ 1 10) 30 (/ 7 10) (/ 4 10) (/ 2)))))

;;(ql:quickload "oct")


(defun sqrt-babylon (s &key (itermax 12) (digits 14) (debug nil))
  "use babylonian method (newton iteration) to find the rational approximation of the square root of the number s"
  (let ((x (rationalize (sqrt s))))
    (loop for i below itermax while (let ((v (abs (- s (* x x)))))
				      (and (not (= 0d0 v)) (< (- digits)
							      (log v 10)))) do
	  
	  (setf x (* 1/2 (+ x (/ s x))))
	  (when debug
	   (format t "~12,12f ~12,12f~%" (* 1d0 (abs (- s (* x x)))) (* 1d0 (log (abs (- s (* x x))) 10)))))
    x))

#+nil
(sqrt-babylon 2 :debug t :digits 14)
;; http://en.wikipedia.org/wiki/Generalized_continued_fraction
;; http://rosettacode.org/wiki/Continued_fraction#C.2B.2B
(defun estimate-continued-fraction (generator n)
  (let ((temp 0))
    (DECLARE (type (or number ratio) temp))
    (loop for n1 from n downto 1
       do (multiple-value-bind (a b) 
	      (funcall generator n1)
	    (setf temp (/ b (+ a temp)))))
    (+ (funcall generator 0) temp)))

#+nil
(format t "sqrt(2) = ~a~%" (estimate-continued-fraction
			    (lambda (n)
			      (values (if (> n 0) 2 1) 1)) 10))
#+nil
(let ((z (rationalize 30))) 
  (log (abs (- z 
	   (log
	    (estimate-continued-fraction
	     (lambda (n)
	       (values (if (< n 2) 
			   n
			   (+ 1 (/ z (- n 1))))
		       (if (= n 1)
			   1
			   (- (/ z (- n 1))))))
	     54))))))

(let ((z 32))
 (estimate-continued-fraction
  (lambda (n)
    (values (cond ((= n 0) 1)
		  ((= n 1) (- 2 z))
		  (t (* 2 (- (* 2 n) 1))))
	    (if (= n 1)
		(* 2 z)
		(expt z 2))))
  54))

;; https://en.wikipedia.org/wiki/Exponential_function gives faster continued fraction

(defun exp-continued-fraction (z &key (iterstart 2) (itermax 300) (digits 5) (debug nil))
  ;; exponential function ez is an entire function with a power series
  ;; expansion that converges uniformly on every bounded domain in the
  ;; complex plane
  ;; http://en.wikipedia.org/wiki/Euler%27s_continued_fraction_formula
  (labels ((fun (maxn)
	    (estimate-continued-fraction
	     (lambda (n)
	       (values (if (< n 2) 
			   n
			   (+ 1 (/ z (- n 1))))
		       (if (= n 1)
			   1
			   (- (/ z (- n 1))))))
	     maxn)))
    (let ((err 100d0)
	  (result 0))
      (declare (type (or number ratio) result))
     (loop for i from iterstart below (+ iterstart itermax) while (< (- digits) err) do
	  (let ((v
		 (fun i)))
	    (setf err (log (abs (- z (log v))) 10))
	    (setf result v)
	    (when debug
	      (format t "~12,12f ~a ~a~%" err (* 1d0 (log v)) v))))
     result)))

(defun exp-continued-fraction* (z &key (n 30))
  (labels ((fun (maxn)
	     (estimate-continued-fraction
	      (lambda (n)
		(values (if (< n 2) 
			    n
			    (+ 1 (/ z (- n 1))))
			(if (= n 1)
			    1
			    (- (/ z (- n 1))))))
	      maxn)))
    (fun n)))
#+nil
(progn (terpri)
 (exp-continued-fraction (rationalize 100) :debug t :digits 5 :itermax 300))

(defun exp-ratio (x &key (itermax 4) (debug nil))
  (declare (type (or number ratio) z))
  "starting from the double-float evaluation of the exponential of x, improve the result using a taylor expansion around this double-float point"
  (let* ((a (rationalize (coerce x 'single-float)))
	 (fa (rationalize (exp a)))
	 (d (- x a)))
    (when debug
      (format t "DIFF ~a~%" (* 1d0 d)))
    (when debug
      (format t "coef: ~{~a~%~}" (loop for i from 0 below itermax collect
				      (* 1d0 (/ (expt (* a d) i)
					  (factorial i))))))
    (* fa

       (loop for i from 0 below itermax sum
	    (/ (expt (* a d) i)
	       (factorial i))))))

(rational (log (exp (/ 6 11))))

(- (rational (/ 6 11))
 (/ 6 11)) 

(let* ((y 35)
       (x (sqrt-babylon y :digits 100)))
  ;;(exp-ratio x)
  
#+nil
  (* 1d0 (- x (rationalize (coerce x 'double-float))))
  
  (let ((v (exp-ratio x :itermax 13 :debug t)))
    ;;v #+nil
    (exp-continued-fraction* x :n 50)
#+nil
    (list (* 1d0 (- v (exp-continued-fraction* x :n 50))) (* 1d0 v) (- y (expt (log v) 2)))
    #+nil(if (= v 0d0) 
	nil
	(log v 10))))

#+nil
(with-open-file (s "/dev/shm/o.dat" :direction :output :if-exists :supersede)
  (let ((mm 30)); loop for mm from 30 upto 30 by 2 do
	(loop for i from -250 upto 250 by 1 do
	      (let ((v (w (/ i 10) (/ 10) mm (/ 7 10) (/ 4 10) (/ 2))))
	       (format s "~12,8f ~12,8f ~a~%" (* .1 i) (* 1d0 v) v)
	       (force-output s)))
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
