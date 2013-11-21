(in-package :autotutor)

;; except for *digit*, the following names are obviously arbitrary,
;; but see the notes in g1.lisp about naming conventions to the macros
;; which define the number classes, methods, etc.  NB!

(defparameter *digit* 10)
(defparameter *tiny* 20)
(defparameter *small* 50)
(defparameter *medium* 100)
(defparameter *big* 1000)
(defparameter *verybig* 10000)
(defparameter *huge* 1000000)
(defparameter *noperands* 3)

;; (defvar +subnat-lt20-pairs+
;;   (let* ((seq (make-array 190 :initial-contents 
;;                           (loop for i from 19 downto 1
;;                              append (loop for j from i downto 1
;;                                    collect (list i (- j)))))))
         
;;     (list 190 seq)))

(defun positive (n)
  (> n 0))

(defun nonnegative (n)
  (>= n 0))

(deftype posint ()
  `(and (satisfies integerp)
	(satisfies positive)))

(deftype nonnegint ()
  `(and (satisfies integerp)
	(satisfies nonnegative)))

(defun random-ndig (n)
  "Returns a nonnegative random number with N digits."
  (random (1- (expt 10 n))))

(defun ensure-nonzero (n)
  "Returns 1 if N is zero, otherwise N."
  (if (zerop n) 1 n))

;;use for building larger numbers in easy subtractions
(defun descending-digit-pair ()
  "Returns a pair of digits with the second less than the first."
  (let ((first (random *digit*)))
    (list first (random (1+ first)))))

;; Use for "sensibly" training digit subtraction
(defun sub-digits ()
  "Returns a sensible pair of digits with the second less than the first."
  (let ((first (rnd-between 2 *digit*)))
    (list first (* -1 (random (1+ first))))))

(defun sub-numbers1 (size)
  (let ((first (random size)))
    (list first (* -1 (random (1+ first))))))

(defun sub-numbers2 (size)
  (let ((first (rnd-between (floor (/ size 2)) size)))
    (list first (* -1 (random (1+ first))))))

(defun degree (int)
  "Returns the polynomial degree of the integer INT."
  (if (< int 10) 0
  (multiple-value-bind (ret throw-away)
      (floor (/ (log int) (log 10)))
    (declare (ignore throw-away))
    ret)))

(defun random-sign (number)
  "Return NUMBER multiplied by -1 in a random fashion."
  (if (zerop (random 2))
      number
      (* -1 number)))

;; (defun rnd-prime-lt (size)
;;   "Return a prime number less than SIZE or 2, if SIZE is less than or
;; equal to 2."
;;   (let ((max (floor (random size))))
;;     (if (<= max 2) 2
;; 	(maxima:$prev_prime max))))

(defun digit-list (n &optional (ret ()))
  "Returns a list of the digits in the natural number N."
  (unless (typep n 'nonnegint) (error "N must be an integer >= 0."))
  (if (zerop n) ret 
      (multiple-value-bind (m r)
	  (floor n 10)
	(digit-list m (push r ret)))))

(defparameter *the-digits* (digit-list 1234567890))

(defun digtribution ()
  ":-)"
  (let ((n (random 30)))
    (cond ((zerop n) 0)
	  ((<= n 2) 2)
	  ((<= n 6) 3)
	  ((<= n 10) 6)
	  ((<= n 14) 8)
	  ((<= n 18) 7)
	  ((<= n 22) 9)
	  ((<= n 26) 4)
	  ((<= n 28) 5)
	  (t 1))))

(defun polylist-to-n (list)
  "Takes a list of coefficients and converts them to a base 10 polynomial."
  (let ((lst (reverse list))
	(ret 0))
    (do ((exp 0 (1+ exp))
	 (rst (cdr lst) (cdr rst))
	 (n (car lst) (car rst)))
	((null n) ret)
      (setf ret (+ ret (* n (expt 10 exp)))))))
    
(defun digits-lt (max &optional (neg t))
  "Returns a pair consisting of a number less than MAX but bigger than
9 and a second number with each digit less than the corrsponding digit
in the first number.  By default NEG is T, which means the second
number is negative."
  (let* ((init (+ 10 (random (- max 10))))
         (first (if (zerop (mod init 10)) (1+ (+ init (random 9))) init))
	 (flst (digit-list first))
	 (digsnd (mapcar #'(lambda (n) (random (1+ n))) flst))
	 (second (polylist-to-n digsnd)))
    (list first (if neg (* -1 (ensure-nonzero second)) second))))

;; (defun digits-lt (max &optional (neg t))
;;   "Returns a pair consisting of a number less than MAX but bigger than
;; 9 and a second number with each digit less than the corrsponding digit
;; in the first number.  By default NEG is T, which means the second
;; number is negative."
;;   (let* ((init (ensure-nonzero (random max)))
;;          (first (if (zerop (mod init 10)) (1+ (+ init (random 9))) init))
;; 	 (flst (digit-list first))
;; 	 (digsnd (mapcar #'(lambda (n) (random (1+ n))) flst))
;; 	 (second (polylist-to-n digsnd)))
;;     (list first (if neg (* -1 (ensure-nonzero second)) second))))

(defun x-digits-lt (max)
  "Special case for illustrative purposes in lesson module, otherwise
like digits-lt"
  (declare (ignore max))
  (let ((a (random 10))
        (b (random 10)))
    (if (>= a b) (list (+ 10 a) (- (+ 10 b)))
        (list (+ 10 b) (- (+ 10 a))))))
    
(defun reverse-sign (n)
  "Returns -1 if N is positive, 1 if N is negative, otherwise 0."
  (if (zerop n) 
      n
      (* -1 (/ n (abs n)))))

(defun positive-sum-digit-list ()
  "Returns a list (of at least two digits) of alternating positive and
negative digits, which is guaranteed to give a positive number or 0 if
the digits are summed."
  (do ((curr (+ 6 (random 4)) (* (reverse-sign curr) (random (abs curr))))
       (ret ())
       (howmany 0))
      ((zerop curr) (if (= howmany 1) (nreverse (push -3 ret)) (nreverse ret)))
    (setf ret (push curr ret)
	  howmany (1+ howmany))))

(defun positive-sum-size-list (size &optional (more nil))
  "Returns a list of *NOPERANDS* (by default 2) numbers, less than
SIZE but bigger than 10, of alternating positive and negative numbers,
which is guaranteed to give a positive number or 0 if the numbers are
summed."
  (let ((howmany (if more *noperands* 2)))
    (do* ((sign 1 (reverse-sign sign))
	  (curr (+ 11 (random (- size 11))) (ensure-nonzero (random curr)))
	  (stop 0 (1+ stop))
	  (ret ()))
	 ((= stop howmany) (nreverse ret))
      (setf ret (push (* sign curr) ret)))))

;; (defun positive-sum-size-list (size &optional (more nil))
;;   "Returns a list of *NOPERANDS* (by default 2) numbers less than SIZE
;; of alternating positive and negative numbers, which is guaranteed to
;; give a positive number or 0 if the numbers are summed."
;;   (let ((howmany (if more *noperands* 2)))
;;     (do* ((sign 1 (reverse-sign sign))
;; 	  (curr (ensure-nonzero (random size)) (ensure-nonzero (random curr)))
;; 	  (stop 0 (1+ stop))
;; 	  (ret ()))
;; 	 ((= stop howmany) (nreverse ret))
;;       (setf ret (push (* sign curr) ret)))))

;; Remove
(defun positive-numbers-size-list (size &optional (more nil))
  "Returns a list of *NOPERANDS* (by default 2) nonnegative numbers
less than SIZE."
  (let ((howmany (if more *noperands* 2)))
    (do* ((stop 0 (1+ stop))
	  (ret ()))
	 ((= stop howmany) (nreverse ret))
      (setf ret (push (random size) ret)))))

(defparameter *prime-denominators* '(2 3 5 7 11))

(defun one-of (l)
  (let ((len (length l)))
    (nth (random len) l)))

(defun random-rat (denominatorsl)
  (let* ((den (one-of denominatorsl))
	 (num (ensure-nonzero (random (apply #'max denominatorsl)))))
    (let ((ret (ignore-errors (/ num den))))
      (if ret ret 2/3))))

(let ((count 1)
      (modulus 1)
      (fn #'values))
  (defun apply-fn-if (n &key (mod nil) (reset nil) (newfn nil))
    (cond ((or reset newfn mod)
	   (when mod (setf modulus mod)) 
	   (when reset (setf count 1))
	   (when newfn (setf fn newfn)))
	  (t
	   (cond ((not (zerop (mod count modulus))) (incf count) n)
		 (t (incf count) (funcall fn n)))))))

(defmacro apply-to-every-nth (fn nth &body lists)
  `(prog2 (apply-fn-if 'ignore :newfn ,fn :reset t :mod ,nth)
       (mapcar #'apply-fn-if ,@lists)
     (apply-fn-if 'ignore :reset t :newfn #'values :mod 1)))


;;TODO should include negative decimals but give positive sum
(defun positive-sum-decimal-list (size &optional (more nil))
    "Returns a list of *NOPERANDS* (by default 2) decimals
\"x.y\" encoded in STRINGS such that x < SIZE and y < SIZE."
  (let ((howmany (if more *noperands* 2)))
    (do* (;;(sign 1 (reverse-sign sign))
	  (currb (random size) (ensure-nonzero (random size)))
	  (curra (ensure-nonzero (random size)) (ensure-nonzero (random size)))
	  (stop 0 (1+ stop))
	  (ret ()))
	 ((= stop howmany) (nreverse ret))
      (setf ret (push (decimal-string currb curra) ret)))))

(defun posorneg-sum-decimal-list (size &optional (more nil))
  "Returns a list of *NOPERANDS* (by default 2) decimals \"x.y\"
encoded in STRINGS such that x < SIZE and y < SIZE. The decimals might
be negative."
  (let ((howmany (if more *noperands* 2)))
    (do* (;;(sign 1 (reverse-sign sign))
	  (currb (random-sign (random size)) (random-sign (ensure-nonzero (random size))))
	  (curra (ensure-nonzero (random size)) (ensure-nonzero (random size)))
	  (stop 0 (1+ stop))
	  (ret ()))
	 ((= stop howmany) (nreverse ret))
      (setf ret (push (decimal-string currb curra) ret)))))

;; TODO neaten up and regularize : use loop everywhere possible
;; seperate the functions that get named in the g1 defmacros, ie that
;; suitable objs to #'make-operands
(defun posum-rat-list (dl &optional (more nil))
  (apply-to-every-nth #'random-sign 2 
    (sort 
     (loop for count from 1 to (if more *noperands* 2) collect (random-rat dl))
     #'>)))

(defun posum-porat-list (dl &optional (more nil))
  (loop for count from 1 to (if more *noperands* 2) collect (random-rat dl)))

(defun random-int (size)
  "Return a random integer x | -SIZE < x < SIZE."
  (random-sign (random size)))

(defun random-between (lower upper)
  (+ lower (random (- upper lower))))

(defun nline-placedig (size)
  "Return a pair of numbers (a b) | SIZE > a >= 0 ,  abs(b) <= 10"
  (list (random size) (random-int 10)))

(defun nat-size-list (size &optional (more nil))
  (loop for count from 1 to (if more *noperands* 2) 
     collect (if (<= size 10) (random size) (random-between 10 size) )))

(defun non-zero-nat-size-list (size &optional (more nil))
  (loop for count from 1 to (if more *noperands* 2) collect (1+ (random (1- size)))))

(defun int-size-list (size &optional (more nil))
  (loop for count from 1 to (if more *noperands* 2) collect (random-int size)))

(defun non-zero-int-size-list (size &optional (more nil))
  (loop for count from 1 to (if more *noperands* 2) collect (random-sign (1+ (random (1- size))))))

(defun div-int-pair (size &optional (ignore nil))
  (declare (ignore ignore))
  (destructuring-bind (f1 f2)
      (non-zero-int-size-list size nil)
    `(,(* f1 f2) ,(/ 1 f2))))

(defun random-denominator (size)
  (let ((n (random size)))
    (if (or (zerop n)
            (= n 1)) 2 n)))

(defun random-pfrac-from-denom (denominator &optional (neg nil) (zero nil))
  (let ((n (random denominator)))
    (if (zerop n) (if zero 0 (if neg 
                                 `(,(random-sign 1) ,denominator)
                                 `(1 ,denominator)))
        (if neg 
            `(,(random-sign n) ,denominator)
            `(,n ,denominator)))))
        
            

;;trial FIXME ?
(defun prop-frac-size-pair (size &optional (more nil))
  (loop for count from 1 to (if more *noperands* 2)
       collect (random-pfrac-from-denom (random-denominator size))))

(defun prop-frac-size-pair-signed (size &optional (more nil))
  (loop for count from 1 to (if more *noperands* 2)
       collect (random-pfrac-from-denom (random-denominator size) t)))

(defun random-digit-mcq (n)
  (let ((alist '((0 "units")
                 (1 "tens")
                 (2 "hundreds")
                 (3 "thousands")
                 (4 "ten thousands")
                 (5 "hundred thousands")
                 (6 "millions")
                 (7 "ten millions")
                 (8 "hundred millions")
                 (9 "billions")))
        (bag '(0 1 2 3 4 5 6 7 8 9))
        (number 0)
        (q '()))
    (labels ((plural (d name)
               (if (= 1 d) (string-right-trim '(#\s) name)
                   name))
             (pair-dc (d i)
               (list d (cadr (assoc i alist :test #'=))))
             (wrong-dc (d i x)
               (when (not (= i x))
                 (list d (cadr (assoc x alist :test #'=)))))
             (prepare-parts (num parts prepared)
               (if (null parts) prepared
                   (let ((part (car parts))
                         (rst (cdr parts)))
                     (prepare-parts num rst (push (prepare-part num part)
                                                  prepared)))))
             (incorrect-part (wrong)
               (let ((d (car wrong))
                     (name (cadr wrong)))
               `(,(format nil "~D ~A:" d (plural d name))
                  ,(format nil "No, try again."))))
             (correct-part (num correct)
               (let* ((d (car correct))
                      (name (cadr correct))
                      (plural (plural d name)))
                 `(,(format nil "The digit ~D in ~D means" d num)
                    (,(format nil "~D ~A:" d plural)
                      ,(format nil "Yes, ~D in ~D means ~D ~A" 
                               d num d plural)))))
             (prepare-part (num part)
               (let ((correct (car part))
                     (wrongs (cadr part)))
                 `(,@(correct-part num correct)
                     (,@(mapcar #'incorrect-part wrongs))))))
      (cond ((> n 9) 
             (random-digit-mcq 9))
            (t (dotimes (i n)
                 (let ((d (nth (random (length bag)) bag)))
                   (setf bag (remove d bag :test #'=)
                         number (+ number (* d (expt 10 i)))
                         q (if (and (= i (1- n))
                                    (zerop d))
                               q
                               (push (let ((wrongs '()))
                                       (dotimes (x n)
                                         (let ((w (wrong-dc d i x)))
                                           (when w (setf wrongs (push w wrongs)))))
                                       (list (pair-dc d i) wrongs)) q)))))
               (prepare-parts number q '()))))))

(defun random-digits-plus-no-carry (&optional (max 8))
  (if (<= max 2) (values 1 1)
  (let* ((tot (+ 2 (random max)))
         (d1 (1+ (random (1- tot))))
         (d2 (- tot d1)))
    (if (and (= 2 d1) (= 2 d2))
        (values 1 1)
        (values d1 d2)))))

(defun random-digits-plus-carry ()
  (let* ((d1 (+ 5 (random 5)))
         (min (- 10 d1))
         (d2 (if (= min 5) 5 (rnd-between (- 10 d1) d1))))
    (values d1 d2)))

(defun z-rep-borrow (size)
  (list size (- (1+ (random (1- size))))))

(defun random-plus-no-carry (size)
  "Returns a list of two positive integers with a non-greater degree to SIZE,
none of the digits resulting in a carry of a power of ten when added
and the coefficent of the highest power of ten in the result of
addition equal to the coefficient of the highest power of ten in
SIZE."
  (let ((max (car (digit-list size))))
    (labels ((rec (deg stop n1 n2)
               (if (= deg stop) 
                   (multiple-value-bind (d1 d2)
                       (random-digits-plus-no-carry max)
                     (list (+ (* d1 (expt 10 deg)) n1)
                           (+ (* d2 (expt 10 deg)) n2)))
                   (multiple-value-bind (d1 d2)
                       (random-digits-plus-no-carry)
                     (rec (1+ deg) stop (+ (* d1 (expt 10 deg)) n1)
                          (+ (* d2 (expt 10 deg)) n2))))))
      (let ((deg (degree size)))
        (unless (>= deg 1)
          (error "Zero degree: size=~D" size))
        (multiple-value-bind (d1 d2)
            (random-digits-plus-no-carry)
          (rec 1 deg d1 d2))))))

(defun random-plus-with-carry (size)
  "Returns a list of 2 numbers less than 50 with the units digits
resulting in a carry when added, if SIZE is less than 100, otherwise a
list of 2 numbers less than 2 * SIZE all of whose coefficients of
powers of ten less than the degree of the number result in a carry
when the numbers are added.  Don't mention it, you're welcome!"
  (let ((max (car (digit-list size))))
    (labels ((rec (deg stop n1 n2)
               (if (= deg stop) 
                   (multiple-value-bind (d1 d2)
                       (random-digits-plus-no-carry max)
                     (list (+ (* d1 (expt 10 deg)) n1)
                           (+ (* d2 (expt 10 deg)) n2)))
                   (multiple-value-bind (d1 d2)
                       (random-digits-plus-carry)
                     (rec (1+ deg) stop (+ (* d1 (expt 10 deg)) n1)
                          (+ (* d2 (expt 10 deg)) n2))))))
      (let ((deg (degree size)))
        (unless (>= deg 1)
          (error "Zero degree: size=~D" size))
        (multiple-value-bind (d1 d2)
            (random-digits-plus-carry)
          (rec 1 deg d1 d2))))))
  
(defun minus-with-borrow (size)
  (let ((ns (random-plus-with-carry size)))
    (list (apply #'+ ns) (- (car ns)))))

;; (defun test-random-plus-no-carry (x)
;;   (dotimes (n x) 
;;     (let* ((p (random-plus-no-carry 20))
;;            (d1 (digit-list (car p))) (d2 (digit-list (cadr p))))
;;       (unless (every #'(lambda (d1 d2) (<= (+ d1 d2) 9)) d1 d2)
;;         (error "Bad lists: ~S" (list d1 d2))))))

;; (defun test-random-digits-no-carry (x)
;;   (dotimes (n x)
;;     (multiple-value-bind (d1 d2)
;;         (random-digits-plus-no-carry)
;;       (unless (<= (+ d1 d2) 9)
;;         (error "Bad pair: ~S, ~S" d1 d2)))))

(defvar  +all-digit-pairs+ 
  (loop for d1 from 1 to 9
     append (loop for d2 from d1 to 9
                collect (list d1 d2))))

(defvar +all-mul-pairs+
  (loop for i from 0 to 10
     append (loop for j from i to 10
               collect (list "*" i j))))

(defun shuffle (list)
  (let ((tagged (mapcar #'(lambda (x) (cons (random 1.0) x)) list)))
    (mapcar #'cdr (sort tagged #'> :key #'car))))

;;the next 2 do the same thing.  n-digit-pairs appears to use fewer
;;cycles

;; (defun random-sort (&optional (inbag (copy-list +all-digit-pairs+)))
;;   "Return the randomly sorted list BAG"
;;   (let ((n (length inbag)))
;;     (do* ((bag inbag (remove-if #'(lambda (p) (eql p it)) bag))
;;           (count 0 (1+ count))
;;           (ln n (1- ln))
;;           (it (nth (if (= ln 1) 0 (random ln)) bag) 
;;               (nth (if (= ln 1) 0 (random ln)) bag))
;;           (gather (list it) (push it gather)))
;;          ((= count (1- n)) gather))))
  
;; (defun gen-digit-add-pairs ()
;;   (let* ((arr (make-array 45))
;;          (idxs (loop for i from 0 to 44 collect i))
;;          (ln (length idxs)))
;;     (labels ((random-index ()
;;                (let* ((r (if (= ln 1) 0 (random ln)))
;;                       (idx (nth r idxs)))
;;                  (setf idxs (remove-if #'(lambda (x) (= x idx)) idxs)
;;                        ln (1- ln))
;;                  idx)))
;;       (loop for d1 from 1 to 9
;;          do (loop for d2 from d1 to 9
;;                do (setf (aref arr (random-index)) (list d1 d2)))))
;;     (coerce arr 'list)))
    
(defun teen-sub-digit-pairs ()
  (loop for u from 8 downto 1
        append (loop for sub from (1+ u) to 9
                 collect (list (+ 10 u) (- sub)))))

(defun digit-sub-digit-pairs ()
  (loop for d1 from 9 downto 2
       append (loop for d2 from (1- d1) downto 1
                   collect (list d1 (- d2)))))

(defun sub-digit-pairs ()
  (append (teen-sub-digit-pairs)
          (digit-sub-digit-pairs)))

(defvar +sub-digit-pairs+
  (sub-digit-pairs))

(defun table-pairs ()
  (loop for n from 1 to 10
       append (loop for m from 1 to 10
                   collect (list n m))))

(defun table-n (n)
  (loop for x from 1 to 10
       collect (list n x)))

(defvar +div-mul-pairs+
  (loop for p in +all-mul-pairs+
     unless (destructuring-bind (op f1 f2)
                p
              (declare (ignore op))
              (and (zerop f1)
                   (zerop f2)))
     append (destructuring-bind (op f1 f2)
                p
              (declare (ignore op))
              (let ((prod (* f1 f2)))
                (cond ((zerop prod)
                       (list (list "/" prod (if (zerop f1) f2 f1))))
                      ((= f1 f2)
                       (list (list "/" prod f1)))
                      (t (list 
                          (list "/" prod f1)
                          (list "/" prod f2))))))))

