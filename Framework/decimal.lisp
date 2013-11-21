;; Copyright 2013 Peter Robert Wood  

;; mail: peter@thetamat.net

;; This file is part of Thetamat.

;; Thetamat is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Thetamat is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Thetamat.  If not, see <http://www.gnu.org/licenses/>.

(in-package :autotutor)

(defparameter *decimal-char* #\.)
(defparameter *decimal-char-code* (char-code *decimal-char*))

(defclass decimal-representation () 
  ((name :initarg :name :initform "0" :accessor d-s)
   (period :initarg :period :initform 0 :accessor d-p)
   (length :initarg :plength :initform 1 :accessor d-l)
   (repeating :initarg :repeating :initform nil :accessor d-r)
   (intpart :initarg :intpart :initform 0 :accessor d-ip)
   (exponent :initarg :exponent :initform 0 :accessor d-ex))
  (:documentation "Class for doing exact decimal arithmetic"))

(defun decimal-parts (str)
  (let* ((pidx (position *decimal-char* str))
	 (before (subseq str 0 pidx))
	 (after (subseq str (1+ pidx))))
    (list before after)))

(defmethod print-object ((d decimal-representation) stream)
  (if *print-readably*
      (call-next-method)
      (let ((ba (decimal-parts (d-s d))))
        (format stream "#:~D~A~D:" (car ba) (string *decimal-char*) (cadr ba)))))

(defun decimal-string (b a)
  (format nil "~D~A~D" b *decimal-char* a))

(defun tarotd (sr)
  "Converts rational SR to a DECIMAL-REPRESENTATION."
  (let* ((neg (if (< sr 0) t nil))
	 (firstiter t)
	 (int 0)
	 (r (if neg (abs sr) sr))
	 (period ())
	 (num (numerator r))
	 (den (denominator r))
	 (ret ()))
    (do ((count 0 (1+ count)))
	(())
      (multiple-value-bind (m rem)
	  (floor num den)
	(let ((found (position rem period :test #'=)))
	  (cond (firstiter 
		 (setf 
		  firstiter nil 
		  int m 
		  num (* rem 10) 
		  period (push rem period)))
		(found
		 (if (zerop m)
		     (return-from tarotd (values neg int ret (1- count) nil))
		     (return-from tarotd (values neg int (push m ret) count (1+ found)))))
		(t 
		 (setf ret (push m ret)
		       num (* rem 10)
		       period (push rem period)))))))))

(defun ndigits (n)
  "Returns the number of digits in N."
  (if (zerop n) n
      (1+ (floor (/ (log n) (log 10))))))

(defun dtorat (d) ;;TODO fix if int is 0 and period is negative
  (let* ((negrep (if (and (d-r d) (or (< (d-p d) 0)
				      (< (d-ip d) 0)))
		     t nil)))
    (if (not (d-r d))
	(if (zerop (d-p d))
	    (/ (d-ip d) (expt 10 (abs (d-ex d))))
	    (/ (+ (* (d-ip d) (expt 10 (d-l d))) (d-p d)) (expt 10 (d-l d))))
	(if negrep (* -1 (dtorat 
			  (make-instance 'decimal-representation
					 :period (abs (d-p d))
					 :plength (d-l d)
					 :repeating (d-r d)
					 :intpart (abs (d-ip d))
					 :exponent (d-ex d))))
	    (let ((subtrahend (floor (d-p d) (expt 10 (d-r d)))))
	      (let ((new (/ (- (d-p d) subtrahend) 
			    (- (expt 10 (d-l d)) (expt 10 (ndigits subtrahend))))))
		(if (not (zerop (d-ip d)))
		    (let ((num (numerator new))
			  (den (denominator new)))
		      (/ (+ num den) den))
		    new)))))))

(defun change-decimal-char (char)
  (setf *decimal-char* char
	*decimal-char-code* (char-code *decimal-char*)))

(defun decimal-digit-p (cc)
  (<= 48 cc 57))

(defun decimal-char-code-p (cc)
  (= cc *decimal-char-code*))

(defun remove-whitespace (str)
  (remove-if #'(lambda (x) (char= x #\Space)) str))

(defun ctoi (char-code)
  (- char-code 48))

(defun parse-decimal (str)
  "Returns a list consisting of an INTEGER, an EXPONENT and a STRING,
such that INTEGER * 10^EXPONENT = the decimal in STR.  The STRING is
STR, stripped of spaces."
  (when (string= str "0") (setf str "0.0"))
  (let* ((s0 (remove-whitespace str))
	 (negative (if (char= (char s0 0) #\-) t nil))
	 (s-0 (string-left-trim '(#\- #\0) s0))
	 (s1 (nreverse (map 'list #'(lambda (x) (char-code x)) s0)))
	 (s (if negative (butlast s1) s1)))
    (if (every #'decimal-digit-p s)
	(let* ((name s-0)
	       (int (parse-integer s-0)))
	  (if negative 
	      (list (* -1 int) 0 (concatenate 'string "-" name "."))
	      (list int 0 (concatenate 'string name "."))))
	(let ((ret 0)
	      (seen nil)
	      (dbp 0)
	      (exp 0))
	  (dolist (v s (list (if negative (* -1 ret) ret) (- dbp) s0))
	    (if (decimal-char-code-p v)
		;;don't increment exp
		;;record decimals 'before' point (it's reversed)
		;;record sighting
		(if seen 
		    (error "Garbage in: ~S" str)
		    (progn 
		      (setf seen t)
		      (setf dbp exp)))
		;;update ret value
		;;increment exponent
		(progn 
		  (setf ret (+ ret (* (ctoi v) (expt 10 exp))))
		  (setf exp (1+ exp)))))))))

(defgeneric make-decimal-representation (thing) 
  (:documentation "Make a decimal from various objects."))

(defmethod make-decimal-representation ((s string)) 
  (let ((canonical (parse-decimal s)))
    (make-instance 'decimal-representation 
		   :name (third canonical)
		   :exponent (cadr canonical) 
		   :intpart (car canonical))))

(defun bystring (init x)
  (concatenate 'string init (string (code-char (+ x 48)))))

(defmethod make-decimal-representation ((r rational))
  (multiple-value-bind (neg int period plength repeating)
      (tarotd r)
    (let* ((ap  (reduce #'bystring (nreverse period) :initial-value ""))
	  (int (if neg (- int) int))
	  (per (if (and neg (zerop int)) (* -1 (parse-integer ap)) (parse-integer ap))))
      (make-instance 
       'decimal-representation 
       :intpart int
       :plength plength
       :repeating repeating
       :exponent 0
       :period per ;(parse-integer ap)
       :name (format nil "~A~D~A~D" (if (and (zerop int) neg) "-" "") int 
		     *decimal-char* ap)))))

(defmethod make-decimal-representation ((i integer))
  (make-decimal-representation (format nil "~D" i)))

(defun mdr (s) (make-decimal-representation s))

(defun show (d)
      `((name ,(d-s d))
	(period ,(d-p d))
	(length ,(d-l d))
	(repeating ,(d-r d))
	(intpart ,(d-ip d))
	(exponent ,(d-ex d))))
	      
(defgeneric d+ (d1 d2)
  (:documentation "Do exact sums with decimals."))

(defgeneric d* (d1 d2)
  (:documentation "Do exact products with decimals."))

(defgeneric d/ (d1 d2)
  (:documentation "Do exact quotients with decimals."))

(defmethod d+ ((d1 decimal-representation) (d2 decimal-representation))
  (let ((r1 (dtorat d1))
	(r2 (dtorat d2)))
    (mdr (+ r1 r2))))

(defmethod d+ ((s1 string) (s2 string))
  (let ((d1 (mdr s1))
	(d2 (mdr s2)))
    (d+ d1 d2)))

(defgeneric d- (d1 d2)
  (:documentation "Do exact sums with decimals."))

(defmethod d- ((d1 decimal-representation) (d2 decimal-representation))
  (let ((r1 (dtorat d1))
        (r2 (dtorat d2)))
    (mdr (- r1 r2))))

(defmethod d- ((s1 string) (s2 string))
  (let ((d1 (mdr s1))
	(d2 (mdr s2)))
    (d- d1 d2)))

(defgeneric d> (d1 d2)
  (:documentation "Return TRUE if d1 > d2."))

(defmethod d> ((d1 decimal-representation) (d2 decimal-representation))
  (let ((r1 (dtorat d1))
	(r2 (dtorat d2)))
    (> r1 r2)))

(defmethod d> ((s1 string) (s2 string))
  (let ((d1 (mdr s1))
	(d2 (mdr s2)))
    (d> d1 d2)))

(defgeneric d< (d1 d2)
  (:documentation "Return TRUE if d1 < d2."))

(defmethod d< ((d1 decimal-representation) (d2 decimal-representation))
  (let ((r1 (dtorat d1))
	(r2 (dtorat d2)))
    (< r1 r2)))

(defmethod d< ((s1 string) (s2 string))
  (let ((d1 (mdr s1))
	(d2 (mdr s2)))
    (d< d1 d2)))

(defmethod d* ((s1 string) (s2 string))
  (let* ((d1 (mdr s1))
	 (d2 (mdr s2))
	 (r1 (dtorat d1))
	 (r2 (dtorat d2))
	 (prod (* r1 r2)))
    (mdr prod)))

(defmethod d* ((d1 decimal-representation) (d2 decimal-representation))
  (let* ((r1 (dtorat d1)) (r2 (dtorat d2)) (prod (* r1 r2)))
    (mdr prod)))

(defmethod d/ ((s1 string) (s2 string))
  (let* ((d1 (mdr s1))
	 (d2 (mdr s2))
	 (r1 (dtorat d1))
	 (r2 (dtorat d2))
	 (quotient (/ r1 r2)))
    (mdr quotient)))

(defmethod d/ ((d1 decimal-representation) (d2 decimal-representation))
  (let* ((r1 (dtorat d1)) (r2 (dtorat d2)) (quotient (/ r1 r2)))
    (mdr quotient)))
