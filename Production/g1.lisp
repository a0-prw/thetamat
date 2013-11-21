(in-package :autotutor)

;; naming convention: 
;; ----------------------
;; natural number -- nat
;; integer --------- int
;; positive int ---- posint
;; negative int ---- negint
;; rational -------- rat
;; positive rat ---- posrat
;; negative rat ---- negrat
;; real ------------ real
;; positive real --- posreal
;; negative real --- negreal
;; algebraic num --- alg
;; positive rat  --- porat
;; negative rat  --- negrat
;; decimal --------- dec
;; positive dec ---- podec
;; negative dec ---- negdec

;; (defclass eint-nat (operands) ())
;; (defclass nat (operands) ())
;; (defclass int-nat (operands) ())
;; (defclass int (operands) ())

;; Special cases

(defclass digit-int-nat (int-nat) ())
(defun digit-int-nat () (make-instance 'digit-int-nat))
(defmethod make-operands ((opclass digit-int-nat) &optional (more nil))
  (let ((pair (sub-digits)));;(sub-numbers1 *digit*)))
    (if (not more)
	(progn (setf (operands-list opclass) pair)
	       opclass)
	(progn (setf (operands-list opclass) (positive-sum-digit-list))
	       opclass))))

(defclass table (operands) ())
(defun table () (make-instance 'table))
(defmethod make-operands ((opclass table) &optional more)
  (declare (ignore more))
  (let ((pair (list (digtribution) (digtribution))))
    (setf (operands-list opclass) pair)
    opclass))

(defclass digit-by-2nat (operands) ())
(defun digit-by-2nat () (make-instance 'digit-by-2nat))
(defmethod make-operands ((opclass digit-by-2nat) &optional more)
  (declare (ignore more))
  (let ((pair (list (random 10) (random 100))))
    (setf (operands-list opclass) pair)
    opclass))

;; for decimal-operands
(defmethod initialize-instance :after ((s sum) &key)
  (let ((ops (operands s))
        (sign (operation-operator s)))
    (cond ((and (typep ops 'decimal-operands)
                (or (typep sign 'plus)
                    (typep sign 'minus)))
           (setf (instruction s) #'d+))
          ((and (typep ops 'decimal-operands)
                (or (typep sign 'times)
                    (typep sign 'divided-by)))
           (setf (instruction s) #'d*))
          (t nil))))

;; End special cases

;; Important note
;;
;; The following macros will only work if you follow the convention
;; that the number types be named somesize-something, and that
;; *somesize* is also defined as a parameter determining the upper
;; limit of the number type.

;; The FUNCTION takes an obligatory argument SIZE which determines the
;; upper limit of the number type.  If MORE is T (default is NIL) then
;; the FUNCTION must take an optional argument MORE, which will mean
;; that it returns a list containing *NOPERANDS* operands, otherwise,
;; the function must return a pair of operands.

(defmacro define-number-type-definer (definer-name operands-type &key function (more nil))
  (if (not more)
      `(defmacro ,definer-name (name)
	 (let* ((namestring (symbol-name name))
                (-pos (position #\- namestring :start 0 :test #'char=))
                (subtype (intern (string-upcase (subseq namestring (1+ -pos)))))
                (parameter (intern (string-upcase (concatenate 'string "*" (subseq namestring 0 -pos) "*")))))
	   `(progn (defclass ,subtype (,',operands-type) ())
                   (defclass ,name (,subtype) () )
		   (defun ,name () (make-instance ',name))
		   (defmethod make-operands ((opclass ,name) &optional (more nil))
		     (declare (ignore more))
		     (let ((pair (,',function ,parameter)))
		       (setf (operands-list opclass) pair)
		       opclass)))))
      `(defmacro ,definer-name (name)
	 (let* ((namestring (symbol-name name))
                (-pos (position #\- namestring :start 0 :test #'char=))
                (subtype (intern (string-upcase (subseq namestring (1+ -pos)))))
		(parameter (intern (string-upcase (concatenate 'string "*" (subseq namestring 0 -pos) "*")))))
	   `(progn (defclass ,subtype (,',operands-type) ())
                   (defclass ,name (,subtype) ())
                   (defun ,name () (make-instance ',name))
                   (defmethod make-operands ((opclass ,name) &optional (more nil))
                     (let ((pair (,',function ,parameter)))
                       (if (not more)
                           (progn (setf (operands-list opclass) pair)
                                  opclass)
                           (progn (setf (operands-list opclass) (,',function ,parameter t))
                                  opclass)))))))))

(define-number-type-definer define-nat-type operands :function nat-size-list :more t)
(define-number-type-definer define-nat-non-zero-type operands :function non-zero-nat-size-list :more t)
(define-number-type-definer define-int-nat-type operands :function positive-sum-size-list :more t)
(define-number-type-definer define-int-type operands :function int-size-list :more t)
(define-number-type-definer define-int-div-type operands :function div-int-pair :more nil)
(define-number-type-definer define-podec-type decimal-operands :function positive-sum-decimal-list :more t)
(define-number-type-definer define-negdec-type decimal-operands :function posorneg-sum-decimal-list :more t)
(define-number-type-definer define-nline-type operands :function nline-placedig :more nil)
(define-number-type-definer define-eint-type operands :function digits-lt :more t)
(define-number-type-definer define-lmsub-type operands :function x-digits-lt :more nil)

(define-number-type-definer define-proper-fraction-type operands :function prop-frac-size-pair-signed :more t)

(define-number-type-definer define-nat-no-carry-type operands :function random-plus-no-carry :more nil) ;;here

(define-number-type-definer define-nat-with-carry-type operands :function random-plus-with-carry :more nil) ;;here

(define-number-type-definer define-nat-minus-with-borrow-type operands :function minus-with-borrow :more nil)
(define-number-type-definer define-nat-minus-with-repeated-borrow-type operands :function z-rep-borrow :more nil)

(define-nat-minus-with-repeated-borrow-type medium-z-nat)

(define-nat-minus-with-borrow-type small-b-nat)
(define-nat-minus-with-borrow-type medium-b-nat)

;; Addition without a carry of any power of ten
(define-nat-no-carry-type tiny-nc-nat)
(define-nat-no-carry-type small-nc-nat)

;; Addition with coefficients of all powers except the highest having a
;; carry

(define-nat-with-carry-type tiny-c-nat)
(define-nat-with-carry-type small-c-nat)


;; Subtraction with digits in subtrahend strictly less than digs in minuend.

(define-eint-type tiny-eint-nat)
(define-eint-type small-eint-nat)
(define-eint-type medium-eint-nat)

;; Same but both between 10 and 20

(define-lmsub-type digit-tsub-nat)

;; Natural number operands of various sizes
(define-nat-non-zero-type digit-nz-nat)
(define-nat-type digit-nat)
(define-nat-type tiny-nat)
(define-nat-type small-nat)
(define-nat-type medium-nat)
(define-nat-type big-nat)
(define-nat-type verybig-nat)
(define-nat-type huge-nat)

;; Integer operands of various sizes that sum to a natural number or zero
(define-int-nat-type tiny-int-nat)
(define-int-nat-type small-int-nat)
(define-int-nat-type medium-int-nat)
(define-int-nat-type big-int-nat)
(define-int-nat-type verybig-int-nat)
(define-int-nat-type huge-int-nat)    


;; Integer operands of various sizes

(define-int-type digit-int)
(define-int-type tiny-int)
(define-int-type small-int)
(define-int-type medium-int)
(define-int-type big-int)
(define-int-type verybig-int)
(define-int-type huge-int)

;; Integer division (p, d) | int p/int d=int x
(define-int-div-type tiny-div-int)
(define-int-div-type medium-div-int)

;; Decimal representation of positive rationals

(define-podec-type digit-podec)
(define-podec-type tiny-podec)
(define-podec-type small-podec)

;; Decimal representation of signed rationals

(define-negdec-type digit-negdec)
(define-negdec-type tiny-negdec)
(define-negdec-type small-negdec)

;; Determining the number a DIGIT away from some number less than SIZE

(define-nline-type digit-int-nline)
(define-nline-type tiny-int-nline)

;; Trial proper-fractions FIXME
(define-proper-fraction-type digit-prop-frac-signed)

;;;; Adding a new type of exercise HOWTO

;; Decide what the numeric compponents of the type will be.

;; Define the function which generates the numeric components in util.lisp

;; Define the number-type-definer here in g1.lisp , (examine the macro
;; for specification ie, parameters, etc)

;; Define methods for displaying the type in questions and calculating
;; an answer in latex.lisp
