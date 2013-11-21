(in-package :autotutor)

(defclass operands ()
  ((list :initarg :list :accessor operands-list))
  (:documentation "Operands suitable for use in a 
mathematical operation."))

(defclass decimal-operands (operands)
  ()
  (:documentation ""))

(defclass operation ()
  ((instruction :initarg :instruction :accessor instruction)
   (operator :initarg :operator :accessor operation-operator)
   (operands :initarg :operands :accessor operands))
   ;;(string :initarg :string :accessor operation-string)
   ;;(question :initarg :texq :accessor question)
   ;;(answer :initarg :texa :accessor answer))
  (:documentation "An operation consists of an INSTRUCTION (eg '+) and 
at least 2 operands of some specific type (eg 'small-natural-number)."))

(defclass instruction ()
  ((symbol :initarg :symbol :reader instruction-symbol))
  (:documentation "Class of instructions, f.ex, sum(+) , product(*), and
  also application specific \"instructions\" such as nline(nl)."))

(defclass add (instruction)
  ((symbol :initform '+)))

(defclass mult (instruction)
  ((symbol :initform '*)))

(defclass place-on-line (instruction)
  ((symbol :initform 'nl)))

(defclass sum (operation)
  ((instruction :initform #'+))
  (:documentation "The operation of addition."))

(defclass product (operation)
  ((instruction :initform #'*))
  (:documentation "The operation of multiplication."))

(defclass nline (operation)
  ((instruction :initform #'identity))
  (:documentation "Placement on a number line."))

(defclass operator () ())

(defclass plus (operator) ())
(defun op-plus ()
  (make-instance 'plus))
(defclass minus (operator) ())
(defun op-minus ()
  (make-instance 'minus))
(defclass times (operator) ())
(defun op-times ()
  (make-instance 'times))
(defclass divided-by (operator) ())
(defun op-divided-by ()
  (make-instance 'divided-by))
(defclass nl-before (operator) ())
(defclass nl-after (operator) ())
(defun op-placed-before ()
  (make-instance 'nl-before))
(defun op-placed-after ()
  (make-instance 'nl-after))

(defun before-or-after ()
  (if (zerop (random 2))
      (op-placed-before)
      (op-placed-after)))
    

(defgeneric make-operands (opclass &optional n)
  (:documentation "Returns operands appropriate to the class of OPCLASS."))
;;the methods for make-operands are defined in g1.lisp in macros

(defgeneric get-operands (op)
  (:documentation "Return the list of operands for the operation OP"))

(defgeneric make-string-reps (operation operator))

(defmacro define-operation-maker (name class)
  `(progn 
     (defun ,name (type operator &optional (more nil) (direct t))
       (let* ((operands (if direct (make-operands type more)
                            type))) ;;if not direct, the operands list
                                    ;;is set some other way
         (make-instance ',class :operands operands :operator operator)))))

;;F.eks.  (make-sum (digit-nat) (op-plus))

          
     ;; (defmethod initialize-instance :after ((s ,class) &key)
     ;;    (let ((q&a (make-string-reps s (operation-operator s))))
     ;;      (setf (question  s) (car q&a)
     ;;            (answer s) (cadr q&a))))))

(define-operation-maker make-sum sum)
(define-operation-maker make-product product)
(define-operation-maker make-nline nline)

(defmethod get-operands ((op operation))
  (operands-list (operands op)))

(defgeneric get-instruction (op))

(defmethod get-instruction ((op operation))
  (instruction op))

(defun rnd-between (min max)
  (unless (and (>= min 0)
	       (> max min))
    (error "RND-BETWEEN requires MAX=~A > MIN=~A >= 0" max min))
  (+ min (random (- (1+ max) min))))

;;Experimental

;; (defclass determine-meaning (instruction)
;;   ((symbol :initform 'dm)))

;; (defclass digit-meaning-decision (operation)
;;   ((instruction :initform #'identity))
;;   (:documentation "Determination of meaning of a digit in a number"))

;; (define-operation-maker make-digit-meaning-question digit-meaning-decision)

;; (defclass dmean (operator) ())

;; (defun op-dmean ()
;;   (make-instance 'dmean))

;; end experimental
