(in-package :autotutor)

;;(eval-when (:compile-toplevel :load-toplevel :execute)
  
(defparameter *defined-latex-commands* nil)
  
(defun update-latex-command-list (name)
  (when (not (find name *defined-latex-commands* :test #'equal))
    (push name *defined-latex-commands*))) ;;)

;;use this in eval_string
;;(defparameter *latex-times* " \\cdot ")

(defmacro make-latex-command (name latexname option-lists)
  (let ((stream (gensym))
	(thing (gensym)))
    (if (null (cddr option-lists))
	`(progn (defun ,name ()
		  (let ((,stream (make-string-output-stream)))
		    (format ,stream "~A~A~A~A~%"
			    *latex-backslash*
			    ,latexname
			    (square-delimiter-comma-list (car ',option-lists))
			    (curly-delimiter-comma-list (cadr ',option-lists)))
		    (get-output-stream-string ,stream)))
		(update-latex-command-list ',name))
	`(progn (defun ,name ()
		  (let ((,stream (make-string-output-stream)))
		    (dolist (,thing ',option-lists)
		      (format ,stream "~A~A~A~A~%"
			      *latex-backslash*
			      ,latexname
			      (square-delimiter-comma-list (car ,thing))
			      (curly-delimiter-comma-list (cadr ,thing))))
		    (get-output-stream-string ,stream)))
		(update-latex-command-list ',name)))))


(defun latex-command (com)
  (concatenate 'string *latex-backslash* com))

(defparameter *latex-directory* "Latex/")

(defun latex-path (name)
  (merge-pathnames (merge-pathnames name  *latex-directory*)))

(defparameter *close-latex-document* 
  (concatenate 'string *latex-backslash* "end{document}"))
(defparameter *display-table-items-per-line* 3)
(defparameter *table-numbering-spacing* (latex-command "quad"))
(defparameter *table-numbering-style* (latex-command "textbf{(~D)}"))
(defparameter *table-item-justification* "l")
(defparameter *table-column-seperation* 10)
(defparameter *array-stretch* 3/2)
(defparameter *new-commands* nil)
(defparameter *current-document* nil)
(defparameter *current-document-facit* nil)

(defparameter *approx-chars* 85) ;;could be smarter
(defparameter *quad-spacer* 3)
(defparameter *ex-numbering* 3)
(defparameter *padding* 3)

(defun new-commands ()
  (if *new-commands* *new-commands* ""))

(defun latex-begin (what &optional (options ""))
  (format nil  "~%~Abegin{~A}~A~%" *latex-backslash* what options))
(defun latex-end (what)
  (format nil "~%~Aend{~A}~%~%" *latex-backslash* what))

(defun latex-environment (what content &key (options  "") (title ""))
"Expects the environment type in WHAT as a string and CONTENT as a
list of strings."
  (concatenate 'string
	       (latex-begin what options)
	       title
	       content
	       (latex-end what)))

(defun dti (operation)
  (floor *approx-chars* 
	 (+ (length (car (list-strings-q&a operation)))  ;;approx ok.
	    *quad-spacer* 
	    *ex-numbering* 
	    *padding*)))

(defun latex-text (&rest strings)
  (reduce #'(lambda (s1 s2) (concatenate 'string s1 s2)) strings))

(defmacro configure-document (newcommands)
  (let ((stream (gensym)))
    `(let ((,stream (make-string-output-stream)))
       (format ,stream "~{~A~}" (list ,@newcommands))
       (setf *new-commands* (get-output-stream-string ,stream)))))

(defun make-table-format-string ()
  (format nil "~A~A~A" *table-numbering-style*
	  *table-numbering-spacing* " ~A"))

(defun display-in-table (list-of-strings &optional (dti *display-table-items-per-line*))
  (let ((length (length list-of-strings))
	(output (make-string-output-stream)))
    (multiple-value-bind (lines blank)
	(ceiling  length dti)
      (declare (ignore lines))
      (do ((extra (abs blank))
	   (count 1 (1+ count)))
	  ((> count length) 
	   (progn 
	     (when (not (zerop extra)) 
	       (format output (make-string (1- extra) :initial-element #\&)))
	     (get-output-stream-string output)))
	(multiple-value-bind (div rem)
	    (floor count dti)
	  (declare (ignore div))
	  (if (zerop rem) 
	      (format output (concatenate 'string (make-table-format-string)
					  "\\\\~%")
		      count (nth (1- count) list-of-strings))
	      (format output (concatenate 'string
					  (make-table-format-string)
					  " & ")
		      count (nth (1- count) list-of-strings))))))))

(defun table-justification (dti &optional (j *table-item-justification*))
  (curly-delimiter-space-list (make-list dti :initial-element j)))


;; TODO defgeneric these
(defun concatenate-document-elements (elements)
  "Joins a list of strings together snd sets *CURRENT-DOCUMENT* to the
resulting string."
  (setf *current-document* 
	(reduce #'(lambda (s1 s2) (concatenate 'string s1 s2)) elements)))

(defun concatenate-document-elements-facit (elements)
  "Joins a list of strings together snd sets *CURRENT-DOCUMENT-FACIT* to the
resulting string."
  (setf *current-document-facit* 
	(reduce #'(lambda (s1 s2) (concatenate 'string s1 s2)) elements)))

(defun output-latex-file (filename content)
  (with-open-file (stream filename 
			  :direction :output
			  :if-exists :supersede)
    (format stream content)))


(defun make-infix (operands stream sign)
  (if (null operands) (get-output-stream-string stream)
      (make-infix (cdr operands)
                  (let* ((curr (car operands))
                         (fmtstr (if (stringp curr) "~A" "~S")))
                    (format stream fmtstr curr)
                    (format stream "~A" (if (cdr operands)
                                            sign
                                            ""))
                    stream)
                  sign)))

(defun make-infix-dec (operands stream sign &optional (first-call t))
  (if (null operands) (get-output-stream-string stream)
      (make-infix-dec (cdr operands)
                  (let* ((curr (car operands))
                         (neg (when (not first-call)
                                (char= (char curr 0) #\-)))
                         (fmt-string (if neg "(~A)" "~A")))
                    (format stream fmt-string (car operands))
                    (format stream "~A" (if (cdr operands)
                                            sign
                                            ""))
                    stream)
                  sign nil)))

(defun make-infix-rat (operands stream sign)
  (if (null operands) (get-output-stream-string stream)
      (make-infix-rat (cdr operands)
                  (progn (format stream "\\frac{~S}{~S}" 
                                 (caar operands)
                                 (cadar operands))
                         (format stream "~A" (if (cdr operands)
                                                 sign
                                                 ""))
                         stream)
                  sign)))


(defgeneric make-question (operands operator))
(defgeneric make-answer (operands operator instruction))
(defgeneric list-strings-q&a (operation)
  (:documentation "Takes an object of type OPERATION (f.ex. SUM, made by 
MAKE-SUM in content/didactic-elements.lisp)"))

(defmethod list-strings-q&a ((operation operation))
  (let* ((operands (operands operation))
         (operator (operation-operator operation))
         (instruction (instruction operation))
         (question (make-question operands operator))
         (answer (make-answer operands operator instruction)))
    (list question answer)))

;; (defmethod list-strings-q&a ((operation nline))
;;   (let* ((operands (operands operation))
;;          (operator (operation-operator operation))
;;          (question (make-question operands operator))
;;          (answer (make-answer operands operator)))
;;     (list question answer)))

(defmethod make-question ((ops div-int) (sign divided-by))
  (destructuring-bind (dividend divisor)
      (operands-list ops)
    (let ((neg (< divisor 0))
          (den (denominator divisor)))
      (make-infix (list dividend (if neg (- den) den))
                  (make-string-output-stream) " \\div "))))

(defmethod make-question ((ops operands) (sign plus))
  (declare (ignore sign))
  (make-infix (operands-list ops) (make-string-output-stream) " + "))

(defmethod make-answer ((ops operands) (opor operator) (f function))
  (declare (ignore opor))
  (format nil "~S" (apply f (operands-list ops))))

(defmethod make-question ((ops small-b-nat) (sign minus))
  (declare (ignore sign))
  (make-infix (mapcar #'abs (operands-list ops)) (make-string-output-stream) " - "))

(defmethod make-question ((ops medium-z-nat) (sign minus))
  (declare (ignore sign))
  (make-infix (mapcar #'abs (operands-list ops)) (make-string-output-stream) " - "))

(defmethod make-question ((ops eint-nat) (sign minus))
  (declare (ignore sign))
  (make-infix (mapcar #'abs (operands-list ops)) (make-string-output-stream) " - "))

(defmethod make-question ((ops digit-tsub-nat) (sign minus))
  (declare (ignore sign))
  (make-infix (mapcar #'abs (operands-list ops)) (make-string-output-stream) " - "))

(defmethod make-question ((ops int-nat) (sign minus))
  (declare (ignore sign))
  (make-infix (mapcar #'abs (operands-list ops)) (make-string-output-stream) " - "))

(defmethod make-question ((ops operands) (sign times))
  (declare (ignore sign))
  (make-infix (operands-list ops) (make-string-output-stream) " * "))

(defmethod make-question ((ops int-nline) (sign nl-after))
  (declare (ignore sign))
  (let ((lis (operands-list ops)))
    (format nil "~D \\,\\text{place~:P after}\\, ~D" (car lis) (cadr lis))))
 ;; (make-infix (operands-list ops) (make-string-output-stream) " places after "))

(defmethod make-answer ((ops int-nline) (sign nl-after) (f function))
  (declare (ignore sign f))
  (format nil "~S" (apply #'+ (operands-list ops))))

(defmethod make-question ((ops int-nline) (sign nl-before))
  (declare (ignore sign))
  (let ((lis (operands-list ops)))
    (format nil "~D \\,\\text{place~:P before}\\, ~D" (car lis) (cadr lis))))

 ;; (make-infix (operands-list ops) (make-string-output-stream) " places before "))
(defmethod make-answer ((ops int-nline) (sign nl-before) (f function))
  (declare (ignore sign f))
  (let ((lis (operands-list ops)))
    (format nil "~S" (+ (* -1 (car lis)) (cadr lis)))))

(defmethod make-answer ((pfs prop-frac-signed) (opor operator) (f function))
  (declare (ignore opor))
  (format nil "~S"
          (apply f (mapcar 
                      (lambda (p) (/ (car p) (cadr p)))
                      (operands-list pfs)))))

(defmethod make-question ((pfs prop-frac-signed) (sign plus))
  (declare (ignore sign))
  (make-infix-rat (operands-list pfs) (make-string-output-stream) " + "))

(defmethod make-question ((ds negdec) (sign plus))
  (declare (ignore sign))
  (make-infix (operands-list ds) (make-string-output-stream) " + "))

(defmethod make-question ((ds negdec) (sign minus))
  (declare (ignore sign))
  (make-infix-dec (operands-list ds) (make-string-output-stream) " - "))

(defmethod make-answer ((ds decimal-operands) (sign plus) (f function))
  (declare (ignore sign f))
  (d-s (apply #'d+ (operands-list ds))))

(defmethod make-answer ((ds decimal-operands) (sign minus) (f function))
  (declare (ignore sign f))
  (d-s (apply #'d- (operands-list ds))))

(defmethod make-answer ((ds decimal-operands) (sign times) (f function))
  (declare (ignore sign f))
  (d-s (apply #'d* (operands-list ds))))

(defmethod make-answer ((ds decimal-operands) (sign divided-by) (f function))
  (declare (ignore sign f))
  (d-s (apply #'d/ (operands-list ds))))
  
 
