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

;; Hard-copy
(defun get-texq (slist)
  (cadr slist))

(defun get-texa (slist)
  (caddr slist))

(defgeneric layout-document (document)
  (:documentation "Lays out documents of differing types."))

(defmethod layout-document ((doc exercise)) ;fixme to use de 
  (let ((probs (mapcar #'cdr (collect-problems doc)))) ;(collect-problems doc))) 
    (concatenate-document-elements
     `(,(begin-document)
	,(include-packages)
	,(latex-environment
	  "document"
	  (do ((collect "")
	       (n 1 (1+ n))
	       (rp (cdr probs) (cdr rp))
	       (ex (car probs) (car rp)))
	      ((null ex) collect)
	    (setf 
	     collect 
	     (concatenate 'string collect
			  (layout-exercise doc n (caar ex) ex))))
	  :title (title doc))))))

(defmethod layout-document ((doc test))
  (let ((probs (mapcar #'cdr (collect-problems doc)))) ; (collect-problems doc)))
    (multiple-value-bind (td tf)
	(do ((facit "")
	     (tst "")
	     (n 1 (1+ n))
	     (rp (cdr probs) (cdr rp))
	     (ex (car probs) (car rp)))
	    ((null ex) (values tst facit))
	  (multiple-value-bind (p f) 
	      (layout-exercise doc n (caar ex) ex)
	    (setf tst (concatenate 'string tst p)
		  facit (concatenate 'string facit f))))
      (values (concatenate-document-elements
	       `(,(begin-document)
		  ,(include-packages)
		  ,(latex-environment
		    "document"
		    td
		    :title (title doc))))
	      (concatenate-document-elements-facit
	       `(,(begin-document)
		  ,(include-packages)
		  ,(latex-environment
		    "document"
		    tf
		    :title (title doc))))))))
	    
(defgeneric layout-exercise (doctype number optype exs)
  (:documentation "Layout the exercises in list EXS according to an
  appropriate specialisation on DOCTYPE (eg,exercise, test, etc) with
  section number NUMBER for exercise of type TYPE."))

(defmethod layout-exercise ((d exercise) (n integer) (type operation) (exs list))
  (let* ((dti (dti type))
	 (qs (display-in-table (get-texq exs) dti))
	 (as (display-in-table (get-texa exs) dti)))
    (latex-text (format nil "\\normalsize~%")
		(format nil "\\section{Øvelse ~D}~%" n)
		(latex-environment
		 "tabular"
		 qs
		 :options (table-justification dti))
		(format nil "\\tiny~%")
		(format nil "\\subsection{Facit til øvelse ~D}~%" n)
		(latex-environment
		 "tabular"
		 as
		 :options (table-justification dti)))))

(defmethod layout-exercise ((d test) (n integer) (type operation) (exs list))
  (let* ((dti (dti type))
	 (qs (display-in-table (get-texq exs) dti))
	 (as (display-in-table (get-texa exs) dti))
	 (exp (latex-text (format nil "\\normalsize~%")
			  (format nil "\\section{Opgave ~D}~%" n)
			  (latex-environment
			   "tabular"
			   qs
			   :options (table-justification dti))))
	 (exf (latex-text (format nil "\\normalsize~%")
			  (format nil "\\section{Facit til opgave ~D}~%" n)
			  (latex-environment
			   "tabular"
			   as
			   :options (table-justification dti)))))
    (values exp exf)))

;; Net

;;unnecessary
(defun make-list-of-numbered-problems (howmany de)
  (let ((retlist ()))
    (dotimes (n howmany retlist)
      (setf retlist (push (cons (1+ n) (funcall (action de))) retlist)))))

(defun get-nth-numprob-pair (n alist)
  (assoc n alist))
