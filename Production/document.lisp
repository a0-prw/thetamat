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

(defclass autotutor-document () ()
  (:documentation "The base class of Autotutor documents."))

(defclass teaching-document (autotutor-document) ()
   (:documentation "A document intended for a class"))

(defclass pupil-document (autotutor-document) 
  ((pupil :initarg :for :reader pd-for))
  (:documentation "A document intended for a pupil."))

(defclass problem-document (pupil-document) ()
  (:documentation "A document containing problems to be solved by a
  pupil."))

(defclass teaching-exercise (teaching-document)
  ((td-tag :initarg :td-tag :reader td-tag)
   (for-teacher :initarg :for :reader for-teacher)
   (how-many :initarg :how-many :reader how-many))
   (:documentation "Throw-away exercises for class work."))

(defclass exercise (problem-document)
  ((doc-title :initarg :doc-title :initform "Exercises" :reader doc-title))
  (:documentation "Exercises."))

(defclass placement (problem-document)
  ((doc-title :initform "Placement exercises")))

(defclass mixed-exercise (exercise)
  ((doc-title :initarg :doc-title :initform "Blandede øvelser." :reader doc-title))
  (:documentation "Mixed exercises"))

(defclass test (problem-document)
  ((doc-title :initarg :doc-title :initform "Prøve" :reader doc-title))
  (:documentation "An autotutor test."))

(defun make-document (name &key type)
  (make-instance type :for name))

(defun make-teaching-document (name de-tag how-many)
  (make-instance 'teaching-exercise :for name :how-many how-many :td-tag de-tag))

;; (defgeneric autotutor-document (doctype forname) 
;;   (:documentation "Produces output specialized on doctype."))

;; (defmethod autotutor-document ((d exercise) (p string))
;;   (make-document 'exercise-document p))

(defmacro operation-handler-generator (name)
  "(OPERATION-HANDLER-GENERATOR FOO) defines a macro FOO-exercise
which handles exercises of type FOO, so SUM-EXERCISE and
PRODUCT-EXERCISE handle SUMS and PRODUCTS. An exercise handler must
return a list consisting of lists of (FOO-OPERATION, the tex
representation of FOO-OPERATION, and the tex representation of the
answer to FOO-OPERATION)"
  (let* ((upname (string-upcase name))
	 (operation-handler-symbol-name 
	  (concatenate 'string (string-upcase (symbol-name name)) "-EXERCISE"))
	 (operation-handler-symbol (intern operation-handler-symbol-name))
	 (maker-symbol (intern (concatenate 'string "MAKE-" upname))))
    `(defmacro ,operation-handler-symbol (howmany type &optional (more nil))
       (let ((n (gensym)) (sum (gensym)) 
	     (sums (gensym)) (ques (gensym)) (ans (gensym)))
	 `(let ((,sums ()) (,ques ()) (,ans ()))
	    (dotimes (,n ,howmany)
	      (let ((,sum (,',maker-symbol ,type ,more)))
		(setf ,sums (push ,sum ,sums)
		      ,ques (push (texq ,sum) ,ques)
		      ,ans (push (texa ,sum) ,ans))))
	    (list ,sums ,ques ,ans))))))

(operation-handler-generator sum)
(operation-handler-generator product)
(operation-handler-generator nline)

(defun get-first-name (name)
  (first-name name))

(defun title (doc)
  (let* ((pupil (pd-for doc))
	 (fn (first-name pupil))
	 (sn (surname pupil)))
    (format nil "\\title{~A til ~A\\footnote{~A}}~%\\maketitle~%" 
	    (doc-title doc) fn (concatenate 'string fn " " sn))))

;;TODO must it be generic? maybe good anyway
(defgeneric collect-problems (doctype)
  (:documentation "Returns a list of lists of exercise objects (eg
SUMS), TeX typeset problems, and the TeX typeset answers for the
problems."))

(defgeneric n-probs-for-doctype (doc didactic-element))

(defmethod n-probs-for-doctype ((d test) de)
  (n-test de))
(defmethod n-probs-for-doctype ((d exercise) de)
  (n-ex de))

(defmethod n-probs-for-doctype ((d placement) de)
  ;;FIXME discriminate fex, more tables, fewer 3 term additions
  5)

(defvar *mult-regex* 
  (cl-ppcre:create-scanner "\\*"))

(defvar *decsep-regex*
  (cl-ppcre:create-scanner "\\."))

(defvar *div-regex*
  (cl-ppcre:create-scanner "\\/"))

(defmethod collect-problems ((ptype problem-document))
  (let* ((pname (pd-for ptype))
         (tname (pupil-teacher pname))
         (del (pupil-didactic-elements pname))
	 (ret ()))
    (dolist (d del ret)
      (let* ((howmany (n-probs-for-doctype ptype d))
             (objs ())
             (qs ())
             (action (if (didactic-element-da d) (action d)
                         (funcall (action d))))
             (as ()))
	(dotimes (n howmany (setf ret (push (list (tag d) objs qs as) ret)))
          (let ((prob (funcall action)))
            (destructuring-bind (q a)
                (list-strings-q&a prob)
              (setf objs (push prob objs) 
		  qs (push (make-latex-q q (operation-operator prob) tname) qs)
		  as (push (make-latex-a a) as)))))))))

(defmethod collect-problems ((te teaching-exercise))
  (let* ((d (get-didactic-element (td-tag te)))
         (tname (for-teacher te))
         (hm (how-many te))
         (action (if (didactic-element-da d) (action d)
                     (funcall (action d))))
         (objs ())
         (qs ())
         (as ()))
    (dotimes (n hm  (list (tag d) objs qs as))
      (let ((prob (funcall action)))
        (destructuring-bind (q a)
            (list-strings-q&a prob)
          (setf objs (push prob objs) 
                qs (push (make-latex-q q (operation-operator prob) tname) qs)
                as (push (make-latex-a a) as)))))))

(defun collect-didactic-element-problems (tag pname)
  (let* ((de (get-didactic-element tag))
         (howmany (n-ex de))
         (action (if (didactic-element-da de) (action de)
                         (funcall (action de))))
         (qs ())
         (as ()))
    (dotimes (n howmany (list tag howmany 
                              (mapcar (lambda (a b) `(,a ,b nil nil)) qs as)))
      (let ((prob (funcall action)))
        (destructuring-bind (q a)
            (list-strings-q&a prob)
          (setf 
           qs (push (make-latex-q q (operation-operator prob) pname) qs)
           as (push (make-latex-a a) as)))))))

;; (defun collect-didactic-element-problems (tag pname)
;;   (let* ((de (get-didactic-element tag))
;;          (howmany (n-ex de))
;;          (qs ())
;;          (as ()))
;;     (dotimes (n howmany (list tag howmany 
;;                               (mapcar (lambda (a b) `(,a ,b nil nil)) qs as)))
;;       (let ((prob (funcall (action de))))
;;         (destructuring-bind (q a)
;;             (list-strings-q&a prob)
;;           (setf 
;;            qs (push (make-latex-q q (operation-operator prob) pname) qs)
;;            as (push (make-latex-a a) as)))))))

(defun make-latex-a (a)
  (concatenate 'string "$" a "$"))

(defgeneric make-latex-q (sq prob pname))

(defmethod make-latex-q ((q string) (op times) (tname string))
  (let ((replacement (cadr (assoc 'times (teacher-latex-syntax tname)))))
    (concatenate 
     'string "$" (cl-ppcre:regex-replace *mult-regex* q replacement) "$")))

(defmethod make-latex-q ((q string) (op divided-by) (tname string))
  (let ((replacement (cadr (assoc 'divided-by (teacher-latex-syntax tname)))))
    (concatenate 
     'string "$" (cl-ppcre:regex-replace *div-regex* q replacement) "$")))

(defmethod make-latex-q ((q string) (op operator) (tname string))
  (declare (ignore op tname))
  (concatenate 'string "$" q "$"))

;; (defmethod collect-problems ((ptype problem-document))
;;   (let* ((del (pupil-didactic-elements (pd-for ptype)))
;; 	 (ret ()))
;;     (dolist (d del ret)
;;       (let* ((howmany (n-probs-for-doctype ptype d))
;; 	    (objs ())
;; 	    (qs ())
;; 	    (as ()))
;; 	(dotimes (n howmany (setf ret (push (list (tag d) objs qs as) ret)))
;; 	  (let ((prob (funcall (action d))))
;; 	    (setf objs (push prob objs) 
;; 		  qs (push (texq prob) qs)
;; 		  as (push (texa prob) as))))))))



