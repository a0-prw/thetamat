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

(defclass module-dependency-graph ()
  ((modules :initarg :modules :accessor graph-modules :initform nil :type list)
   (nm :initform 0 :accessor graph-nm :type integer)
   (dependencies :initarg :dependencies :accessor graph-dependencies 
                 :initform nil :type list)
   (nd :initform 0 :accessor graph-nd :type integer)
   (scc :initform nil :accessor graph-scc :type list)))

(defvar *current-modules* (make-hash-table))
(defvar *module-id* 0)
(defvar *modules-by-id* (make-array 100 :adjustable t));;FIXME size
(defvar *current-graph* (make-instance 'module-dependency-graph))
(defvar *special-modules* ())

(defun special-modulep (mod)
  (member mod *special-modules* :test #'eql))

(defun module-by-id (id)
  (let ((length (car (array-dimensions *modules-by-id*))))
    (if (>= id length) nil
        (aref *modules-by-id* id))))

(defun set-module-by-id (id v)
    (let ((length (car (array-dimensions *modules-by-id*))))
      (cond ((>= id length)
             (adjust-array *modules-by-id* (+ 10 id))
             (setf (aref *modules-by-id* id) v))
            (t (setf (aref *modules-by-id* id) v)))))

(defsetf module-by-id set-module-by-id)

(defclass module ()
  ((label :initarg :label :reader module-label :type symbol)
   (title :initarg :title :reader module-title :type string)
   (description :initarg :description :reader module-description :type string)
   (creation-index :reader module-id :type integer)
   (payload :initform () :accessor module-payload :type list)
 ;;  (index :initform -1 :accessor module-index :type integer)
 ;;  (lowlink :initform -1 :accessor module-lowlink :type integer)
   (degree :initform 0 :accessor module-degree :type integer)
   (indegree :initform 0 :accessor module-indegree :type integer)
   (dependencies :initarg :dependencies :accessor module-dependencies :initform () :type list)))

(defun define-module (label title description dependencies &key (special-module nil))
  (multiple-value-bind (mod found)
      (gethash label *current-modules*)
    (when found (error "Module exists already: ~S" mod))
    (when special-module (setf *special-modules* (push label *special-modules*)))
    (make-instance 'module
                   :title (lang title)
                   :label label
                   :description (lang description)
                   :dependencies dependencies)))

(defun list-module-description (label)
  (multiple-value-bind (mod found)
      (gethash label *current-modules*)
    (if found
        (let ((short (module-title mod))
              (long (module-description mod)))
          (list label short long))
        (list label "false" "false"))))

(defmethod initialize-instance :after ((v module) &key)
  (setf (graph-modules *current-graph*) (push v (graph-modules *current-graph*))
        (slot-value v 'creation-index) *module-id*
        (module-by-id *module-id*) v
        *module-id* (1+ *module-id*)
        (module-dependencies v) (mapcar 
                                 #'(lambda (from) 
                                     (make-directed-dependency 
                                      (module from) v)) (module-dependencies v))
        (gethash (module-label v) *current-modules*) v)
  (incf (graph-nm *current-graph*)))

(defmethod print-object ((v module) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (label creation-index) v
      (format stream "~S:~D" label creation-index))))

 (defun find-module-by-id (id)
   (find id (graph-modules *current-graph*) :key #'module-id :test #'=))

(defun find-module (label)
  (gethash label *current-modules*))

(defgeneric module (ident))

(defmethod module ((label symbol))
  (or (find-module label)
      (make-instance 'module :label label)))

(defun pupil-didactic-elements (pupil)
  (let ((remaining (pupil-modules pupil)))
    (if remaining
        (module-payload (module (car remaining)))
        (let* ((curr (pupil-last-access pupil))
               (mod (if curr curr (last-module))))
          (unless curr 
            (setf (pupil-last-access pupil) mod))
          (module-payload (module mod))))))

;; (defun pupil-didactic-elements (pupil)
;;   (module-payload (module (car (pupil-modules pupil)))))

(defmethod module ((id integer))
  (find-module-by-id id))
  
(defclass dependency ()
  ((v :reader dependency-v :type module :initarg :v)
   (w :reader dependency-w :type module :initarg :w)))

(defclass direction ()
  ((direction :initarg :direction :type cons :reader dependency-direction)))

(defun dependency-from (dependency)
  (car (dependency-direction dependency)))

(defun dependency-to (dependency)
  (cdr (dependency-direction dependency)))

(defclass directed-dependency (dependency direction) ())

(defgeneric make-directed-dependency (from to))

(defmethod make-directed-dependency ((from module) (to module))
  (make-instance 'directed-dependency :v from :w to :direction (cons from to)))

(defmethod initialize-instance :after ((ddependency directed-dependency) &key)
  (let ((from (dependency-from ddependency))
        (to (dependency-to ddependency)))
    (setf (graph-dependencies *current-graph*) (push ddependency (graph-dependencies *current-graph*))
          (slot-value from 'dependencies) (push ddependency (slot-value from 'dependencies))
          (slot-value to 'dependencies) (push ddependency (slot-value to 'dependencies)))
    (incf (graph-nd *current-graph*))
    (incf (module-degree from))
    (incf (module-indegree to))))

(defmethod print-object ((e directed-dependency) stream)
  (print-unreadable-object (e stream :type t)
    (with-slots (direction) e
      (format stream "from ~S to ~S" (car direction) (cdr direction)))))

(defun reset-graph ()
  (setf *current-graph* (make-instance 'module-dependency-graph)
        *current-modules* (make-hash-table)
        *module-id* 0)
  *current-graph*)

(defun reset-modules-by-id () ;;FIXME size
  (setf *modules-by-id* (make-array 100 :adjustable t)))

(defun clear-dependencies (&optional (graph *current-graph*))
  (setf (graph-dependencies graph) nil)
  (loop for v in (graph-modules graph)
     do (setf (module-dependencies v) nil)))

;; (defun init-graph (graph)
;;   (loop for v in (graph-modules graph)
;;        do (setf (module-index v) -1
;;                 (module-lowlink v) -1))
;;   *current-graph*)

(defun rebuild-graph ()
 ;; (clrhash *didactic-elements*)
  (reset-graph)
  (reset-modules-by-id)
  (load "didactic-elements.lisp"))

    
(defun topsort (&optional (graph *current-graph*))
  (let ((counter 0)
        (stop (graph-nm graph))
        (sorted ())
        (queue (remove-if-not 
                 #'(lambda (x) (zerop (module-indegree x))) 
                 (graph-modules graph))))
    (loop until (null queue)
       for v = (pop queue)
       do (let ((v.dependencies (module-dependencies v)))
            (push v sorted)
            (incf counter)
            (dolist (e v.dependencies)
              (let ((to (dependency-to e)))
                (decf (module-indegree to))
                (when (zerop (module-indegree to))
                  (push to queue))))))
    (unless (= counter stop) (error "problemo!"))
    (nreverse sorted)))
             
(defun to-dot (name &optional (stream *standard-output*) (graph *current-graph*))
  (format stream 
          "digraph ~A {
~:{~S -> ~S;~%~}~%}" 
          name (loop for ed in (mapcar #'dependency-direction (graph-dependencies graph))
                  collect (list (symbol-name (module-label (car ed))) 
                                (symbol-name (module-label (cdr ed)))))))

(defun write-testdot (&optional (name "testdot.dot"))
  (with-open-file (str name :direction :output :if-exists :supersede)
      (to-dot "test" str)))

(defclass didactic-element () 
  ((tag :initarg :tag :reader tag)
   (display :initarg :display :initform t :reader didactic-element-display)
   (caption :initarg :caption :reader caption)
   (short-description :initarg :sd :reader sd)
   (long-description :initarg :ld :reader ld)
   (action :initarg :action :reader action)
   (direct-action 
    :initarg :direct-action :initform t :reader didactic-element-da)
   (atype :initarg :atype :reader atype)
   (reply :initarg :reply :initform 10 :reader reply)
   (spmenu-level :initarg :spmenu-level :initform 0 :reader de-spmenu-level)
   ;;reply is the max input field length required to answer qs of type
   (exercise :initarg :exercise :initform 10 :accessor n-ex)
   (pass :initarg :pass :initform 0.9 :reader didactic-element-pass)
   (mixed :initarg :mixed :initform 2 :reader n-mix)
   (test :initarg :test :initform 5 :reader n-test)
   (modules :initarg :modules :initform () :reader didactic-element-modules :type list))
  (:documentation "A didactic element is an ability or concept, which
  can be practised and tested."))

(defmethod initialize-instance :after ((de didactic-element) &key)
  (dolist (m (didactic-element-modules de))
    (multiple-value-bind (module found)
        (gethash m *current-modules*)
      (unless found (error "Module named ~S does not exist" m))
      (setf (module-payload module)
            (push de (module-payload module))))))

(defmethod print-object ((de didactic-element) stream)
  (print-unreadable-object (de stream :type t)
    (format stream "~S" (tag de))))
   
(defvar *didactic-elements* (make-hash-table)) ;;FIXME size

(defun get-didactic-element (de)
  (gethash de *didactic-elements*))

(defun add-didactic-element (de)
  (setf (gethash (tag de) *didactic-elements*) de))

(defun didactic-element-operator (de) ;;NOONE CALLS ME! FIXME
      (let ((el (get-didactic-element de)))
	(unless de (error 'didactic-element-does-not-exist de))
	(operation-operator (funcall (action el)))))

;; (defun define-didactic-element
;; ;;how many to do in placement program is defined in document.lisp
;;     (tag caption short-description long-description spmenu-level pass
;;      action atype reply modules)
;;   ;; (let ((found (get-didactic-element tag)))
;;   ;;   (if found (error 'didactic-element-exists :name tag) 27/02/2013
;; 	(let ((de (make-instance 'didactic-element
;; 				 :tag tag
;;                                  :pass pass
;; 				 :caption caption
;; 				 :sd short-description
;; 				 :ld long-description
;;                                  :spmenu-level spmenu-level
;; 				 :action action
;;                                  :atype atype
;;                                  :modules modules
;; 				 :reply reply)))
;;           (add-didactic-element de)
;;           de))

(defun define-didactic-element
;;how many to do in placement program is defined in document.lisp
    (tag caption short-description long-description spmenu-level pass
     action atype reply modules)
  ;; (let ((found (get-didactic-element tag)))
  ;;   (if found (error 'didactic-element-exists :name tag) 27/02/2013
	(let ((de (make-instance 'didactic-element
				 :tag tag
                                 :pass pass
				 :caption (lang caption)
				 :sd short-description
				 :ld (lang long-description)
                                 :spmenu-level spmenu-level
				 :action action
                                 :atype atype
                                 :modules modules
				 :reply reply)))
          (add-didactic-element de)
          de))

(defun define-indirect-didactic-element
;;how many to do in placement program is defined in document.lisp
    (tag caption short-description long-description spmenu-level pass
     action atype reply modules)
  ;; (let ((found (get-didactic-element tag)))
  ;;   (if found (error 'didactic-element-exists :name tag) 27/02/2013
	(let ((de (make-instance 'didactic-element
				 :tag tag
                                 :pass pass
                                 :direct-action nil
				 :caption (lang caption)
				 :sd short-description
				 :ld (lang long-description)
                                 :spmenu-level spmenu-level
				 :action action
                                 :atype atype
                                 :modules modules
				 :reply reply)))
          (add-didactic-element de)
          de))

(defmacro define-indirect-action (init direct-form)
  (let ((els (length (eval init))))
    `(lambda () 
      (let ((els ,els)
            (arr (make-array ,els :initial-contents ,init)))
        (lambda ()
          (labels ((initialize ()
                     (setf els ,els 
                           arr (make-array ,els :initial-contents ,init))))
            (when (zerop els) (initialize))
            (let ((it (aref arr (random els)))
                  (obj ,direct-form))
              (decf els)
              (setf arr (delete it arr))
              (setf (slot-value (slot-value obj 'operands) 'list) it)
              obj)))))))

(defun define-internal-didactic-element  
    (tag caption short-description long-description spmenu-level pass
     action atype reply modules)
  ;; (let ((found (get-didactic-element tag)))
  ;;   (if found (error 'didactic-element-exists :name tag) 27/02/2013
	(let ((de (make-instance 'didactic-element
				 :tag tag
                                 :pass pass
                                 :direct-action t
                                 :display nil
				 :caption (lang caption)
				 :sd short-description
				 :ld (lang long-description)
                                 :spmenu-level spmenu-level
				 :action action
                                 :atype atype
                                 :modules modules
				 :reply reply)))
          (add-didactic-element de)
          de))


;; example usage, also see didactic-elements.lisp:
;; (define-indirect-action  
;;     (loop for i from 9 downto 1
;;        append (loop for j from i downto 1
;;                  collect (list i (- j))))
;;     (make-sum (tiny-eint-nat) (op-minus) nil nil))

(defun rebuild-didactic-elements () ;;FIXME remove when fixed
  (reset-graph)
  (reset-modules-by-id)
  (clrhash *didactic-elements*)
  (load "modules.lisp")
  (load "didactic-elements.lisp"))


