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

;; the next 2 from Doug Hoyte's book "Let over Lambda"

;; (defun tree-leaves%% (tree test result)
;;   (if tree
;;       (if (listp tree)
;;           (cons
;;            (tree-leaves%% (car tree) test result)
;;            (tree-leaves%% (cdr tree) test result))
;;           (if (funcall test tree)
;;               (funcall result tree)
;;               tree))))

;; (defmacro tree-leaves (tree test result)
;;   `(tree-leaves%% ,tree
;;                   (lambda (x)
;;                     (declare (ignorable x))
;;                     ,test)
;;                   (lambda (x)
;;                     (declare (ignorable x))
;;                     ,result)))

(defmacro define-common-user-objects (user-type-list)
  (let ((code (loop for user in user-type-list 
		 for upname = (symbol-name user) 
		 for dirvar = (intern (format nil "*~A-DIRECTORY*" upname)) 
		 for downame = (string-downcase upname) 
		 for path = (format nil  "~As/" downame)
		 for hashn = (intern (format nil "*~AS*" upname))
		 for lockn = (intern (format nil "*LOCK-~AS*" upname))
		 for regn = (intern (format nil "REGISTER-LOADED-~A-NAME" upname))
		 for isloadedn = (intern (format nil "~A-LOADED" user))
		 for existsn = (intern (format nil "~A-EXISTS-P" user))
		 for ufilen =  (intern (format nil "~A-FILE" user))
		 for loaditn = (intern (format nil "LOAD-~A" user))
		 for lalln = (intern (format nil "LIST-ALL-~AS" user))
		 for withn = (intern (format nil "WITH-UPDATED-~A" user))
		 append
		   `(;;(defvar ,dirvar ,path);;can drop
		     (defvar ,hashn 
		       (make-hash-table ,@(cond ((eql user 'pupil)
						 '(:size 7500))
						(t '(:size 300)))))
		     (defvar ,lockn (bt:make-lock))
		     (defun ,regn (name obj)
		       (bt:with-lock-held (,lockn)
			 (setf (gethash (intern name) ,hashn) obj)
			 t))
		     (defun ,isloadedn (name)
		       (bt:with-lock-held (,lockn)
			 (if (gethash (intern name) ,hashn) t nil)))
		     (defun ,existsn (pathname)
		       (probe-file pathname))
		     ;; (defun ,ufilen (system-name)
                     ;;   ;;still need ufilen _name_ but not this fn
		     ;;   (merge-pathnames 
		     ;;    (merge-pathnames system-name ,dirvar)))
		     (defun ,loaditn (name)
		       (let ((file (,ufilen name)))
			 (when (not (,existsn file))
			   (error (make-condition 
				   'user-does-not-exist :name name)))
			 (let ((obj (cl-store:restore file)))
			   (,regn name obj)
			   t)))
		     ;; (defun ,lalln ()
		     ;;   (mapcar (lambda (p) (pathname-name p))
		     ;;           (directory (,ufilen "*"))))
                     (defun ,lalln ()
                       (loop for subd in (directory ,dirvar)
                          unless (string= (car (last (pathname-directory subd))) 
                                          "records")
                          append (loop for user in 
                                      (directory (merge-pathnames "*" subd))
                                    collect (pathname-name user))))

                     ))))
    
    `(progn ,@code)))

(defmacro define-safe-access-for-role (funname role slot-name copy-function)
  (let* ((rname (symbol-name role))
	 (role-place (intern (concatenate 'string "*" rname "S*")))
	 (lock-place (intern (concatenate 'string "*LOCK-" rname "S*")))
	 (file-access-fn (intern (concatenate 'string rname "-FILE")))
	 (setname (intern (concatenate 'string "SET-" (symbol-name funname))))
	 (file (gensym "FILE"))
	 (obj1 (gensym "OBJA")))
    `(progn 
       (defun ,funname (system-name)
	 (bt:with-lock-held (,lock-place)
	   (let* ((,file (,file-access-fn system-name))
		  (,obj1 (or (gethash (intern system-name) ,role-place)
			     (if (not (probe-file ,file))
				 (error 'user-does-not-exist :name system-name)
				 (let ((user (cl-store:restore ,file)))
				   (setf (gethash (intern system-name) ,role-place) user)
				   user)))))
	     (,copy-function (slot-value ,obj1 ',slot-name)))))
       (defun ,setname (system-name new-value)
	 (bt:with-lock-held (,lock-place)
	   (let* ((,file (,file-access-fn system-name))
		  (,obj1 (or (gethash (intern system-name) ,role-place)
			     (if (not (probe-file ,file))
				 (error 'user-does-not-exist :name system-name)
				 (let ((user (cl-store:restore ,file)))
				   (setf (gethash (intern system-name) ,role-place) user)
				   user)))))
	     (setf (slot-value ,obj1 ',slot-name) new-value)
	     (cl-store:store ,obj1 ,file)
	     new-value)))
       (defsetf ,funname ,setname))))

(defmacro define-safe-access-for-user (funname slot-name copy-function)
  (let* ((setname (intern (concatenate 'string "SET-" (symbol-name funname))))
	 (file (gensym "FILE"))
	 (obj1 (gensym "OBJA")))
    `(progn 
       (defun ,funname (system-name)
	 (multiple-value-bind (rslist found)
	     (local-user-register 'query system-name nil)
           (unless found (error 'user-does-not-exist :name system-name))
           (let ((role (car rslist)))
             (cond ((eql role 'pupil)
                    (bt:with-lock-held (*lock-pupils*)
                      (let* ((,file (pupil-file system-name))
                             (,obj1 (or (gethash (intern system-name) *pupils*)
                                        (if (not (probe-file ,file))
                                            (error 'user-does-not-exist :name system-name)
                                            (let ((user (cl-store:restore ,file)))
                                              (setf (gethash (intern system-name) *pupils*) user)
                                              user)))))
                        (,copy-function (slot-value ,obj1 ',slot-name)))))
                   ((eql role 'teacher)
                    (bt:with-lock-held (*lock-teachers*)
                      (let* ((,file (teacher-file system-name))
                             (,obj1 (or (gethash (intern system-name) *teachers*)
                                        (if (not (probe-file ,file))
                                            (error 'user-does-not-exist :name system-name)
                                            (let ((user (cl-store:restore ,file)))
                                              (setf (gethash (intern system-name) *teachers*) user)
                                              user)))))
                        (,copy-function (slot-value ,obj1 ',slot-name)))))
                   (t (error 'unknown-role :role role))))))
         (defun ,setname (system-name new-value)
           (multiple-value-bind (rslist found)
               (local-user-register 'query system-name nil)
             (unless found (error 'user-does-not-exist :name system-name))
             (let ((role (car rslist)))
               (cond ((eql role 'pupil)
                      (bt:with-lock-held (*lock-pupils*)
                        (let* ((,file (pupil-file system-name))
                               (,obj1 (or (gethash (intern system-name) *pupils*)
                                          (if (not (probe-file ,file))
                                              (error 'user-does-not-exist :name system-name)
                                              (let ((user (cl-store:restore ,file)))
                                                (setf (gethash (intern system-name) *pupils*) user)
                                                user)))))
                          (setf (slot-value ,obj1 ',slot-name) new-value)
                          (cl-store:store ,obj1 ,file)
                          new-value)))
                     ((eql role 'teacher)
                      (bt:with-lock-held (*lock-teachers*)
                        (let* ((,file (teacher-file system-name))
                               (,obj1 (or (gethash (intern system-name) *teachers*)
                                          (if (not (probe-file ,file))
                                              (error 'user-does-not-exist :name system-name)
                                              (let ((user (cl-store:restore ,file)))
                                                (setf (gethash (intern system-name) *teachers*) user)
                                                user)))))
                          (setf (slot-value ,obj1 ',slot-name) new-value)
                          (cl-store:store ,obj1 ,file)
                          new-value)))
                     (t (error 'unknown-role :role role))))))
         (defsetf ,funname ,setname))))

;;return lists of data (lock held once only -- should be more
;;efficient)
;; (defmacro define-teacher-slots-returner (name &rest slots)
;;   `(defun ,name (system-name)
;;      (multiple-value-bind (rslist found)
;;          (local-user-register 'query system-name nil)
;;        (unless found (error 'user-does-not-exist :name system-name))
;;        (let ((role (car rslist)))
;;          (cond
;;            ((eql role 'pupil)
;;             (error 'unknown-role :role role))
;;            ((eql role 'teacher)
;;             (bordeaux-threads:with-lock-held (*lock-teachers*)
;;               (let* ((tfile (teacher-file system-name))
;;                      (tobj
;;                       (or (gethash (intern system-name) *teachers*)
;;                           (if (not (probe-file tfile))
;;                               (error 'user-does-not-exist :name system-name)
;;                               (let ((user (cl-store:restore tfile)))
;;                                 (setf (gethash (intern system-name) *teachers*)
;;                                       user)
;;                                 user)))))
;;                 (list ,@(mapcar (lambda (sln)
;;                                   `(slot-value tobj ',sln))
;;                                 slots)))))
;;            (t (error 'unknown-role :role role)))))))

(defmacro with-teacher-object (system-name &body body)
  "Captures the variable TEACHER-OBJECT with value the teacher object
named by SYSTEM-NAME, for use in BODY.  Do ****not**** use any
TEACHER-xyz functions in body, or you may have threading bugs."
  (let ((rslist (gensym "RSL"))
        (found (gensym "FND"))
        (role (gensym "ROL"))
        (tfile (gensym "TFL"))
        (user (gensym "USE")))
    `(multiple-value-bind (,rslist ,found)
         (local-user-register 'query ,system-name nil)
       (unless ,found (error 'user-does-not-exist :name ,system-name))
       (let ((,role (car ,rslist)))
         (cond
           ((eql ,role 'pupil)
            (error 'unknown-role :role ,role))
           ((eql ,role 'teacher)
            (bordeaux-threads:with-lock-held (*lock-teachers*)
              (let* ((,tfile (teacher-file ,system-name))
                     (teacher-object
                      (or (gethash (intern ,system-name) *teachers*)
                          (if (not (probe-file ,tfile))
                              (error 'user-does-not-exist :name ,system-name)
                              (let ((,user (cl-store:restore ,tfile)))
                                (setf (gethash (intern ,system-name) *teachers*)
                                      ,user)
                                ,user)))))
                (progn ,@body))))
           (t (error 'unknown-role :role ,role)))))))
  


