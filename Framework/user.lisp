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

(declaim (special +modular-progression+))

(setf crypto:*prng* (crypto:make-prng :fortuna))

(defvar *system-user-register* (make-hash-table :size 3000 :test 'equal))
(defvar *system-user-register-place* (merge-pathnames "system/register"))
(defvar *system-user-register-lock* (bt:make-lock))

(defun place-register ()
  (unless (probe-file *system-user-register-place*)
    (cl-store:store *system-user-register* *system-user-register-place*)))

(defun initialize-user-registry ()
    (setf *system-user-register* (cl-store:restore *system-user-register-place*)))

(defgeneric local-user-register (key user data)
  (:documentation 
   "It is imperative for the system's integrity that USER is a
globally (across all connected systems) unique name.  See the code for
UNIQUE-NAME in login-server."))

(defmethod local-user-register ((key (eql 'query)) (user string) (data null))
  (bt:with-lock-held (*system-user-register-lock*)
    (gethash user *system-user-register*)))

(defmethod local-user-register 
    ((key (eql 'query)) (user string) (data (eql 'teacher)))
  (bt:with-lock-held (*system-user-register-lock*)
    (multiple-value-bind (udata found)
        (gethash user *system-user-register*)
      (values (eql (car udata) 'teacher) found))))

(defmethod local-user-register ((key (eql 'setpass)) (user string) (pdata cons))
  (bt:with-lock-held (*system-user-register-lock*)
    (let* ((current (gethash user *system-user-register*))
           (role (car current)))
      (setf (gethash user *system-user-register*) (list role pdata))
      (cl-store:store *system-user-register* *system-user-register-place*)
      pdata)))

(defmethod local-user-register ((key (eql 'delt)) (user string) (data null))
  (bt:with-lock-held (*system-user-register-lock*)
    (remhash user *system-user-register*)
    (cl-store:store *system-user-register* *system-user-register-place*)))

(defmethod local-user-register ((key (eql 'addt)) (user string) (data null))
  (bt:with-lock-held (*system-user-register-lock*)
    (setf (gethash user *system-user-register*) (list 'teacher))
    (cl-store:store *system-user-register* *system-user-register-place*)))

(defmethod local-user-register ((key (eql 'addt)) (user string) (pdata cons))
  (bt:with-lock-held (*system-user-register-lock*)
    (setf (gethash user *system-user-register*) (list 'teacher pdata))
    (cl-store:store *system-user-register* *system-user-register-place*)))

(defmethod local-user-register ((key (eql 'addp)) (user string) (data null))
  (bt:with-lock-held (*system-user-register-lock*)
    (setf (gethash user *system-user-register*) (list 'pupil))
    (cl-store:store *system-user-register* *system-user-register-place*)))

(defmethod local-user-register ((key (eql 'addp)) (user string) (pdata cons))
  (bt:with-lock-held (*system-user-register-lock*)
    (setf (gethash user *system-user-register*) (list 'pupil pdata))
    (cl-store:store *system-user-register* *system-user-register-place*)))

(defun make-salt ()
  (coerce (loop for n from 1 to 8 collect (random 256))
	  '(vector (unsigned-byte 8))))

(defun make-salted (salt password)
  (crypto:digest-sequence 
   :md5 (concatenate 
	 '(vector (unsigned-byte 8)) salt
	 (flexi-streams:string-to-octets password))))

(defun make-unlock-list  (password)
  (let* ((salt (make-salt))
	 (salted (make-salted salt password)))
    (list salt salted)))

(defun salt (unlock-list)
  (car unlock-list))

(defun salted (unlock-list)
  (cadr unlock-list))

(defun get-system-unlock-list (system-name)
  (cadr (local-user-register 'query system-name nil)))

(defun sesame-opens (system-name pass)
  ;;(cl-log:log-message :info "(SESAME-OPENS ~S ~S)" system-name pass)
  (let ((opensesame  (get-system-unlock-list system-name)))
    ;;(cl-log:log-message :info "opensesame: ~S" opensesame)
    (when opensesame
      ((lambda ()
	 (equalp (salted opensesame) (make-salted (salt opensesame) pass)))))))

(defun get-salt (system-name)
  (salt (get-system-unlock-list system-name)))

(defun generate-password-chars ()
  (append (loop for n from 0 to 3 ;;lower
	       collect (code-char (+ 97 (crypto:strong-random (- 122 97)))))
	  (loop for n from 0 to 1 ;;upper
	       collect (code-char (+ 65 (crypto:strong-random (- 90 65)))))
	  (loop for n from 0 to 1 ;;numeric
	       collect (code-char (+ 48 (crypto:strong-random (- 57 48)))))
	  ;;nonalpha
	  (list (nth (crypto:strong-random 6) '(#\+ #\- #\_ #\? #\$ #\*)))))

(defun suggest-password ()
  (let* ((list (generate-password-chars))
	 (len (length list))
	 (idxs (loop for n from 0 below len collect n)))
      (with-output-to-string (string)
	(dotimes (n len string)
	  (let ((idx (nth (if (= len 1) 0 (crypto:strong-random len)) idxs)))
	    (write-char (nth idx list) string)
	    (decf len)
	    (setf idxs (remove-if (lambda (x) (= x idx)) idxs)))))))

(defmacro with-session-name (capture &body body)
  "For use in ajax calls to check that the call is accessing a
resource which is allowed to the user named in the current hunchentoot
session."
  (let ((req (gensym "REQ"))
        (sess (gensym "SESS")))
    `(let* ((,req (when (boundp 'tbnl:*request*) (symbol-value 'tbnl:*request*)))
            (,sess (when ,req (tbnl:session-verify ,req))))
       (if ,req
           ;;(log-request ,req ,capture)
           (cond ((not ,sess) (redirect-login))
                 ((not (string= ,capture (tbnl:session-value 'name)))
                  (redirect-login))
                 (t (progn ,@body)))
           (progn ,@body)))))

(defmacro with-dangerous-session-name (&body body)
  "For use in ajax calls to capture the name of the user named in the
current hunchentoot session in the variable DANGEROUS-NAME. Obviously
a security risk, only used on initial load."
  (let ((req (gensym "REQ"))
        (sess (gensym "SESS")))        
    `(let* ((,req (when (boundp 'tbnl:*request*) (symbol-value 'tbnl:*request*)))
            (,sess (when ,req (tbnl:session-verify ,req)))
            (dangerous-name (when ,req (tbnl:session-value 'name))))
       (if ,req
           (if (not ,sess)
               (redirect-login)
               (progn
                 ,@body))
           (progn ,@body)))))

(defclass user () 
  ((first-name :initarg :first-name)
   (surname :initarg :surname)
   (system-name :initarg :system-name :reader system-name)
   (last-access :initarg :last-access)
   (first-access :initarg :first-access))
  (:documentation "The base class for all autotutor users."))

(defclass user-token ()
  ((system-name :initarg :system-name :reader system-name)))

(defclass administrator (user)
  ((email :initarg :email)
   (nauthorized :initarg :nauth :initform 0)
   (expires :initarg :expires)
   (data :initarg :data :initform nil)
   (teachers :initarg :teachers :initform nil)))

(defclass didactic-user (user)
  ((work-on-screen :initarg :work-on-screen :initform "false") ))

(defclass teacher (didactic-user)
  ((admin :initarg :admin :initform nil)
   (email :initarg :email :initform nil)
   (price-data :initarg :price-data :initform "")
   (nauthorized :initarg :nauth :initform 0)
   (latex-syntax :initform *default-latex-syntax*)
   (expires :initarg :expires :initform nil)
   (groups :initarg :groups :initform nil)
   (pupils :initarg :pupils :initform nil)
   (demo :initarg :demo :initform nil)))

(defclass teacher-token (user-token) ())

(defclass pupil (didactic-user)
  (;(latex-syntax :initform *default-latex-syntax*)
   (last-access :initform nil)
   (first-access :initform nil)
   (placement :initarg :placement :initform t)
   (modules :initarg :modules :initform nil)
   (view-lessons :initarg :view-lesson :initform nil)
   (basic-add :initarg :basic-add :initform '((:false :false :false)
                                              (:false :false :false)))
   ;(basic-sub :initarg :basic-sub :initform '(nil nil nil))
   (basic-mul :initarg :basic-mul :initform '((:false :false :false)
                                              (:false :false :false)))
   ;(basic-div :initarg :basic-div :initform '(nil nil nil))
   (viewed :initarg :viewed :initform nil)
   (cycle :initarg :cycle :initform 1)
   (current-problems :initarg :current-problems :initform nil)
   ;;(work-on-screen :initarg :work-on-screen :initform 'false) 
   (teacher :initarg :teacher))
  (:documentation "A user who is practising elementary mathematics in
  the system."))

;;;;These macros are defined in safe-access-macros.lisp.  
(define-common-user-objects (pupil teacher))

(define-safe-access-for-user first-name first-name copy-seq)
(define-safe-access-for-user surname surname copy-seq)
;; (define-safe-access-for-user last-access last-access copy-date)
;;(define-safe-access-for-user first-access first-access copy-date)
(define-safe-access-for-role teacher-email teacher email identity)
(define-safe-access-for-role teacher-price-data teacher price-data copy-seq)
(define-safe-access-for-role teacher-admin teacher admin identity)
;;(define-safe-access-for-role teacher-offer-id teacher offer-id copy-seq)
;;(define-safe-access-for-role teacher-token teacher token copy-seq)
;;(define-safe-access-for-role teacher-subscription teacher subscription copy-seq)
(define-safe-access-for-role teacher-nauthorized teacher nauthorized identity)
;;(define-safe-access-for-role teacher-expires teacher expires copy-date)
(define-safe-access-for-role teacher-groups teacher groups copy-tree)
(define-safe-access-for-role teacher-latex-syntax teacher latex-syntax copy-tree)
(define-safe-access-for-role teacher-demo teacher demo copy-tree)
(define-safe-access-for-role teacher-work-on-screen teacher work-on-screen copy-seq)
(define-safe-access-for-role teacher-pupils teacher pupils copy-tree)
;;pupil-didactic-modules is defined in content/learning-space
(define-safe-access-for-role pupil-view-lessons pupil view-lessons identity)
;;(define-safe-access-for-role pupil-lesson-review pupil lesson-review copy-list)
(define-safe-access-for-role pupil-basic-add pupil basic-add copy-tree)
(define-safe-access-for-role pupil-first-access pupil first-access identity)
(define-safe-access-for-role pupil-last-access pupil last-access identity)
(define-safe-access-for-role pupil-basic-mul pupil basic-mul copy-tree)
(define-safe-access-for-role pupil-modules pupil modules copy-list)
(define-safe-access-for-role pupil-viewed pupil viewed copy-list)
(define-safe-access-for-role pupil-placement pupil placement identity)
(define-safe-access-for-role pupil-cycle pupil cycle identity)

(define-safe-access-for-role pupil-current-problems pupil current-problems copy-tree)
(define-safe-access-for-role pupil-work-on-screen pupil work-on-screen copy-seq)
(define-safe-access-for-role pupil-teacher pupil teacher copy-seq)

;;NON standard teacher-* stuff

(defun teacher-free-used (system-name)
  "Returns as 2 values the number of pupils which the teacher can
  enroll and the number of pupils which the teacher has enrolled."
  (with-teacher-object system-name
    (values (slot-value teacher-object 'nauthorized)
            (length (slot-value teacher-object 'pupils)))))

(defun teacher-load (system-name)
  "Returns the number of pupils which the teacher is subscribed to."
  (with-teacher-object system-name
    (+ (slot-value teacher-object 'nauthorized)
       (length (slot-value teacher-object 'pupils)))))

(defun teacher-dynamic-start-values (system-name)
  (with-teacher-object system-name
    (list (slot-value teacher-object 'work-on-screen)
          (slot-value teacher-object 'nauthorized)
          (length (slot-value teacher-object 'pupils)))))

(defun list-all-users ()
  (append (list-all-teachers) (list-all-pupils)))

(defun user-password (system-name)
  "Returns the unlock list of the user with SYSTEM-NAME."
  (multiple-value-bind (rslist found)
      (local-user-register 'query system-name nil)
    (unless found (error 'user-does-not-exist :name system-name))
    (cadr rslist)))

(defun set-user-password (system-name password)
  "Sets the unlock list of the user with SYSTEM-NAME from PASSWORD."
  (multiple-value-bind (rslist found)
      (local-user-register 'query system-name nil)
    (declare (ignore rslist))
    (unless found (error 'user-does-not-exist :name system-name))
    (local-user-register 'setpass system-name (make-unlock-list password))))

(defsetf user-password set-user-password)

(defun define-teacher (first-name surname nauthorized);; pridat)
  (let ((system-name (add-unique-name first-name)))
    (local-user-register 'addt system-name nil)
    (let ((teacher (make-instance 
                    'teacher
                    :system-name system-name
                    :first-name first-name
                    :surname surname
                    :first-access nil
                    :last-access nil
                    :groups nil
                    :pupils nil
                    ;;:email email
                    :nauth nauthorized
                    )))
      (register-loaded-teacher-name system-name teacher);;here
      (cl-store:store teacher 
                      (ensure-directories-exist (teacher-file system-name)))
      system-name)))

(defun define-pupil (teacher system-name first-name surname first-grade)
  (let* ((teacher-exists (local-user-register 'query teacher 'teacher)) ;;FIXME ?
         (password (suggest-password)))
    (when (not teacher-exists)
      (error 'user-does-not-exist :name teacher)) ;;?also superfluous?
    (local-user-register 'addp system-name (make-unlock-list password))
      (let* ((pupil (make-instance 
		     'pupil 
		     :system-name system-name
		     :teacher teacher
		     :first-name first-name
		     :surname surname
		     :first-access nil ;;unused
		     :last-access nil
		     :current-problems nil
                     :modules (if first-grade +modular-progression+
                                  (cdr +modular-progression+))
                     :viewed (if first-grade nil (list 
                                                  (car +modular-progression+)))
                     :placement (if first-grade nil t)
                     :cycle 1)))
	(register-loaded-pupil-name system-name pupil);;here
        (cl-store:store pupil (ensure-directories-exist
                               (pupil-file system-name)))
        (setf (teacher-nauthorized teacher) (1- (teacher-nauthorized teacher)))
	(let ((other-pupils (teacher-pupils teacher)))
	  (setf (teacher-pupils teacher) 
		(cons (list system-name (format nil "~A ~A" first-name surname))
		      other-pupils))))
      (list (list system-name password) (teacher-pupils teacher))))

(defun define-pupil-from-form (teacher form)
  (destructuring-bind (uname (first-name surname first-grade))
      form
      (let ((password (suggest-password)))
        (local-user-register 'addp uname (make-unlock-list password))
        (let* ((pupil (make-instance 
                       'pupil 
                       :system-name uname
                       :teacher teacher
                       :first-name first-name
                       :surname surname
                       :first-access 0
                       :last-access 0
                       :current-problems nil
                       ;;:didactic-elements 
                       :modules (if first-grade +modular-progression+
                                    (cdr +modular-progression+))
                       :viewed (if first-grade nil (list 
                                                    (car +modular-progression+)))
                       :placement (if first-grade nil t)
                       :cycle 1)))
          (register-loaded-pupil-name uname pupil);;here
          (cl-store:store pupil (ensure-directories-exist
                                 (pupil-file uname)))
          (setf (teacher-nauthorized teacher) (1- (teacher-nauthorized teacher)))
          (let ((other-pupils (teacher-pupils teacher)))
            (setf (teacher-pupils teacher) 
                  (cons (list uname (format nil "~A ~A" first-name surname))
                        other-pupils)))
          (values (format nil "~A ~A" first-name surname) uname password)))))


(defun delete-pupil (name teacher-name)
  (let ((file (pupil-file name))
	;;user-exists error checking has passed if the next binding
	;;succeeds. It also guarantees that pupil is loaded into
	;;*pupils* hash so that remhash later can succeed.
	(tname (pupil-teacher name)))
    (unless (string= tname teacher-name)
      (error 'no-teacher-pupil-relationship 
	     :teacher-name teacher-name
	     :pupil-name name))
    (if 
     ;;if all these succeed we remove the persistent pupil file and
     ;;increment the teachers number of authorized pupils.
     (let ((sym (intern name)))
       (if (bt:with-lock-held (*system-user-register-lock*)
             ;;remove from user register
             (when (remhash name *system-user-register*)
               (cl-store:store *system-user-register* *system-user-register-place*)))
           (if (bt:with-lock-held (*lock-pupils*)
                 ;;remove from loaded pupils hash
                 (remhash sym *pupils*))
               (let ((all (teacher-pupils teacher-name)))
                 ;;remove from teachers pupil-list
                 ;;FIXME remove from groups ?
                 
                   (setf (teacher-pupils teacher-name)
                         (remove name all :test #'string= :key #'car))
                 t)
               (error 'removal-error :place '*pupils* :name name))
           (error 'removal-error :place '*system-user-register :name name)))
       ;;remove persistent file: goodbye pupil
       (let ((rf (record-file name)))
         (delete-file file)
         (when (probe-file rf)
           (delete-file rf))
         (setf (teacher-nauthorized teacher-name)
               (1+ (teacher-nauthorized teacher-name)))))))
;;(error 'removal-error :place file :name name))))

(defun delete-teacher-locally (name)
  (let ((pupils (mapcar #'car (teacher-pupils name)))
        (sym (intern name))
        (file (teacher-file name)))
    (dolist (p pupils)
      (delete-pupil p name))
    ;; (bt:with-lock-held (*system-user-register-lock*)
    ;;   (remhash name *system-user-register*))
    ;;FOXME cl-store?
    (local-user-register 'delt name nil)
    ;;(update-expiry-list 'remn name)
    (bt:with-lock-held (*lock-teachers*)
      (remhash sym *teachers*))
    (delete-file file)
    (deregister-unique-names name)
    ))


(defun clean-up-hash (teacher hash)
  "If it returns T, things should be ok. Otherwise check hash table manually!"
  (let ((pupils (mapcar (lambda (x) (intern (car x))) (teacher-pupils teacher)))
        (rems ())
        (defs ()))
    (maphash (lambda (k v) 
               (let ((pname (symbol-name k)))
                 (when (eql v 'pupil)
                   (if (and (pupil-exists-p (pupil-file pname))
                            (string= teacher (pupil-teacher pname)))
                       (push k defs)
                       (push k rems)))))
             hash)
    (dolist (r rems)
      (remhash r hash))
    (= (length pupils) (length defs))))
    
