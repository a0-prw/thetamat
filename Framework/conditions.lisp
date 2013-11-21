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

(define-condition autotutor-error (simple-error) () )

(define-condition dag-error (autotutor-error)
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to))
  (:report (lambda (c s) (format s "The vertex ~S which you are connecting to from ~S has an ancestor in common with ~S." (to c) (from c) (from c)))))

(define-condition group-exists (autotutor-error)
  ((name :initarg :name :reader name))
  (:report  (lambda (c s) (format s "The group ~S already exists." (name c)))))

(define-condition first-name-contains-digits (autotutor-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "The first name (~S) may not contain digits." (name c)))))

(define-condition system-exhausted (autotutor-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "Pupil: ~A appear to have completed
  all the available exercises in the system.  Congratulations, ~A!" 
				 (name c)
				 (first-name (name c))))))

(define-condition group-does-not-exist (autotutor-error)
  ((name :initarg :name :reader name))
  (:report  (lambda (c s) (format s "The group ~S does not yet exist." (name c)))))

(define-condition system-name-taken (autotutor-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "The name ~S is already in use." (name c)))))

(define-condition user-does-not-exist (autotutor-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "The user ~S does not exist." (name c)))))

(define-condition user-already-exists (autotutor-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "The user ~S already exists." (name c)))))

(define-condition didactic-element-exists (autotutor-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "The element ~S already exists." (name c)))))

(define-condition didactic-element-does-not-exist (autotutor-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "The element ~S does not exist." (name c)))))

(define-condition pupil-definition-error (autotutor-error) () )
;;; FIXME ^

(define-condition invalid-user-entry (autotutor-error)
  ((inform :initarg :inform :reader inform)
   (value :initarg :value :reader value))
  (:report (lambda (c s)
	     (format s "~A" (format nil (inform c) (value c))))))

(define-condition unknown-role (autotutor-error)
  ((role :initarg :role :reader role))
  (:report (lambda (c s) (format s "The role: ~S is unknown." (role c)))))

(define-condition no-teacher-pupil-relationship (autotutor-error)
  ((teacher :initarg :teacher :reader teacher-name)
   (pupil :initarg :pupil :reader pupil-name))
  (:report (lambda (c s)
	     (format s "The system can not establish a teacher-pupil 
association between system-name ~S and system-name ~S.
Please contact the system administrator" (teacher-name c) (pupil-name c)))))

(define-condition teacher-authorization-exhausted (pupil-definition-error)
  ((name :initarg :name :reader name))
  (:report 
   (lambda (c s)
     (format s "The teacher (~S) is not authorized to define any more pupils." 
	     (name c)))))

(define-condition feature-not-implemented (autotutor-error)
  ((feature :initarg :feature :reader feature)
   (requestor :initarg :requestor :reader requestor))
  (:report 
   (lambda (c s)
     (format s 
	     "Sorry, ~A.  The feature you requested: ~S,  is not yet implemented."
	     (requestor c) (feature c)))))

(define-condition removal-error (autotutor-error)
  ((place :initarg :place :reader place)
   (name :initarg :name :reader name))
  (:report 
   (lambda (c s)
     (format s 
	     "An error occurred in removing user: ~S from place: ~S"
	     (name c) (place c)))))


(define-condition user-has-no-password (autotutor-error)
  ((name :initarg :name :reader name))
  (:report 
   (lambda (c s)
     (format s 
	     "User ~S: login not allowed."
	     (name c)))))

(define-condition user-logged-out (autotutor-error)
  ((name :initarg :name :reader name))
  (:report 
   (lambda (c s)
     (format s 
	     "User ~S: logged out."
	     (name c)))))

(define-condition global-nameregister-failure (autotutor-error)
  ((status :initarg :status :reader status))
  (:report 
   (lambda (c s)
     (format s 
	     "Global nameregister failed with status: ~S" 
             (status c)))))

(define-condition global-formsregister-failure (autotutor-error)
  ((status :initarg :status :reader status))
  (:report 
   (lambda (c s)
     (format s 
	     "Global formsregister failed with status: ~S" 
             (status c)))))

(define-condition global-deregister-failure (autotutor-error)
  ((status :initarg :status :reader status))
  (:report 
   (lambda (c s)
     (format s 
	     "Global deregister failed with status: ~S" 
             (status c)))))

(define-condition notify-invoice-failure (autotutor-error)
  ((status :initarg :status :reader status))
  (:report
   (lambda (c s)
     (format s
             "Invoice notification error: status is ~S" (status c)))))

(define-condition tm-parse-error (autotutor-error)
  ((input :initarg :input :reader input))
  (:report
   (lambda (c s)
     (format s
             "Nasty input string: ~S" (input c)))))

(define-condition both-not-numbers (autotutor-error)
  ((input :initarg :input :reader input)
   (system :initarg :system :reader system))
  (:report
   (lambda (c s)
     (format s
             "The system does not recognise ~S and ~S as numbers." (input c) (system c)))))

(define-condition input-not-a-number (autotutor-error)
  ((input :initarg :input :reader input))
   (:report
    (lambda (c s)
      (format s
              "The system does not recognise ~S as a number." (input c)))))

(define-condition system-value-not-a-number (autotutor-error)
  ((system :initarg :system :reader system))
   (:report
    (lambda (c s)
      (format s
              "The system does not recognise ~S as a number." (system c)))))


