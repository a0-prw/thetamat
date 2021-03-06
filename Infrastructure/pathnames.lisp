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

;; (defvar *user-directory*
;;   (make-pathname :directory '(:relative "users")))

  ;;FIXME (make-pathname :directory '(:absolute "home" "prw"
  ;; "quicklisp" "local-projects" "autotutor" "users")))

;;(defvar *pupil-report-directory* "reports")

;; (defun user-subd (sysname)
;;   (let* ((subdscan (cl-ppcre:create-scanner "^(\\d+)(\\w*)"))
;;          (dig (cl-ppcre:register-groups-bind (dig alph)
;;                   (subdscan sysname)
;;                 (declare (ignore alph))
;;                 dig)))
;;     (if dig dig (string-upcase (subseq sysname 0 1)))))

(defun user-subd (sysname)
  (let* ((parts (split-sequence:split-sequence #\_ sysname))
         (fn (or (cadr parts)
                 (car parts))))
    (string-upcase (subseq fn 0 1))))

(defmacro define-user-file-name-function (user-type)
  (let* ((symn (string-upcase (symbol-name user-type)))
         (dirname (concatenate 'string (string-downcase symn) "s"))
         (varsym (symb '* symn '-directory*))
         (funsym (symb symn '-file)))
    `(progn (defvar ,varsym
              (merge-pathnames 
               "*" (make-pathname :directory `(:relative "users" ,',dirname))))
            (defun ,funsym (system-name)
              (let ((subd (user-subd system-name)))
                (ensure-directories-exist 
                 (merge-pathnames
                  (make-pathname :directory `(:relative "users" ,',dirname ,subd)
                                 :name system-name))))))))

;; (defun ,lalln ()
;;   (mapcar (lambda (p) (pathname-name p))
;;           (directory (,ufilen "*"))))))))
;; (defun list-all-pupils ()
;;   (loop for subd in (directory *pupil-directory*) 
;;      unless (string= (car (last (pathname-directory subd))) "records")
;;      append (loop for pupil in (directory (merge-pathnames "*" subd))
;;                collect (pathname-name pupil))))

(define-user-file-name-function pupil)
(define-user-file-name-function teacher)
(define-user-file-name-function administrator)

(defun record-file (sysname)
  (let ((subd (user-subd sysname)))
    (ensure-directories-exist
     (merge-pathnames
      (make-pathname :directory `(:relative "users" "pupils" "records" ,subd)
                     :name (concatenate 'string sysname "-record"))))))
                                                         

;; macroize





  
  

    
