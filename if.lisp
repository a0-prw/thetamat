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

;;;; WARNING: The hardcopy still "works", but lines run off the page
;;;; sometimes.  I don't remember where the variable is that controls
;;;; linelength
(defun pexfp (name)
  (let ((cycle (pupil-cycle name)))
    (cond ((= cycle 4)
	   (let ((q-file (concatenate 'string name "-q.tex"))
		 (a-file (concatenate 'string name "-a.tex")))
	     (setf (pupil-cycle name) 1)
	     (multiple-value-bind (q a)
		 (layout-document 
		  (make-document name :type 'test))
	       (output-latex-file (latex-path q-file) q)
	       (output-latex-file (latex-path a-file) a))))
	    ;;todo mixed exercise for cycle 2
	  (t (setf (pupil-cycle name) (1+ cycle))
	     (output-latex-file 
	      (latex-path (concatenate 'string name ".tex"))
	      (layout-document 
	       (make-document name :type 'exercise)))))))

(defun update-pupil-modules (system-name)
  (let* ((pmods (pupil-modules system-name))
         (curr (car pmods)))
    (when curr 
      (let ((rst (member curr +modular-progression+)))
        (when rst
          (when (set-difference rst pmods)
            (setf (pupil-modules system-name) rst)))))))
    


	
