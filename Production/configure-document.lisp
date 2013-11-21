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

(make-latex-command include-packages 
		    "usepackage" 
		    ((("danish") ("babel"))
		     (("T1") ("fontenc"))
		     (("utf8")("inputenc"))
		     (NIL ("amsmath" "amssymb" "amsthm")) 
		     (NIL ("multicol"))
		     (NIL ("enumerate")) 
		     (("pdftex") ("graphicx"))))

(make-latex-command begin-document 
		    "documentclass"
		    (("12pt" "a4paper") ("article")))

(configure-document 
 ((latex-command (format nil "setlength{~Atabcolsep}{~Dpt}~%"
			 *latex-backslash* *table-column-seperation*))
  (latex-command (format nil "renewcommand{~Aarraystretch}{~,1,,,F}"
			 *latex-backslash* *array-stretch*))))


  
;; \usepackage[T1]{fontenc}
;; \usepackage[utf8]{inputenc} 
