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
