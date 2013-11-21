(in-package :autotutor)

(eval-when (:compile-toplevel :load-toplevel);;  :execute)

  (defparameter *delimiters*
    '((square ("[" "]"))
      (paren ("(" ")"))
      (table (" " " \\\\~%"))
      (dollar ("$" "$"))
      (curly ("{" "}"))))
  
  
  (defun get-opening-delimiter (name)
    (caadr (assoc name *delimiters*)))
  
  (defun get-closing-delimiter (name)
    (cadadr (assoc name *delimiters*)))
  
  (defparameter *seperators*
    '((comma ",")
      (dot ".")
      (ampersand " & ")
      (space " ")
      (plus "+")
      (minus "-")))
  
  (defun get-seperator (key)
    (cadr (assoc key *seperators*)))

  (defparameter *defined-delimiter-lists* nil)

  (defun update-defined-delimiter-lists (name)
    (when (not (find name *defined-delimiter-lists* :test #'equal))
      (push name *defined-delimiter-lists*)))
)

(defmacro def-delimiter-list (delimiter-type seperator)
  (let ((name (intern (concatenate 'string (symbol-name delimiter-type)
				   "-DELIMITER-" (symbol-name seperator)
				   "-LIST") :autotutor))
	(open (get-opening-delimiter delimiter-type))
	(close (get-closing-delimiter delimiter-type))
	(sep (get-seperator seperator)))
    `(progn
       (defun ,name (list-of-options)
	 (with-output-to-string (string)
	   (if (null list-of-options)
	       (format string "")
	       (progn
		 (format string "~A" ,open)
		 (do ((opts (cdr list-of-options) (cdr opts))
		      (curr (car list-of-options) (car opts)))
		     ((null opts) (format string "~A~A" curr ,close))
		   (format string "~A~A" curr ,sep))))))
       (update-defined-delimiter-lists ',name))))

(def-delimiter-list curly comma)
(def-delimiter-list square comma)
(def-delimiter-list table ampersand)
(def-delimiter-list curly space)

;;eg (table-delimiter-ampersand-list (make-list 4 :initial-element "~A"))
