(in-package :autotutor)

(defun login-script ()
  (ps:who-ps-html 
   (:script 
    :type "text/javascript"
    (ps:ps 

      (defvar banner-height 40)
      
      (defun init-banner ()
	(let ((ban (doc-get-el-by-id "banner")))
	  (setf (style-height ban) (strcat banner-height "px"))))

      (setf (dots window onload) (init-banner))))))
