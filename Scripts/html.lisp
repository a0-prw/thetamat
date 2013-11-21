(in-package :autotutor)

(defvar *script-seperator* (string #\Newline))

(defgeneric user-page-scripts (token))

;;FIXME remove spaces
(defmethod user-page-scripts ((token (eql 'pupil)))
  (declare (ignore token))
  (princ (concatenate 
	  'string *script-seperator*
	  ;;(setup-mathjax) *script-seperator*
	  (banner) *script-seperator*
	  (generate-prologue *pupil-script-processor*) *script-seperator*
	  (common-script) *script-seperator*
	  (pupil-script) *script-seperator*
	  )))

(defmethod user-page-scripts ((token (eql 'teacher)))
  (declare (ignore token))
  (princ (concatenate
	  'string *script-seperator*
	  ;;(setup-mathjax) *script-seperator*
	  (banner) *script-seperator*
	  (generate-prologue *teacher-script-processor*) *script-seperator*
	  (common-script) *script-seperator*
	  (teacher-script) *script-seperator*
	  )))

(defgeneric generate-user-page (token))

(defmethod generate-user-page ((token (eql 'teacher)))
  (lambda ()
    (who:with-html-output-to-string 
	(*standard-output* nil :prologue t :indent t) ;;FIXME remove indent
      (:html (:head
	      (:title "")
	      (:link :rel "stylesheet" :type "text/css" :href "/css/base.css" 
                     :media "screen")
              (:link :rel "stylesheet" :type "text/css" :href "/css/print.css" 
                     :media "print")
	      (:link :rel "shortcut icon" :href "/img/favicon.ico")
              (:script :type "text/x-mathjax-config"
                       (who:fmt (ps:ps 
                                  (ps:chain 
                                   ((ps:@ -math-jax -hub -config) 
                                    (ps:create tex2jax 
                                               (ps:create  
                                                inline-math 
                                                (ps:array 
                                                 (ps:array "$" "$") 
                                                 (ps:array "\\(" "\\("))
                                                preview
                                                (ps:array
                                                 "loading"))
                                               te-x
                                               (ps:create extensions
                                                          (ps:array "AMSmath.js"
                                                                    "AMSsymbols.js"))
                                               ))))))
              (:script :type "text/javascript"
                       ;; If you want to serve MathJax from your own server then
                       ;;download it into the www/MathJax/ directory and replace
                       ;;the :src with the commented line below
                       ;;"/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
                       :src "https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"))
	     (:body :id "page-container" 
                    :data-owner ""
                    :data-work-on-screen "false"
	      (:div :id "page"
		    (:div :id "banner"
			  (:h1 :class "bannertext" :id "stdh" "&Theta;MaT")
                          (:h1 :class "bannertext" :id "logout" "Logout"))
		    (:div :id "display-area" (:div :id "content" 
                                                   (:noscript (:div "You must have javascript enabled to use this website."))
				(:div :id "caption")
				(:div :id "graphics")
				(:a :id "pager" :value "Next Exercise" 
                                    :class "clickable" :style "display:none" :onclick  "Javascript:pageNextTable();" 
				    (:div :id "pager-text" "Next Exercise" ))))
                    (:div :class "tpanel" :id "right" 
			  (:h1 "Pupil Information")))
              (:div :id "select-work-area-form"
                    "Work area:"
                    (:input :type "radio" :name "work-area" 
                            :id "work-on-screen" :value "on-screen")
                    "On screen"
                    (:input :type "radio" :name "work-area" 
                            :id "work-off-screen" :value "off-screen")
                    "Off screen")
	      (user-page-scripts token))))))

(defmethod generate-user-page ((token (eql 'pupil)))
  (lambda ()
    (who:with-html-output-to-string 
	(*standard-output* nil :prologue t :indent t) ;;FIXME remove indent
      (:html (:head
	      (:title "") ;;(page-title name))
	      (:link :rel "stylesheet" :type "text/css" :href "/css/base.css"
		     :media "screen")
              (:link :rel "stylesheet" :type "text/css" :href "/css/print.css" 
                     :media "print")
	      (:link :rel "shortcut icon" :href "/img/favicon.ico"))
	     (:body :id "page-container" 
                    :data-owner ""
                    :data-work-on-screen "false"
                    ;;(if (pupil-work-on-screen name)
                    ;;"true" "false")
	      (:div :id "page"
		    (:div :id "banner"
			  (:h1 :class "bannertext" :id "stdh" "&Theta;MaT")
                          (:h1 :class "bannertext" :id "logout" "Logout"))
		    (:div :id "display-area"
			  (:div :id "content"
				(:noscript (:div "You must have javascript enabled to
	      use this website."))
				(:div :id "caption")
				(:div :id "graphics")
				(:a :id "pager" :value "Next Exercise"  :onclick 
				    "Javascript:pageNextTable();" "Next Exercise" )))
                    
		    (:div :class "ppanel" :id "right"))
              (:div :id "select-work-area-form"
                    "Work area:"
                    (:input :type "radio" :name "work-area" 
                            :id "work-on-screen" :value "on-screen")
                    "On screen"
                    (:input :type "radio" :name "work-area" 
                            :id "work-off-screen" :value "off-screen")
                    "Off screen")
	      (user-page-scripts token))))))

(defun standard-login-page ()
  (who:with-html-output-to-string 
      (*standard-output* nil :prologue t :indent t) ;;FIXME remove indent
    (:html :xmlns "http://www.w3.org/1999/xhtml"
	   (:head
	    (:title "Login to Thetamat")
		 (:link :rel "stylesheet" :type "text/css" :href "/css/base.css"
			:media "screen")
		 (:link :rel "shortcut icon" :href "/img/favicon.ico"))
		(:body
		 (:div :id "page"
		       (:div :id "banner"
			     (:h1 :class "bannertext" :id "stdh" "&Theta;MAT"))
		       (:div :id "display-area"
			     (:div 
                              :id "content"
                              (:div :id "caption" "Login")
                              (:div :id "graphics")
                              (:form :method :get
                                     (:div :class "formrow" :id "namerow" 
                                           (:div :class "formcell"
                                                 "Username: ")
                                           (:div :class "formcell"
                                                 (:input :type :text 
                                                         :name "user")))
                                     (:div :class "formrow" :id "passwordrow" 
                                           (:div :class "formcell"
                                                 "Password: ")
                                           (:div :class "formcell"
                                                 (:input :type :password 
                                                         :name "password")))
                                     (:br)
                                     (:div :class "formrow" :id "inputrow" 
                                           (:div :class "formcell"
                                                 :id "emptycell")
                                           (:div :class "formcell"
                                                 (:input :type :submit 
                                                         :value "Enter" 
                                                         :action "login"))))
                              )))
		 (princ (login-script))
		 (princ (banner))))))

(defvar *standard-login-page* (standard-login-page))

(defvar *teacher-page* (funcall (generate-user-page 'teacher)))

(defvar *pupil-page* (funcall (generate-user-page 'pupil)))

;;FIXME rem
(defun redef (which)
  (case which
    ((teacher) (setf *teacher-page* (funcall (generate-user-page 'teacher))))
    ((pupil) (setf *pupil-page* (funcall (generate-user-page 'pupil))))
    (t (error "Unknown which: ~S" which))))
