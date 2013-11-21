(defpackage :autotutor 
  (:use :cl)
  (:nicknames :at))

(defpackage :thetamat-common
  (:use :cl)
  (:nicknames :ctmt)
  (:export :*autotutor-name* :*private-key* :*certificate*
           ;;on ALL HOSTS (home servers) in an installation
           :*in-production*
           :*autotutor-server* :*autotutor-server-port*))

(in-package :ctmt)

(defvar *in-production* nil)

(defvar *private-key*)
(defvar *certificate*)

(defvar *autotutor-server*)
(defvar *autotutor-server-port* 4040)

;; frontpage (old name thetamat) 146.185.140.78 
;; loginserver 	146.185.136.74 
;; autotutor 	146.185.136.175


(progn (if *in-production*
           
           (setf *autotutor-server* "a.b.c.d"
                 ;; configure the above with an ip-address
                 *private-key* "ssl/blahblah.key"
                 ;; your private key above
                 *certificate* "ssl/blahblah.pem")
                 ;; your certificate above
           
           (setf *autotutor-server* "localhost"
                 *private-key* "ssl/server-20120805.key" 
                 *certificate* "ssl/server-20120805.crt" )))

(defvar *autotutor-name* "blah.foo.org")

(in-package :autotutor)

(defparameter *latex-backslash* "\\")
(defvar *default-latex-syntax*
  '((plus "+") (minus "-") (times "\\cdot") (divided-by "\\div") (dec ".")))

(defvar *language* 'american-english)
;;can be 'danish 'british-english or 'american-english

(setf (who:html-mode) :html5)
;;(setf ps:*use-strict* t)

(defvar *host* ctmt:*autotutor-server*)
(defvar *port* ctmt:*autotutor-server-port*)

(defvar *default-uri* (format nil "https://~A:~D/" *host* *port*))



