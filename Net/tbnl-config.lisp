(in-package :autotutor)

(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))

(defparameter tbnl:reply-external-format 
  (flex:make-external-format :utf-8 :eol-style :lf))

(setf ;; parenscript might get confused with lisp strings
      ps:*js-string-delimiter* #\"
      tbnl:*hunchentoot-default-external-format* *utf-8*
      ;; (flex:make-external-format :utf-8 :eol-style :lf)
      tbnl:*default-content-type* "text/html; charset=utf-8"
      tbnl:*rewrite-for-session-urls* nil)

(defun user-logout (&optional user)
  (declare (ignore user))
  ;;(log-request tbnl:*request* "user-logout")
  (let* ((req (when (boundp 'tbnl:*request*) (symbol-value 'tbnl:*request*)))
         (session (when req (tbnl:session-verify req))))
    (when session (tbnl:remove-session session))
    (tbnl:set-cookie "tmpublic" 
                     :secure t 
                     :http-only t 
                     :expires #.(encode-universal-time 0 0 0 1 1 1970 ))
    :false))

(define-redirectfn
    redirect-login ()
  (st-json:jso "redirect" 
               #.(format nil "https://~A/login/" 
                         ctmt::*autotutor-name*))
  (st-json:jso "redirect" 
               #.(format nil "https://~A:~A/login/" *host* *port*)))

(define-redirectfn
    redirect-cancelled ()
  #.(format nil "https://~A/login/" ctmt::*autotutor-name*)
  #.(format nil "https://~A:~A/login/" *host* *port*)
  )

(define-redirectfn
    redirect-reenabled (name)
  (format nil "https://~A/~A" #.ctmt::*autotutor-name* name)
  (format nil "https://~A:~A/~A" *host* *port* name))

(define-redirectfn
    redirect-user (user)
  (tbnl:redirect (uri-for-name user) 
                 :host #.ctmt::*autotutor-name*
                 :protocol :https)
  (tbnl:redirect (uri-for-name user)
                 :host *host*
                 :port *port*
                 :protocol :https))

(defun session-okayed (request)
  (let* ((salt (tbnl:session-value 'salt))
         (secret-handshake (tbnl:session-value 'secret-handshake))
         (cookie (tbnl:cookie-in "tmpublic" request))
         (pubpass (when cookie (tbnl:url-decode cookie))))
    (cond ((and pubpass salt secret-handshake 
                (equalp secret-handshake 
                        (crypto:digest-sequence 
                         :md5 (concatenate ;;FIXME md5 not secure
                               '(vector (unsigned-byte 8))
                               salt
                               (flexi-streams:string-to-octets
                                pubpass))))) t)
          (t (tbnl:remove-session tbnl:*session*)
             nil))))

(defun authorize-new (user password)
  (multiple-value-bind (rslist found)
      (local-user-register 'query user nil)
    (when found
      (unless (cadr rslist)
        (error 'user-has-no-password :name user))
      (destructuring-bind (role (salt salted))
          rslist
        (if (equalp salted (make-salted salt password))
          ;;ok, they're in.
            (let* ((pubpass (suggest-password))
                   (secret-handshake (crypto:digest-sequence 
                                      :md5 (concatenate ;;FIXME md5 not secure
                                            '(vector (unsigned-byte 8))
                                            salt
                                            (flexi-streams:string-to-octets
                                             pubpass)))))
              (tbnl:start-session)
              (setf (tbnl:session-max-time tbnl:*session*) 3600
                    (tbnl:session-value 'role) role
                    (tbnl:session-value 'name) user
                    (tbnl:session-value 'salt) salt
                    (tbnl:session-value 'secret-handshake) secret-handshake)
              (tbnl:set-cookie "tmpublic" :secure t :http-only t :value 
                               (tbnl:url-encode ;;Unnecessary fixme
                                ;;(flexi-streams:octets-to-string 
                                pubpass))
              t)
            nil)))))

(defun handle-login (req)
  (let ((user (tbnl:parameter "user" req))
	(password (tbnl:parameter "password" req))
        (session (tbnl:session-verify req)))
    (when session (tbnl:remove-session session))
    ;;(log-request req)
    (tbnl:set-cookie "tmpublic" :secure t :http-only t :expires #.(encode-universal-time 0 0 0 1 1 1970 ))
    (if (and user password)
        (let ((ok (authorize-new user password)))
          (if ok
              (redirect-user user)
              ;;(standard-login-page)))
              *standard-login-page*))
        ;;(standard-login-page))))
        *standard-login-page*)))


(defparameter *server* nil)

(defclass tm-acceptor (tbnl:acceptor) () )
(defclass tm-ssl-acceptor (tbnl:ssl-acceptor) ())

(defun make-server (&key (ssl nil) (address *host*)) ;;"localhost"))
  (let ((server (if (not ssl)
		    (make-instance 
		     'tm-acceptor 
                     :address address ;;FIXME
		     :document-root (merge-pathnames "www/")
                     :error-template-directory (merge-pathnames "www/errors/")
		     :port *port*)
		    (make-instance 
		     'tm-ssl-acceptor
                     :address address
		     :ssl-certificate-file #.ctmt:*certificate* 
                     ;;"ssl/server-20120805.crt"
		     :ssl-privatekey-file #.ctmt:*private-key*
                     ;;"ssl/server-20120805.key"
		     :document-root (merge-pathnames "www/")
                     :error-template-directory (merge-pathnames "www/errors/")
		     :port *port*))))
    (setf *server* server)
    (defun start ()
      (tbnl:start server))
    (defun stop ()
      (tbnl:stop server :soft t))
  *server*))


(defmacro define-handle-static (name mime-type)
  `(defun ,name (acc req)
     (let ((doc-root (namestring (tbnl:acceptor-document-root acc)))
	   (request-path (tbnl:request-pathname req)))
       (when (null request-path)
	 (setf (tbnl:return-code*) tbnl:+http-forbidden+)
	 (tbnl:abort-request-handler))
       (tbnl:handle-static-file 
	(merge-pathnames request-path doc-root) ,mime-type))))

(define-handle-static handle-css "text/css")
(define-handle-static handle-jpeg "img/jpeg")
(define-handle-static handle-mathjax "text/javascript")

(defun handle-text (acc req)
  (let ((doc-root (namestring (tbnl:acceptor-document-root acc)))
        (request-path (tbnl:request-pathname req)))
    (when (null request-path)
      (setf (tbnl:return-code*) tbnl:+http-forbidden+)
      (tbnl:abort-request-handler))
    (setf (tbnl:header-out :content-disposition) "attachment") 
    (tbnl:handle-static-file 
     (merge-pathnames request-path doc-root) "text/plain; charset=utf-8")))

(defmethod tbnl:acceptor-dispatch-request 
    ((acc tm-ssl-acceptor) (req tbnl:request))
  (setf *package* (find-package :autotutor))
  ;;(setf (tbnl:header-out 'x-frame-options) "SAMEORIGIN")
  (let* ((uri (tbnl:request-uri req))
         (split (split-sequence:split-sequence #\/ uri))
         (base-string (cadr split))
	 (base (intern base-string))
         (session (tbnl:session-verify req)))
    (cond ((eql base '|MathJax|) (handle-mathjax acc req))
	  ((eql base '|css|) (handle-css acc req))
	  ((eql base '|img|) (handle-jpeg acc req))
          ((eql base '|examples|) (handle-text acc req))
          ((or (eql base '|login|)
               (not session))
           (if (or (eql base '|pajax|)
                   (eql base '|tajax|))
               ;;ajax request - need to send redirection to callback fn,
               ;;not return a page
               (st-json:write-json-to-string (redirect-login))
               (handle-login req)))
	  (t ;;We can count on a not-stale, non-null session 
           (if (session-okayed req) ;;which also passed cookie/session test
               (let ((role (tbnl:session-value 'role))
                     (name (tbnl:session-value 'name)))
                 ;; The lisp function which gets called from ajax
                 ;; 'function' is wrapped in with-session-name which
                 ;; tests for name / session name agreement
                 (cond ((eql base '|pajax|)
                        (if  (not (eql role 'pupil))
                             (handle-login req)
                             (call-lisp-function *pupil-script-processor*)))
                       ((eql base '|tajax|)
                        (if (not (eql role 'teacher))
                            (handle-login req)
                            (call-lisp-function *teacher-script-processor*)))
                       (t (cond ((not (string= base-string name))
                                 (handle-login req))
                                (t 
                                 (cond ((eql role 'teacher)
                                        *teacher-page*)
                                       ((eql role 'pupil)
                                        *pupil-page*)
                                       (t (handle-login req))))))))
               (progn ;;(log-request req "BOTTOM")
                 (handle-login req)))))))

;; (defun log-request (req id)
;;   (let ((name (concatenate 'string (symbol-name (gensym "REQ")) ".log")))
;;     (with-open-file (str name
;;                          :direction :output
;;                          :if-exists :supersede
;;                          :if-does-not-exist :create)
;;       (format str "ID: ~S~%
;;                    request-acceptor: ~S~%
;;                    headers-in: ~S~%
;;                    request-method: ~S~%
;;                    request-uri: ~S~%
;;                    server-protocol: ~S~%
;;                    local-addr: ~S~%
;;                    local-port: ~S~%
;;                    remote-addr: ~S~%
;;                    remote-port: ~S~%
;;                    cookies-in: ~S~%
;;                    get-parameter: ~S~%
;;                    post-parameters: ~S~%
;;                    script-name: ~S~%
;;                    query-string: ~S~%
;;                    session: ~S~%
;;                    session-verify: ~S~%
;;                    session-value 'NAME: ~S~%"
;;               id
;;               (tbnl:request-acceptor req) 
;;               (tbnl:headers-in* req)       
;;               (tbnl:request-method* req)   
;;               (tbnl:request-uri* req)      
;;               (tbnl:server-protocol* req)  
;;               (tbnl:local-addr* req)       
;;               (tbnl:local-port* req)       
;;               (tbnl:remote-addr* req)      
;;               (tbnl:remote-port* req)      
;;               (tbnl:cookies-in* req)       
;;               (tbnl:get-parameters* req)    
;;               (tbnl:post-parameters* req)   
;;               (tbnl:script-name* req)      
;;               (tbnl:query-string* req)     
;;               (tbnl:session req)
;;               (tbnl:session-verify req)
;;               (tbnl:session-value 'name)))))

