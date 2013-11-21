;;;;; I have adapted this file from Martin Loetzsch's ht-simple-ajax.
;;;;; In compliance with the licence, I include the following required
;;;;; notice.  I hereby assert copyright over the derived work,
;;;;; contained in this file, and extend Martin Loetzsch's original
;;;;; licence to the derived work contained in this file.  

;;;;; Copyright (c) 2012 Peter Wood
;;;;; All rights reserved.

;;;;; Copyright (c) 2010, Martin Loetzsch
;;;;; All rights reserved.

;;;;; Redistribution and use in source and binary forms, with or
;;;;; without modification, are permitted provided that the following
;;;;; conditions are met:

;;;;;  Redistributions of source code must retain the above copyright
;;;;;  notice, this list of conditions and the following disclaimer.

;;;;;  Redistributions in binary form must reproduce the above
;;;;;  copyright notice, this list of conditions and the following
;;;;;  disclaimer in the documentation and/or other materials provided
;;;;;  with the distribution.

;;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;;;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
;;;;; THE POSSIBILITY OF SUCH DAMAGE.

(in-package :autotutor)

(defclass ajax-processor ()
  ((lisp-fns 
    :accessor lisp-fns :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "Maps the symbol names of the exported functions to
                    their symbols")
   (js-fns
    :accessor js-fns :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "Maps the symbol names of the exported functions to
                    a javascript code that can call the function from
                    within the client page")
   (server-uri 
    :initarg :server-uri :initform "/ajax" :accessor server-uri
    :type string
    :documentation "The uri which is used to handle ajax request")
   (content-type :initarg :content-type :type string
     :accessor content-type :initform "text/xml; charset=\"utf-8\""
     :documentation "The http content type that is sent with each response")
   (reply-external-format 
    :initarg :reply-external-format :type flexi-streams::external-format
    :accessor reply-external-format :initform tbnl::+utf-8+
    :documentation "The format for the character output stream"))
  (:documentation "Maintains a list of lisp function that can be
                   called from a client page."))

(defclass autotutor-ajax-processor (ajax-processor)
  ((server-uri-length)))

(defmethod initialize-instance :after ((processor autotutor-ajax-processor) &key)
  (setf (slot-value processor 'server-uri-length) 
	   (length (server-uri processor))))

(defclass response-encoding () 
  ((response :initarg :response :reader response))
  (:documentation "Base class for encoding of response from ajax-processor"))

(defclass xml-response-encoding (response-encoding) ())

(defun xml-response (response)
  "Use this in defun-ajax if the reply from the server should be xml."
  (make-instance 'xml-response-encoding :response response))

(defclass json-response-encoding (response-encoding) ())

(defun json-response (response)
  "Use this in defun-ajax if the reply from the server should be json."
  (make-instance 'json-response-encoding :response response))

(defun create-ajax-dispatcher (processor)
  "Creates a hunchentoot dispatcher for an ajax processor"
  (tbnl:create-prefix-dispatcher (server-uri processor)
                            #'(lambda () (call-lisp-function processor))))

(defun make-js-symbol (symbol)
  "helper function for making 'foo_bar_' out of 'foo-bar?' "
  (loop with string = (string-downcase symbol)
     for c across "?-<>"
     do (setf string (substitute #\_ c string))
     finally (return string)))

(defun make-ps-symbol (symbol)
  (loop with string = (symbol-name symbol)
     for c across "?-<>"
     do (setf string (substitute #\_ c string))
     finally (return string)))

;; This is Martin Loetzsch's original.  I prefer to use parenscript 
;; (defmacro defun-ajax (name params (processor response-type) &body body)
;;   "Declares a defun that can be called from a client page.
;; Example: (defun-ajax func1 (arg1 arg2) (*ajax-processor*)
;;    (do-stuff))"
;;   (let ((js-fn (format nil "

;; function ~a (~{~a, ~}callback) {
;;     ajax_call('~a', callback, ~2:*[~{~a~^, ~}]);
;; }" 
;;                        (concatenate 'string "ajax_" (make-js-symbol name))
;;                        (mapcar #'make-js-symbol params)
;;                        name)))
;;     `(progn
;;        (defun ,name ,params (,response-type ,@body))
;;        (setf (gethash (symbol-name ',name) (lisp-fns ,processor)) ',name)
;;        (setf (gethash (symbol-name ',name) (js-fns ,processor)) ',js-fn))))

(defmacro defun-ajax (name params (processor response-type) &body body)
  "Declares a defun that can be called from a client page.
Example: (defun-ajax func1 (arg1 arg2) (*ajax-processor*)
   (do-stuff))"
  (let* ((fname (intern 
                 (concatenate 'string "AJAX_" (make-ps-symbol name))))
         (fparams (mapcar 
                   (lambda (p) (intern 
                                (make-ps-symbol p))) params))
         (js-fn (ps:ps 
                  (ps:lisp
                     `(defun ,fname (,@fparams callback)
                        (ajax_call ,(symbol-name name) callback (ps:array ,@fparams)))))))
    `(progn
       (defun ,name ,params (,response-type ,@body))
       (setf (gethash (symbol-name ',name) (lisp-fns ,processor)) ',name)
       (setf (gethash (symbol-name ',name) (js-fns ,processor)) ',js-fn))))
;; js-fn is a string - why is he quoting it?? -prw

(defmacro defun-shared-ajax (name params (processor-list response-type) &body body)
  "Like defun-ajax, except the javascript functions can be in different script processors, while sharing the same lisp function (only defined once)."
  (let* ((lvar (gensym))
         (pl (gensym))
         (fname (intern 
                 (concatenate 'string "AJAX_" (make-ps-symbol name))))
         (fparams (mapcar 
                   (lambda (p) (intern 
                                (make-ps-symbol p))) params))
         (js-fn (ps:ps 
                  (ps:lisp
                     `(defun ,fname (,@fparams callback)
                        (ajax_call ,(symbol-name name) callback (ps:array ,@fparams)))))))
    `(let ((,pl (list ,@processor-list)))
       (defun ,name ,params (,response-type ,@body))
       (dolist (,lvar ,pl)
         (setf (gethash (symbol-name ',name) (lisp-fns ,lvar)) ',name)
         (setf (gethash (symbol-name ',name) (js-fns ,lvar)) ',js-fn)))))

(defgeneric generate-prologue (processor))

(defmethod generate-prologue ((processor ajax-processor))
  "Creates a <script> ... </script> html element that contains all the
   client-side javascript code for the ajax communication. Include this 
   script in the <head> </head> of each html page"
  (concatenate 
   'string 
   "<script type='text/javascript'>"
   (with-output-to-string (string-stream)
     (ps:ps-to-stream string-stream       
       (defun fetch-u-r-i (uri callback)
              (let ((request (ps:new (-x-m-l-http-request))))
          (if (not request)
              (alert "The browser couldn't make a request object.")
              (progn ((ps:@ request open) "GET" uri true)
                     (setf (ps:@ request onreadystatechange)
                           (lambda ()
                             (when (not (= (ps:@ request ready-state) 4))
                               ;;(return null))
                               return)
                             (if (or (and (>= (ps:@ request status) 200)
                                          (< (ps:@ request status) 300))
                                     (= (ps:@ request status) 304))
                                 (let ((data (or (ps:@ request response-x-m-l)
                                                 (ps:@ request response-text))))
                                   (if callback (funcall callback data)
                                       (alert (concatenate 
                                               'string
                                               "Error while fetching URI "
                                               uri)))))))
                     ((ps:@ request send) null)
                     (delete request)
                    ))))
       (defun ajax_call (func callback args)
         (let ((uri (concatenate 
                     'string
                     (ps:lisp (server-uri processor))
                     "/"
                     (encode-u-r-i-component func)
                     "/")))
           (when (> (ps:@ args length) 0)
             (setf uri (concatenate 'string uri "?")))
           (loop for i from 0 below (ps:@ args length)
              do (progn (when (> i 0) (setf uri (concatenate 'string uri "&")))
                        (setf uri (concatenate 'string uri "arg" i "="
                                               (encode-u-r-i-component
                                                (aref args i))))))
           (fetch-u-r-i uri callback))))
     (format string-stream "~{~A~}" 
             (loop for js being the hash-values of (js-fns processor)
                collect js)))
   "</script>"))

;; (defmethod generate-prologue ((processor ajax-processor))
;;   "Creates a <script> ... </script> html element that contains all the
;;    client-side javascript code for the ajax communication. Include this 
;;    script in the <head> </head> of each html page"
;;   (apply #'concatenate 'string
;;          `("<script type='text/javascript'>
;; //<![CDATA[

;; function fetchURI(uri, callback) {
;;   var request;
;;   if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
;;   else {
;;     try { request = new ActiveXObject(\"Msxml2.XMLHTTP\"); } catch (e) {
;;       try { request = new ActiveXObject(\"Microsoft.XMLHTTP\"); } catch (ee) {
;;         request = null;
;;       }}}
;;   if (!request) alert(\"Browser couldn't make a request object.\");

;;   request.open('GET', uri, true);
;;   request.onreadystatechange = function() {
;;     if (request.readyState != 4) return;
;;     if (((request.status>=200) && (request.status<300)) || (request.status == 304)) {
;;       var data = request.responseXML;
;;       data == null ? (data = request.responseText) : null;
;;       if (callback!=null) { callback(data); }
;;     }
;;     else { 
;;       alert('Error while fetching URI ' + uri);
;;     }
;;   }
;;   request.send(null);
;;   delete request;
;; }

;; function ajax_call(func, callback, args) {
;;   var uri = '" ,(server-uri processor) "/' + encodeURIComponent(func) + '/';
;;   var i;
;;   if (args.length > 0) {
;;     uri += '?'
;;     for (i = 0; i < args.length; ++i) {
;;       if (i > 0) { uri += '&' };
;;       uri += 'arg' + i + '=' + encodeURIComponent(args[i]);
;;     }
;;   }
;;   fetchURI(uri, callback);
;; }
;; "
;;   ,@(loop for js being the hash-values of (js-fns processor)
;;        collect js)
;;   "
;; //]]>
;; </script>")))

(defgeneric server-uri-length (processor))

(defmethod server-uri-length ((processor ajax-processor))
  (length (server-uri processor)))

(defmethod server-uri-length ((processor autotutor-ajax-processor))
  (slot-value processor 'server-uri-length))

(defgeneric call-lisp-function (processor))

(defgeneric encode-processor-response (processor response))

(defmethod encode-processor-response ((processor ajax-processor) (response t))
  (concatenate 'string "<?xml version=\"1.0\"?>
<response xmlns='http://www.w3.org/1999/xhtml'>"
	       response "</response>"))

;;;; Using json-response-encoding means you don't have to produce
;;;; strings in defun-ajax defined functions.  Ie, (apply fn args)
;;;; does not have to make a string, which it does if you are just
;;;; using xml-encoding.  
(defmethod encode-processor-response 
    ((processor autotutor-ajax-processor) (response xml-response-encoding))
  (concatenate 'string "<?xml version=\"1.0\"?>
 <response xmlns='http://www.w3.org/1999/xhtml'>"
               (response response) "</response>"))

(defmethod st-json:write-json-element ((element symbol) stream)
  (if (null element) 
      (write-string "[]" stream)
      (let ((name (symbol-name element)))
       (cond ((string= name "T") (write-string "true" stream))
	     ((eql element :false) (write-string "false" stream))
	     ((or (eql element :null)
		  (eql element :undef)) (write-string "null" stream))
	     (t (format stream "~S" name))))))

(defmethod encode-processor-response 
    ((processor autotutor-ajax-processor) (response json-response-encoding))
  (setf (tbnl:content-type* tbnl:*reply*) "application/json")
  (with-output-to-string (sm) 
    (st-json:write-json (response response) sm)))

(defmethod call-lisp-function ((processor ajax-processor))
  "This is called from hunchentoot on each ajax request. It parses the 
   parameters from the http request, calls the lisp function and returns
   the response."
  (let* ((fn-name (string-trim "/" (subseq (tbnl:script-name* tbnl:*request*)
                                           (server-uri-length processor))))
         (fn (gethash fn-name (lisp-fns processor)))
         (args (mapcar #'cdr (tbnl:get-parameters* tbnl:*request*))))
    (unless fn
      (error "Error in call-lisp-function: no such function: ~A" fn-name))
    
    (setf (tbnl:reply-external-format*) (reply-external-format processor))
    (setf (tbnl:content-type*) (content-type processor))
    (tbnl:no-cache)
    (encode-processor-response processor (apply fn args))))
;;;;  This next code fragment is the default method of ht-simple-ajax
;;;;  and is still used if you just use xml-response in defun-ajax
;;     (concatenate 'string "<?xml version=\"1.0\"?>
;; <response xmlns='http://www.w3.org/1999/xhtml'>"
;;                  (apply fn args) "</response>")))

(defvar *pupil-script-processor*
  (make-instance 'autotutor-ajax-processor :server-uri "/pajax"))

(defvar *teacher-script-processor*
  (make-instance 'autotutor-ajax-processor :server-uri  "/tajax"))

