(in-package :autotutor)

(defvar *lesson-db*
  (make-hash-table))

;;teach() is like communicate() but in a display with right and left arrows                     

(defun gather-globals (tree)
  "This function crawls the TREE looking for global (relative to the
lesson script) variables: A variable is a triple (:variable varname
code).  The CODE is a list will be processed by parenscript to produce
a string which can be compiled by the browser's javascript
implementation into a javascript function which will _set_ the variable
VARNAME when it is called."
  (labels ((crawl (ll ret)
             (cond ((null ll) ret)
                   
                   ((atom ll) nil)
                   ((eql (car ll) :variable)
                    (crawl (cadddr ll) 
                           (cons (list (cadr ll) (ps:ps 
                                                   (ps:lisp 
                                                    `(lambda ()
                                                       (with-lesson-module-slots 
                                                         ,(caddr ll)))))                                                ) ret)))
                   (t  (append (crawl (car ll) ret) (crawl (cdr ll) ret))))))
    (crawl tree ())))

;; Example:

;; (gather-globals '((:p "foo bar") (:variable "foo" (lambda () (numberline-id (random-int -10 10))))))
;;  ==>
;; (("foo" "(function () {
;;     return numberlineId(randomInt(-10, 10));
;; });"))


(defun gather-q&a-variables (list hashtable)
  "This function crawls the LIST looking for q&a variables: A q&a
variable is a triple (:q&a-var varname didactic-element).  The return
value is a list of pairs of question and answer variables derived from
VARNAMEs.  They look like ((VARNAME:q question) (VARNAME:a answer)).
A function 'B' is generated from the DIDACTIC-ELEMENT, which when
called will list 2 new pairs of question and answer values . 'B' is
stored in the HASHTABLE under the symbol (intern VARNAME) and gets
called when the browser wants to set new values for the variable in
the lesson in which the variable is defined.  The answer sent to the
browser when it requests SET-LESSON is a json response consisting of
[[VARNAME:q, question], [VARNAME:a, answer]]."
  (labels ((crawl (ll ret)
               (cond ((null ll) ret)
                     ((atom ll) nil)
                     ((eql (car ll) :q&a-var)
                      (let* ((rest (cadddr ll))
                             (varname (cadr ll))
                             (didel (caddr ll))
                             (fn (generate-q&a-variable-values-fn varname didel)))
                        (setf (gethash (intern varname) hashtable) fn)
                        (crawl rest  
                             (cons (list varname (funcall fn)) ret))))
                     (t  (append (crawl (car ll) ret) (crawl (cdr ll) ret))))))
      (crawl list ())))

(defun generate-q&a-variable-values-fn (name did-el)
  (let* ((d (get-didactic-element did-el))
         (action (if (didactic-element-da d)
                     (action d) (funcall (action d)))))
    (when (not d)
        (error 'didactic-element-does-not-exist :name did-el))
    (cond ((eql did-el 'digsubn)
           (lambda ()
             (labels ((again ()
                        (let* ((q&a (list-strings-q&a (funcall action)))
                               (q (car q&a))
                               (ts (cl-ppcre:split "\\s" q))
                               (t2 (caddr ts)))
                          (if (string= t2 "0") ;;ok we know this only returns ints
                              (again)
                              (list (list (format nil "~A:q" name) (car q&a))
                                    (list (format nil "~A:a" name) (cadr q&a)))))))
             (again))))
          (t (lambda ()
               (let ((q&a (list-strings-q&a (funcall action))))
                 (list (list (format nil "~A:q" name) (car q&a))
                       (list (format nil "~A:a" name) (cadr q&a)))))))))

(defun list-lesson-script-variables (closures)
  (reduce #'append (mapcar #'funcall closures)))


;; (defun say (step html)
;;   (ps:ps (ps:lisp `(lambda ()
;;                      (with-lesson-module-slots
;;                        (let ((step ,step))
;;                          (funcall teach 
;;                                   ,(who::string-list-to-string
;;                                     (who::tree-to-template
;;                                      html)) )))))))

(defun sayif (step switch html)
  (ps:ps (ps:lisp `(lambda ()
                     (with-lesson-module-slots
                         (if ,switch
                             (let ((step ,step))
                               (funcall teach 
                                        ,(who::string-list-to-string
                                          (who::tree-to-template
                                           html)) 'false))
                             (if (or (not-defined from-direction)
                                     (string= from-direction "ltr"))
                                 (funcall next-lesson-text)
                                 (funcall previous-lesson-text))))))))

(defun say-with-y-or-n (step yorn-spec html typeset)
  (destructuring-bind (rid ytxt ntxt next)
      yorn-spec
    (ps:ps (ps:lisp `(lambda ()
                       (with-lesson-module-slots
                           (let ((step ,step))
                             (funcall teach
                                      ,(who::string-list-to-string
                                        (who::tree-to-template
                                           html)) ,(if typeset 'true 'false)))
                         (funcall y-or-n ,rid ,ytxt ,ntxt ,next 
                                  ,(if typeset 'true 'false))))))))
  
(defun say (step html typeset)
  (ps:ps (ps:lisp `(lambda ()
                     (with-lesson-module-slots
                       (let ((step ,step))
                         (funcall teach 
                                  ,(who::string-list-to-string
                                    (who::tree-to-template
                                     html)) ,(if typeset 'true 'false))))))))

;; (defun say (step html typeset)
;;   (ps:ps (ps:lisp `(lambda ()
;;                      (with-lesson-module-slots
;;                        (let ((step ,step))
;;                          (funcall teach 
;;                                   ,(who::string-list-to-string
;;                                     (who::tree-to-template
;;                                      html)) ,(if typeset 'true 'false))))))))

(defun summary (step first-do html typeset)
  (ps:ps (ps:lisp `(lambda ()
                     (with-lesson-module-slots
                         (setf in-summary true)
                         ,first-do
                         (let ((step ,step))
                           (funcall teach 
                                    ,(who::string-list-to-string
                                      (who::tree-to-template
                                       html)) ,(if typeset 'true 'false))))))))
  

;; (defun varval (step varname)
;;   (ps:ps (lambda ()
;;            (let ((att (strcat "step" (ps:lisp step) ":" (ps:lisp varname))))
;;              (get-attribute (doc-get-el-by-id "lesson-text-display") att)))))

(defun current-q (varname modname)
  (let* ((globals (cadr (gethash modname *lesson-db*)))
        (found
         (cadr (assoc 
                (concatenate 'string varname ":q") globals :test #'string=))))
    found))

(defun different-var-value (varname fn curr)
  (let ((vn (concatenate 'string varname ":q")))
    (loop for i fixnum from 0
       for nv = (funcall fn)
       until (not (string= curr (cadr (assoc vn nv :test #'string=))))
       while (< i 10)
       finally (return nv))))

(defmacro with-event-handler (evh stackfn)
  (let ((cevh (gensym))
        (cstackfn (gensym)))
    `(let ((,cevh (ps:ps (ps:lisp ,evh)))
           (,cstackfn (ps:ps (ps:lisp ,stackfn))))
       (list ,cevh ,cstackfn))))

;;evh: car =  name, cadr = new-handler-id, 
;;message-text = text to display in lesson step.

(defun continue-click-handler (evh message-text)
  (let* ((evcode `(setf (dots lesson-module ephemeral ,(ps:lisp (car evh)))
                        (lambda (ev)
                          (with-lesson-module-slots
                              (remove-event-no-capture (doc-get-el-by-id "previous-step")
                                                       "click" previous-lesson-text)
                            (add-event-no-capture (doc-get-el-by-id "previous-step")
                                                  "click" special-previous-lesson)
                            (remove-event-no-capture (doc-get-el-by-id ,(ps:lisp (cadr evh)))
                                                     "click" ,(ps:lisp (car evh)))
                            (add-event-no-capture (doc-get-el-by-id "next-step")
                                                  "click" next-lesson-text)
                            (funcall add-or-remove-wrong-click-nl-handlers ,(ps:lisp (cadr evh)) false)
                            (funcall next-lesson-text)))))
         (spec-prev-code `(setf (dots lesson-module ephemeral  special-previous-lesson)
                                (lambda (ev)
                                  (with-lesson-module-slots
                                      (remove-event-no-capture 
                                       (doc-get-el-by-id ,(ps:lisp (cadr evh)))
                                       "click" ,(ps:lisp (car evh)))
                                    (funcall add-or-remove-wrong-click-nl-handlers ,(ps:lisp (cadr evh)) false)
                                    (add-event-no-capture 
                                     (doc-get-el-by-id "next-step")
                                     "click" next-lesson-text)
                                    (remove-event-no-capture 
                                     (doc-get-el-by-id "previous-step")
                                     "click" special-previous-lesson)
                                    (add-event-no-capture 
                                     (doc-get-el-by-id "previous-step")
                                     "click" previous-lesson-text)
                                    (funcall previous-lesson-text)))))
         (spec-prevh (ps:ps (ps:lisp spec-prev-code)))
         (cevh (ps:ps (ps:lisp evcode)))
         (stckcode `(lambda ()
                      (with-lesson-module-slots 
                      (cond ((< last-lesson-text current-lesson-text)
                             (remove-event-no-capture 
                              (doc-get-el-by-id "previous-step")
                              "click" previous-lesson-text)
                             (add-event-no-capture
                              (doc-get-el-by-id "previous-step")
                              "click" special-previous-lesson)
                             (remove-event-no-capture
                              (doc-get-el-by-id "next-step") "click"
                              next-lesson-text)
                             (add-event-no-capture
                              (doc-get-el-by-id ,(ps:lisp (cadr evh)))
                              "click"
                              ,(ps:lisp (car evh)))
                             (funcall add-or-remove-wrong-click-nl-handlers ,(ps:lisp (cadr evh)) true)
                             (let ((p (create-element "p"))
                                   (tn (make-text-node 
                                        ,(ps:lisp message-text)))
                                   (ltd (doc-get-el-by-id 
                                         "lesson-text-display")))
                               (append-child p tn)
                               (append-child ltd p)))
                            (t 
                             (funcall previous-lesson-text))))))
           (cstackfn
            (ps:ps (ps:lisp stckcode))))
    (list `(,cevh ,spec-prevh) cstackfn)))

(defmacro define-lesson (module-name &rest lesson)
  (let ((sequential (group lesson 2)) ;;(gensym "SEQUENTIAL"))
        (closures (gensym "CLOSURES"))
        (globalvars (gensym "GLOBALVARS"))
        (globalfns (gensym "GLOBALFNS"))
        (stack (gensym "STACK"))
        (count (gensym "COUNT"))
        (type (gensym "TYPE"))
        (content (gensym "CONTENT"))
        (q&a-vars (gensym "Q&A-VARS"))
        (script-gen (gensym "SCRIPT-GEN"))
        ;;(cls (gensym "CLS"))
        (code (gensym "CODE"))
        (fns (gensym "FNS")))
        ;;(cevh (gensym "CEVH"))
        ;;(cstackfn (gensym "CSTACKFN")))
    `(let ((,closures (make-hash-table))
           (,script-gen (get ',module-name 'script-generator))
           (,globalvars ())
           (,globalfns ())
           (,stack ())
           (,count 0))
       (if ,script-gen
           (setf ,script-gen (funcall ,script-gen))
           "")
       (dolist (item ',sequential) 
         (let ((,type (car item))
               (,content (cadr item)))
           ;;(incf count)
           (case ,type
             (:globals (setf ,globalvars 
                             (append (gather-globals ,content) ,globalvars)))
             (:step (setf ,stack 
                       (push (say ,count ,content nil) ,stack)))
             (:stepj (setf ,stack 
                           (push (say ,count ,content t) ,stack)))
             (:stepif (setf ,stack 
                           (push (sayif ,count (car ,content)
                                        (cadr ,content)) ,stack)))
             (:y-or-n (setf ,stack
                            (push (say-with-y-or-n ,count (car ,content)
                                                   (cadr ,content) nil) ,stack)))
             (:y-or-nj (setf ,stack
                            (push (say-with-y-or-n ,count (car ,content)
                                                   (cadr ,content) t) ,stack)))
             (:summary (setf ,stack
                             (push (summary ,count (car ,content)
                                            (cadr ,content) nil) ,stack)))
             ;; (:q&r (setf ,stack
             ;;                   ,stack))
             (:using-q&a-vars
              (let* ((,q&a-vars (gather-q&a-variables ,content ,closures))
                     (,code (cadr ,content)))
                (dolist (var ,q&a-vars)
                  (setf ,globalvars
                        (append (cadr var) ,globalvars)))
                (setf ,stack 
                      (push (ps:ps (ps:lisp 
                                    `(lambda ()
                                       (with-lesson-module-slots 
                                           ,,code)))) ,stack))))
             (:replace-default-next-step
              (let ((,fns (continue-click-handler (car ,content) (cadr ,content))))
                (setf ,globalfns (append (car ,fns) ,globalfns)
                      ,stack (push (cadr ,fns) ,stack))))           
             (:display (setf ,stack
                             (push (ps:ps 
                                      (ps:lisp 
                                       `(lambda ()
                                          (with-lesson-module-slots 
                                            ,,content)))) ,stack)))
             ;; (:display (setf ,stack
             ;;                 (push (ps:ps 
             ;;                          (ps:lisp 
             ;;                           `(lambda () ,,content))) ,stack)))
             (:complete? (setf ,stack
                               (push (ps:ps 
                                       (lambda () 
                                         (with-lesson-module-slots 
                                           (funcall complete-lesson)))) ,stack)))
             (t nil))
           (incf ,count)))
       (setf ,stack (nreverse ,stack))
       (setf (gethash ',module-name *lesson-db*) `(,,closures ,,globalvars ,,globalfns ,,stack ,,script-gen ,,(module-description (module module-name))))
       )))

