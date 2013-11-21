(in-package :autotutor)

(defvar *current-lesson*)
(defvar *current-namespace*)
(defvar *Xlesson-db* (make-hash-table))

(defclass lesson ()
  ((name :initarg :lesson-name :initform nil :reader lesson-name)
   (namespace :initarg :lesson-namespace :initform nil :accessor lesson-namespace)
   (description :initarg :description :initform nil :accessor lesson-description)
   (stack :initform () :initarg :stack :accessor lesson-stack)
   (global-functions :initform () :initarg :global-functions
                     :accessor lesson-global-functions)
   (global-vars :initform () :initarg :global-vars :accessor lesson-global-vars)
   (closure-forms :initform (make-hash-table) :initarg :closure-forms
                  :accessor lesson-closure-forms)
   (closures :initform (make-hash-table) :accessor lesson-closures)))

(defmacro Xdefine-lesson (name namespace)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-instance 'lesson :lesson-name ',name
                    :lesson-namespace (intern (symbol-name ',namespace) :ps))))

(defmethod initialize-instance :after ((l lesson) &key)
  (setf (lesson-description l) (module-description (module (lesson-name l)))
        (gethash (lesson-name l) *Xlesson-db*) l))

        ;; *current-lesson* (find-lesson (lesson-name l))
        ;; *current-namespace* (lesson-namespace l)))

(defmethod make-load-form ((les lesson) &optional environment)
  (declare (ignore environment))
  (values `(make-instance ',(class-of les)
                          :lesson-name ',(slot-value les 'name)
                          :lesson-namespace ',(slot-value les 'namespace)
                          :description ',(slot-value les 'description)
                          :stack ',(slot-value les 'stack)
                          :global-functions ',(slot-value les 'global-functions)
                          :global-vars ',(slot-value les 'global-vars)
                          :closure-forms ',(slot-value les 'closure-forms))
          `(setf (lesson-closures ',les) ',(slot-value les 'closures))))

          ;; `(loop for key being each hash-key of (lesson-closure-forms ,les)
          ;;       do (setf (gethash key (lesson-closures ,les))
          ;;                (eval (gethash key (lesson-closure-forms ,les)))))))

;; (defmethod make-load-form ((les lesson) &optional environment)
;;   (declare (ignore environment))
;;   (values `(make-instance 'lesson
;;                           :lesson-name (lesson-name ,les)
;;                           :namespace (lesson-namespace ,les)
;;                           :description (lesson-description ,les)
;;                           :stack (lesson-stack ,les)
;;                           :global-functions (lesson-global-functions ,les)
;;                           :global-vars (lesson-global-vars ,les)
;;                           :closure-forms (lesson-closure-forms ,les))
;;           `(loop for key being each hash-key of (lesson-closure-forms ,les)
;;                 do (setf (gethash key (lesson-closures ,les))
;;                          (eval (gethash key (lesson-closure-forms ,les)))))))

(defun find-lesson (name)
  (gethash name *Xlesson-db*))

(defmacro in-lesson (name);; namespace)
  (let ((found (gensym))
        (nsname (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((,found (find-lesson ',name))
              (,nsname (lesson-namespace ,found)))
         ;;(intern (symbol-name ',namespace) :ps)))
         ;; (unless ,found
         ;;   (error "Lesson ~S does not exist." ',name))
         ;; (unless (sc:get-namespace ,nsname)
         ;;   (error "Namespace ~S does not exist." ',namespace))
         (setf *current-lesson* ,found
               *current-namespace* ,nsname)))))

;; (defmacro in-lesson (name namespace)
;;   (let ((found (gensym))
;;         (nsname (gensym)))
;;     `(eval-when (:compile-toplevel :load-toplevel :execute)
;;        (let* ((,found (find-lesson ',name))
;;               (,nsname (intern (symbol-name ',namespace) :ps)))
;;          (unless ,found
;;            (error "Lesson ~S does not exist." ',name))
;;          (unless (sc:get-namespace ,nsname)
;;            (error "Namespace ~S does not exist." ',namespace))
;;          (setf *current-lesson* ,found
;;                *current-namespace* ,nsname)))))


(defmacro exit-current-lesson ()
  (let ((lname (lesson-name *current-lesson*)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (lesson-stack ,*current-lesson*) 
             (nreverse (lesson-stack ,*current-lesson*))
             (gethash ',lname *xlesson-db*) ,*current-lesson*
             (gethash ',lname *lesson-db*)
             (list (lesson-closures ,*current-lesson*)
                   (lesson-global-vars ,*current-lesson*)
                   (lesson-global-functions ,*current-lesson*)
                   (lesson-stack ,*current-lesson*)
                   (sc:process-component ,lname ps:@)
                   (module-description (module ',lname)))
             *current-lesson* nil
             *current-namespace* nil))))

(defmacro define-ifstep (name &key switch html)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sc:define-ps-component
         ,name
         (lambda ()
           (if ,switch
               (funcall teach 
                        ,(tree-to-html html))
               (if (or (not-defined from-direction)
                       (string= from-direction "ltr"))
                   (funcall next-lesson-text)
                   (funcall previous-lesson-text)))) 
       :namespace ,*current-namespace* )
     (push (sc:process-component ,name) (lesson-stack ,*current-lesson*))))

(defmacro define-step (name &key html (typeset nil))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sc:define-ps-component
       ,name
       (lambda ()
         (funcall teach 
                  ,(tree-to-html html)
                  ,(if typeset 'true 'false)))
     :namespace ,*current-namespace*)
     (push (sc:process-component ,name) (lesson-stack ,*current-lesson*))))

(defmacro define-display (name &key code)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sc:define-ps-component
         ,name
         (lambda () ,code) 
       :namespace ,*current-namespace*)
     (push (sc:process-component ,name) (lesson-stack ,*current-lesson*))))

(defmacro define-replace-default-next-step
    (name &key event-handler-id message-text)
  (let* ((cat-name (concatenate 'string "-" (symbol-name (lesson-name
                                                          *current-lesson*)) "-"))
         (spec-prev-name (intern (concatenate 'string (symbol-name
                                                       name) cat-name "SPN") :at))
         (stack-code-name (intern (concatenate 'string (symbol-name
                                                        name) cat-name "SCN") :at)))
    (destructuring-bind (eh id)
        event-handler-id
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (sc:define-ps-component
             ,name
             (lambda (ev)
               (remove-event-no-capture (doc-get-el-by-id "previous-step") "click" 
                                        previous-lesson-text)
               (add-event-no-capture (doc-get-el-by-id "previous-step") "click" 
                                     special-previous-lesson)
               (remove-event-no-capture (doc-get-el-by-id ,id) "click" ,eh)
               (add-event-no-capture (doc-get-el-by-id "next-step") "click" 
                                     next-lesson-text)
               (funcall add-or-remove-wrong-click-nl-handlers ,id false)
               (funcall next-lesson-text))
           :namespace ,*current-namespace*
           :assignable-name (lesson-module ephemeral ,eh))
         (sc:define-ps-component
             ,spec-prev-name
             (lambda (ev)
               (remove-event-no-capture  (doc-get-el-by-id ,id) "click" ,eh)
             (funcall add-or-remove-wrong-click-nl-handlers ,id false)
             (add-event-no-capture (doc-get-el-by-id "next-step") "click"
                                   next-lesson-text)
             (remove-event-no-capture (doc-get-el-by-id "previous-step") "click" 
                                      special-previous-lesson)
             (add-event-no-capture (doc-get-el-by-id "previous-step") "click" 
                                   previous-lesson-text)
             (funcall previous-lesson-text))
         :namespace ,*current-namespace*
         :assignable-name (lesson-module ephemeral special-previous-lesson))
         (sc:define-ps-component
             ,stack-code-name
             (lambda ()
               (cond ((< last-lesson-text current-lesson-text)
                      (remove-event-no-capture (doc-get-el-by-id "previous-step") "click" 
                                               previous-lesson-text)
                      (add-event-no-capture (doc-get-el-by-id "previous-step") "click" 
                                            special-previous-lesson)
                      (remove-event-no-capture (doc-get-el-by-id "next-step") "click"
                                               next-lesson-text)
                      (add-event-no-capture (doc-get-el-by-id ,id) "click" ,eh)
                      (funcall add-or-remove-wrong-click-nl-handlers ,id true)
                      (let ((p (create-element "p"))
                            (tn (make-text-node ,message-text))
                            (ltd (doc-get-el-by-id "lesson-text-display")))
                        (append-child p tn)
                        (append-child ltd p)))
                     (t 
                      (funcall previous-lesson-text))))
           :namespace ,*current-namespace*)
         (push (sc:process-component ,name ps:@) (lesson-global-functions ,*current-lesson*))
         (push (sc:process-component ,spec-prev-name ps:@) (lesson-global-functions ,*current-lesson*))
         (push (sc:process-component ,stack-code-name) (lesson-stack ,*current-lesson*))
         ))))

(defmacro define-globals (name &key name-setter-pairs)
  (let ((code ())
        (string-marker (symbol-name name)))
    (dolist (pair name-setter-pairs)
      (let ((gname (gensym string-marker)))
        (destructuring-bind (vname setter)
            pair
          (push `(progn (sc:define-ps-component 
                            ,gname
                            (lambda () ,setter)
                          :namespace ,*current-namespace*)
                        (push (list ,vname (sc:process-component ,gname))
                              (lesson-global-vars ,*current-lesson*))) code))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(nreverse code))))

(defmacro define-using-q&a-variables (name &key name-didactic-element code)
  (let ((retcode ())
        (fn (gensym "FN"))
        (var-val-pair (gensym "VVP")))
    (destructuring-bind (vname didactic-element)
      name-didactic-element
      (push `(progn
               (sc:define-ps-component
                   ,name
                   (lambda () ,code)
                 :namespace ,*current-namespace*)
               (let* ((,fn (generate-q&a-variable-values-fn 
                            ,vname ',didactic-element))
                      (,var-val-pair (funcall ,fn)))
                 (push (sc:process-component ,name) 
                       (lesson-stack ,*current-lesson*))
                 (setf (lesson-global-vars ,*current-lesson*)
                       (append ,var-val-pair 
                               (lesson-global-vars ,*current-lesson*)))
                 (setf (gethash (intern ,vname)
                                (lesson-closure-forms ,*current-lesson*))
                       `(generate-q&a-variable-values-fn
                         ,,vname (quote ,',didactic-element)))
                 (setf (gethash (intern ,vname) 
                                (lesson-closures ,*current-lesson*)) ,fn)))
            retcode)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@retcode))))

;;Called to ensure new value from ajax-code
(defun different-var-value (varname fn curr)
  (let ((vn (concatenate 'string varname ":q")))
    (loop for i fixnum from 0
       for nv = (funcall fn)
       until (not (string= curr (cadr (assoc vn nv :test #'string=))))
       while (< i 10)
       finally (return nv))))

(defmacro define-y-or-n (name &key y-or-n-spec html (typeset nil))
  (destructuring-bind (rid ytxt ntxt)
      y-or-n-spec
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (sc:define-ps-component
           ,name
           (lambda ()
             (funcall teach 
                      ,(tree-to-html html) 
                      ,(if typeset 'true 'false))
             (funcall y-or-n ,rid ,ytxt ,ntxt))
       :namespace ,*current-namespace*)
       (push (sc:process-component ,name) (lesson-stack ,*current-lesson*)))))

(defmacro define-summary (name &key (first-do nil) html (typeset nil))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sc:define-ps-component
         ,name
         (lambda ()
           (setf in-summary true)
           ,first-do
           (funcall teach 
                    ,(tree-to-html html) 
                    ,(if typeset 'true 'false)))
       :namespace ,*current-namespace*)
     (push (sc:process-component ,name) (lesson-stack ,*current-lesson*))))

(defmacro define-lesson-end (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sc:define-ps-component
         ,name
         (lambda ()
           (funcall complete-lesson))
       :namespace ,*current-namespace*)
     (push (sc:process-component ,name) (lesson-stack ,*current-lesson*))))

  
  
                          
  
