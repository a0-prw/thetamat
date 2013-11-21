(in-package :autotutor)

;;(defvar +test-group+ nil)

(defvar *default-empty-groups* '((|Top|) (|_top_|)))

;; (defun top-group (teacher)
;;   "If the teacher has not made any groups use this."
;;   (append (make-group "All pupils") (teacher-pupils teacher)))

(defun top-group ()
  (make-group "All groups"))

(defun make-group (name &optional group)
  (if group
      (if (find-group name group)
	  (error 'group-exists :name name)
	  (append group (list (make-group name))))
      (list (intern name))))

(defun find-group (name groups)
  (if (null groups) 
      nil
      (let ((curr (car groups)))
	(if (symbolp curr)
	    (or (when (eql curr (intern name))
		  groups)
		(find-group name (cdr groups)))
	    (let ((subgroup? (symbolp (car curr))))
	      (if subgroup?
		  (or (find-group name curr)
		      (find-group name (cdr groups)))
		  (find-group name (cdr groups))))))))

(defun sysn-fn (system-name)
  (list system-name 
	(format nil "~A ~A" (first-name system-name) (surname system-name))))

(defun add-to-group (name groupname groups)
  (let ((old (find-group groupname groups)))
    (unless old (error 'group-does-not-exist :name groupname))
    (let ((new (append old (list (sysn-fn name)))))
      (subst new old groups :test #'equal))))

(defun update-group (teacher names groupname)
  (let ((groups (teacher-groups teacher))
	(pupils (teacher-pupils teacher)))
    (labels ((name-map (sysname)
	       (let ((found (find sysname pupils :test #'string= :key #'car)))
		 (unless found (error 'user-does-not-exist :name sysname))
		 found)))
      (let* ((old (find-group groupname groups))
	     (next-level (next-level old)))
	(setf (teacher-groups teacher)
	      (subst (append 
		      (make-group groupname) 
		      (mapcar #'name-map names)
		      next-level) old groups))
	))))

(defun make-subgroup (newname undername groups)
  (let* ((old (find-group undername groups))
	 (newg (make-group newname old))
	 (keyval (cons old newg)))
    (sublis (list keyval) groups :test #'equal)))

(defun group-p (x)
  (and (consp x)
       (symbolp (car x))))

(defun group-name (group)
  (car group))

(defun next-level (groups)
  (let ((res nil))
    (dolist (thing groups (nreverse res))
      (when (group-p thing) (push thing res)))))

(defun system-name/fullname (sysnames)
  (mapcar #'sysn-fn sysnames))

(defun find-parent (groupname groups &optional (parent nil))
  (if (null groups)
      nil
      (let ((curr (car groups)))
	(if (symbolp curr)
	    (or (when (eql curr (intern groupname))
		  parent)
		(find-parent groupname (cdr groups) curr))
	    (let ((subgroup? (symbolp (car curr))))
	      (if subgroup?
		  (or (find-parent groupname curr parent)
		      (find-parent groupname (cdr groups) parent))
		  (find-parent groupname (cdr groups) parent)))))))

(defun get-sysname (fullname teacher)
  (car (rassoc (list fullname) (teacher-pupils teacher) :test #'equal)))

;;FIXME remove
(defun get-members-of-group (gname groups)
  (cdr (assoc gname groups :test #'eql)))

(defun pupils-in-group (group)
  (remove-if #'group-p (cdr group)))

(defun groups-in-group (group)
  (mapcar #'car (remove-if-not #'group-p (cdr group))))

(defun pupils-and-groups (teacher &optional groupname)
  (let* ((group (if groupname 
                    (find-group groupname (teacher-groups teacher))
                    (teacher-groups teacher))))
    (list (pupils-in-group group)
	  (groups-in-group group))))

(defun pupil-pair-p (x)
  (and (consp x)
       (stringp (car x))
       (stringp (cadr x))))

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun all-group-names (groups)
  (remove-if-not #'symbolp (flatten groups)))

(defun replace-group-in-groups (groupname newgroup groups)
  (let ((old (find-group groupname groups))) ;;FIXME error check
    (sublis (list (cons old newgroup)) groups :test #'equal)))

(defun weed-groups (thing groups fn)
  (if (atom groups)
      groups
      (mapcar (lambda (subgroups) (weed-groups thing subgroups fn))
              (remove-if fn groups))))


(defun remove-deleted-pupil-from-groups (teacher pupil)
  (let ((groups (teacher-groups teacher)))
    (setf (teacher-groups teacher) 
          (weed-groups pupil groups
                       (lambda (x) (and (pupil-pair-p x)
                                        (string= (car x) pupil)))))))

(defun remove-deleted-groups-from-groups (teacher groups)
  (let ((syms (mapcar #'(lambda (g) (intern (tbnl:escape-for-html g))) groups))
        (old (teacher-groups teacher)))
    (setf (teacher-groups teacher)
          (weed-groups syms old
                       (lambda (x) (and (group-p x)
                                        (member (car x) syms :test #'eql)))))))

;;EXPERIMENTAL with emphasis on mental

(defun group-tree-to-divs (gt &optional (ret nil))
  (if (null gt) ret
      (let ((first (car gt))
            (rest (cdr gt)))
        (cond ((pupil-pair-p first)
               (group-tree-to-divs
                rest (append `((:li :id ,(concatenate 'string "pg_" (car first))
                                     :data-type "pupil"
                                     :class "group-menu-item"
                                     :style "display:none"
                                  ,(cadr first))) ret)))
              ((group-p first)
               (cons (group-tree-to-divs first nil)
                     (group-tree-to-divs rest ret)))
              ((symbolp first)
               (let ((name (symbol-name first)))
                  `(:ul :id ,(concatenate
                               'string "gr_" name)
                         :data-type "group"
                         :class "group-menu-item"
                         :style "display:none"
                         ;;:visibility "hidden"
                         ,name ,@(group-tree-to-divs rest nil))))
              (t (error "huh?"))))))

(defun groups-to-html (teacher)
  (let ((gt (teacher-groups teacher)))
    (when (null gt)
      (setf (teacher-groups teacher) *default-empty-groups*
            gt *default-empty-groups*))      
    (who::string-list-to-string
     (who::tree-to-template
      (group-tree-to-divs (cdadr gt))))))

;; (defun groups-to-html (teacher)
;;   (who::string-list-to-string
;;    (who::tree-to-template
;;     (group-tree-to-divs (cdadr (teacher-groups teacher))))))

;; (defun groups-to-html (teacher)
;;   (who::string-list-to-string
;;    (who::tree-to-template
;;     (group-tree-to-divs (teacher-groups teacher)))))
