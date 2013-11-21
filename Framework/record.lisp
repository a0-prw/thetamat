(in-package :autotutor)

(defun record-completion (name date module-label repeat 
                              &optional (placement nil))
  "Record completion of a module."
  (destructuring-bind (mods spc)
      (get-pupil-record name)
    (let ((newm `(,module-label ,date ,repeat ,placement)))
      (cond ((null mods)
             (set-pupil-record name `((,newm) ,spc)))
            (t (set-pupil-record name `((,@mods ,newm) ,spc)))))))

(defun pupil-update-memory-game (name new game)
  (case game
    (|Addition| 
      (let ((upd (list new (cadr (pupil-basic-add name)))))
        (setf (pupil-basic-add name) upd)
        (record-memory-game name upd game)))
    (|Subtraction| 
      (let ((upd (list (car (pupil-basic-add name)) new)))
        (setf (pupil-basic-add name) upd)
        (record-memory-game name upd game)))
    (|Multiplication| 
      (let ((upd (list new (cadr (pupil-basic-mul name)))))
        (setf (pupil-basic-mul name) upd)
        (record-memory-game name upd game)))
    (|Division| 
      (let ((upd (list (car (pupil-basic-mul name)) new)))
        (setf (pupil-basic-mul name) upd)
        (record-memory-game name upd game)))))

(defun record-memory-game (name data game)
  (let ((new (list game data)))
    (destructuring-bind (mods spc)
        (get-pupil-record name)
      (cond ((and (null mods)
                  (null spc))
             (set-pupil-record name `(() (,new))))
            ((null spc)
             (set-pupil-record name `(,mods (,new))))
            (t
             (if (find game spc :test #'eql :key #'car)
                 (set-pupil-record 
                  name
                  `(,mods ,(subst-if 
                            new (lambda (x) 
                                  (when (consp x) 
                                    (eq (car x) game))) spc)))
                 (set-pupil-record name `(,mods ,(cons new spc)))))))))

(defun get-pupil-record (pname)
  (with-open-file (stream (record-file pname) :if-does-not-exist :create)
    (read stream nil '(nil nil) nil)))

(defun set-pupil-record (pname record) 
  (with-open-file (stream (record-file pname)
                          :direction :output 
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (prin1 record stream)))






