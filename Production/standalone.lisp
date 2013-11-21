(in-package :autotutor)

(setf crypto:*prng* (crypto:make-prng :fortuna))

(defvar *unique-name-count* (make-hash-table :test 'equal :size 5000))
(defvar *lock-name-count* (bt:make-lock))
(defvar *name-count-store* (merge-pathnames "system/name-count"))

(defun get-unique-name-number (name)
  (bt:with-lock-held (*lock-name-count*)
    (multiple-value-bind (record found)
        (gethash name *unique-name-count*)
      (if found
          (destructuring-bind (count free)
              record
            (if free 
                (prog1 (car free)
                  (setf (gethash name *unique-name-count*) `(,count ,(cdr free)))
                  (cl-store:store *unique-name-count* *name-count-store*))
                (let ((newcount (1+ count)))
                  (setf (gethash name *unique-name-count*) `(,newcount nil))
                  (cl-store:store *unique-name-count* *name-count-store*)
                  newcount)))
          (prog1 0
            (setf (gethash name *unique-name-count*) '(0 nil))
            (cl-store:store *unique-name-count* *name-count-store*))))))

(defun unique-name (name)
  (let* ((new (get-unique-name-number name))
         (id (number-to-id new)))
    (format nil "~A~A" id name)))

(defun number-to-id (num)
  (let ((string-inc (floor num 26000)))
    (multiple-value-bind (letter count)
        (floor num 1000)
      (let* ((letter (mod letter 26))
             (char (+ 97 letter))
             (string (coerce (loop for lettr from 0 to string-inc
                       collect (code-char char)) 'string)))
        (format nil "~A~D_" string count)))))

(defun id-to-number (id)
  (multiple-value-bind (begin end rega1 rega2)
      (cl-ppcre:scan "\\d+" id)
    (declare (ignore rega1 rega2))
    (when begin
      (let* ((letters (subseq id 0 begin))
             (ltrlis (map 'list #'character letters))
             (reps (length ltrlis))
             (val (- (char-code (car ltrlis)) 97))
             (count (ignore-errors 
                      (parse-integer (subseq id begin end)))))
        (+ (* 26000 (1- reps))
           (* 1000 val)
           count)))))

(defun free-name (unique-name)
  (let* ((parts (cl-ppcre:split "\\_" unique-name))
         (id (car parts))
         (name (cond ((= (length parts) 2)
                      (cadr parts))
                     ((> (length parts) 2)
                      (coerce (cdr parts) 'string))
                     (t nil))))
    (when name
      (let ((freeid (id-to-number id)))
        (bt:with-lock-held (*lock-name-count*)
          (multiple-value-bind (record found)
              (gethash name *unique-name-count*)
            (when found
              (destructuring-bind (count free)
                  record
                (unless (member freeid free :test #'=)
                  (if (and (zerop count) 
                           (null free))
                      (remhash name *unique-name-count*)
                      (setf (gethash name *unique-name-count*) 
                            `(,count (,freeid ,@free))))
                  (cl-store:store *unique-name-count* *name-count-store*)
                  found)))))))))

(defun deregister-unique-names (nlist)
  (if (atom nlist)
      (free-name nlist)
      (dolist (p nlist t)
        (free-name p))))

(defun add-unique-name (first)
  (unique-name first))

(defun add-unique-forms-names (forms)
  (let ((ret ()))
    (dolist (form forms ret)
      (destructuring-bind (first surname g1)
          form
        (let ((un (unique-name first)))
          (setf ret (push `(,un (,first ,surname ,g1)) ret)))))))


;; ;; WARNING: These functions operate outside the lock and may only be
;; ;; used on startup/restart to initialize the databases - BEFORE - the server
;; ;; is running.

(defun place-name-count-store ()
  (unless (probe-file *name-count-store*)
    (cl-store:store *unique-name-count* *name-count-store*)))

(defun initialize-name-count ()
  (setf *unique-name-count* (cl-store:restore *name-count-store*)))
;; END WARNING
