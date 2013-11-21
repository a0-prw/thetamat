;; Copyright 2013 Peter Robert Wood  

;; mail: peter@thetamat.net

;; This file is part of Thetamat.

;; Thetamat is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Thetamat is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Thetamat.  If not, see <http://www.gnu.org/licenses/>.

(in-package :autotutor)

;; FIXME macro?
(defun ensure-teacher-pupil-relationship (teacher pupil)
  (if
   (and 
    (member pupil (teacher-pupils teacher) :test #'string= :key #'car)
    (string= (pupil-teacher pupil) teacher))
   t
   (error 'no-teacher-pupil-relationship :teacher teacher :pupil pupil)))

(defun tree-to-html (tree)
  (who::string-list-to-string
   (who::tree-to-template tree)))

(defvar *no-record-yet*
  (tree-to-html '((:div (:p "The pupil has not yet completed any modules or games.")))))

(defun handler-name (name)
  (intern (string-upcase (concatenate 'string name "-page"))))

(defun uri-for-name (name)
  (concatenate 'string "/" name))

(defun page-title (user)
  (princ (first-name user)))

(defun ques (prob)
  (first prob))

(defun ans (prob)
  (second prob))

(defun wrong-ans (prob)
  (car (last prob)))
  
(defun with-equality-sign (texq)
  (format nil "$~A=$" (string-trim '(#\$) texq)))

(defun attemptedp (prob)
  (car (last prob)))

(defun pandr (name) 
  (let ((cp (pupil-current-problems name)))
    (if (not (null cp))
	cp
	(let* ((ret1 ())
               (placing (pupil-placement name))
	       (probs (if placing (collect-problems
                                   (make-document name :type 'placement))
			  (collect-problems 
			   (make-document name :type 'exercise))))
	       (ex 0)
	       (cp (dolist (p probs (list ex (nreverse ret1)))
		     (incf ex)
		     (let* 
			 ((count 0)
			  (type (car p))
			  (content (cddr p))
			  (ret (mapcar 
				(lambda (a b) 
				  (incf count) 
				  `(,a ,b nil nil)) (car content) (cadr content))))
		       (push `(,type ,count ,ret) ret1)))))
	  (setf (pupil-current-problems name) cp)))))

(defun demo-exercise (tname didel-tag howmany)
  ;;TODO if howmany doesn't agree with existing N replace.
  (labels ((gen-demo ()
             (let* ((ret1 ())
                    (ex 0)
                    (probs (collect-problems 
                            (make-teaching-document tname didel-tag howmany))))
               (destructuring-bind (tag objs qs as)
                   probs
                 (declare (ignore objs))
                 (incf ex)
                 (let* ((count 0)
                        (ret (mapcar 
                              (lambda (a b) 
                                (incf count) 
                                `(,a ,b nil nil)) qs as)))
                   (push `(,tag ,count ,ret) ret1)))
               (list ex (nreverse ret1)))))
    (let ((found (teacher-demo tname)))
      (if found
          (destructuring-bind (discard ((tag n exs)))
              found
            (declare (ignore discard) (ignore n) (ignore exs))
            (if (eql didel-tag tag) 
                found
                (setf (teacher-demo tname) (gen-demo))))
          (setf (teacher-demo tname) (gen-demo))))))

(defun make-demo-q&a-div (demo)
  (destructuring-bind (ign1 ((tag max ps)))
      demo
    (declare (ignore ign1))
    (let* ((label (symbol-name tag))
           (didel (get-didactic-element tag))
           (caption (caption didel))
           (concise (sd didel))
           (menu-level (de-spmenu-level didel))
           (verbose (ld didel))
           (maxinput (reply didel))
           (maxcols 2))
      `((:div :id ,(encode-table-id 0) ;;"demo-exercise"
              :class "exercise-table"
              :data-de ,label
              :data-spmenu-level ,menu-level
              :data-concise ,concise
              :data-verbose ,verbose
              :data-caption ,caption
              ,@(loop for p from 0 below max by maxcols collect
                     `(:div :class "exercise-row"
                            ,@(loop for probidx from p below (+ p maxcols)
                                 while (< probidx max)
                                 for prob = (problem-n probidx ps) ;;(problems ex))
                                 with q and a
                                 for nil = (multiple-value-setq (q a) 
                                             (display-problem prob 0 probidx maxinput))
                                 append
                                   `((:div :class "exercise-question-cell" 
                                           :id ,(encode-table-cell-id 0 probidx 'q) ,q)
                                     (:div :class "exercise-response-cell" 
                                           :id ,(encode-table-cell-id 0 probidx 'a) ,a))))))))))

(defun list-didactic-elements ()
  (loop for k being each hash-key of *didactic-elements*
     for v being each hash-value of *didactic-elements*
       when (didactic-element-display v)
       collect `(,k ,(caption v) ,(sd v) ,(ld v))))

;; ex: (demo-exercise "peter" 'tiny-c-plus 5)

(defun teacher-get-current-problems-for-pupil (teacher-name pupil-name)
  (let ((teacher-pupils  (teacher-pupils teacher-name))
	(pupil-teacher (pupil-teacher pupil-name))
	(problems (pupil-current-problems pupil-name)))
    (unless (and (string= teacher-name pupil-teacher)
		 (member pupil-name teacher-pupils  :test #'string= :key #'car))
      (error 'no-teacher-pupil-relationship :teacher teacher-name 
	     :pupil pupil-name))
    problems))

(defun encode-table-cell-id (ex n type-symbol)
  (format nil "~A_~D_~D" (symbol-name type-symbol) ex n ))

(defun encode-table-id (table-index)
  (format nil "table_~D" table-index))

(defun table-cell-type (lid)
  (first lid))

(defun table-cell-exidx (lid)
  (second lid))

(defun table-cell-probidx (lid)
  (third lid))

(defun parse-table-cell-id (id)
  (let* ((refs (split-sequence:split-sequence #\_ id))
	 (ex-idx (parse-integer (table-cell-exidx refs)))
	 (prob-idx (parse-integer (table-cell-probidx refs)))
	 (cell-type (intern (string-upcase (table-cell-type refs)))))
    (list cell-type ex-idx prob-idx)))

(defun n-exercises (pandr)
  (car pandr))

(defun exercises (pandr)
  (cadr pandr))

(defun exercise-n (n pandr)
  (nth n (cadr pandr)))

(defun exercise-de (ex)
  (car ex))

(defun problems (ex)
  (caddr ex))

(defun n-problems (ex)
  (cadr ex))

(defun problem-n (n problems)
  (nth n problems))

(defun update-module-unless-exists (pupil module)
  (let ((viewed (pupil-viewed pupil)))
    (unless (or (member module viewed :test #'eql)
                (not (member module +modular-progression+ :test #'eql)))
      (setf (pupil-viewed pupil) (push module viewed)))))

(defun last-module ()
  (car (last +modular-progression+)))

(defun update-module-from-games (pupil)
  (let* ((placing (pupil-placement pupil))
         (modules (pupil-modules pupil))
         (module (car modules)))
    (update-module-unless-exists pupil module)
    (setf (pupil-modules pupil) (cdr modules)
          (pupil-cycle pupil) 1
          (pupil-current-problems pupil) nil)
    (record-completion pupil (get-universal-time)
                       ;;(symbol-name module) nil placing)
                        module nil placing)
    (format nil "You completed the games for this module and will now continue with the the next module.")))

(defun update-module-continue-placement (pupil modules)
    (let ((module (car modules))
          (next (cdr modules)))
      (update-module-unless-exists pupil module)
      (setf (pupil-modules pupil) next
            (pupil-current-problems pupil) nil
            (pupil-cycle pupil) 1)
      (record-completion pupil (get-universal-time)
                         ;;(symbol-name module) 
                         module nil t)
      (cond ((special-modulep (car next))
             (format nil "Well done!  You successfully completed all the exercises in the placement module!~%The next module is a game, and after that more placement exercises will be loaded."))
            ((null next)
             (format nil "Well done!  You successfully completed all the exercises that are currently available!~% When you continue, you will be able to choose exercises to practice, and can review lessons~%."))
            (t
             (format nil "Well done!  You successfully completed all the exercises in the placement module!~%When you continue, a new placement module will be loaded.")))))

(defun no-update-module-stop-placing (pupil modules)
  (setf (pupil-placement pupil) nil
        (pupil-current-problems pupil) nil)
  (record-completion pupil (get-universal-time)
                     ;;(symbol-name (car modules))
                     (car modules) 'placed t)
  (format nil "Well done!  You completed all the exercises in the placement module!~%You made at least one mistake, so when you continue,~% you will be given the same type of exercises to practice."))

(defun update-module-not-placing (pupil modules)
  (let ((module (car modules))
        (next (cdr modules)))
    (update-module-unless-exists pupil module)
    (setf (pupil-modules pupil) next
          (pupil-cycle pupil) 1
          (pupil-current-problems pupil) nil)
    (record-completion pupil (get-universal-time)
                       ;;(symbol-name module) 
                       module nil nil)
    (cond ((special-modulep (car next))
           (format nil "Well done!  You successfully completed all the exercises in the module!~%The next module is a game, and after that more exercises will be loaded."))
          ((null next)
           (format nil "Well done!  You successfully completed all the exercises that are currently available!~% When you continue, you will be able to choose exercises to practice, and can review lessons~%."))
          (t
           (format nil "Well done!  You successfully completed all the exercises!~%When you continue, new exercises will be loaded.")))))

(defun no-update-all-wrong (pupil modules repeat)
  (setf (pupil-current-problems pupil) nil)
  (record-completion pupil (get-universal-time)
                     ;;(symbol-name (car modules)) 
                     (car modules) repeat nil) ;;last was t, but that must be wrong
  (format nil "Well done! You completed the exercises!~% You made some mistakes, so when you continue,~%the same type of exercises will be loaded."))

(defun no-update-repeat-these (pupil cp modules repeat)
  (let ((ex (exercises cp)))
    (dolist (idx repeat)
      (setf (nth idx ex)
            (collect-didactic-element-problems 
             (exercise-de (exercise-n idx cp)) pupil)))
    (setf (cdr cp) (list ex))
    (setf (pupil-current-problems pupil) cp)
    (record-completion pupil (get-universal-time) (car modules) repeat)
    (format nil "Well done! You completed the exercises!~%You made some mistakes, so when you continue,~%the exercise(s) in which you made too many mistakes~%will be repeated.")))

(defun process-update-module (pupil results)
  (cond ((eql results :true) ;;:true means update module from games
         (update-module-from-games pupil))
        ((eql results :false) ;;false means games were started
         ;;voluntarily, so don't update module
         (format nil "When you continue, your usual page will open."))
        (t 
         (let ((modules (pupil-modules pupil)))
           (if (null modules)
               "Exercise review complete!"
               (let* ((cp (pupil-current-problems pupil))
                      (placing (pupil-placement pupil))
                      (n (n-exercises cp))
                      (repeat ()));;a list of indexes of exercises to be
                 ;;repeated in module
                 (dolist (r results)
                   (destructuring-bind (index correct total)
                       r
                     (let ((pass (didactic-element-pass 
                                  (get-didactic-element 
                                   (exercise-de (exercise-n index cp))))))
                       (when (< (/ correct total) pass)
                         (push index repeat)))))
                 (let ((nr (length repeat)))     
                   (cond ((and (zerop nr) placing)
                          (update-module-continue-placement pupil modules))
                         (placing
                          (push 'placed repeat)
                          (no-update-module-stop-placing pupil modules))
                         ((zerop nr) 
                          (update-module-not-placing pupil modules))
                         ((= nr n) ;;failed enough to repeat all
                          (no-update-all-wrong pupil modules repeat))
                         (t 
                          (no-update-repeat-these pupil cp modules repeat))))))))))

;; (defun process-update-module (pupil results)
;;   (cond ((eql results :true) ;;:true means update module from games
;;          (update-module-from-games pupil))
;;         ((eql results :false) ;;false means games were started
;;          ;;voluntarily, so don't update module
;;          (format nil "When you continue, your usual page will open."))
;;         (t 
;;          (let* ((cp (pupil-current-problems pupil))
;;                 (modules (pupil-modules pupil))
;;                 (placing (pupil-placement pupil))
;;                 (n (n-exercises cp))
;;                 (repeat ()));;a list of indexes of exercises to be
;;                             ;;repeated in module
;;            (dolist (r results)
;;              (destructuring-bind (index correct total)
;;                  r
;;                (let ((pass (didactic-element-pass 
;;                             (get-didactic-element 
;;                              (exercise-de (exercise-n index cp))))))
;;                  (when (< (/ correct total) pass)
;;                    (push index repeat)))))
;;            (let ((nr (length repeat)))     
;;              (cond ((and (zerop nr) placing)
;;                     (update-module-continue-placement pupil modules))
;;                    (placing
;;                     (push 'placed repeat)
;;                     (no-update-module-stop-placing pupil modules))
;;                    ((zerop nr) 
;;                     (update-module-not-placing pupil modules))
;;                    ((= nr n) ;;failed enough to repeat all
;;                     (no-update-all-wrong pupil modules repeat))
;;                    (t 
;;                     (no-update-repeat-these pupil cp modules repeat))))))))
                    

;; If bugs turn up in this, here is the old one for comparison

;; (defun process-update-module (pupil results)
;;   (cond ((eql results :true) ;;:true means update module from games
;;          (let* ((modules (pupil-modules pupil))
;;                 (module (car modules)))
;;            (update-module-unless-exists pupil module)
;;            (setf (pupil-modules pupil) (cdr modules)
;;                  (pupil-cycle pupil) 1
;;                  (pupil-current-problems pupil) nil)
;;            (record-completion pupil (get-universal-time)
;;                               (symbol-name module) nil)
;;            (format nil "You completed the games for this module and will
;;                 now continue with the the next module.")))
;;         ((eql results :false) ;;false means games were started
;;          ;;voluntarily, so don't update module
;;          (format nil "When you continue, your usual page will open."))
;;         (t
;;          (let* ((cp (pupil-current-problems pupil))
;;                 (modules (pupil-modules pupil))
;;                 (placing (pupil-placement pupil))
;;                 (n (n-exercises cp))
;;                 (repeat ()));;a list of indexes of exercises to be
;;                             ;;repeated in module
;;            (dolist (r results)
;;              (destructuring-bind (index correct total)
;;                  r
;;                (let ((pass (didactic-element-pass 
;;                             (get-didactic-element 
;;                              (exercise-de (exercise-n index cp))))))
;;                  (when (< (/ correct total) pass)
;;                    (push index repeat)))))
;;            (let ((nr (length repeat)))     
;;              (cond ((and (zerop nr) placing)
;;                     (let ((module (car modules)))
;;                       (update-module-unless-exists pupil module)
;;                       (setf 
;;                        (pupil-modules pupil) (cdr modules)
;;                        (pupil-current-problems pupil) nil
;;                        (pupil-cycle pupil) 1)
;;                       (record-completion pupil (get-universal-time)
;;                                          (symbol-name module) repeat)
;;                       (format nil "Well done!  You successfully
;;                       completed all the exercises in the placement
;;                       module!~%When you continue, a new placement
;;                       module will be loaded.")))
;;                    (placing
;;                     (setf (pupil-placement pupil) nil
;;                           (pupil-current-problems pupil) nil)
;;                     (record-completion pupil (get-universal-time)
;;                                        (symbol-name (car modules)) (push 'placed repeat))
;;                     (format nil "Well done!  You completed all the
;;                     exercises in the placement module!~%You made at
;;                     least one mistake, so when you continue,~% you
;;                     will be given the same type of exercises to
;;                     practice."))
;;                    ((zerop nr) 
;;                     (let ((module (car modules)))
;;                       (update-module-unless-exists pupil module)
;;                       (setf (pupil-modules pupil) (cdr modules)
;;                             (pupil-cycle pupil) 1
;;                             (pupil-current-problems pupil) nil)
;;                       (record-completion pupil (get-universal-time)
;;                                          (symbol-name module) repeat)
;;                       (format nil "Well done!  You successfully
;;                       completed all the exercises!~%When you continue,
;;                       new exercises will be loaded.")))
;;                    ((= nr n) ;;failed all!
;;                     (setf (pupil-current-problems pupil) nil)
;;                     (record-completion pupil (get-universal-time)
;;                                        (symbol-name (car modules)) repeat)
;;                     (format nil "Well done! You completed the
;;                     exercises!~% You made some mistakes, so when you
;;                     continue,~%the same type of exercises will be
;;                     loaded"))
;;                    (t (let ((ex (exercises cp)))
;;                         (dolist (idx repeat)
                          
;;                           (setf (nth idx ex)
;;                                 (collect-didactic-element-problems 
;;                                  (exercise-de (exercise-n idx cp)) pupil)))
;;                         (setf (cdr cp) (list ex))
;;                         (setf (pupil-current-problems pupil) cp)
;;                         (format nil "Well done! You completed the
;;                         exercises!~%You made some mistakes, so when
;;                         you continue,~%the exercise(s) which you made
;;                         mistakes in~%will be repeated.")))))))))

(defun completed-modules (pupil)
  (set-difference +modular-progression+ (pupil-modules pupil)))

(defun problem-answer (problem)
  (let ((answered (fourth problem)))
    (if answered 
	(let ((good (third problem)))
	  (if good
	      (values t (second problem))
	      (values nil answered)))
	nil)))

(defun make-q&a (s1 s2)
  (let ((s2 (concatenate 'string (string-trim '(#\$) s2) "$")))
    (format nil "~A=~A" 
	    (string-right-trim '(#\$) s1)
	    (string-left-trim '(#\$) s2))))

(defun make-q (initial)
  (concatenate 'string (string-right-trim '(#\$) initial) "=$"))

(defun list-or-values (val1 val2 listp)
  (if listp 
      (list (string-trim '(#\$) val1) val2)
      (values val1 val2)))

(defun return-js-input-field (tdid inpid maxinput)
  (let* ((code `(when (= (ps:@ event key-code) 13) 
		  (progn
                    (check-sum ,tdid 
                               (ps:chain (ps:@ (get-element-by-id ,inpid) value)))
                    ;;(maybe-promote)
                  )))
	 (size (+ 3 maxinput))
	 (javascript (concatenate 'string "Javascript:" (ps:ps* code))))
    (who:with-html-output-to-string (str)
      (:input :id inpid :size size :maxlength size :type "text" :onKeydown javascript))))

(defvar +input-error+ 1)

(defun display-problem (problem exidx probidx &optional (maxinput 20) (listp nil) (errorp nil))
  (if errorp (list +input-error+ errorp)
      (destructuring-bind (initial facit good-response bad-response)
          problem
        (cond ((and (not good-response) ;;problem has not been attempted
                    (not bad-response))
               (list-or-values 
                (make-q initial)
                (return-js-input-field (encode-table-cell-id exidx probidx 'a)
                                       (encode-table-cell-id exidx probidx 'i) 
                                       maxinput)
                listp))
              (good-response ;;problem has been answered correctly
               (list-or-values 
                (make-q&a initial facit)
                (return-image (correct) (encode-table-cell-id exidx probidx 'p))
                listp))
              ((and bad-response ;;problem has been answered incorrectly
                    (not good-response))
               (list-or-values
                (make-q&a initial bad-response)
                (return-image (incorrect) (encode-table-cell-id exidx probidx 'p))
                listp))
              (t (error "Garbage in problem: ~S" problem))))))

(defun make-q&a-divs (name &optional pandr)
  (let* ((pandr  (if pandr pandr (pandr name)))
         (placement (if (pupil-placement name) "true" "false"))
         (view-lessons (if (pupil-view-lessons name) "must" "may"))
         (cycle (pupil-cycle name))
	 (exs (exercises pandr))
         (label (car (pupil-modules name)))
         (mod (symbol-name label))
	 (maxcols 2))
    (loop for ex in exs for exidx from 0
       for max = (n-problems ex)
       for didel = (get-didactic-element (exercise-de ex))
       for caption = (caption didel)
       for concise = (sd didel)
       for verbose = (ld didel)
       for menu-level = (de-spmenu-level didel)
       for maxinput = (reply didel)
	 collect
	 `(:div :id ,(encode-table-id exidx)
		:class "exercise-table"
                :data-module ,mod
                :data-spmenu-level ,menu-level
                :data-cycle ,cycle
		:data-caption ,caption
                :data-lessons ,view-lessons
                :data-concise ,concise
                :data-verbose ,verbose
                :data-placement ,placement
		,@(loop for p from 0 below max by maxcols collect
		       `(:div :class "exercise-row"
			      ,@(loop for probidx from p below (+ p maxcols)
				   while (< probidx max)
				   for prob = (problem-n probidx (problems ex))
				   with q and a
				   for nil = (multiple-value-setq (q a) 
					       (display-problem prob exidx probidx maxinput))
				   append
				     `((:div :class "exercise-question-cell" 
					     :id ,(encode-table-cell-id exidx probidx 'q) ,q)
				       (:div :class "exercise-response-cell" 
					     :id ,(encode-table-cell-id exidx probidx 'a) ,a)))))))))

(defun teacher-make-q&a-divs (teacher-name pupil-name)
  (let ((pandr (teacher-get-current-problems-for-pupil teacher-name pupil-name)))
    (if (null pandr) ;;FIXME inform if working on games
        (let* ((games (car (pupil-modules pupil-name)))
               (gnames (cond ((eql games 'mod-digs-nat)
                              "addition and subtraction")
                             ((eql games 'mod-table)
                              "multiplication and division")
                             (t "unknown"))))
          `((:div :id "no_exercises" :data-games ,gnames )))
	(make-q&a-divs pupil-name pandr))))

;;;;; NB important note.  DO NOT REMOVE! We need to generate dynamic
;;;;; html, but cl-who generates html with a macro which expects a
;;;;; static (at compile-time) page.  Therefore we access the internal
;;;;; functions of cl-who directly in the script pages ( pupil.lisp
;;;;; and teacher.lisp) which get called by the macro.  This might
;;;;; break if cl-who changes internally in the used functions.  The
;;;;; used version of cl-who is 1.1.1, so get that if you experience
;;;;; breakages with later versions of cl-who. (I consider this
;;;;; unlikely, since at time of writing (21/10/2012) cl-who has not
;;;;; changed in 4 years) If necessary, the relevant internal bits
;;;;; could be ripped out and put in autotutor.  This obviates the
;;;;; need of the 'eval in a defun-ajax, which was even nastier!

;; From pupil.lisp 

;;  (defun-ajax get-all-div-tables (name) 
;;     (*pupil-script-processor* xml-response)
;;   (eval (div-tables-to-html (make-q&a-divs name))))

;; ;; From this file (page-utils.lisp)

;; (defun div-tables-to-html (divtables)
;;   `(who:with-html-output-to-string (str nil :prologue nil :indent t)  
;;      ,@divtables))

(defgeneric parse-n (atype string))
;;(defgeneric at= (n1 n2))

(defmethod parse-n ((typ t) (s string))
  (safe-read-from-string s))

;; (defmethod parse-n ((typ (eql 'natural)) (s string))
;;   (parse-integer s))
;;;; FIXME more decimals
(defmethod parse-n ((typ (eql 'decimal)) (s string))
  (let ((auth (safe-read-from-string s)))
    (when (numberp auth)
      (dtorat (make-decimal-representation s)))))
;;FIXME error checking

(defvar *ratscan* (cl-ppcre:create-scanner "^(-?\\d+)\\/(-?\\d+)$"))

(defun format-answer-for-display (atype answer)
  (if (eql atype 'rational)
      (let* ((disp (cl-ppcre:register-groups-bind (num den)
                       (*ratscan* answer)
                     (format nil "\\frac{~A}{~A}" num den))))
        (if disp disp answer))
      answer))

;; (defmethod parse-n ((typ (eql 'rational)) (s string))
;;   (let* ((num (cl-ppcre:register-groups-bind (num den)
;;                   (*ratscan* s)
;;                 (when (and num den)
;;                   (let ((n (parse-integer num))
;;                         (d (parse-integer den)))
;;                     (if (not (zerop d)) (/ n d)
;;                         (error 'division-by-zero :operands (list n d) :operation s)
;;                         ))))))
;;     (if num num (parse-integer s))))

;;FIXME: drop all this crap and use safe-read-from-string for numbers???

;;FIXME not right! Matches "-." and "."
;;(defvar number-regex "^(-)?(\\d)*(\\.)?(\\d)+$") 

(defun good-answer (atype answer good)
  ;;TODO FIXME for decimals (maybe?)
  ;; Maybe give it the operation to discriminate on ??
  (let ((aa (parse-n atype (string-trim '(#\$ #\Space) answer)))
	(gg (parse-n atype (string-trim '(#\$) good))))
    (cond ((and (not (numberp aa))
                (not (numberp gg)))
           (error 'both-not-numbers :input (tbnl:escape-for-html answer)
                  :system (tbnl:escape-for-html good)))
          ((not (numberp aa))
           (error 'input-not-a-number :input (tbnl:escape-for-html answer)))
          ((not (numberp gg))
           (error 'system-value-not-a-number :system (tbnl:escape-for-html good)))
          (t (= aa gg)))))

(defun incorrect ()
  "img/mywrong.jpg")

(defun correct ()
  "img/mytick.jpg")

(defun blank ()
  "img/blank.jpg")

(defun return-image (src id)
  (who:with-html-output-to-string (str)
    (:img :id id :src src)))

(defun mark-problem (problem id)
  (if (not (attemptedp problem))
      (error "Can not mark ~A. It has not been attempted." problem)
      (let ((ans (wrong-ans problem)))
	(if (stringp ans)
	    (return-image (incorrect) id)
	    (return-image (correct) id)))))

;;;;;;;;;; Between comments below are adapted
;;;;;;;;;; From Doug Hoyte's book: Let Over Lambda
(defvar read-from-string-blacklist '(#\# #\: #\| #\;))

(let ((rt (copy-readtable nil)))
  (defun safe-reader-error (strm clch)
    (declare (ignore strm clch))
    (error "safe-read-from-string failure"))
  (dolist (c read-from-string-blacklist)
    (set-macro-character c #'safe-reader-error nil rt))
  (defun safe-read-from-string (s &optional (escape nil) (fail (make-condition 'error)))
    (if (stringp s)
        (let ((*readtable* rt) *read-eval*)
          (handler-bind
              ((error
                (lambda (c)
                  (return-from safe-read-from-string c)))                   
               ;; (error (lambda (condition)
               ;;          (declare (ignore condition))
               ;;          (return-from safe-read-from-string fail)))
               )
            (if escape 
                (multiple-value-bind (res pos)
                    (read-from-string s)
                  (values (mapcar #'(lambda (item)
                                      (if (stringp item)
                                          (tbnl:escape-for-html item)
                                          item)) res) pos))
                (read-from-string s))))
        fail)))
;;;;;;;;;;;;


;; Experimental
;; (defun collect-pupil-definitions (defs)
;;   (let ((passed ()))
;;     (dolist (def defs passed)
;;       (destructuring-bind (first surname pass fg)
;;           def
;;         (unless (and (stringp first)
;;                      (not (> (length first) 50))
;;                      (stringp second)
;;                      (not (> (length second) 50))
;;                      (or (null pass)
;;                          (and (stringp pass)
;;                               (not (> (length pass) 15))))
;;                      (or (null fg)
;;                          (eql fg t)))
;;         (error "Bad pupil definition: ~A" def))
;;         (setf passed (push def passed))))))

(defun test-form (form)
  (ignore-errors 
    (destructuring-bind (first surname fg)
        form
      (if (and (stringp first)
               (not (zerop (length first)))
               (not (> (length first) 50))
               (stringp surname)
               (not (zerop (length surname)))
               (not (> (length surname) 50))
               (or (null fg)
                   (eql fg t)))
          t nil))))

;; (defvar exp00 "(\"Apple\" \"Banana\" \"qw45__Rs\" nil)(\"pineapple\" \"banana pie\" nil nil)(\"Mango\" \"Lettuce-Saladski\" \"zzz123\" t)(\"Bread\" \"Fruit\" nil nil)")

;; (defvar exp01 "(\"Apple\" \"banana\" \"qw45__Rs\" #.(crash system))(\"pineapple\" \"banana pie\" nil nil)(\"Mango\" \"Lettuce-Saladski\" \"zzz123\" t)(\"Bread\" \"Fruit\" nil nil)")

(defvar maximum-file-string 10000) ;;FIXME how big is this?

(defun process-defs-from-file (teacher contents &optional collect (total 0))
  (if (string= contents "") 
      (define-pupils-from-forms teacher collect)
      (multiple-value-bind (form pos)
          (safe-read-from-string contents t)
        (if (> total maximum-file-string) 
            (define-pupils-from-forms teacher collect)
            (if (test-form form)
                (process-defs-from-file 
                 teacher
                 (subseq contents pos) (cons form collect) (+ pos total))
                (define-pupils-from-forms teacher 'error-in-pupil-file))))))

;; (defun test-process-defs-from-file (contents &optional collect (total 0))
;;   (if (string= contents "") ;;FIXME remove
;;       collect
;;       (multiple-value-bind (form pos)
;;           (safe-read-from-string contents)
;;         (if (> total maximum-file-string) 
;;             collect
;;             (if (test-form form)
;;                 (test-process-defs-from-file 
;;                  (subseq contents pos) (cons form collect) (+ pos total))
;;                 collect)))))

;; (defun define-pupils-from-forms (teacher tested-forms)
;;   "Returns either a string with a message describing what went wrong,
;; or a list of defined pupils."  
;;     (if (consp tested-forms) ;;passed tests & not null
;;         (let ((authorized (teacher-nauthorized teacher))
;;               (requested (length tested-forms))
;;               (max-reached nil))
;;           (if (> authorized 0)
;;               (progn
;;                 (when (> requested authorized)
;;                   (setf max-reached t
;;                         tested-forms (subseq tested-forms 0 requested)))
;;                 (let ((gather ()))
;;                   (dolist (form tested-forms gather)
;;                     (destructuring-bind (name surname passwd fg)
;;                         form
;;                       (ignore-errors ;;Fixme
;;                         (destructuring-bind ((u1 p) ((u2 full) &rest rest))
;;                             (define-pupil teacher name surname :password passwd 
;;                                           :first-grade fg)
;;                           (declare (ignore u2 rest))
;;                           (setf gather
;;                                 (push `(:fullname ,full :username ,u1 :password ,p)
;;                                       gather))))))))
;;               "You have exhausted your pupil quota."))
;;         (if (null tested-forms) ;;null tested forms but no error
;;             "No acceptable pupil definitions in file."
;;             "Error in pupil file."))) ;;TODO if list was truncated due
;;                                       ;;to auth exhaustion, inform of
;;                                       ;;this fact and provide link to
;;                                       ;;"buy more pupils page"


 ;; "TESTED-FORMS, if it is CONSP, have passed the initial tests (of
 ;;  structure, types and length) and have been read in using
 ;;  SAFE-READ-FROM-STRING.  Returns an st-json object with fields
 ;;  'status', 'curtailed', 'pupils' and 'data'.  If _any_ pupils were
 ;;  defined, the status is 'ok', otherwise 'error'.  If not all the
 ;;  requested pupils were enrolled, because the teacher has exceeded
 ;;  the number of authorized enrollments, 'curtailed' is true,
 ;;  otherwise false. 'data' contains either formatted html for
 ;;  displaying the pupils which were enrolled (in which case,
 ;;  'status' is 'ok', or a plaintext error message"
(defun define-pupils-from-forms (teacher tested-forms)
  (labels ((create-jso (status curtailed data)
             (st-json:jso "status" status
                          "curtailed" curtailed
                          "pupils" (teacher-pupils teacher)
                          "data" data))
           (format-gather (gathered)
             (tree-to-html
              `((:div :id "new-enrollment"
                     (:h1 "New Pupils")
                     (:p "The following pupils were enrolled:")
                     (:ul ,@gathered))))))
    (if (consp tested-forms) ;;passed tests & not null
        (let ((authorized (teacher-nauthorized teacher))
              (requested (length tested-forms))
              (max-reached nil))
          (if (> authorized 0)
              (progn
                (when (> requested authorized)
                  (setf max-reached t
                        tested-forms (subseq tested-forms 0 requested)))
                (let ((gather ())
                      (uname-forms (add-unique-forms-names tested-forms)))
                       ;;(register-pupil-forms-globally tested-forms teacher)))
                  (dolist (form uname-forms 
                           (create-jso "ok" (if max-reached :true :false)
                                       (format-gather gather)))
                    (multiple-value-bind (full u1 p)
                        (define-pupil-from-form teacher form)
                      (setf gather
                            (push `(:li (:ul ,full 
                                             (:li "Username: " ,u1)
                                             (:li "Password: " ,p)))
                                  gather))))))
              (create-jso "error" :true "You have exhausted your pupil quota.")))
        (if (null tested-forms) ;;null but no error from safe-read-fs
            (create-jso "error" :false "No acceptable pupil definitions in file.")
            (create-jso "error" :false "Error in pupil file.")))))

                    ;; (destructuring-bind (uname (name surname fg))
                    ;;     form
                    ;;   (ignore-errors 
                    ;;     (let ((p (suggest-password)))
                    ;;       (destructuring-bind ((u1 p) ((u2 full) &rest rest))
                    ;;           (define-pupil teacher uname name surname :password p
                    ;;                         :first-grade fg)
                    ;;         (declare (ignore u2 rest))

