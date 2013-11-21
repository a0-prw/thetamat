(in-package :autotutor) 

(defun date-string (ut)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
        (decode-universal-time ut)
    (declare (ignore second minute hour day daylight-p zone))
    (case *language*
      (american-english (format nil "~10,2,'0,,R/~10,2,'0,,R/~D" month date year))
      (british-english (format nil "~10,2,'0,,R/~10,2,'0,,R/~D" date month year))
      (danish (format nil "~10,2,'0,,R-~10,2,'0,,R-~D" date month year))
      (t (format nil "~10,2,'0,,R/~10,2,'0,,R/~D" date month year)))))

(defun report-continue-regular (modtit ds lessd pluralex didels last)
  (declare (ignore last))
  `(:div :class "mod-record"
         (:h2 "Placement Completed " (:u ,modtit) " " ,ds)
         (:p " Lesson Content: " ,lessd)
         (:p ,(if pluralex "Exercises:" "Exercise:")
             (:ol ,@(mapcar #'(lambda (de) `(:li ,(ld de))) didels)))))

(defun report-continue-placement (modtit ds lessd pluralex didels last)
  `(:div :class "mod-record"
         (:h2 "Completed " (:u ,modtit) " " ,ds)
         (:p ,(if last "(continuing placement)"
                  "(continued placement)"))
         (:p "Lesson Content: " ,lessd)
         (:p ,(if pluralex "Exercises:" 
                  "Exercise:")
             (:ol ,@(mapcar #'(lambda (de) `(:li ,(ld de))) didels)))))

(defun report-no-repeat (modtit ds lessd pluralex didels)
  `(:div :class "mod-record"
         (:h2 "Completed " (:u ,modtit) " " ,ds)
         (:p "Lesson Content: " ,lessd)
         (:p ,(if pluralex "Exercises:" "Exercise:")
             (:ol ,@(mapcar #'(lambda (de) `(:li ,(ld de))) didels)))))

(defun report-repeat (modtit repeat ds lessd pluralex didels two more current)
  `(:div :class "mod-record"
         (:h2 "Completed " (:u ,modtit) " (requiring repetition) " ,ds)
         (:p "Lesson Content"  (:q ,lessd))
         (:p ,(if pluralex "Exercises:"
                 "Exercise:")
             (:ol ,@(mapcar #'(lambda (de) `(:li ,(ld de))) didels))
             ,(cond (current 
                     (strcat
                      "Currently repeating "
                      (cond (two (format nil "nrs. ~D and ~D above." (car repeat) (cadr repeat)))
                            (more (format nil "nrs. ~{~D, ~} and ~D above." (butlast repeat) (car (last repeat))))
                            (t (format nil "nr. ~D above." (car repeat))))))
                    (t
                     (strcat
                      "Repeated " 
                      (cond (two (format nil "nrs. ~D and ~D above." (car repeat) (cadr repeat)))
                            (more (format nil "nrs. ~{~D, ~} and ~D above." (butlast repeat) (car (last repeat))))
                            (t (format nil "nr. ~D above." (car repeat))))))))))

(defun report-final-repeat (modtit ds)
  `(:div :class "mod-record"
         (:h2 ,(strcat modtit ": Repetition"))
         (:p "The pupil completed the module on " ,ds)))

;; (defun report-continue-regular (modtit ds lessd pluralex didels last)
;;   `(:div :class "mod-record"
;;          (:h2 "Completed Module")
;;          (:p "The pupil completed the placement module with title " 
;;              (:q ,modtit) " on " ,ds " and " 
;;              ,(if last "will continue with regular exercises in the following modules." 
;;                   "has continued with regular exercises in the following modules.")
;;              " The lesson content of this module is: " (:q ,lessd))
;;          (:p ,(if pluralex "The exercises in this module are described as follows:" 
;;                   "The exercise in this module is described as follows:")
;;              (:ol ,@(mapcar #'(lambda (de) `(:li ,(ld de))) didels)))))

;; (defun report-continue-placement (modtit ds lessd pluralex didels last)
;;   `(:div :class "mod-record"
;;          (:h2 "Completed Module")
;;          (:p "The pupil completed the placement module with title " 
;;              (:q ,modtit) " on " ,ds " and " 
;;              ,(if last "will continue with the placement program." 
;;                   "continued with the placement program."))
;;          " The lesson content of this module is: " (:q ,lessd)
;;          (:p ,(if pluralex "The exercises in this module are described as follows:" 
;;                   "The exercise in this module is described as follows:")
;;              (:ol ,@(mapcar #'(lambda (de) `(:li ,(ld de))) didels)))))

;; (defun report-no-repeat (modtit ds lessd pluralex didels)
;;   `(:div :class "mod-record"
;;          (:h2 "Completed Module")
;;          (:p "The pupil successfully completed the module with title "
;;              (:q ,modtit) " on " ,ds ". The lesson content of this module is: " (:q ,lessd))
;;          (:p ,(if pluralex "The exercises in this module are described as follows:" 
;;                   "The exercise in this module is described as follows:")
;;              (:ol ,@(mapcar #'(lambda (de) `(:li ,(ld de))) didels)))))

;; (defun report-repeat (modtit repeat ds lessd pluralex didels two more current)
;;   `(:div :class "mod-record"
;;          (:h2 "Completed Module (requiring repetition)")
;;          (:p "The pupil completed the module with title " (:q ,modtit) " on " ,ds " with some errors."
;;              " The lesson content of this module is: "  (:q ,lessd))
;;          (:p ,(if pluralex "The exercises in this module are described as follows:" 
;;                  "The exercise in this module is described as follows:")
;;              (:ol ,@(mapcar #'(lambda (de) `(:li ,(ld de))) didels))
;;              ,(cond (current 
;;                      (strcat
;;                       "The pupil is currently repeating "
;;                       (cond (two (format nil "exercises ~D and ~D above." (car repeat) (cadr repeat)))
;;                             (more (format nil "exercises ~{~D, ~} and ~D above." (butlast repeat) (car (last repeat))))
;;                             (t (format nil "exercise ~D above." (car repeat))))))
;;                     (t
;;                      (strcat
;;                       "It was necessary for the pupil to repeat " 
;;                       (cond (two (format nil "exercises ~D and ~D above." (car repeat) (cadr repeat)))
;;                             (more (format nil "exercises ~{~D, ~} and ~D above." (butlast repeat) (car (last repeat))))
;;                             (t (format nil "exercise ~D above." (car repeat))))))))))

;; (defun report-final-repeat (modtit ds)
;;   `(:div :class "mod-record"
;;          (:h2 ,(strcat modtit ": Repetition"))
;;          (:p "The pupil completed the module on " ,ds)))

(defun parse-module-record (mr &optional (last nil) (current nil))
  (destructuring-bind (label ut repeat placement)
      mr
    (let* ((ds (date-string ut))
           (mod (module label))
           (modtit (module-title mod))
           (lessd (module-description mod))
           (didels (module-payload mod))
           (pluralex (when (< 1 (length didels)) t)))
      (cond ((eql repeat 'placed)
             (report-continue-regular modtit ds lessd pluralex didels last))
            (placement ;;is also t if repeat is 'placed, but dealt with above
             (report-continue-placement modtit ds lessd pluralex didels last))
            ((null repeat) ;;not in placement program, did not repeat
             (report-no-repeat modtit ds lessd pluralex didels))
            (t (let* ((reps (length repeat))
                      (two (= 2 reps))
                      (more (< 2 reps))) ;;repeat is not nil and not eql 'placed (processed above)
                 (report-repeat modtit repeat ds lessd pluralex didels two more current)))))))
             
(defun parse-special-content-record (special-content)
  (destructuring-bind (name ((bt1 imp1 tot1) (bt2 imp2 tot2)))
      special-content
    (multiple-value-bind (strname best improvements total)
        (if (or (eql name '|Addition|)
                (eql name '|Multiplication|))
            (values (format nil "~A" name) bt1 imp1 tot1)
            (values (format nil "~A" name) bt2 imp2 tot2))
      (if (eql best :false)
           `(:div :class "mg-record"
                  (:p "The pupil has not completed the "
                      (:q ,strname) "  memory game."))
           `(:div :class "mg-record"
                  (:h2 "Completed Memory Game")
                  (:p "The pupil has completed the " 
                      (:q ,strname) "  memory game "
                      ,(if (= total 1) "once."
                           (format nil "~D times." total))
                      "  In this game, the pupil has achieved a best time of " ,best " seconds."
                      ,(if (not (eql improvements :false))
                           (format nil 
                                   " The pupil has also improved their best time in this game ~A."
                                   (if (= improvements 1) "once"
                                       (format nil "~D times" improvements)))
                           "")))))))

(defun parse-current (current completed?)
  (let* ((ds (date-string (get-universal-time)))
         (intro (if completed? 
                    (concatenate 'string
                    "The pupil has completed all the available exercises and is currently (" 
                    ds 
                    ") revising the module titled ")
                    (concatenate 'string
                    "The pupil is currently (" 
                    ds 
                    ") on the module titled ")))
         (mod (module current))
         (modtit (module-title mod))
         (lessd (module-description mod))
         (didels (module-payload mod))
         (pluralex (when (< 1 (length didels)) t)))
    `(:div :class "mod-current"
           (:p ,intro
               (:u ,modtit) ". This module's lesson description is, "
               (:q ,lessd))
           (:p ,(if pluralex "The exercises in this module are described as follows:" 
                    "The exercise in this module is described as follows:")
               (:ol ,@(mapcar #'(lambda (de) `(:li ,(ld de))) didels))))))
 
(defun parse-record (record current completed?)
  (destructuring-bind (module-records special-content-records)
      record
    (let ((pc? t)
          (modinfo ())
          (sp-info ())
          (modentries (length module-records))
          (sc-entries (length special-content-records))
          (butlast (butlast module-records))
          (most-recent (car (last module-records))))
      (cond ((zerop modentries)
             nil)
            ((= modentries 1)
             (let ((mrec (car module-records)))
               (if (eql (car mrec) current)
                   (progn (setf pc? nil)
                          (push (parse-module-record mrec t current) modinfo))
                   (push (parse-module-record mrec t nil) modinfo)
                   )))
            (t (dolist (mrec butlast)
                 (push (parse-module-record mrec nil nil) modinfo))
               (if (eql (car most-recent) current)
                   (progn (setf pc? nil)
                          (push (parse-module-record most-recent t current) modinfo))
                   (push (parse-module-record most-recent t nil) modinfo))
               ))
      (when pc? (push (parse-current current completed?) modinfo))
      (if (zerop sc-entries)
          nil 
          (dolist (sc special-content-records)
            (push (parse-special-content-record sc) sp-info)))
      (append modinfo sp-info))))
