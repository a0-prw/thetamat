(in-package :autotutor)

;;FIXME string-trim etc on input and username abbrev
	 
(defun teacher-script () ;;name
  (ps:who-ps-html 
     (:script 
      :type "text/javascript"
      (ps:ps 

        ;;(ps:lisp (format nil "use strict"))


	(defvar owner "") 
        (defvar work-on-screen false) 
        (defvar sub-data 
          (ps:create
           ;; interface-id "chgsub"
           ;; ccy (lambda ()
           ;;       (with-sub-data 
           ;;           currency))
           ;; eu-currency-p (lambda (cc)
           ;;                 (or (string= cc "EUR")
           ;;                     (string= cc "GBP")
           ;;                     (string= cc "DKK")))
           ;; dansk-moms (lambda (units)
           ;;              (parse-int (to-fixed (* units 1.25) 0)))
           ;; per-teacher (lambda ()
           ;;               (with-sub-data
           ;;                   (ps:getprop price-data "per-teacher")))
           ;; per-pupil (lambda ()
           ;;             (with-sub-data
           ;;                   (ps:getprop price-data "per-pupil")))
           ;; update-free-used (lambda ()
           ;;                   (with-sub-data
           ;;                       (let ((cl (length pupil-list)))
           ;;                         (if (zerop cl)
           ;;                             false
           ;;                             (let ((diff (- enrolled cl)))
           ;;                               (if (zerop diff)
           ;;                                   false
           ;;                                   (setf nauthorized (+ nauthorized diff)
           ;;                                         unused nauthorized
           ;;                                         enrolled cl)))))))
           ;; get-current-selection (lambda ()
           ;;                         (with-sub-data
           ;;                             (let ((exists (dgebid interface-id)))
           ;;                               (if exists
           ;;                                   (+ (parse-int
           ;;                                       (or (get-attribute 
           ;;                                            exists "data-new-value")
           ;;                                           nauthorized))
           ;;                                      enrolled)
           ;;                                   (+ enrolled nauthorized)))))
           ;; ccyfmt (lambda (n)
           ;;          (let ((amt (/ n 100)))
           ;;            (cond ((zerop (mod n 100))
           ;;                 (strcat amt ".00"))
           ;;                ((zerop (mod n 10))
           ;;                 (strcat amt "0"))
           ;;                (t (strcat amt "")))))

           ;; ;; current (lambda ()
           ;; ;;           (with-sub-data
           ;; ;;               (when nauthorized
           ;; ;;                 (funcall ccyfmt 
           ;; ;;                          (+ (funcall per-teacher)
           ;; ;;                             (* (funcall per-pupil)
           ;; ;;                                (funcall get-current-selection)))))))

           ;; current (lambda (moms)
           ;;           (with-sub-data
           ;;               (when nauthorized
           ;;                 (let* ((cie (funcall ccy))
           ;;                        (eup (funcall eu-currency-p cie))
           ;;                        (preprice (+ (funcall per-teacher)
           ;;                             (* (funcall per-pupil)
           ;;                                (funcall get-current-selection))))
           ;;                        (dkmoms (if eup (- (funcall dansk-moms preprice)
           ;;                                           preprice)
           ;;                                    0)))
           ;;                   (if moms
           ;;                       (funcall ccyfmt dkmoms)
           ;;                       (funcall ccyfmt (+ dkmoms preprice)))))))

           ;; form (lambda ()
           ;;        (with-sub-data
           ;;            (funcall update-free-used)
           ;;            (let* ((container (div-id-class "sub-data-form" 
           ;;                                            "teacher-form"))
           ;;                   (nrvarray (make-integer-roller 
           ;;                              "rollmeover"
           ;;                              "1"
           ;;                              "100"
           ;;                              "1"
           ;;                              nauthorized
           ;;                              "3"))
           ;;                   ;;("change" update)))
           ;;                   (roller (aref nrvarray 0))
           ;;                   (vdator (aref nrvarray 1))
           ;;                   (cie (funcall ccy))
           ;;                   (eup (funcall eu-currency-p cie))
           ;;                   (preprice (+ (funcall per-teacher)
           ;;                             (* (funcall per-pupil)
           ;;                                (funcall get-current-selection))))
           ;;                   (npuh (make-formrow
           ;;                          (make-formcell :text "Unused pupil places: ")
           ;;                          (make-formcell :element roller)))
           ;;                   (enph (make-formrow
           ;;                          (make-formcell :text "Enrolled pupils: ")
           ;;                          (make-formcell :text enrolled)))
           ;;                   (ccyh (make-formrow
           ;;                          (make-formcell :text "Currency: ")
           ;;                          (make-formcell :text cie)))
           ;;                   (tepr (make-formrow
           ;;                          (make-formcell :text "Price/teacher: ")
           ;;                          (make-formcell :text (/ (funcall per-teacher)
           ;;                                                  100))))
           ;;                   (pupr (make-formrow
           ;;                          (make-formcell :text "Price/pupil: ")
           ;;                          (make-formcell :text (/ (funcall per-pupil)
           ;;                                                  100))))
           ;;                   (taxr (make-formrow
           ;;                            (make-formcell :text "EU VAT (Danish rate)")
           ;;                            ;;(make-formcell :text (funcall current true))))
           ;;                            (make-formcell :element
           ;;                                           (let ((taxcell
           ;;                                                  (div-id "taxcell")))
           ;;                                             (append-text taxcell
           ;;                                                          (funcall current true))
           ;;                                             taxcell))))
           ;;                   (totc (make-formrow
           ;;                          (make-formcell :text "Cost/month: ")
           ;;                          (make-formcell :element
           ;;                                         (let ((costcell 
           ;;                                                (div-id "costcell")))
           ;;                                           (append-text costcell
           ;;                                                        (funcall current))
           ;;                                           costcell)))))
           ;;              (add-event-no-capture roller "change" (funcall make-updater vdator))
           ;;              (append-children container
           ;;                               npuh enph ccyh tepr pupr taxr totc)
           ;;              container)))
           
           ;; ;; form (lambda ()
           ;; ;;        (with-sub-data
           ;; ;;            (funcall update-free-used)
           ;; ;;            (let* ((container (div-id-class "sub-data-form" 
           ;; ;;                                            "teacher-form"))
           ;; ;;                   (nrvarray (make-integer-roller 
           ;; ;;                              "rollmeover"
           ;; ;;                              "1"
           ;; ;;                              "100"
           ;; ;;                              "1"
           ;; ;;                              nauthorized
           ;; ;;                              "3"))
           ;; ;;                   ;;("change" update)))
           ;; ;;                   (roller (aref nrvarray 0))
           ;; ;;                   (vdator (aref nrvarray 1))
           ;; ;;                   (cie (funcall ccy))
           ;; ;;                   (npuh (make-formrow
           ;; ;;                          (make-formcell :text "Unused pupil places: ")
           ;; ;;                          (make-formcell :element roller)))
           ;; ;;                   (enph (make-formrow
           ;; ;;                          (make-formcell :text "Enrolled pupils: ")
           ;; ;;                          (make-formcell :text enrolled)))
           ;; ;;                   (ccyh (make-formrow
           ;; ;;                          (make-formcell :text "Currency: ")
           ;; ;;                          (make-formcell :text (funcall ccy))))
           ;; ;;                   (tepr (make-formrow
           ;; ;;                          (make-formcell :text "Price/teacher: ")
           ;; ;;                          (make-formcell :text (/ (funcall per-teacher)
           ;; ;;                                                  100))))
           ;; ;;                   (pupr (make-formrow
           ;; ;;                          (make-formcell :text "Price/pupil: ")
           ;; ;;                          (make-formcell :text (/ (funcall per-pupil)
           ;; ;;                                                  100))))
           ;; ;;                   (taxrow (make-formrow
           ;; ;;                            (make-formcell :text "EU VAT (Danish rate)")
           ;; ;;                            (make-formcell :text (/ (- (dansk-moms
           ;; ;;                                                        (funcall current))
           ;; ;;                                                       (funcall current))
           ;; ;;                                                    100))))
           ;; ;;                   (totc (make-formrow
           ;; ;;                          (make-formcell :text "Cost/month: ")
           ;; ;;                          (make-formcell :element
           ;; ;;                                         (let ((costcell 
           ;; ;;                                                (div-id "costcell")))
           ;; ;;                                           (append-text costcell 
           ;; ;;                                                        (funcall current))
           ;; ;;                                           costcell)))))
           ;; ;;              (add-event-no-capture roller "change" (funcall make-updater vdator))
           ;; ;;              (append-children container
           ;; ;;                               npuh enph ccyh tepr pupr totc)
           ;; ;;              container)))
           
           ;; make-updater (lambda (vdator)
           ;;                (lambda ()
           ;;                  (with-sub-data
           ;;                      (let* ((roller (dgebid "rollmeover"))
           ;;                             (v (funcall vdator (value roller)))
           ;;                             (tc (dgebid "taxcell"))
           ;;                             (cc (dgebid "costcell")))
           ;;                        (setf (value roller) v)
           ;;                        (set-attribute (dgebid interface-id) "data-new-value" v))
           ;;                        (setf (text-content-of tc) (funcall current true)
           ;;                              (text-content-of cc) (funcall current)))))
           enrolled false
           nauthorized false
           ;; cancelled false
           server false
           unused false
           ;;price-data false
           ))

	;;(define-display-xrecord)
        
	(defvar select-pupils-names (ps:array))

	(defvar selected-pupils-to-add (ps:array))
        (defvar selected-pupils-to-delete (ps:array))
        
        (defun test-array (arr test)
          (let ((found false))
            (dolist (ob arr)
              (when (funcall test ob)
                (setf found true)
                break))
            found))

        (defun save-and-block-default-answer-handlers ()
          false) ;;REMOVE (FIXME but it's called in a try throw in a
                 ;;lesson)

        ;; (defun test-cm (initial) ;;FIXME remove just for development
        ;;   (labels ((click-on-id (id)
        ;;              (let ((event (doc-create-event "MouseEvents"))
        ;;                    (cell (doc-get-el-by-id id)))
        ;;                (init-event event "click" false false)
        ;;                (when cell (dispatch-event cell event))))
        ;;            (click-on-obj (obj)
        ;;              (let ((event (doc-create-event "MouseEvents")))
        ;;                (init-event event "click" false false)
        ;;                (dispatch-event obj event)))
        ;;            (after-wait ()
        ;;              (let ((obj (aref (get-elements-by-tag-name
        ;;                                (dgebid "lesson-menu") "div") 3)))
        ;;                (if obj (funcall click-on-obj obj)
        ;;                    (throw "obj not found"))
        ;;                (when initial
        ;;                  (with-slots (ephemeral) lesson-module
        ;;                    (with-slots (tio make-tutor-input summon-tutor) 
        ;;                        ephemeral
        ;;                      ((dots window set-timeout) 
        ;;                       (lambda ()
        ;;                         (setf tio (funcall make-tutor-input "plus" 67 99))
        ;;                         (funcall summon-tutor tio)) 500)))))))
        ;;     (funcall click-on-id "display-lesson-menu")
        ;;     ((dots window set-timeout) after-wait 500)))

        (defun set-work-on-screen (bool)
          (setf work-on-screen bool)
          (labels ((callback (response)
                     ;;FIXME we do nothing with the response
                     ))
            (ajax_teacher_set_work_on_screen owner bool callback)))

        (defun replace-off-screen-answer-handlers (just-block)
          (loop for input in 
               (get-elements-by-tag-name
                (doc-get-el-by-id (table-id 0)) "input")
             for id = (id-of input)
             do (progn (set-attribute input "onkeydown" nil)
                       (set-attribute 
                        input "onclick" (if just-block nil
                                            (strcat "Javascript: "
                                                    'summon-scratch-pad
                                                    "(event, '" id "')"))))))

        (defun save-off-screen-answer-handlers ()
          (loop for input in 
               (get-elements-by-tag-name
                (doc-get-el-by-id (table-id 0)) "input")
             for id = (id-of input)
             do (setf (ps:getprop off-screen-answer-handlers id)
                      (get-attribute input "onkeydown"))))      
        
        (defun close-groups-menu ()
          (let ((grmenu  (ul-menu-open-p "new-gmenu-content")))
            (when grmenu
              (remove-children grmenu))))
                  
       	(defun clear-xrecord ()
	  (let* ((panel (doc-get-el-by-id "right")))
	    (remove-children-from-nth panel 2)))
        
	(defun build-xrecords (tables)
	  (loop for tidx from 0 below (how-many-children tables)
             for table = (aref (children-of tables) tidx)
             for xrec = (ps:create total 0 wrong 0 right 0)
             do  (loop for rowidx from 0 below (how-many-children table)
                    do (let ((row  (aref (children-of table) rowidx)))
                         (loop for cellidx from 0 below (how-many-children row)
                            do (let* ((cell (aref (children-of row) cellidx))
                                      (react (answer-cell-p (id-of cell))))
                                 (when react 
                                   (incf (dots xrec total))
                                   (let ((mark (get-attribute (nth-of-children cell 0) "src"))
                                         (action (get-attribute (nth-of-children cell 0) "onkeydown")))
                                     (if mark
                                         (if (marked-right mark)
                                             (incf (dots xrec right))
                                             (incf (dots xrec wrong)))
                                         (when action 
                                           (add-event-no-capture (nth-of-children cell 0) "keydown" teacher-sum-input)
                                           ))))))))
               (setf (aref xrecords tidx) xrec)))
        
        (defun teacher-sum-input (ev)
          (prevent-default ev)
          (setf (value this) "")
          (alert "Teachers can not enter answers for a pupil."))

        (defvar pupil-table-retrieval-functions 
          (ps:create))
        
	(defun get-table-retrieval-function (pupil fname)
          (let ((fn (ps:getprop pupil-table-retrieval-functions pupil)))
            (if fn fn
                (let ((fn (lambda (event)
                            (cancel-bubble event)
                            (retrieve-table pupil fname))))
                  (setf (ps:getprop pupil-table-retrieval-functions pupil)
                        fn)
                  fn))))

        (defun retrieve-table (pupil fname)
          (setf current-table 0)
          (labels ((callback (response)
                     (setf xrecords (ps:array))
                     (let* ((pr (parse-json response))
                            (tables  (plain-div))
                            (content (doc-get-el-by-id "content"))
                            (waf (dgebid "select-work-area-form")))
                       (set-display waf "none")
                       (set-inner-html tables pr)
		       (clear-content content) ;;FIXME
		       (let ((ntables (how-many-children tables)))
			 (if (or (not (= ntables 1)) (not (equal (id-of (aref (children-of tables) 0))
                                                                 "no_exercises")))
			     (progn
			       (build-xrecords tables)
			       (setf all-tables ntables)
			       (loop for tidx from 0 below all-tables 
                                    for tid = (table-id tidx) ;;here use strict
				  do
				    ;;(setf tid (table-id tidx))
				    (insert-before content
						   (aref (children-of tables) tid)
						   (doc-get-el-by-id "pager"))
				    (when (not (= tidx current-table))
				      (setf (dots (doc-get-el-by-id tid) style display) "none")))
			       (mj-hub-queue (array "Typeset" mj-hub "content"))
                               (if (> all-tables 1)
                                   (set-display (doc-get-el-by-id "pager") "block")
                                   (set-display (doc-get-el-by-id "pager") "none"))
			       (set-caption false)
			       (clear-xrecord)
			       (display-pupil-full-name pupil fname)
			       (mj-hub-queue display-xrecord))
			     (let* ((pager (doc-get-el-by-id "pager"))
                                    (gnames (get-attribute (aref (children-of tables) 0) "data-games"))
                                    (dsptxt (if (string= gnames "unknown")
                                                "The pupil is in the placement program but has not yet logged in."
                                                (strcat "This pupil is working on the "
                                                                             gnames
                                                                             " memory games."))))
                               (clear-xrecord)
                               (display-pupil-full-name pupil fname)
			       (insert-before content 
					      (make-text-for-display "tmp-no-ex" "info-no-ex" 
                                                                     dsptxt)
                                                                     
                                                                     ;;"This pupil has not started any exercises yet.")
                                              pager)
			       (setf (dots pager style display) "none"))
			     )))))
	    (ajax_teacher_get_all_div_tables owner pupil callback)))

        (defun display-demo (tag howmany)
          (with-slots (status data) reply
            (labels ((callback (response)
                       (let* ((reply (parse-json response))
                              (ok status)
                              (demotxt data))
                         (if (string= ok "error")
                             (my-alert data (lambda () false))
                             (let ((demo (plain-div))
                                   (pg (dgebid "pager"))
                                   (content (dgebid "content"))
                                   (bholder (div-class "centering"))
                                   (refresh (make-button "refresh-demo" 
                                                         "New"
                                                         ("click" 
                                                          refresh-demo-content))))
                               (setf all-tables 1)
                               (append-child bholder refresh)
                               (setf (inner-html demo) demotxt)
                               (insert-before content demo pg)
                               (insert-before content bholder pg)
                               (mj-hub-queue (array "Typeset" mj-hub "content"))
                               (mj-hub-queue (array set-caption true))
                               )))))
              (clear-display)
              (let ((right (dgebid "right")))
                (when right (remove-children-from-nth right 1)))
              (ajax_get_demo_exercise owner tag howmany callback))))

        (defun refresh-demo-content (ev)
          (let ((tag (get-attribute (dgebid (table-id current-table)) "data-de")))
            (display-demo tag 0)))
     
	(defun display-pupil-full-name (pupil fname)
	  (let* ((h (make-header fname))
		 (display-area (doc-get-el-by-id "right"))
		 (current (dgebid "pupil-name")))
            (if current
                (replace-child display-area h current)
                (append-child display-area h))
            (set-attributes h "id" "pupil-name" "data-id" pupil
                            "class" "pupil-info")
            (add-event-no-capture h "click" display-pupil-report)))          

        (defun display-pupil-report (ev)
          (let ((self this))
          ;;(alert (strcat "NYI: display-pupil-record: " 
            (labels ((callback (response)
                       (let ((htmltxt (parse-json response))
                             (pr (div-id "pupil-report"))
                             (h (make-header (strcat "Pupil Report: " (text-content-of self))))
                             (but (make-button "print-report" "Print" 
                                               ("click" (lambda (ev)
                                                          ((dots window print))))))
                             (replace (plain-div)))
                         (append-children pr h replace but)
                         (setf (inner-html replace) htmltxt)
                         (clear-display)
                         (display-standard-form pr))))
              (ajax_get_pupil_report owner (get-attribute this "data-id")
                                     callback))))

	(defun group-p (thing)
	  (if (stringp thing)
	      t
	      (let ((next (aref thing 1)))
		(and (stringp (aref thing 0))
		     (or (equal next undefined)
			 (equal (dots next constructor) -array))))))

        (defun enroll-pupil ()
          (with-slots (status data) reply
	  (let-by-id 
	   ((fn "firstname") (sn "surname");; (pw "password") 
            (fg "yes1"))
	   (let* ((firstname (value fn))
                  (surname (value sn))
                  (fullname (strcat firstname " " surname))
                  ;;(password (value pw))
                  (firstgrade (checked fg)))
	     (cond ((or (string= firstname "")
			(string= surname ""))
		    (alert "A pupil must have a first name and a surname")
		    (display-enroll-form))
		   (t
		    (setf (value fn) "")
		    (setf (value sn) "")
		    ;;(setf (value pw) "")
		    (setf (checked fg) false)
		    (labels ((callback (response)
			       (clear-display)
			       (let ((reply (parse-json response)))
                                 (if (string= status "ok")
                                     (let* ((newp (aref data 0))
                                           (all-p (aref data 1))
                                           (usern (aref newp 0))
                                           (pw (aref newp 1))
                                           (npm (doc-get-el-by-id "new-pmenu"))
                                           (pupil (ps:array usern fullname)))
                                       (alert 
                                        (strcat 
                                         (ps:lisp 
                                          (format nil "Pupil enrolled~%username: "))
                                         usern
                                         (ps:lisp (format nil "~%password: ")) pw))
                                       (when npm
                                         (insert-pupil pupil npm))
                                       (setf pupil-list all-p)
                                       (clear-display))
                                     (my-alert "The pupil could not be enrolled, because you have exhausted your quota of pupils.  You can purchase an extension to your quota by accessing your account from the control panel." clear-display)))))
		      (ajax_enroll_pupil owner
					 firstname
					 surname
					 ;;password
					 firstgrade
					 callback) )))))))
        
        (defun cancel-change-group-members ()
	  (setf selected-group-name "")
	  (setf selected-pupils-to-add (ps:array))
	  ;;(setf selected-pupils-names (ps:array)) ;;here use strict
	  (clear-display))
	
	(defun select-or-deselect-pupil (id)
	  (let ((myself (doc-get-el-by-id id)))
	    (if (is-checked myself)
		(add-to-selected-pupils-to-add id)
		(remove-from-selected-pupils-to-add id))))		
	
	(defun select-pupils-for-group ()
	  (setf selected-pupils-to-add (ps:array))
	  (let ((ul (children-of (doc-get-el-by-id "pupils-in-group"))))
	    (dolist (li ul)
	      (let ((dis (aref (get-elements-by-tag-name li "input") 0)))
		(setf (checked dis) false)))
	    (dolist (p select-pupils-names)
	      (let* ((id (aref p 0))
		     (to-check (doc-get-el-by-id id)))
		(push id selected-pupils-to-add)
		(setf (checked to-check) true)))))

	(defun set-checked-names ()
	  (dolist (p select-pupils-names)
	    (let* ((id (aref p 0))
		   (inp (doc-get-el-by-id id)))
	      (setf (checked (doc-get-el-by-id (aref p 0))) true))))
	
        (defun make-selected-pupils-names-setter (group)
	  (lambda (event)
	    (setf selected-group-name group)
	    (let ((lis (get-elements-by-tag-name 
			(doc-get-el-by-id "group-selection-list") "li")))
	      (when lis 
		(dolist (li lis)
		  (let ((inp (aref (get-elements-by-tag-name li "input") 0)))
		    (when (not (string= (id-of inp) group))
		      (setf (checked inp) false)))))
	      (labels ((callback (response)
			 (setf select-pupils-names (parse-json response))
			 ;;I can't let you do that, Dave
			 (select-pupils-for-group)
			 ;;Danger, Will Robinson, Danger!
		       ))
		(ajax_get_select_pupils_list owner group callback)))))
	
	(defun close-all-pupils-menu ()
	  (let ((apm (doc-get-el-by-id "show-all-pupils")))
	    (when (not (equal (nth-of-children apm 0) undefined))
	      (teachers-pupils-folddown))))
		
        
	(defun confirm-delete-pupils (ev)
	  (let* ((form (dgebid "delete-pupil-form"))
		 (inpcoll (gebtag form "input"))
		 (ustr "")
                 (nchecked 0)
		 (del-arr (ps:array)))
	    (dolist (inp inpcoll)
	      (let ((checked (checked inp)))
		(when checked
		  (let ((id (id-of inp)))
		    (setf ustr (strcat ustr (ps:lisp (format nil "~%"))
				       (text-content-of (parent-node inp))))
		    (push id del-arr)
                    (incf nchecked)))))
            (if (> nchecked 0)
                (let ((yes (confirm (strcat 
                                     "WARNING: Deleted pupils can not be restored!"
                                     (ps:lisp (format nil "~%"))
                                     "Delete these pupils?" 
                                     (ps:lisp (format nil "~%")) 
                                     ustr))))
                  (if yes 
                      (security-check (lambda (pw)
                                        (remove-and-delete pw del-arr)))
                      (clear-display)))
                (clear-display))))

        (defun make-select-pupil-and-input-for-display (id)
          (setf selected-pupil "")
          (let ((parent (create-element "form"))
                (arr pupil-list) 
                (ul (create-element "ul")))
            (setf (id-of ul) id)
            (dolist (pupil arr)
              (let* ((name (aref pupil 1))
                     (pid (aref pupil 0))
                     (li (create-element "li"))
                     (div (div-class "display-item"))
                     (butt (create-radiobutton (strcat "sel_" pid) 
                                               :control-name "selpass")))
                (append-text div name)
                (append-child div butt)
                (add-event-no-capture butt "click" select-and-clear-other-pupils)
                (append-child li div)
                (append-child ul li)))
            (append-child parent ul)
            (let ((inp (create-text-input "set-ppass")))
              (append-child parent inp))
            parent))

        (defun make-delete-pupil-form ()
          (let ((deldiv (div-id-class "delete-pupil-form" "teacher-form"))
                (h (make-header "Delete pupils")))
            (append-child deldiv h)
            deldiv))

        (defun make-pupil-radiobox-list (form)
          (setf selected-pupil "") ;;this should be set here??
          (let ((parent (create-element "form"))
                (arr pupil-list) 
                (ul (create-element "ul")))
                    (dolist (pupil arr)
              (let* ((name (aref pupil 1))
                     (pid (aref pupil 0))
                     (li (create-element "li"))
                     (div (div-class "display-item"))
                     (butt (create-radiobutton (strcat "sel_" pid) 
                                               :control-name "ppass")))
                (append-text div name)
                (append-child div butt)
                (add-event-no-capture butt "click" select-and-clear-other-pupils)
                (append-child li div)
                (append-child ul li)))
            (append-child parent ul)
            (append-child form parent)))

        ;;HERE HERE
          (defun display-delete-pupil-form (ev)
          (with-updated-pupils
              (let ((delpupilform (make-delete-pupil-form))
                    (delbut (make-button "" "Delete"))
                    (pupils-display (make-pupil-selection-list 
                                     (create-element "ul"))))
                (setf (id-of pupils-display) "pupils-to-delete")
                (append-child delpupilform pupils-display)
                (append-child delpupilform delbut)
                (add-event-no-capture delbut "click" confirm-delete-pupils)
                (display-standard-form delpupilform))))

        (defun make-pupil-selection-list (list-top handler)
          (dolist (pupil pupil-list list-top) ;; modified list-top returned
            (let* ((li (create-element "li"))
                   (div (div-class "display-item"))
                   (pname (aref pupil 1))
                   (pid (aref pupil 0))
                   (cbx (create-checkbox pid)))
              (when handler (add-event-no-capture cbx "click" handler))
              (append-text div pname)
              (append-child div cbx)
              (append-child li div)
              (append-child list-top li))))

        (defun remove-and-delete (pw todel)
          (labels ((callback (response)
                     (let* ((jso (parse-json response))
                           (status (dots jso status))
                           (newplist (dots jso pupils)))
                       (setf pupil-list newplist)
                       (if (string= status "ok")
                           (dolist (p todel)
                             (let* ((pmp (dgebid (strcat "pm_" p)))
                                    (pmpp (when pmp (parent-node pmp))))
                               (when pmpp (remove-child pmpp pmp))
                               (loop for pgp = (dgebid (strcat "pg_" p)) 
                                  until (not-defined pgp)
                                  for pgpp = (parent-node pgp)
                                  do (remove-child pgpp pgp))
                               (let* ((pinfo (dgebid "right"))
                                      (currp (aref (gebclan pinfo "pupil-info") 
                                                   0)))
                                 (when currp (remove-child pinfo currp)))
                               (clear-display)))
                         (alert "Password error.")))))
            (ajax_delete_pupils owner pw (stringify-json todel) callback)))

        (defun make-set-password-form ()
          (let ((div (div-id-class "set-password-form" "teacher-form"))
                (h (make-header "Set Password"))
                (txt "Select the pupil and click the Set button."))
            (append-child div h)
            (append-text div txt)
            div))
        
        (defun display-pupil-set-password-form (ev)
          (with-updated-pupils 
              (let ((form (make-set-password-form)))
                (make-pupil-radiobox-list form)
                ;;(append-child form (create-text-input "set-ppass"))
                (append-child form (make-button "set-password" "Set" 
                                                ("click" update-pupil-password)))
                (append-child form (make-button "cancel-set-password" "Cancel"
                                                ("click" clear-display)))
                (display-standard-form form))))
        
        (defun update-pupil-password () 
          (labels ((callback (response)
                     (let ((reply (parse-json response)))
                       
                       (my-alert (strcat "Password for: " selected-pupil
                                      (ps:lisp (format nil "~%")) reply) (lambda ()
                                                                           (setf selected-pupil "")
                                                                           (clear-display)
                                                                           true)))))
            (let ((yes (confirm (strcat 
                                 "WARNING: The pupil will not be able to login until YOU give him/her the new password!"))))
              (if yes 
                  (ajax_update_pupil_password 
                   owner selected-pupil callback)
                  (progn (setf selected-pupil "")
                         (clear-display))))))
        
	(defun set-popup-text (pu text)
	  (setf (dots (first-child
		       (nth-of-children pu 0))
		      node-value) 
		(strcat "You can set up " text " pupils.")))

        ;; PUPIL MENU

	(defvar pupil-list (ps:array))

        (defun insert-pupil (p menu) ;;MACRO ??
          (let* ((lookup (aref p 0))
                 (id (strcat "pm_" lookup))
                 (fname (aref p 1))
                 (pupildiv 
                  (div-id-class id "pupil-menu-item")))
            (append-text pupildiv fname)
            (append-child menu pupildiv)
            (add-event-no-capture 
             pupildiv "click"
             (get-table-retrieval-function lookup fname))))
              
        (defun insert-pupils-menu (ev)
          (labels ((build-menu (plist)
                     (let ((menu (div-id-class "new-pmenu" "bottom-menu"))
                           (h1 (create-element "h1"))
                           (txt "All Pupils"))
                       (insert-el menu t)
                       (append-text h1 txt)
                       (append-child menu h1)
                       (setf pupil-list plist)
                       (dolist (p plist)
                         (insert-pupil p menu))
                       menu))
                   (callback (response)
		     (let* ((arr (parse-json response)))
		       (if (not-defined (aref arr 0))
			   (display-enroll-form)
                           (build-menu arr)))))
            (let ((already (doc-get-el-by-id "new-pmenu")))
              (if (not already)
                  (if (not-defined (aref pupil-list 0))
                      (ajax_get_teachers_pupils_list owner callback)
                      (build-menu pupil-list))                  
                  (delete-el already)))))
        
        ;; PUPIL MENU

        (defun make-enroll-pupil-form ()
          (let* ((form (div-id-class "define-pupil-form" "teacher-form"))
                 (h (make-header "Enroll a pupil"))
                 (info (div-id "enroll-pupil-info"))
                 (defpform (div-id "defpupilform"))
                 (fnamerow
                  (make-formrow  
                   (make-formcell :text "First name: ")
                   (make-formcell :element (make-input-type "firstname" "firstname" "text"))))
                 (snamerow 
                  (make-formrow  
                   (make-formcell :text "Surname: ")
                   (make-formcell :element (make-input-type "surname" "surname" "text"))))
                 (g1row 
                  (make-formrow 
                   (make-formcell :text "Grade 1? ")
                   (make-formcell 
                    :text "Yes"
                    :append ((let ((inp1 (make-input-type "yes1" "firstgrade" "radio"))) inp1)
                             (let ((br (create-element "br"))) br)
                             (let ((tn (make-text-node "No"))) tn)
                             (let ((inp2 (make-input-type "no1" "firstgrade" "radio")))
                               (setf (checked inp2) true)
                               inp2)))))
                 (buttrow (make-formrow 
                           (make-formcell :empty t)
                           (make-formcell 
                            :element (make-button "" "Enter" ("click" enroll-pupil))))))
            (setf (inner-html info) (get-enroll-pupil-info))
            (append-children defpform fnamerow snamerow g1row buttrow)
            (append-children form h info defpform))
          (add-event-no-capture (aref (gebtag info "a") 0) "click" upload-file)
          form)
        
        (defun upload-file (ev)
          (prevent-default ev)
          (clear-display)
          (display-upload-form))
        
        (defun make-upload-form ()
          (let ((div (div-id "upload-file-form"))
                (h (make-header "Upload a pupil-list file"))
                (fn (make-input-type "user-filename" "control-me" "file"))
                (butt (make-button "get-help" "Help" 
                                   ("click" upload-instructions)))
                (upl (make-button "get-file" "upload" 
                                  ("click" get-the-file))))
            (set-attribute fn "accept" "text/plain")
            (append-children div h fn butt upl)
            div))

        (defun upload-instructions (ev)
          (clear-display)
          (let ((div (div-id "upload-instruction-form"))
                (h (make-header "Upload Instructions"))
                (info (plain-div))
                (butt (make-button "" "Dismiss" ("click" upload-file))))
            (setf (inner-html info) (get-upload-pupil-file-info))
            (append-children div h info butt)
            (clear-display)
            (display-standard-form div)))

        (defun display-upload-form ()
          (let ((form (make-upload-form)))
            (display-standard-form form "inline-block")))

        (define-file-object-access)

        (defun file-upload-enroll (file)
          (with-slots (status curtailed data pupils) reply
            (labels ((make-display-new (el)
                       (lambda ()
                         (display-standard-form el "inline-block")))
                     (callback (response)
                       (let ((reply (parse-json response)))
                         (if (string= status "ok")
                             (let* ((ps (dgebid "new-pmenu"))
                                    (pstrue (if ps true false)))
                               (when pstrue (let ((parent (parent-node ps)))
                                          (remove-child parent ps)))
                               (setf pupil-list pupils)
                               (when pstrue (insert-pupils-menu))
                               (let ((disp (plain-div));;(div-id "display-enrolled"))
                                     (butt (make-button "print-new-enrollment" 
                                                        "Print" 
                                                        ("click" 
                                                         (lambda () 
                                                           ((dots window print)))))))
                                 (setf (inner-html disp) data) ;;sets div to server generated div with id 'new-enrollment'
                                 (append-child (first-child disp) butt)
                               (if curtailed
                                   (my-alert
                                    "Your pupil quota was exceeded, so not all your requested pupils were enrolled."
                                    (make-display-new (first-child disp)))
                                   (funcall (make-display-new (first-child disp))))))
                             (my-alert data clear-display)))))
              (let* ((reader (ps:new (-file-reader))))
                (setf (dots reader onload) 
                      (lambda (ev)
                        (ajax_file_upload_enroll owner (dots ev target result) callback)))
                ((dots reader read-as-text) file)))))
       
        (defun get-the-file (ev)
          (with-file-object-access
              (let* ((inp (dgebid "user-filename"))
                     (fo (aref (funcall files inp) 0)))
                (when fo
                  (file-upload-enroll fo)))))

        (defun display-enroll-form ()
          (let ((form (make-enroll-pupil-form)))
            (display-standard-form form "table")))
        
        ;; GROUP MENU   

        (defun group-has-hidden-children-p (group)
          (let ((kids (children-of group))
                (ret false))
            (loop for k in kids unless ret
                 do (when (string= (get-style k display) "none")
                      (setf ret true)))
            ret))

        (defvar group-menu-state
          
          (ps:create
           saved-state (ps:new (-object))

           foreach-li
           (lambda (el)
             (with-slots (saved-state) group-menu-state
            (let ((parent (parent-node el)))
              (when (not (string= (get-style parent display) "none"))
                (let* ((pid (id-of parent))
                       (mid (id-of el))
                       (found (ps:getprop saved-state pid)))
                  (if (not found)
                      (when (not (string= (get-style el display) "none"))
                        ;;unless its already saved there (by pre-save)
                        (setf (ps:getprop saved-state pid)
                              (ps:array mid)))
                      (when (not (string= (get-style el display) "none"))
                        (unless (is-included-in mid found)
                          (push mid (ps:getprop saved-state pid))))))))))
        
           foreach-ul
           (lambda (el)
             (with-slots (saved-state) group-menu-state
               (when (not (string= (get-style el display) "none"))
                   (let* ((uid (id-of el))
                          (found (ps:getprop saved-state uid)))
                     (when (not found)
                       (setf (ps:getprop saved-state uid)
                             (ps:array)))))))

           pre-save ;;calling function must check display is not none
           (lambda (group pidlist)
             (with-slots (saved-state) group-menu-state
               (if pidlist ;;means its list items
                   (let ((ul (dgebid group))
                         (found (ps:getprop saved-state group)))
                     (dolist (pid pidlist)
                       (let ((pgpid (strcat "pg_" pid)))
                         (if found (push pgpid (ps:getprop saved-state group))
                             (setf (ps:getprop saved-state group) (ps:array pgpid)
                                   found (ps:getprop saved-state group))))))
                   ;;otherwise its a ul.
                   (let ((found (ps:getprop saved-state group)))
                     (unless found 
                       (setf (ps:getprop saved-state group) (ps:array)))))))
        
           save-state 
           (lambda ()
             (with-slots (foreach-li foreach-ul) group-menu-state
             (let* ((ngmc (dgebid "new-gmenu-content"))
                    (ull (gebtag ngmc "ul"))
                    (lil (gebtag ngmc "li")))
               ((dots -array prototype for-each call) ull foreach-ul)
               ((dots -array prototype for-each call) lil foreach-li))))

           clear-state
           (lambda ()
             (with-slots (saved-state) group-menu-state
             (setf saved-state (ps:new (-object)))))

           restore-state
           (lambda ()
             (with-slots (keys) -object
               (with-slots (saved-state) group-menu-state
                 (let* ((ngmc (dgebid "new-gmenu-content"))
                        (saved))
                   ;;(klist 
                   (dolist  (group (funcall keys saved-state))
                     (let* ((ul (dgebid group))
                            (ms (children-of ul)))
                       (setf (get-style ul display) "list-item"
                             saved (ps:getprop saved-state group))
                       ((dots -array prototype for-each call) 
                        ms (lambda (el) 
                             (when (is-included-in (id-of el) saved) 
                               (setf (get-style el display) "list-item"))))))))))
           
           ))
        
        (defvar selected-group-name "")
        
        (defun select-and-clear-other-groups (ev)
          (setf selected-group-name (subseq (id-of this) 4)))

        (defun select-and-clear-other-pupils (ev)
          (setf selected-pupil (subseq (id-of this) 4)))
        
        (defvar selected-pupil "")

        (defun add-to-selected-pupils-to-add (id)
	  (push id selected-pupils-to-add))

        (defun remove-from-selected-pupils-to-add (id)
	  (setf selected-pupils-to-add
		(remove-string selected-pupils-to-add id)))

        (defun make-select-groups-form ()
          (let* ((sgf (div-id "select-groups-form"))
                 (h1 (make-header "Select group"))
                 (slcont (div-id "selection-lists-container"))
                 (ulgholder (div-id "group-selection-list-container"))
                 (ulg (create-element "ul"))
                 (arr (html-collection-to-array
                       (get-elements-by-tag-name
                        (doc-get-el-by-id 
                         "new-gmenu-content") "ul"))))
            (setf (id-of ulg) "group-selection-list")
            (append-child sgf h1)
            (append-para-text sgf "Select the group, then select the
             pupils you want to be in the group.")
            (dolist (el arr)
              (let* ((group (subseq (id-of el) 3))
                     (li (create-element "li"))
                     (butt (create-radiobutton group))
                     (div (div-class "display-item")))
                (add-event-no-capture butt  "click" 
                                      (make-selected-pupils-names-setter 
                                       group))
                (append-text div group)
                (append-child div butt)
                (append-child li div)
                (append-child ulg li)))
            (append-child ulgholder ulg)
            (append-child slcont ulgholder)
            (append-child sgf slcont)
            sgf))
        
        ;; Form to add or remove pupils from groups
        (defun display-include-pupils-form (ev)
          (with-updated-pupils
              (with-updated-groups
                  (let ((container (div-id-class "add-rem-pupils-form" "teacher-form"))
                        (butc (div-id "a-r-buttc"))
                        (yesdo (make-button "change-group-members" "Change"))
                        (nodo 
                         (make-button "cancel-change-group-members" "Dismiss"))
                        (plcont (div-id "pupil-list-container"))
                        (form (make-select-groups-form))
                        (pupils-display (make-pupil-selection-list
                                          (create-element "ul")
                                          (lambda (ev)
                                            (let ((id (id-of this)))
                                              (if (checked this)
                                                 (add-to-pupils-selected-to
                                                  include id)
                                                 (remove-from-pupils-selected-to
                                                  include id)))))))
                    (setf (id-of pupils-display) "pupils-in-group")
                    (append-child plcont pupils-display)
                    (append-child container form)
                    (add-event-no-capture yesdo "click" change-selected-group)
                    (add-event-no-capture nodo "click" cancel-change-group-members)
                    (append-children butc yesdo nodo)
                    (append-child container butc)
                    (display-standard-form container)
                    (append-child (dgebid "selection-lists-container") plcont)))))
        
        (defun insert-new-group-menu ()
          (let ((menu (div-id-class "new-gmenu" "top-menu"))
                (placeholder (div-id "new-gmenu-content"))
                (h1 (create-element "h1"))
                (txt "All Groups"))
            (append-text h1 txt)
            (append-children menu h1 placeholder)            
            (append-child (dots document body) menu)
            menu))

        (defun create-selgr ()
	  (let ((sgf (div-id "select-groups-form"))
                (h1 (make-header "Select group")))
            (append-child sgf h1)
            (append-para-text sgf "Select the group, then select the
             pupils you want to be in the group.")
	    sgf))

	(defun create-addrempf ()
	  (let ((arpf (div-id "add-rem-pupils-form")))
	    arpf))

        (defun change-selected-group ()
          (let* ((gr selected-group-name)
                 (ps selected-pupils-to-add)
                 (gid (strcat "gr_" gr))
                 (group (dgebid gid)))
            (if group 
                (progn
                  (with-slots (pre-save saved-state) group-menu-state
                    (when (and (not (string= (get-style group display)
                                             "none"))
                               (not (group-has-hidden-children-p group)))
                      (funcall pre-save gid ps)))
                  (labels ((callback (response)
                             (with-restored-group-state
                                 (let ((reply (parse-json response)))
                                   (process-response-group-structure)))))
                    (if (string= gr "")
                        (clear-display)
                        (ajax_change_selected_group 
                         owner (stringify-json ps) gr callback))))
                (alert "Please first create a group."))))

        ;; (defun change-selected-group ()
        ;;   (let ((gr selected-group-name)
        ;;         (ps selected-pupils-to-add))
        ;;     (with-slots (pre-save saved-state) group-menu-state
        ;;       (let* ((gid (strcat "gr_" gr))
        ;;              (group (dgebid gid)))
        ;;         (when (and (not (string= (get-style group display)
        ;;                                  "none"))
        ;;                    (not (group-has-hidden-children-p group)))
        ;;           (funcall pre-save gid ps))))
        ;;     (labels ((callback (response)
        ;;                (with-restored-group-state
        ;;                    (let ((reply (parse-json response)))
        ;;                      (process-response-group-structure)))))
        ;;       (if (string= gr "")
        ;;           (clear-display)
        ;;           (ajax_change_selected_group 
        ;;            owner (stringify-json ps) gr callback)))))

        (defun just-delete-groups (event)
          (labels ((callback (response)
                     (with-restored-group-state
                         (let ((reply (parse-json response)))
                           (process-response-group-structure
                            (clear-display))))))
            (let* ((delform (doc-get-el-by-id "groups-to-delete"))
                   (lis (children-of delform))
                   (arr (ps:array)))
              (dolist (li lis)
                (let ((inp (aref (get-elements-by-tag-name li "input") 0)))
                  (when (checked inp)
                    (let* ((id (subseq (id-of inp) 4))
                           (gid (strcat "gr_" id)))
                    (push id arr)
                    (let* ((grul (dgebid gid))
                           (pn (if grul (parent-node grul) false)))
                      (when pn
                        (remove-child pn grul)))))))
              (if (not-defined (aref arr 0))
                  (progn
                    ;;(setf current-groups-list (ps:array)) ;;remove??????
                    (clear-display))
                  (ajax_just_delete_groups owner (stringify-json arr) callback)))))

        (defun display-remove-group-form (ev)
          (with-updated-groups
              (let* ((form (div-id-class "remove-group-form" "teacher-form"))
                     (h1 (make-header "Select groups to remove"))
                     (butt (make-button "delete-groups-button" "Remove"))
                     (ul (create-element "ul"))
                     (arr (html-collection-to-array
                           (get-elements-by-tag-name 
                            (doc-get-el-by-id "new-gmenu-content") "ul"))))
                (setf (id-of ul) "groups-to-delete")
                (dolist (el arr)
                  (let ((group (subseq (id-of el) 3))
                        (div (div-class "display-item"))
                        (li (create-element "li")))
                    (append-text div group)
                    (append-child div (create-checkbox (strcat "del_" group)))
                    (append-child li div)
                    (append-child ul li)))
                (add-event-no-capture butt "click" just-delete-groups)
                (append-children form h1 ul butt)
                (display-standard-form form))))      
          
        (defun add-group-or-subgroup (event)
          (labels ((callback (response)
                     (with-restored-group-state
                         (process-response-group-structure
                          (let ((inp (or (dgebid "say-sub")
                                         (dgebid "add-a-group"))))
                            (if inp (setf (value inp) "")
                                (clear-display)))))))
            (let* ((defined (doc-get-el-by-id "say-sub"))
                   (inp (if defined defined (previous-sibling this)))
                   (top (if defined selected-group-name "_top_"))
                   (gnu (value inp))
                   ;;push onto saved-state if parent is _top_ or is not hidden
                   (fl (char-code-at gnu 0)))
              (if (and defined (string= "" selected-group-name))
                  (alert "You must first select the group in which you
                  want the subgroup to be placed.")
                  (if (string= gnu "")
                      (clear-display)
                      (cond  ((test-array ;;current-groups-list 
                               (html-collection-to-array 
                                (gebtag (dgebid "new-gmenu-content") "ul"))
                               (lambda (x)
                                 (string= gnu (subseq (id-of x) 3))))
                              (alert "Groups names must be unique.")
                              (setf (value inp) ""))
                         ((not (and (<= 65 fl) (>= 90 fl)))
                          (alert (strcat "Group names must start with a"
                                         (ps:lisp #\Newline)
                                          "capital letter from A to Z inclusive."))
                          (setf (value inp) ""))                          
                         (t
                          (with-slots (pre-save saved-state) group-menu-state
                            (let ((parent (if (string= top "_top_")
                                              (dgebid "new-gmenu-content")
                                              (dgebid (strcat "gr_" top)))))
                              (when (and (not (group-has-hidden-children-p
                                                   parent))
                                         (or (string= top "_top_")
                                             (not 
                                              (string= (get-style parent display) 
                                                       "none"))))
                              (funcall pre-save (strcat "gr_" gnu) false))))
                          (ajax_add_this_group owner top gnu callback))))))))
        
        (defun display-add-group-form (ev)
          (with-updated-groups
              (let* ((form (div-id-class "add-group-form" "teacher-form"))
                     (h1 (make-header "Add a group"))
                     (inp (create-element "input"))
                     (butt (make-button "add-this-group-button" "Add")))
                (setf (id-of inp) "add-a-group")
                (set-attribute inp "type" "text")
                (append-child form h1)
                (append-para-text 
                 form 
                 "Group names must be unique and start with a capital letter.")
                (append-children form inp butt)
                (add-event-no-capture butt "click" add-group-or-subgroup)
                (display-standard-form form))))

        (defun display-add-subgroup-form (ev)
          (with-updated-groups
              (setf selected-group-name "")
            (if (zerop (length (get-elements-by-tag-name
                                (dgebid "new-gmenu-content") "ul")))
                (alert "Please first define a group, before adding subgroups.")
            (let ((form (div-id-class "add-subgroup-form" "teacher-form"))
                  (h1 (make-header "Add a subgroup"))
                  (button (make-button "add-subgroup-button" "Add"))
                  (parent (create-element "form"))
                  (ul (create-element "ul"))
                  (arr (html-collection-to-array
                        (get-elements-by-tag-name 
                         (doc-get-el-by-id "new-gmenu-content") "ul"))))
              (add-event-no-capture button "click" add-group-or-subgroup)
              (append-child form h1)
              (setf (id-of ul) "add-a-subgroup")
              (dolist (el arr)
                (let* ((group (subseq (id-of el) 3))
                      (li (create-element "li"))
                      (div (div-class "display-item"))
                      (rad (create-radiobutton (strcat "sel_" group)
                                               :control-name "selg"
                                               )))
                  (add-event-no-capture rad "click" select-and-clear-other-groups)
                  (append-text div group)
                  (append-child div rad)
                  (append-child li div)
                  (append-child ul li)))
              (append-child parent ul)
              (let ((inp (create-text-input "say-sub")))
                (append-child parent inp))
              (append-children form parent button)
              (display-standard-form form)))))

        (defun display-standard-form (form special)
          (let ((content (doc-get-el-by-id "content"))
                (pager (doc-get-el-by-id "pager")))
            (clear-display)
            (setf (get-style pager display) "none")
            (setf xrecords (ps:array)
                  current-table 0)
            (clear-xrecord)
            (insert-before content form pager)
            (setf (get-style form display) (if special special "block"))))

        ;; DOM STRUCTURE/DISPLAY AND VARIABLE FOR GROUPS
        ;; (doc-get-el-by-id "new-gmenu-content"))

        (defun get-groups-html-structure (ev)
          (with-slots (init-element) drag-drop
            (labels ((callback (response)
                       (let ((script (parse-json response))
                             (ngm (doc-get-el-by-id "new-gmenu"))
                             (placeholder (doc-get-el-by-id "new-gmenu-content"))
                             (ngmc (plain-div)))
                         (setf (inner-html ngmc) script)
                         (replace-child ngm ngmc placeholder)
                         (setf (id-of ngmc) "new-gmenu-content")
                         (install-gmenu-handlers))))
              (let ((ngm (doc-get-el-by-id "new-gmenu")))
                (if ngm
                    (let ((gmd (style-display ngm)))
                      (if (string= gmd "none")
                          (setf (style-display ngm) "inline-block")
                          (setf (style-display ngm) "none")))
                    (progn 
                      (insert-new-group-menu)
                      (funcall init-element "new-gmenu");;now gmenu exists
                      (ajax_get_groups_html owner callback)))))))

        ;;GAMES MENU

        ;;(defun display-games-menu ()
          
        (defun display-special-content-menu ()
          (with-slots (init-element) drag-drop
            (let ((men (div-id-class "replay-menu" "top-menu shift-right"))
                  (h1 (create-element "h1")))
              (append-text h1 "Memory Games")
              (append-child men h1)
              (append-child (dots document body) men)
              (add-event-no-capture men "click" open-special-content-menu)
              (funcall init-element men)
              men)))

        
        (defun open-games-menu (ev)
          (let ((menu (dgebid "replay-menu")))
            (if menu (let ((md (style-display menu)))
                       (if (string= md "none")
                           (setf (style-display menu) "inline-block")
                           (setf (style-display menu) "none")))
                (append-child (dots document body) 
                              (display-special-content-menu)))))

        ;;(alert "Menu missing!"))))

        ;;EXERCISES MENU

        (defun display-exercises-menu ()
          (with-slots (init-element) drag-drop
            (let ((exm (div-id-class "exercise-menu" "top-menu"))
                  (h (make-header "Exercises")))
              (append-child exm h)
              ;;(display-standard-form lessm "inline-block")
              (append-child (dots document body) exm)
              ;;(funcall init-element exm)
              exm)))

        (defun open-exercise-menu (ev)
          (with-slots (status data current) jso
            (let ((menu (dgebid "exercise-menu")))
              (if menu (let ((md (style-display menu)))
                         (if (string= md "none")
                             (setf (style-display menu) "inline-block")
                             (setf (style-display menu) "none")))
                  (progn
                    (setf menu (display-exercises-menu))
                    (labels ((callback (response)
                               (let ((jso (parse-json response)))
                                 (dolist (desc data)
                                   (if (string= (aref desc 0) current)
                                       (append-child menu (make-class-exercise-item
                                                           desc true))
                                       (append-child menu (make-class-exercise-item
                                                           desc false))))
                                 (mj-hub-queue (array "Typeset" mj-hub menu))
                                 false)))
                      (ajax_get_exercises_didactic_elements owner callback)))))))

        (defun make-class-exercise-item (desc current)
          (let* ((tag (aref desc 0))
                 (caption (aref desc 1))
                 (short (aref desc 2))
                 (long (aref desc 3))
                 (item (div-class "exercise-menu-item"))
                 (menudiv (div-class "hover-class"))
                 (popupdiv (plain-div))
                 (ptn (make-text-node long)))            
            (append-text menudiv (strcat caption ": " short ))
            (when current (setf (id-of menudiv) "current-exercise"))
            (set-width popupdiv "20em")
            (append-child popupdiv ptn)
            (append-child menudiv popupdiv)
            (append-child item menudiv)
            (add-event-no-capture item "click" 
                                  (make-class-exercise-item-handler tag 10))
            ;;(mj-hub-queue (array "Typeset" mj-hub item))
            item))

        (defun make-class-exercise-item-handler (tag n)
          (lambda (ev)
            (let ((prev (dgebid "current-exercise")))
              (when (not (equal prev this))
                (when prev (setf (id-of prev) ""))
                (setf (id-of this) "current-exercise")))
            (display-demo tag n)))

        (defun check-sum (aid value)
          (with-slots (communicate problem-stack) scratchpad
            (with-slots (empty ps-next) problem-stack
              (labels ((callback (response)
                         (let* ((taxfree (parse-json response))
                                (newmath (aref taxfree 0)))
                           (if (and (numberp newmath)
                                    (= newmath 1))
                               (alert-input-error (aref taxfree 1))
                               (let* ((qid (id-to aid "Q"))
                                      (oldmath (aref (mj-get-all-jax qid) 0))
                                      (tempdiv (plain-div))
                                      (mark (progn (set-inner-html tempdiv (aref taxfree 1))
                                                   (first-child tempdiv)))
                                      (answer-cell (doc-get-el-by-id aid))
                                      (spmsg (doc-get-el-by-id "messages"))
                                      (input-field (nth-of-children answer-cell 0)))
                                 (mj-hub-queue (array "Text" oldmath newmath))
                                 (replace-child answer-cell mark input-field)
                                 (cond ((equal (get-attribute mark "src") correct)
                                        ;;(incf (dots (aref xrecords current-table) right))
                                        (when spmsg
                                          (funcall communicate "Correct!")))
                                       (t 
                                        ;;(incf (dots (aref xrecords current-table) wrong))
                                        (when spmsg
                                          (funcall communicate "No, that was wrong."))))
                                 ;;(display-xrecord)
                                 ;;(maybe-update-module)
                                 ))
                           )))
                (if (string= value "") nil
                    (ajax_teacher_check_sum owner aid value callback))))))

        (defun hide-interactive-teacher-menu (morid)
          (when (string= (typeof morid) "string")
            (setf morid (dgebid morid)))
          (when morid
            (setf (style-display morid) "none")))

        ;;LESSON REVIEW MENU

        (defun display-teacher-lessons-menu ()
          (with-slots (init-element) drag-drop
          (let ((lessm (div-id-class "lesson-menu" "top-menu"))
                (h (make-header "Lessons")))
            (append-child lessm h)
            ;;(display-standard-form lessm "inline-block")
            (append-child (dots document body) lessm)
            (funcall init-element lessm)
            lessm)))

        (defun open-lesson-review-menu (ev)
          (with-slots (status data) jso
            (let ((menu (dgebid "lesson-menu")))
              (if menu (let ((md (style-display menu)))
                         (if (string= md "none")
                             (setf (style-display menu) "inline-block")
                             (setf (style-display menu) "none")))
                  (progn
                    (setf menu (display-teacher-lessons-menu))
                    (labels ((callback (response)
                               (let ((jso (parse-json response)))
                                 (dolist (desc data)
                                   (append-child menu 
                                                 (make-lesson-review-item desc)))
                                 false)))
                      (ajax_teacher_lesson_review_items owner callback)))))))
        
        (defun make-lesson-review-item-handler (modname)
          (lambda (ev)
            (cancel-bubble ev)
            (setf can-quit-lesson true)
            ;;(let ((rev (doc-get-el-by-id "lesson-menu")))
            (set-lesson modname)))
        
        (defun set-lesson (imod)
          (with-slots (initialized) lesson-module
            (if (not initialized)
                (load-non-ephemeral-first imod)
                (load-ephemeral imod))))
        
        ;; CONTROL PANEL

        (defun popup-center-form (form)
        (let ((cd (dgebid "content")))
          (grey-out)
          (setf (z-index form) (1+ grey-out-z-index))
          (append-child cd form)))

        (defun remove-form (form)
        (let ((pn (parent-node form)))
          (remove-child pn form)
          (remove-grey-out)))
        
        (defun security-value ()
        (let* ((form (dgebid "security"))
               (ret (value (aref (gebtag form "input") 0))))
          (remove-form form)
          ret))

        (defun security-check (fun)
          (let ((sc (div-id-class "security" "popup-center"))
                (hdr (make-header "Password"))
                (inph (plain-div))
                (inp (make-text-input-element 20 20 :password t))
                (but (make-button "seccheck" "OK" 
                                  ("click" (lambda ()
                                             (funcall fun (security-value)))))))
            (append-child inph inp)
            (append-children sc hdr inph but)
            (popup-center-form sc)))

        (defun make-ifo-okbutt-handler (pw ifoid)
          ;; (cond ((string= ifoid "chgsub")
          ;;        (lambda ()
          ;;          (alert "CHGSUB disabled")))
          ;;       ;; ((string= ifoid "cansub")
          ;;       ;;  (lambda ()
          ;;       ;;    (alert "CANSUB disabled")))
          ;;       (t
          (lambda ()
            (let ((nval (get-ifo-value ifoid))
                  (ifo (dgebid ifoid))
                  (ppf (make-patience-please)))
              (if nval
                  (progn
                    (ajax_configure_account
                     owner pw ifoid
                     nval (make-config-account-responder ifoid))
                    (when ifo (remove-form ifo))
                    (popup-center-form ppf))
                  (remove-form ifo)))))
        
        (defun make-config-account-responder (ifid)
          (cond ((string= ifid "chgsub")
                 (lambda (resp)
                   (let* ((ifo (dgebid ifid))
                          (ppf (dgebid "patience-please"))
                          (jso (parse-json resp))
                          (status (dots jso status))
                          (nauth (dots jso nauth))
                          (response (dots jso response)))
                     (cond ((string= status "ok")
                            (setf (dots sub-data nauthorized) (parse-int nauth))
                            (alert (strcat response ": " nauth)))
                           (t
                            (alert (strcat status ": "  response))))
                     (when ppf (remove-form ppf))
                     (when ifo (remove-form ifo))
                     status)))
                (t
                 (lambda (resp)
                   (let* ((ifo (dgebid ifid))
                          (ppf (dgebid "patience-please"))
                          (jso (parse-json resp))
                          (status (dots jso status))
                          (response (dots jso response)))
                     (alert (strcat status ": " response))
                     (when (string= ifid "cansub")                           
                       (funcall (dots location reload) true))
                     (when ppf (remove-form ppf))
                     (when ifo (remove-form ifo))
                     status)))))
            
        (defun make-patience-please ()
        (let ((ppf (div-id-class "patience-please" "popup-center")))
          (append-text ppf "Please be patient: Your request is being processed.")
          ppf)) 

        (defun get-ifo-value (idoro)
          (let ((el (if (stringp idoro)
                        (dgebid idoro)
                        idoro)))
            (when el (get-attribute el "data-new-value"))))
        
        ;; (defun validate-email (estr)
        ;; ;;approximation
        ;; (let ((re (ps:regex "/^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,6}$/i")))
        ;;   (and (not (string= estr ""))
        ;;        (matches re estr))))
        
        ;; (defun make-na-validator (defa)
        ;;   (lambda (na)
        ;;     (cond ((< na 0) defa)
        ;;           ((> na 100) defa)
        ;;           (t na))))                

        ;; (defun set-data-new-email ()
        ;;   (let* ((p (aref (gebtag (dgebid "nemail") "p") 0))
        ;;          (ema1 (dgebid "nema1"))
        ;;          (sema1 (value ema1))
        ;;          (ema2 (dgebid "nema2"))
        ;;          (sema2 (value ema2)))
        ;;     (cond ((and (string= sema1 sema2)
        ;;                 ;;(not (string= sema1 "")))
        ;;                 (validate-email sema1))
        ;;            (remove-class p "underline")
        ;;            (set-attribute (dgebid "nemail") "data-new-value" sema1))
        ;;           ((and (not (string= sema1 ""))
        ;;                 (not (string= sema2 "")))
        ;;            (add-class p "underline")
        ;;            (setf (value ema1) ""
        ;;                  (value ema2) ""))
        ;;           ((and (string= sema1 sema2)
        ;;                 (not (string= sema1 ""))
        ;;                 (not (validate-email sema1)))
        ;;            (setf (value ema1) ""
        ;;                  (value ema2) ""))
        ;;           (t false))))

        ;; (defun make-change-email-if (pw)
        ;;   (let* ((sc (div-id-class "nemail" 
        ;;                            "popup-center teacher-form"))
        ;;          (hdr (make-header "Update your email address"))
        ;;          (inph (div-class "table-input"))
        ;;          (em1 (make-formrow
        ;;                (make-formcell :text "New email  ")
        ;;                (make-formcell :element
        ;;                               (make-input-type 
        ;;                                "nema1" "ema1" "text"
        ;;                                ("blur" set-data-new-email)
        ;;                                ))))
        ;;          (em2 (make-formrow
        ;;                (make-formcell :text "Repeat email ")
        ;;                (make-formcell :element
        ;;                               (make-input-type 
        ;;                                "nema2" "ema2" "text"
        ;;                                ("blur" set-data-new-email)))))
        ;;          (but (make-button "genbut" "OK" 
        ;;                            ("click" 
        ;;                             (make-ifo-okbutt-handler pw "nemail")))))
        ;;     (append-child sc hdr )
        ;;     (append-para-text sc "Please ensure that your email entries match exactly and are correct.")
        ;;     (append-children inph em1 em2)
        ;;     (append-children sc inph but)
        ;;     sc))

        ;; Replaced by make-ifo-okbutt-handler
        ;; (lambda ()
        ;;   (let ((nval (get-ifo-value sc)))
        ;;     (if nval
        ;;         (ajax_configure_account
        ;;          owner pw "nemail"
        ;;          nval (make-config-account-responder "nemail"))
        ;;         (remove-form sc))))

        (defun make-change-passw-if (pw)
          (let* ((sc (div-id-class "npass" 
                                   "popup-center teacher-form"))
                 (hdr (make-header "Request a new password"))
                 (inph (div-class "table-input"))
                 (em1 (make-formrow
                       (make-formcell :text "Yes, I want a new password.")
                       (make-formcell :element
                                      (create-radiobutton 
                                       "npass1" 
                                       :handler ("click" 
                                                 (lambda ()
                                                   (set-attribute sc "data-new-value" true)))
                                       :control-name "newpass"))))
                 (em2 (make-formrow
                       (make-formcell :text "No, cancel the request.")
                       (make-formcell :element
                                      (create-radiobutton 
                                       "npass2" 
                                       :handler ("click" 
                                                 (lambda ()
                                                   (remove-attribute sc "data-new-value")))
                                       :control-name "newpass"))))
                 (but (make-button "genbut" "OK" 
                                   ("click" (make-ifo-okbutt-handler pw "npass")))))
            (append-child sc hdr )
            (append-para-text sc "If you are sure you want a new password select 'Yes'. Remember to make a written record of your new password.")
            (append-children inph em1 em2)
            (append-children sc inph but)
            sc))

        ;; (defun make-already-cancelled-if (pw)
        ;;   (let* ((sc (div-id-class "cansub" 
        ;;                            "popup-center teacher-form"))
        ;;          (hdr (make-header "Cancel subscription"))
        ;;          (inph (div-class "table-input"))
        ;;          (em1 (make-formrow
        ;;                (make-formcell :text "Yes, I want to reenable my subscription.")
        ;;                (make-formcell :element
        ;;                               (create-radiobutton 
        ;;                                "cansub1" 
        ;;                                :handler ("click" 
        ;;                                          (lambda ()
        ;;                                            (set-attribute sc "data-new-value" true)))
        ;;                                :control-name "cansub"))))
        ;;          (em2 (make-formrow
        ;;                (make-formcell :text "No, I want it to stay cancelled.")
        ;;                (make-formcell :element
        ;;                               (create-radiobutton 
        ;;                                "npass2" 
        ;;                                :handler ("click" 
        ;;                                          (lambda ()
        ;;                                            (remove-attribute sc "data-new-value")))
        ;;                                :control-name "cansub"))))
        ;;          (but (make-button "genbut" "OK" 
        ;;                            ("click" (make-ifo-okbutt-handler pw "cansub")))))
        ;;     (append-child sc hdr )
        ;;     (append-para-text sc "Your subscription is already cancelled.  If you want to reenable it, select 'Yes'.")
        ;;     (append-children inph em1 em2)
        ;;     (append-children sc inph but)
        ;;     sc))

        ;; (defun make-cancel-subs-if (pw)
        ;;   (with-sub-data
        ;;       (if cancelled
        ;;           (make-already-cancelled-if pw)
        ;;           (let* ((sc (div-id-class "cansub" 
        ;;                                    "popup-center teacher-form"))
        ;;                  (hdr (make-header "Cancel subscription"))
        ;;                  (inph (div-class "table-input"))
        ;;                  (em1 (make-formrow
        ;;                        (make-formcell :text "Yes, I want to cancel my subscription.")
        ;;                        (make-formcell :element
        ;;                                       (create-radiobutton 
        ;;                                        "cansub1" 
        ;;                                        :handler ("click" 
        ;;                                                  (lambda ()
        ;;                                                    (set-attribute sc "data-new-value" true)))
        ;;                                        :control-name "cansub"))))
        ;;                  (em2 (make-formrow
        ;;                        (make-formcell :text "No, I want to keep my subscription.")
        ;;                        (make-formcell :element
        ;;                                       (create-radiobutton 
        ;;                                        "npass2" 
        ;;                                        :handler ("click" 
        ;;                                                  (lambda ()
        ;;                                                    (remove-attribute sc "data-new-value")))
        ;;                                        :control-name "cansub"))))
        ;;                  (but (make-button "genbut" "OK" 
        ;;                                    ("click" (make-ifo-okbutt-handler pw "cansub")))))
        ;;             (append-child sc hdr )
        ;;             (append-para-text sc "If you are sure you want to cancel your subscription, select 'Yes'.")
        ;;             (append-children inph em1 em2)
        ;;             (append-children sc inph but)
        ;;             sc))))

        ;; (defun make-cancel-subs-if (pw)
        ;;   (let* ((sc (div-id-class "cansub" 
        ;;                            "popup-center teacher-form"))
        ;;          (hdr (make-header "Cancel subscription"))
        ;;          (inph (div-class "table-input"))
        ;;          (em1 (make-formrow
        ;;                (make-formcell :text "Yes, I want to cancel my subscription.")
        ;;                (make-formcell :element
        ;;                               (create-radiobutton 
        ;;                                "cansub1" 
        ;;                                :handler ("click" 
        ;;                                          (lambda ()
        ;;                                            (set-attribute sc "data-new-value" true)))
        ;;                                :control-name "cansub"))))
        ;;          (em2 (make-formrow
        ;;                (make-formcell :text "No, I want to keep my subscription.")
        ;;                (make-formcell :element
        ;;                               (create-radiobutton 
        ;;                                "npass2" 
        ;;                                :handler ("click" 
        ;;                                          (lambda ()
        ;;                                            (remove-attribute sc "data-new-value")))
        ;;                                :control-name "cansub"))))
        ;;          (but (make-button "genbut" "OK" 
        ;;                            ("click" (make-ifo-okbutt-handler pw "cansub")))))
        ;;     (append-child sc hdr )
        ;;     (append-para-text sc "If you are sure you want to cancel your subscription, select 'Yes'.")
        ;;     (append-children inph em1 em2)
        ;;     (append-children sc inph but)
        ;;     sc))
        
        ;; (defun make-chg-subs-if (pw)
        ;;   (let* ((sc (div-id-class "chgsub" 
        ;;                            "popup-center teacher-form"))
        ;;          (hdr (make-header "Change subscription"))
        ;;          (inph (div-class "table-input"))
        ;;          (em1 (make-formrow
        ;;                (make-formcell :text "Yes, change how many pupils I am able to enroll.")
        ;;                (make-formcell :element
        ;;                               (create-radiobutton 
        ;;                                "chgsub1" 
        ;;                                :handler ("click" 
        ;;                                          (lambda () true))
        ;;                                            ;;(set-attribute sc "data-new-value" true))) ;;FIXME not 'true' but number chosen from scrolldown
                                                   
        ;;                                :control-name "chgsub"))))
        ;;          (em2 (make-formrow
        ;;                (make-formcell :text "No, keep my subscription as it is.")
        ;;                (make-formcell :element
        ;;                               (create-radiobutton 
        ;;                                "chgsub2" 
        ;;                                :handler ("click" 
        ;;                                          (lambda ()
        ;;                                            (remove-attribute sc "data-new-value")))
        ;;                                :control-name "chgsub"))))
        ;;          (subdf (with-sub-data
        ;;                     (funcall form)))
        ;;          (but (make-button "genbut" "OK" 
        ;;                            ("click" (make-ifo-okbutt-handler pw "chgsub")))))
        ;;     (append-child sc hdr )
        ;;     (append-para-text sc "If you are sure you want to change your subscription, select how many pupils you want to be able to enroll then select 'Yes'.")
        ;;     (append-children inph subdf em1 em2)
        ;;     (append-children sc inph but)
        ;;     sc))

        ;; (defun make-no-change-if ()
        ;;   (let* ((sc (div-id-class "chgsub" 
        ;;                            "popup-center teacher-form"))
        ;;          (hdr (make-header "Change subscription"))
        ;;          (but (make-button "genbut" "OK" 
        ;;                            ("click" (lambda ()
        ;;                                       (remove-form sc))))))
        ;;     (append-child sc hdr)
        ;;     (append-para-text sc "Only uncancelled, paid subscriptions can be altered.")
        ;;     (append-child sc but)
        ;;     sc))

        
        ;; (defun change-email ()
        ;;    (lambda (pw)
        ;;        (popup-center-form (make-change-email-if pw))))
               
         (defun new-password ()
           (lambda (pw)
             (popup-center-form (make-change-passw-if pw))))
         
         ;; (defun cancel-subscription ()
         ;;   (lambda (pw)
         ;;     (popup-center-form (make-cancel-subs-if pw))))

         ;; (defun change-subscription ()
         ;;  (lambda (pw)
         ;;    (popup-center-form (make-chg-subs-if pw))))

        ;; (defun cant-change-yet ()
        ;;   (popup-center-form (make-no-change-if)))

        (defun make-config-form ()
          (let ((form (div-id-class "config-menu-form" "teacher-form"))
                (hdr (create-element "h1"))
                ;; (ema (make-formrow 
                ;;       (make-formcell :text "Change email")
                ;;       (make-formcell :element
                ;;                      (create-radiobutton
                ;;                       "emabutt" 
                ;;                       :handler ("click" (lambda ()
                ;;                                           (security-check
                ;;                                            (change-email))))
                ;;                       :control-name "confacc"))))
                (psw (make-formrow
                      (make-formcell :text "New Password")
                      (make-formcell :element
                                     (create-radiobutton 
                                      "pswbutt" 
                                      :handler ("click" (lambda ()
                                                          (security-check
                                                           (new-password))))
                                      :control-name "confacc"))))
                ;; (csb (make-formrow
                ;;       (make-formcell :text "Cancel subscription")
                ;;       (make-formcell :element 
                ;;                      (create-radiobutton
                ;;                       "csbbut" 
                ;;                       :handler ("click" (lambda ()
                ;;                                           (security-check
                ;;                                            (cancel-subscription))))
                ;;                       :control-name "confacc"))))
                ;; (chg (make-formrow
                ;;       (make-formcell :text "Change subscription")
                ;;       (make-formcell :element 
                ;;                      (create-radiobutton
                ;;                       "chgbut" 
                ;;                       :handler ("click" (lambda ()
                ;;                                           (with-sub-data
                ;;                                               (if nauthorized
                ;;                                                   (security-check 
                ;;                                                    (change-subscription))
                ;;                                                   (cant-change-yet)))))
                ;;                       :control-name "confacc"))))
                (dish (plain-div))
                (dis (make-formrow
                      (make-formcell :empty "confacc-e")
                      (make-formcell :element 
                                     (make-button "disscacc"
                                                  "Dismiss"
                                                  ("click" (lambda (ev)
                                                             (let ((cm (dgebid "config-menu-form"))
                                                                   (cmp (dgebid "content")))
                                                               (remove-child cmp cm))))
                                                  ))))
                (tit (make-text-node "Configure account")))
            (append-child hdr tit)
            (append-child dish dis)
            (add-class dish "spacer")
            ;;(append-children form hdr ema psw csb chg dish)
            (append-children form hdr psw dish)
            form))

        (defun display-config-menu (ev)
          (let ((form (make-config-form)))
            (display-standard-form form)))
        
        (defun insert-control-panel ()
          (let ((ctrl (div-id-class "control-panel" "ctrl"))
                (h1 (make-header "Control Panel"))
                (ah (make-header "Account" "h2"))
                (th (make-header "Teaching" "h2"))
                (gh (make-header "Act on groups" "h2"))
                (ph (make-header "Act on pupils" "h2"))
                (displess (make-ctrl-item "display-lesson-menu" teaching "Lessons"
                                         open-lesson-review-menu))
                (dispex (make-ctrl-item "display-exercise-menu" teaching "Exercises"
                                        open-exercise-menu))
                (games (make-ctrl-item "display-games-menu" teaching "Games"
                                       open-games-menu))
                ;;PREV NEW
                (confacc (make-ctrl-item "configure-account" account "Configure"
                                         display-config-menu))
                (openpupils (make-ctrl-item "open-pmenu" pupil "Show/Hide Pupils"
                                            insert-pupils-menu))
                (setppass (make-ctrl-item "set-ppass" pupil "Set password"
                                          display-pupil-set-password-form))
                (enrollpupil (make-ctrl-item "enroll-pupil" pupil 
                                             "Enroll a pupil"
                                             display-enroll-form))
                (delpupil (make-ctrl-item "del-pupil" pupil "Delete a pupil"
                                          display-delete-pupil-form))
                (opengroups (make-ctrl-item "open-gmenu" group "Show Groups"
                                            get-groups-html-structure))
                (addp2g (make-ctrl-item "add-pupil" group "Include pupils"
                                        display-include-pupils-form))
                (addgroup (make-ctrl-item "add-group" group "Add a group"
                                          display-add-group-form))
                (delgroup (make-ctrl-item "del-group" group "Delete a group"
                                          display-remove-group-form))
                (addsubg 
                 (make-ctrl-item "Add-subgroup" group "Add a subgroup"
                                 display-add-subgroup-form)))
            (append-children ctrl
                             h1
                             ah
                             confacc
                             th
                             displess
                             dispex
                             games
                             gh
                             opengroups
                             addgroup
                             delgroup
                             addsubg
                             addp2g
                             ph
                             openpupils
                             setppass
                             enrollpupil
                             delpupil
                             )
            (insert-el ctrl)))      

        (defun show-new-gmenu (ev)
          (let ((content (doc-get-el-by-id 
                          "new-gmenu-content")))
            (toggle-display-list-items false content)))

        (defun install-gmenu-handlers ()
          (let* ((gmenu (doc-get-el-by-id "new-gmenu"))
                 (top (first-child gmenu))
                 (els (get-elements-by-class-name gmenu "group-menu-item")))
            (add-event-no-capture top "click" show-new-gmenu)
            (dolist (el els)
              (let ((type (get-attribute el "data-type")))
                (cond ((string= type "group")
                       (add-event-no-capture el "click" toggle-display-list-items))
                      ((string= type "pupil")
                       (let ((pupil (subseq (id-of el) 3))
                             (fname (text-content-of el)))
                         (add-event-no-capture el "click" 
                                               (get-table-retrieval-function pupil fname))))
                      
                      (t (ps:throw (strcat "Unknown data-type: " type))
                         ))))))

        (defun toggle-display-list-items (ev ul)
          (when ev (cancel-bubble ev))
          (let ((list-items (children-of (if ul ul this))))
            (dolist (li list-items)
              (let ((display (get-style li display)))
                (if (string= display "none")
                    (setf (get-style li display) "list-item")
                    (setf (get-style li display) "none"))))))
;;;;; ADDING SPC

        ;;ROLE SPECIFIC NOT COMMON
        (defun load-special-content-script (modname cname data)
          (with-slots (init-script teacher-demo
                       start voluntary 
                       voluntary-game game-name) content-object
            (labels ((callback (response)
                       (let* ((rep (parse-json response))
                            (type (aref rep 0))
                            (script (aref rep 1)))
                       (if (and type script (string= type "SCRIPT"))
                           (eval script)
                           (alert (strcat "Unknown special-content: "
                                          (ps:lisp #\Newline)
                                          type)))
                       (dolist (el (ps:array "lesson-menu"
                                             "new-gmenu"
                                             "new-pmenu"
                                             "exercise-menu"))
                         (hide-interactive-teacher-menu el))
                       (with-hidden-elements
                           ((pager "pager") (revmenu "review-menu")
                            (cpanel "control-panel")
                            (repmenu "replay-menu") (right "right"))
                         (setf game-name cname
                               voluntary true
                               teacher-demo true
                               start (lambda ()
                                       (funcall voluntary-game 
                                                (aref data 0)
                                                (aref data 1))))
                         (funcall start)))))
            (ajax_get_voluntary_special_content modname callback))))

        ;;ROLE SPECIFIC NOT COMMON
      (defun open-special-content-menu (ev)
        (remove-event-no-capture this "click" open-special-content-menu)
        (let ((menu this))
          (labels ((callback (response)
                     (let* ((garr (parse-json response))
                            (exists (aref (aref garr 0) 0))) ;;false when :false
                       (if exists
                           (dolist (desc garr)
                             (append-child menu
                                           (make-special-content-item desc)))
                           (append-text menu "Nothing yet")))))
            (ajax_get_special_content_todata owner callback))))

      ;;;;; END ADDING

      ;;(defun wacky-spinner  () false)
      
      (defun initialize-page ()
        (let ((right (doc-get-el-by-id "right"))
              (lout (dgebid "logout"))
              (callback (lambda (resp)
                          (let* ((dat (parse-json resp))
                                 (un (dots dat owner))
                                 (wos (dots dat twos))
                                 ;;(pridat (dots dat pridat))
                                 (natst (dots dat na))
                                 (na (if (is-na-n (parse-int natst))
                                         false natst))
                                 (server (dots dat server))
                                 ;; (cancelled (if (string= "true"
                                 ;;                         (dots dat cancelled))
                                 ;;                true
                                 ;;                false))
                                 (unused (dots dat unused))
                                 (used (dots dat used))
                                 (pc (dgebid "page-container")))
                            (set-attribute pc "data-owner" un)
                            (set-attribute pc "data-work-on-screen" wos)
                            (setf (dots window document title) un
                                  owner un
                                  work-on-screen (if (string= wos "true")
                                                     true
                                                     false)
                                  ;;(dots sub-data price-data) pridat
                                  (dots sub-data server) server
                                  (dots sub-data unused) unused
                                  ;;(dots sub-data cancelled) cancelled
                                  (dots sub-data nauthorized) na
                                  (dots sub-data enrolled) used
                                  )))))
          (setf (dots window document title) "Loading ")
          (setf (style-height (doc-get-el-by-id "banner")) 
                (strcat banner-height "px"))
          (add-event-no-capture lout "click" logout)
          (insert-control-panel)
          (prepare-input-menu-holder "input-menu-holder" "select-input-menu"
                                     "scratchpad-menu" "Scratchpad Menu")
          (prepare-calced-canvas "svg-canvas" "svg-container"         
                                 "message-display" "scratch-pad-info" 
                                 "problem-display" "scratch-pad-info")
          ;;ADDED
          ;;(insert-special-content-menu)
          (ajax_get_dynamic_start_values callback)
          ))
      
      (add-event-no-capture window "load" initialize-page)
      
      ))))

(defun-ajax teacher-get-all-div-tables (teacher-name pupil-name)
    (*teacher-script-processor* json-response)
  (with-session-name teacher-name
    (who::string-list-to-string
     (who::tree-to-template
      (teacher-make-q&a-divs teacher-name pupil-name)))))

(defun-ajax get-teachers-pupils-list (name)
    (*teacher-script-processor* json-response)
  (with-session-name name
    (teacher-pupils name)))

(defun-ajax get-pupil-report (teacher pupil)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (when (ensure-teacher-pupil-relationship teacher pupil)
      (let* ((current (car (pupil-modules pupil)))
             (completed? (null current))
             (rep (parse-record (get-pupil-record pupil) 
                                (or current
                                    (pupil-last-access pupil))
                                completed?)))
        (if rep (tree-to-html rep)
            *no-record-yet*)))))

;; (defun-ajax get-teachers-groups-list (name)
;;     (*teacher-script-processor* json-response)
;;   (next-level (teacher-groups name)))

;; (defun-ajax get-pupils-and-groups (teacher groupname)
;;     (*teacher-script-processor* json-response)
;;   (if (string= groupname "NIL") (pupils-and-groups teacher)
;;       (pupils-and-groups teacher groupname)))

;; (defun-ajax get-teachers-quota (teacher)
;;     (*teacher-script-processor* json-response)
;;   (teacher-nauthorized teacher))

(defun-ajax enroll-pupil (teacher firstname surname grade1)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (progn
      (if (string= grade1 "true")
          (setf grade1 t)
          (setf grade1 nil))
      (setf firstname (tbnl:escape-for-html firstname)
            surname (tbnl:escape-for-html surname))
      (handler-case 
          (cond ((<= (teacher-nauthorized teacher) 0)
                 (st-json:jso "status" "error"
                              "data" :false))
                (t (let ((system-name (add-unique-name firstname)))
                          ;;(register-pupil-globally firstname surname teacher)))
                     (st-json:jso 
                      "status" "ok"
                      "data" (define-pupil teacher system-name
                               firstname surname grade1)))))
        (global-nameregister-failure (c)
          (st-json:jso "status" "error"
                       "data" (status c)))))))

;; (defun-ajax enroll-pupil (teacher firstname surname password grade1)
;;     (*teacher-script-processor* json-response)
;;   (progn
;;     (when (string= password "") (setf password nil))
;;     (if (string= grade1 "true")
;; 	(setf grade1 t)
;; 	(setf grade1 nil))
;;     (define-pupil teacher firstname surname :password password :first-grade grade1)))

(defun-ajax delete-pupils (teacher passw arr)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (if (sesame-opens teacher passw)
        (let ((unlist (st-json:read-json-from-string arr))
              (count 0))
          (dolist (p unlist count)
            (delete-pupil p teacher)
            (remove-deleted-pupil-from-groups teacher p)
            (incf count))
          ;;(deregister-pupils-globally arr)
          (deregister-unique-names (st-json:read-json-from-string arr))
          (st-json:jso "status" "ok"
                       "pupils" (teacher-pupils teacher)))
        (st-json:jso "status" "error"
                     "pupils" (teacher-pupils teacher)))))

;; (defun-ajax delete-pupils (teacher arr)
;;     (*teacher-script-processor* json-response)
;;   (with-session-name teacher
;;     (let ((unlist (st-json:read-json-from-string arr))
;;           (count 0))
;;       (dolist (p unlist count)
;;         (delete-pupil p teacher)
;;         (remove-deleted-pupil-from-groups teacher p)
;;         (incf count))
;;       (deregister-pupils-globally arr)
;;       (teacher-pupils teacher))))

;; (defun-ajax get-select-group-list (teacher) 
;;     ;; It is currently necessary to have *default-empty-groups*
;;     (*teacher-script-processor* json-response)
;;   (let ((grs (teacher-groups teacher)))
;;     (when (or (null grs) (null (cdr grs)))
;;         (setf (teacher-groups teacher) *default-empty-groups*))
;;     (all-group-names (cdr (teacher-groups teacher)))))

(defun-ajax get-select-pupils-list (teacher group)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (pupils-in-group (find-group group (teacher-groups teacher)))))

(defun-ajax change-selected-group (teacher names groupname)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (progn (update-group teacher (st-json:read-json-from-string names) groupname)
           (groups-to-html teacher))))

(defun-ajax just-delete-groups (teacher groups)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (progn (remove-deleted-groups-from-groups  
            teacher (st-json:read-json-from-string groups))
           (groups-to-html teacher))))

(defun-ajax add-this-group (teacher parent group) 
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (let ((group (tbnl:escape-for-html (string-trim '(#\Space) group))))
      (setf (teacher-groups teacher)
            (make-subgroup group parent (teacher-groups teacher)))
      (groups-to-html teacher))))

(defun-ajax update-pupil-password (teacher pupil)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (cond ((not 
            (and 
             (member pupil (teacher-pupils teacher) :test #'string= :key #'car)
             (string= (pupil-teacher pupil) teacher)))
           (error 'no-teacher-pupil-relationship :teacher teacher :pupil pupil))
          (t (let ((gnu (suggest-password)))
               (setf (user-password pupil) gnu)
               gnu)))))

;; (defun-ajax update-pupil-password (teacher pupil gnu)
;;     (*teacher-script-processor* json-response)
;;   (with-session-name teacher
;;     (cond ((not 
;;             (and 
;;              (member pupil (teacher-pupils teacher) :test #'string= :key #'car)
;;              (string= (pupil-teacher pupil) teacher)))
;;            (error 'no-teacher-pupil-relationship :teacher teacher :pupil pupil))
;;           (t (setf (user-password pupil) gnu)
;;              'done))));;FIXME check error (give message)???
    
;; (defun-ajax get-group-tree (teacher)
;;     (*teacher-script-processor* json-response)
;;   (teacher-groups teacher))

(defun-ajax get-groups-html (teacher)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (groups-to-html teacher)))

(defun-ajax file-upload-enroll (teacher filestring)
    (*teacher-script-processor* json-response)
  (with-session-name teacher 
    (process-defs-from-file teacher filestring)))

(defun-ajax teacher-lesson-review-items (teacher)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (if (teacher-exists-p (teacher-file teacher)) ;;?necessary??
        (st-json:jso "status" "ok"
                     "data" 
                     (mapcar #'list-module-description +modular-progression+))
        (st-json:jso "status" "error"
                     "data" :false))))

(defun-ajax get-demo-exercise (tname tag howmany)
    (*teacher-script-processor* json-response)
      ;;FIXME modify safe-read-from-string to accept an optional
      ;;max-chars to read. ;;SECURITY
  (with-session-name tname
    (let ((hm (safe-read-from-string howmany)))
      (if (and (teacher-exists-p (teacher-file tname))
               hm)
          (let ((n (if (>= 15 hm) hm 15))
                (didel-tag (intern tag)))
            (when (zerop n)
              (setf (teacher-demo tname) nil
                    n 10)) 
            ;;FIXME not 10, but teacher-n-demo
            ;; (destructuring-bind (ign1 ((tag ign2 ign3)))
            ;;     (declare (ignore ign1 ign2 ign3))
            ;;breakin attempt? if
            ;;15 limit is hardcoded
            ;;into page html
            (st-json:jso "status" "ok"
                         ;;"datatag" tag
                         "data"
                         (tree-to-html 
                          (make-demo-q&a-div 
                           (demo-exercise tname didel-tag n)))))
          (st-json:jso "status" "error" ;;FIXME, see above
                       "data" "Possible breakins are being logged!")))))

(defun-ajax teacher-set-work-on-screen (name bool)
    (*teacher-script-processor* json-response)
  (with-session-name name
    (let ((val (st-json:read-json bool)))
      (setf (teacher-work-on-screen name) (if (eql val :true) 
                                              "true"
                                              "false")))))

(defun-ajax get-exercises-didactic-elements (teacher)
    (*teacher-script-processor* json-response)
  (with-session-name teacher
    (let ((current (teacher-demo teacher))
          (ctag))
      (when current (setf ctag
                          (destructuring-bind (ign1 ((tag ign2 ign3)))
                              current
                            (declare (ignore ign1 ign2 ign3))
                            tag)))
      (st-json:jso "status" "ok"
                   "data" (list-didactic-elements)
                   "current" ctag))))

(defun-ajax teacher-check-sum (name tdid answer) 
    (*teacher-script-processor* json-response) 
  (with-session-name name
    (destructuring-bind (&whole pandr ign1 ((tag nex problems)))
        (teacher-demo name)
      (declare (ignore ign1) (ignore nex))
      (let* ((lid (parse-table-cell-id tdid))
             (de (get-didactic-element tag))
             (atype (atype de))
             (weed (remove-whitespace answer))
             (dispanswer (format-answer-for-display atype weed))
             (maxinput (reply de))
             (probidx (table-cell-probidx lid))
             (prob (problem-n probidx problems))
             (good (second prob)))
        (handler-case 
            (let ((ga (good-answer atype weed good)))
              (if ga
                  (setf prob (append (subseq prob 0 1) `(,dispanswer t t)))
                  (setf prob (append (subseq prob 0 2) `(nil ,dispanswer))))
              (setf (nth probidx problems) prob)
              (setf (teacher-demo name) pandr)
              (display-problem prob 0 probidx maxinput t))
          (input-not-a-number (c)
            (let ((input (input c)))
              (display-problem nil nil nil nil nil input))))))))

  ;; (division-by-zero (c)
  ;;                   (let ((opands (arithmetic-error-operands c))
  ;;                         (operation (arithmetic-error-operation c)))
  ;;             (display-problem nil nil nil nil nil (list opands operation)))))))))

(defun-ajax get-special-content-todata (tname)
    (*teacher-script-processor* json-response)
  (labels ((to-acc-fun (poaf)
             (cond
               ((eql poaf #'pupil-basic-mul)
                (lambda (name)
                  (declare (ignore name))
                  '((:FALSE :FALSE :FALSE) 
                    (:FALSE :FALSE :FALSE))))
               ((eql poaf #'pupil-basic-add)
                (lambda (name)
                  (declare (ignore name))
                  '((:FALSE :FALSE :FALSE) 
                    (:FALSE :FALSE :FALSE))))
               (t (lambda (name)
                    (declare (ignore name))
                    '((:FALSE :FALSE :FALSE) 
                      (:FALSE :FALSE :FALSE)))))))
    (with-session-name tname
      (let ((seen (reverse +modular-progression+))
            (gather ()))
        (dolist (m seen (reverse gather))
          (let ((poacc (get m 'pupil-object-special-content-data)))
            (when poacc
              (destructuring-bind (names access-fn)
                  poacc
                (setf access-fn (to-acc-fun access-fn))
                (let ((data (funcall access-fn tname)))
                  (setf gather
                        (append (mapcar #'(lambda (a)
                                            (list a m data))
                                        names) gather)))))))
        gather))))

;; (defun-ajax get-dynamic-values ()
;;     (*teacher-script-processor* json-response)
;;   (with-dangerous-session-name
;;     (destructuring-bind (wos spridat)
;;         (teacher-start-data dangerous-name)
      
;;       (st-json:jso "owner" dangerous-name
;;                    "twos" wos
;;                    "pridat" (st-json:read-json-from-string spridat)))))

;; (defun-ajax get-dynamic-start-values ()
;;     (*teacher-script-processor* json-response)
;;   (with-dangerous-session-name
;;     (destructuring-bind (wos spridat na used exp can)
;;         (teacher-dynamic-start-values dangerous-name)
;;       (st-json:jso "owner" dangerous-name
;;                    "twos" wos
;;                    "pridat" (st-json:read-json-from-string spridat)
;;                    "na" (if exp "You can only change a paid subscription."
;;                             na)
;;                    "server" (format nil "https://~A:~A/" *host* *port*)
;;                    "unused" na
;;                    "cancelled" (if can "true" "false")
;;                    "used" used))))

(defun-ajax get-dynamic-start-values ()
    (*teacher-script-processor* json-response)
  (with-dangerous-session-name
    (destructuring-bind (wos na used)
        (teacher-dynamic-start-values dangerous-name)
      (st-json:jso "owner" dangerous-name
                   "twos" wos
                   "na" na
                   "server" (format nil "https://~A:~A/" *host* *port*)
                   "unused" (- na used)
                   "used" used))))

;; (defun-ajax get-subscription-details (tname)
;;     (*teacher-script-processor* json-response)
;;   (with-session-name tname
;;     (destructuring-bind (nauth expires)
;;         (teacher-sub-details-list tname)
;;       (cond (expires
;;              (st-json:jso "status" "ok"
;;                           "response" "You can only change a paid subscription."))
;;             (t (st-json:jso "status" "ok"
;;                             "response" (format nil "~D" nauth)))))))

(defun-ajax configure-account (tname password id nval)
    (*teacher-script-processor* json-response)
  (with-session-name tname
    (if (sesame-opens tname password)
        (let ((sym (intern id)))
          (cond ;; ( (eql sym '|nemail|)
                ;;  (change-email tname nval))
                 
                ;; ((eql sym '|chgsub|)
                ;;  (if (teacher-expires tname)
                ;;      (st-json:jso "status" "error"
                ;;                   "response" "Trial subscriptions and cancelled subscriptions may not be altered.")
                ;;      (change-subscription tname nval)))
                ((eql sym '|npass|)
                 (if (string= nval "true")
                     (let ((sugg (suggest-password)))
                       (setf (user-password tname) sugg)
                       (st-json:jso "status" "ok"
                                    "response" (format nil "Your new password is: ~S" sugg)))
                     (st-json:jso "status" "error"
                                  "response" (format nil "What? Got unknown value: ~A" nval))))
                 ;; ((eql sym '|cansub|)
                 ;; (if (string= nval "true")
                 ;;     (let ((exp (teacher-expires tname))
                 ;;           (adm (teacher-admin tname)))
                 ;;       (cond ((and exp adm)
                 ;;              (reenable-subscription tname))
                 ;;             (exp (cancel-teacher tname t)) ;;was
                 ;;                                           ;;'non-expired-cancel
                 ;;             (t (cancel-teacher tname nil))))
                 ;;     (st-json:jso "status" "error"
                 ;;                  "response" (format nil "What? Got
                 ;;                  unknown value: ~A" nval))))
                (t (st-json:jso "status" "error"
                                "response" (format nil "What? ~A~%Got ~A" id nval)))))
        (st-json:jso "status" "error"
                     "response" "Bad password"))))

;; (defun-ajax reload-sub-data (tname)
;;     (*teacher-script-processor* json-response)
;;   (with-session-name tname
;;     (multiple-value-bind (na used)
;;         (teacher-free-used tname)
;;       (st-json:jso "status" "ok"
;;                    "nauthorized" na
;;                    "enrolled" used))))

