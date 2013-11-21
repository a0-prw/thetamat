(in-package :autotutor)
;;FIXME remember to setf ps:*ps-print-pretty* nil

;;score-code is defined in dom-utils.lisp

(define-special-content mod-digs-nat 
    (("Addition" "Subtraction") #'pupil-basic-add) "SCRIPT" 
    (start 
     voluntary-game gen-add-pairs gen-sub-pairs gen-teen-sub-pairs
     gen-digit-sub-pairs reset-game-record game-record pupil-record-object
     toggle-checkbox-visibility 
     time start-time stop-time game-name get-game-name
     get-best-time update-remaining-field update-errors-field
     update-correct-field update-numeric-field update-time-field
     make-face emote neutral-mouth draw-arrow
     svg-canvas communicate cq-handler
     stack fill-stack empty-stack
     start-play repeat-or-update ;;uprc-fun
     make-question make-answer-handler
     trainer-handlers insert-trainer-checkboxes
     make-trainer-cbx-handler 
     make-game-stat-display ;;FIXME change show to update show-stats
     ;; make-stat-controller make-stat-update-fn stat-controller stats
     switch switch-test training-pairs collect-questions
     correct-your-error make-error-correction-handler
     current-questions make-switch-control
     make-game-holder game-start game-stop set-game-start-stats
     insert-game-holder hide-intro cancel-game-play 
     add-won sub-won make-header insert-start-button 
     insert-quit-button intro intro-text create-blackboard 
     complete-game teacher-demo voluntary restore-page-state
     get-addsub-scores set-local-add-score set-local-sub-score
     add-score sub-score game-won
     special-previous-lesson 
     handle-nl-click0 handle-nl-clickpos handle-nl-clickneg
     )

  teacher-demo false

  voluntary 
  false
  
  time
  (ps:create timekeeper 
             (with-slots (timekeeper start passed show-time) time
               (with-slots (get-time) stamp
                 (lambda ()
                   (let* ((stamp (ps:new (-date)))
                          (newt (- (funcall get-time) start)))
                     (setf passed (/ (floor (/ newt 100)) 10))
                     (when (= (round passed) (parse-float passed))
                       (setf passed (strcat passed ".0")))
                     (funcall update-time-field passed show-time)
                     ))))
             show-time false
             start false
             passed "0.0"
             interval 1000
             interval-id false)
  
  start-time
  (with-slots (timekeeper start passed interval interval-id show-time) time
    (with-slots (set-interval) window
      (lambda (show)
        (setf start (ps:new (-date))
              show-time (if show true false)
              passed "0.0"
              interval-id (funcall set-interval timekeeper interval)))))

  stop-time
  (with-slots (timekeeper start passed interval interval-id) time
    (with-slots (get-time) stamp
      (with-slots (clear-interval) window
        (lambda ()
          (when interval-id 
            (funcall clear-interval interval-id))
          (let* ((stamp (ps:new (-date)))
                 (newt (- (funcall get-time) start))
                 (final  (/ (floor (/ newt 100)) 10)))
            (if (= (round final) (parse-float final))
                (strcat final ".0")
                (strcat final "")))))))

  game-name
  false

  get-game-name
  (with-slots (best-time) add-score
    (lambda ()
      (if game-name game-name
          (if (not best-time)
              "Addition"
              "Subtraction"))))
  
  get-best-time
  (with-slots (best-time) pupil-record
    (lambda (pupil-record)
      (let ((bt best-time))
        (if (not bt)
            "No best time yet"
            bt))))
            ;;"FIXME: parse time stamp"))))
  
  update-numeric-field 
  (lambda (place inc)
    (let ((val (parse-int (text-content-of place))))
      (if inc (setf (text-content-of place) (1+ val))
          (setf (text-content-of place) (1- val)))))
  
  update-errors-field
  (lambda ()
    (funcall update-numeric-field 
             (doc-get-el-by-id "errors-field") true))

  update-correct-field
  (lambda ()
    (funcall update-numeric-field
             (doc-get-el-by-id "correct-field") true))
  
  update-remaining-field
  (lambda ()
    (funcall update-numeric-field
             (doc-get-el-by-id "remaining-field") false))
  
  update-time-field
  (lambda (gnew show)
    (when show
      (let ((tf (doc-get-el-by-id "time-field")))
        (if gnew (setf (text-content-of tf) gnew)
            (set (text-content-of tf) "0.0")))))

  svg-canvas
  (ps:create ;;heightem "9em" widthem "9em"
   height 9 width 9
   canvas-height 12 canvas-width 12
   correct-arrow false
   incorrect-arrow false)
  
  make-face
  (lambda ()
    (with-slots (width height canvas-height canvas-width) svg-canvas
      (let* ((canvas (svg-create-element "svg"))
             (msg (div-id-class "game-message" "game-info"))
             (exp (doc-get-el-by-id "express"))
             (face (svg-create-element "circle"))
             (viewbox (strcat "0 0 " (* input-menu-icon-pixels width) " " 
                              (* input-menu-icon-pixels height)))
             (face-cx (/ canvas-width 2)) ;;(/ width 2))
             (face-cy (/ canvas-height 2)) ;;(/ height 2))
             (face-r (floor (/ width 2)))
             (l-eye (svg-create-element "circle"))
             (l-eye-cx (/ canvas-width 3))
             (l-eye-cy (/ canvas-height 3))
             (l-eye-r (/ width 18))
             (r-eye (svg-create-element "circle"))
             (r-eye-cx (* 2 (/ canvas-width 3)))
             (r-eye-cy l-eye-cy)
             (r-eye-r l-eye-r)
             (mouth-y (* 2 (/ canvas-height 3)))
             (mouthg (svg-draw-line canvas l-eye-cx mouth-y
                                    r-eye-cx mouth-y "black" "0.25em"
                                    "mouth" false)))
        (append-child exp msg)
        (set-display msg "block")
        (insert-before exp canvas msg)
        
        (set-attributes canvas "version" "1.2" "baseProfile" "basic"
                        "id" "canvas" "height" (ems canvas-height) 
                        "width" (ems canvas-width) 
                        "viewbox" viewbox)
        (set-attributes face "cx" 
                        (ems face-cx) "cy" (ems face-cy) "r" (ems face-r)
                        "stroke" "black" "fill" "yellow")
        (set-attributes l-eye "cx" (ems l-eye-cx) "cy" (ems l-eye-cy) 
                        "r" (ems l-eye-r) "stroke" "black" "fill" "black")
        (set-attributes r-eye "cx" (ems r-eye-cx) "cy" (ems r-eye-cy) 
                        "r" (ems r-eye-r) "stroke" "black" "fill" "black")
        (append-children canvas face l-eye r-eye mouthg)
        face)))
  
  neutral-mouth
  (with-slots (width height canvas-height canvas-width) svg-canvas
    (lambda ()
      (let* ((l-eye-cx (/ canvas-width 3))
             (mouth-y  (* 2 (/ canvas-height 3)))
             (r-eye-cx (* 2 (/ canvas-width 3)))
             (mouthg (svg-draw-line canvas l-eye-cx mouth-y
                                    r-eye-cx mouth-y "black" "0.25em"
                                    "mouth" false)))
        mouthg)))
  
  draw-arrow
  (lambda (correct)
    (with-slots (canvas-height canvas-width width height
                               correct-arrow incorrect-arrow) svg-canvas
      (labels ((down (y) (strcat " -" y))
               (back (x) (strcat " -" x))
               (up (y) (strcat " " y))
               (forward (x) (strcat " " x))
               (draw (x y) (strcat " l" x " " y))
               (at (x) (strcat " M " x)))
        (let* ((unit input-menu-icon-pixels)
               (half (* unit 0.5))
               (no-y " 0 ")
               (no-x " 0 ") 
               (start (if correct 
                          (strcat 0  " " (* unit (/ canvas-height 2)))
                          (strcat (* unit canvas-width) " " 
                                  (* unit (/ canvas-height 2)))))
               (arrow (svg-create-element "path")))
          (set-attributes arrow "id" (if correct "correct-arrow" "incorrect-arrow")
                          "stroke" "black" "fill" (if correct "green" "red")
                          "d"
                          (if correct
                              (strcat (at start) 
                                      (draw (forward unit) (up unit))
                                      (draw no-x (down half)) 
                                      (draw (forward half) no-y)
                                      (draw no-x (down unit))
                                      (draw (back half) no-y)
                                      (draw no-x (down half))
                                      (draw (back unit) (up unit)))
                              (strcat (at start)
                                      (draw (back unit) (down unit))
                                      (draw no-x (up half))
                                      (draw (back half) no-y)
                                      (draw no-x (up unit))
                                      (draw (forward half) no-y)
                                      (draw no-x (up half))
                                      (draw (forward unit) (down unit)))))
          arrow))))
  

  emote
  (lambda (happy)
    (with-slots (width height canvas-width canvas-height
                       correct-arrow incorrect-arrow) svg-canvas
      (let* ((mouth (doc-get-el-by-id "mouth"))
             (parent (parent-node mouth))
             (arrow-to-show (if happy correct-arrow incorrect-arrow))
             (current-arrow (or (doc-get-el-by-id "correct-arrow")
                                (doc-get-el-by-id "incorrect-arrow")))
             (third (* input-menu-icon-pixels (/ canvas-width 3)))
             (mouth-x1 third)
             (mouth-x2 (* 2 third))
             (mouth-y (* input-menu-icon-pixels (* 2 (/ canvas-height 3))))
             (expr (if happy (strcat 
                              "M " mouth-x1 " " mouth-y " Q " 
                              (+ mouth-x1 (/ (- mouth-x2 mouth-x1) 2)) " "
                              (+ mouth-y (* 1.5 input-menu-icon-pixels)) ", "
                              mouth-x2 " " mouth-y)
                       (strcat "M " mouth-x1 " " mouth-y " Q " 
                               (+ mouth-x1 (/ (- mouth-x2 mouth-x1) 2)) " "
                               (- mouth-y (* 1.5 input-menu-icon-pixels)) ", "
                               mouth-x2 " " mouth-y)))
             (smile (svg-create-element "path")))
        (set-attributes smile "id" "mouth"
                        "d" expr
                        "stroke" "black" "fill" "none" 
                        "stroke-width" "0.25em")
        (replace-child parent smile mouth)
        (when (not (eql arrow-to-show current-arrow))
          (if (not-defined current-arrow)
              (append-child parent arrow-to-show)
              (replace-child parent arrow-to-show current-arrow)))
        smile)))

  
  repeat-or-update
  (with-slots (bad-answers no-errors) game-record
    (with-slots (best-time improvements optotal) pupil-record
      (with-slots (train test) switch
        (lambda (pupil-record game-name)
          (labels ((callback (response)
                     ;;successful game completion - if both have been
                     ;;completed or game was called voluntarily from
                     ;;user page return to exercises
                     (if (or voluntary
                             (and (funcall add-won)
                                  (funcall sub-won)))
                         (funcall complete-game 0)
                         ;;otherwise empty-stack and change
                         ;;current-questions
                         (let ((name game-name))
                           (cond ((string= name "Addition")
                                  (setf current-questions 
                                        (ps:array (jsn sub-pairs))))
                                 ((string= name "Multiplication")
                                  (setf current-questions 
                                        (ps:array (jsn div-pairs))))
                                 (t (throw (strcat "Game name error: "
                                                   name))))
                           (funcall empty-stack)
                           (funcall game-stop))))
                   (uprc-fun ()
                     (if (not teacher-demo)
                         (ajax_update_pupil_record owner game-name 
                                                   (stringify-json 
                                                    (list best-time 
                                                          improvements optotal))
                                                   callback)
                         (funcall restore-page-state))))
            (let* ((repeats bad-answers)
                   (empty-repeats (not-defined (aref repeats 0))))
              (cond ((and empty-repeats no-errors)
                     (let* ((fin (funcall stop-time))
                            (cbt (when test best-time)))
                       (setf optotal (1+ optotal)) 
                       (if cbt ;;FIXME do not update best time in train mode.
                           (if (> (- (parse-float cbt) (parse-float fin)) 0)
                               (progn 
                                 (setf best-time fin)
                                 (setf improvements
                                       (if improvements (1+ improvements) 1))
                                 (my-alert (strcat "Well done! New best time: " 
                                                   fin) uprc-fun))
                               (my-alert "Well done! No errors!"
                                         uprc-fun))
                           (let ((msg (if best-time ;;if best-time is
                                                    ;;not false, timer
                                                    ;;is displaying
                                          (strcat "Well done! No errors!"
                                                  (ps:lisp #\Newline)
                                                  "Your time was: " 
                                                  fin " seconds.")
                                          "Well done! No errors!")))
                             (when test (setf best-time fin))
                             (my-alert msg uprc-fun)))))                          
                    (empty-repeats
                     (let ((se (text-content-of 
                                (doc-get-el-by-id "errors-field"))))
                       (funcall complete-game se)))
                    (t 
                     (let* ((se (text-content-of 
                                 (doc-get-el-by-id "errors-field")))
                            (phrase1 (quantify-noun se "errors" true))
                            (phrase2 (quantify-noun se "sums")))
                       (my-alert (strcat ;;Game not over there are repeats
                                  "You had " phrase1 " in " phrase2
                                  " , which will now be repeated.")
                                 (lambda ()
                                   (setf stack bad-answers
                                         bad-answers (ps:array))
                                   (funcall make-question))))))))))))
  
  ;;give-test-response
    
  complete-game
  (with-slots (test train) switch
    (lambda (nerrors)
      (labels ((callback (response)
                 (let ((msg (parse-json response)))
                   (funcall restore-page-state)
                   (alert msg)
                   (get-all-tables-and-start))))
        (let ((update (if voluntary false true)))
          (if (zerop nerrors) ;;parse-int?
              (if test
                  (ajax_update_module owner update callback)
                  (funcall game-stop))
              (my-alert (strcat "You completed the game with " 
                                (quantify-noun nerrors "errors")
                                ".  Try again!") (lambda ()
                                                   (funcall game-stop))))))))
  restore-page-state
  (lambda ()
    (let ((gh (doc-get-el-by-id "game-holder"))
          (cp (when teacher-demo (dgebid "control-panel")))
          (ith (doc-get-el-by-id "intro-text-holder"))
          (lrm (doc-get-el-by-id "review-menu"))
          (rm (doc-get-el-by-id "replay-menu"))
          (rp (doc-get-el-by-id "right"))
          (pg (doc-get-el-by-id "pager")))
      (funcall stop-time)
      (remove-child (dots document body) gh)
      (when ith (remove-child (dots document body) ith))
      (when cp (set-display cp "inline-block"))
      (when lrm (set-display lrm "inline-block"))
      (when rm (set-display rm "inline-block"))
      (when rp (set-display rp "inline-block"))
      (when (> all-tables 1) (set-display pg "block"))
      (setf content-object (ps:new (-object)))))
  
  ;; start-next-game ;; FIXME
  ;; (lambda ()
  ;;   (my-alert "START-NEXT-GAME"))

  make-question
  (with-slots (bad-answers no-errors) game-record
    (lambda ()
      (let ((pair (pop stack)))
        (if (not-defined pair)
            (let* ((game-name (text-content-of (doc-get-el-by-id "game-field")))
                   (pupil-record (cond ((string= game-name "Addition")
                                        add-score)
                                       ((string= game-name "Subtraction")
                                        sub-score)
                                       ((string= game-name "Multiplication")
                                        mul-score)
                                       ((string= game-name "Division")
                                        div-score)
                                       (t (throw 
                                              (strcat 
                                               "gameName error: " game-name))))))
              (funcall repeat-or-update pupil-record game-name))
            (let* ((t1 (aref pair 0))
                   (t2 (aref pair 1))
                   (ans (+ t1 t2))
                   (sans (strcat "" ans))
                   (lans (length sans))
                   (inp (make-text-input-element lans lans))
                   (neg (< t2 0))
                   (st1 (strcat "" t1))
                   (s (strcat "" t2))
                   (same (and (not neg) (string= s st1)))
                   (st2 (if neg (aref (split s "-") 1) s))
                   (sign (if neg "-" "+"))
                   (qh (div-id "current-question"))
                   (tx1 (strcat st1 sign st2 ))
                   (tx2 (if (or neg same) false (strcat st2 sign st1)))
                   (place (doc-get-el-by-id "game-interface-holder")))
              (setf cq-handler (funcall make-answer-handler pair tx1 tx2 sans lans))
              (prune-tree-from-nth place 0)
              (append-child place qh)
              (append-para-text qh (mj-bracket tx1) true)
              (when tx2 (append-para-text qh (mj-bracket tx2) true))
              (append-child qh inp)
              (focus inp)
              (add-event-no-capture inp "keydown" cq-handler))))))
  
  make-answer-handler
  (with-slots (bad-answers no-errors) game-record
    (lambda (pair tx1 tx2 sans lans)
      (lambda (ev)
      (prevent-default ev)
      (if (numeric-input ev)
          (let* ((input (strcat (value this) (key-char ev)))
                 (dh (or (doc-get-el-by-id "display-answer")
                         (div-id "display-answer")))
                 (com (doc-get-el-by-id "game-message"))
                 (p (aref (get-elements-by-tag-name com "p") 0))
                 (old-place (parent-node dh))
                 (lni (length input)))
            (when p (remove-child com p))
            (when old-place (remove-child old-place dh))
            (if (= lans lni)
                (let* ((ga (string= sans input))
                       (cf1 (strcat tx1 "=" (if ga sans input)))
                       (cf2 (if tx2 (strcat tx2 "=" (if ga sans input)) false))
                       (place (doc-get-el-by-id (if ga 
                                                    "goodcards"
                                                    "badcards"))))
                  (when (not ga)
                    (setf no-errors false)
                    (push pair bad-answers))
                  (prune-tree-from-nth dh 0)
                  (append-para-text dh (mj-bracket cf1) true)
                  (when cf2 (append-para-text dh (mj-bracket cf2) true))
                  (append-child place dh)
                  (setf (value this) input)
                  (if ga (funcall update-correct-field)
                      (funcall update-errors-field))
                  (funcall update-remaining-field)
                  (funcall emote (if ga true false))
                  (if ga (funcall make-question)
                      (funcall correct-your-error this sans lans)))
                (setf (value this) input)))
          (when (is-backspace ev)
            (let ((input (value this)))
              (setf (value this) (subseq input 0 (1- (length input))))))
          ))))
  
  correct-your-error
  (lambda (inp sans lans)
    (remove-event-no-capture inp "keydown" cq-handler)
    (append-para-text (doc-get-el-by-id "game-message")
                      "Correct error to continue")
    (setf (value inp) "")
    (add-event-no-capture inp "keydown" 
                          (funcall make-error-correction-handler sans lans)))

  make-error-correction-handler
  (lambda (sans lans)
    (lambda (ev)
      (prevent-default ev)
      (when (numeric-input ev)
        (let* ((input (strcat (value this) (key-char ev)))
               (lni (length input)))
          (if (= lans lni)
              (if (string= input sans)
                  (let* ((badc (doc-get-el-by-id "badcards"))
                         (old-mouthg (parent-node (doc-get-el-by-id "mouth")))
                         (new-mouthg (funcall neutral-mouth))
                         (com (doc-get-el-by-id "game-message"))
                         (p (aref (get-elements-by-tag-name com "p") 0)))
                    (if old-mouthg
                        (replace-child (doc-get-el-by-id "canvas")
                                       new-mouthg old-mouthg)
                        (append-child (doc-get-el-by-id "canvas")
                                      new-mouthg))
                    (prune-tree-from-nth badc 0)
                    (remove-child com p)
                    (append-para-text com "Ok. Error corrected!")
                    (funcall make-question))
                  (setf (value this) ""))
              (setf (value this) input))))))      
  
  stack
  (ps:array)

  fill-stack
  (lambda ()
    (setf stack (shuffle (funcall collect-questions))))
  
  empty-stack
  (lambda ()
    (setf stack (ps:array)))
  
  insert-trainer-checkboxes 
  (lambda ()
    (let ((holder (doc-get-el-by-id "config-left"))
          (cbxholder (div-id "checkbox-holder"))
          (handler (funcall make-trainer-cbx-handler))
          (plusnames (list (list (jsn 1+) "1 + ") 
                           (list (jsn 2+) "2 + ")
                           (list (jsn 3+) "3 + ")
                           (list (jsn 4+) "4 + ")
                           (list (jsn 5+) "5 + ")
                           (list (jsn 6+) "6 + ")
                           (list (jsn 7+) "7 + ")
                           (list (jsn 8+) "8 + ")
                           (list (jsn 9+) "9 + ")))
          (minusnames (list (list "digitsub" "x - ")
                            (list "teensub" "1x - "))))
      (dolist (type (list plusnames minusnames))
        (let ((ul (create-element "ul"))
              (ulholder (div-class "config-left-checkboxes")))
          (dolist (n type ul)
            (let* ((li (create-element "li"))
                   (id (aref n 0))
                   (txt (aref n 1))
                   (cbx (create-checkbox id)))
              (setf (text-content-of li) txt)
              (append-child li cbx)
              (add-event-no-capture cbx "click" handler)
              (append-child ul li)))
          (append-child ulholder ul)
          (append-child cbxholder ulholder)))
      (set-display cbxholder "none") ;;start without them
      (append-child holder cbxholder)
      holder))
  
  make-trainer-cbx-handler
  (lambda ()
    (with-slots (test train) switch
      (lambda (ev)
        (let ((trainer-mode train))
          (if trainer-mode
              (if (checked this)
                  (push (id-of this) current-questions)
                  (array-remove (id-of this) current-questions))
              (setf (checked this) false))))))
  
  toggle-checkbox-visibility
  (lambda (restore)
    (let ((el (doc-get-el-by-id "checkbox-holder")))
      (if restore
          (set-display el "block")
          (set-display el "none"))))
  
  trainer-handlers
  (ps:new (-object))
  
  switch
  (ps:create train false
             test true)
  
  collect-questions
  (lambda ()
    (let ((tmp (array)))
      (dolist (ref current-questions)
        (dolist (pair (aref training-pairs ref))
          (push pair tmp)))
      tmp))
  
  training-pairs
  (ps:create "1plus" (loop for i from 1 to 9 collect (list 1 i))
             "2plus" (loop for i from 1 to 9 collect (list 2 i))
             "3plus" (loop for i from 1 to 9 collect (list 3 i))
             "4plus" (loop for i from 1 to 9 collect (list 4 i))
             "5plus" (loop for i from 1 to 9 collect (list 5 i))
             "6plus" (loop for i from 1 to 9 collect (list 6 i))
             "7plus" (loop for i from 1 to 9 collect (list 7 i))
             "8plus" (loop for i from 1 to 9 collect (list 8 i))
             "9plus" (loop for i from 1 to 9 collect (list 9 i))
             "teensub" (do ((u 8 (1- u)) (tmp (ps:array)))
                           ((= u 0) tmp)
                         (do ((sub (1+ u) (1+ sub)) (tmp2 (ps:array)))
                             ((= sub 10) (setf tmp (append tmp2 tmp)))
                           (push (list (+ 10 u) (- sub)) tmp2)))
             "digitsub" (do ((d1 9 (1- d1)) (tmp (ps:array)))
                            ((= d1 1) tmp)
                          (do ((d2 (1- d1) (1- d2))
                               (tmp2 (ps:array)))
                              ((= d2 1) (setf tmp (append tmp2 tmp)))
                            (push (list d1 (- d2)) tmp2))))
  

;; Removed because 'use strict' won't allow 'with' which ps generates
;; for many loops

;;teensub
;; (loop for u from 8 downto 1 
;;                           append (loop for sub from (1+ u) to 9 
;;                                 collect (list (+ 10 u) (- sub))))

;;digitsub
;; (loop for d1 from 9 downto 2
;;                            append (loop for d2 from (1- d1) downto 1
;;                                      collect (list d1 (- d2))))
  
  
  switch-test
  (with-slots (train test) switch
    (lambda (ev)
      (cond ((eql (id-of this) "set-test")
             (let ((cbxs (get-elements-by-tag-name 
                          (doc-get-el-by-id "checkbox-holder") "input"))
                   (add (funcall add-won)))
               (setf train false
                     test true)
               (funcall toggle-checkbox-visibility)
               (dolist (cbx cbxs)
                 (setf (checked cbx) false))
               (cond ((string= game-name "Addition")
                      (setf current-questions (ps:array))
                      (push (jsn add-pairs) current-questions))
                     ((or add (string= game-name "Subtraction"))
                      (setf current-questions (array))
                      (push (jsn sub-pairs) current-questions))
                     (t 
                      (setf current-questions (array))
                      (push (jsn add-pairs) current-questions)))))
            ((eql (id-of this) "set-train")
             (funcall toggle-checkbox-visibility true)
             (setf test false
                   train true
                   current-questions (array)))
            (t (ps:throw "switch-test error")))
      test))

  current-questions
  (ps:array)
    
  make-game-holder
  (lambda ()
    (let* ((gs (ps:lisp (correct)))
           (bs (ps:lisp (incorrect)))
           (gh (div-id "game-holder"))
           (gout (div-id-class "game-out" "game-output"))
           (gin (div-id-class "game-in" "game-input"))
           (good (div-id-class "good" "game-even game-output game-cell"))
           (goodcards (div-id-class "goodcards" "card-holder"))
           (goodh (funcall make-header gs)); "Correct answers"))
           (bad (div-id-class "bad" "game-output game-even game-cell"))
           (badcards (div-id-class "badcards" "card-holder"))
           (badh (funcall make-header bs));; "Incorrect answers"))
           (show (div-id-class "express" "game-odd game-output game-cell"))
           (gcl (div-id-class "config-left" "game-input game-odd game-cell"))
           (stats (div-id-class "game-stats" "game-output game-odd game-cell"))
           (ifh (div-id-class "game-interface-holder" "game-input game-even game-cell"))
           (ques (div-id-class "game-questions" "game-action"))
           (inph (div-id-class "game-input-holder" "game-action")))
      (append-children gh gout gin)
      (append-children gout good show bad)
      (append-children good goodh goodcards)
      (append-children bad  badh badcards)
      (append-children gin gcl ifh stats)
      (append-children ifh ques inph)
      (funcall insert-start-button ifh)
      gh))

  insert-start-button
  (lambda (parent)
    (let ((butt (make-button "start-game" "Start")))
      (append-child parent butt)
      (add-event-no-capture butt "click" game-start)
      butt))

  reset-game-record
  (lambda ()
    (setf game-record
          (ps:create 
           bad-answers (ps:array)
           no-errors true)))

  game-record
  (ps:create 
   bad-answers (ps:array)
   no-errors true)

  make-game-stat-display
  (lambda ()
    (let ((stat (doc-get-el-by-id "game-stats")))
      (make-info-display stat
                         ("status" 
                          nil 
                          ("game" "game-name" "Game:" "game-field")
                          ("mode" "mode-name" "Mode:" "mode-field")
                          ("best-time" "best-time-name" "Best Time:" "best-time-field")
                          
                          ("remaining" "remaining-name" "Remaining:" "remaining-field")
                          ("correct" "correct-name" "Correct:" "correct-field")
                          ("errors" "errors-name" "Errors:" "errors-field")
                          ("time" "time-name" "Time:" "time-field")))))

  pupil-record-object
  (lambda (gname)
    (cond ((string= gname "Addition")
           add-score)
          ((string= gname "Subtraction")
           sub-score)
          (t (my-alert (strcat "NYI: " gname)))))

  set-game-start-stats
  (with-slots (test train) switch
    (lambda ()
      (let ((gf (doc-get-el-by-id "game-field"))
            (mf (doc-get-el-by-id "mode-field"))
            (bf (doc-get-el-by-id "best-time-field"))
            (rf (doc-get-el-by-id "remaining-field"))
            (cf (doc-get-el-by-id "correct-field"))
            (ef (doc-get-el-by-id "errors-field"))
            (tf (doc-get-el-by-id "time-field"))
            (gname (funcall get-game-name)))
        (setf (text-content-of gf) gname
              (text-content-of mf) (if test "Play" "Train")
              (text-content-of bf) (funcall get-best-time 
                                            (funcall pupil-record-object gname))
              (text-content-of rf) (length stack)
              (text-content-of cf) "0"
              (text-content-of ef) "0"
              (text-content-of tf) "Not timing"))))
  
  
  game-start
  (lambda (ev)
    (with-slots (incorrect-arrow correct-arrow) svg-canvas
      (funcall fill-stack)
      (if (not-defined (aref stack 0))
          (my-alert "No questions selected in training mode!")
          (let ((parent (parent-node this))
                (train (parent-node (doc-get-el-by-id "set-train")))
                (stop (make-button "stop-game" "Stop"))
                (contr (doc-get-el-by-id "config-left"))
                (ga (funcall draw-arrow true))
                (ba (funcall draw-arrow false)))
            (add-event-no-capture stop "click" game-stop)
            (setf correct-arrow ga
                  incorrect-arrow ba)
            (set-display train "none")
            (remove-child parent this)
            (funcall make-game-stat-display)
            (funcall set-game-start-stats)
            (append-child contr stop) ;;HERE
            (funcall make-face)
            (if (or game-name ;;FIXME a bit more maybe
                    (and (funcall add-won)
                         (funcall sub-won)))
                (funcall start-time true) ;;dont start timer in training mode?
                (funcall start-time false))
            (funcall make-question)))))

  game-stop
  (lambda (ev)
    (let ((ifh (doc-get-el-by-id "game-interface-holder"))
          (contr (doc-get-el-by-id "config-left"))
          (exp (doc-get-el-by-id "express"))
          (info (doc-get-el-by-id "game-stats"))
          (train (parent-node (doc-get-el-by-id "set-train")))
          (gc (doc-get-el-by-id "goodcards"))
          (bc (doc-get-el-by-id "badcards"))
          (sb (if ev this (doc-get-el-by-id "stop-game"))))
      (funcall reset-game-record)
      (funcall stop-time)
      (when (or ev sb) (remove-child contr sb))
      (prune-tree-from-nth ifh 0)
      (prune-tree-from-nth exp 0)
      (prune-tree-from-nth info 0)
      (prune-tree-from-nth gc 0)
      (prune-tree-from-nth bc 0)
      (setf stack (ps:array))
      (when (or ev sb)
        (funcall insert-start-button ifh)
        (set-display train "block"))))

  make-header 
  (lambda (src txt)
    (let ((img (create-element "img"))
          (h (create-element "h1"))
          (tn (if txt (make-text-node txt) false)))
      (set-attribute img "src" src)
      (append-child h img)
      (when tn (append-child h tn))
      h))

  make-switch-control
  (lambda ()
    (let* ((form (create-element "form"))
           (ond (plain-div))
           (on (make-radio-input-element "mode"))
           (offd (plain-div))
           (off (make-radio-input-element "mode")))
      (set-attribute on "id" "set-test")
      (setf (checked on) true)
      (set-attribute off "id" "set-train")
      (append-child ond on)
      (append-text ond "Play")
      (append-child offd off)
      (append-text offd "Practice")
      (append-children form ond offd)
      form))

  ;; identify ;;Remove FIXME
  ;; (lambda (el)
  ;;   (append-text el (id-of el)))

  ;; id-all ;; Remove
  ;; (lambda ()
  ;;   (dolist (el '("good" "bad" "timer-holder" "config-left"
  ;;                 "config-right" "game-questions" "game-input-holder"))
  ;;     (funcall identify (doc-get-el-by-id el))))

  hide-intro
  (lambda ()
    (let ((ith (doc-get-el-by-id "intro-text-holder")))
      (when ith (set-display ith "none"))))

  insert-game-holder
  (lambda ()
    (let ((gh (funcall make-game-holder))
          (m (funcall make-switch-control)))
      (funcall hide-intro)
      (append-child (dots document body) gh)
      (set-top gh (strcat (+ 5 banner-height) "px"))
      (let ((cfl (doc-get-el-by-id "config-left")))
        (append-child cfl m)
        ;;(add-event-no-capture 
        (set-display gh "inline-block")))
    (when voluntary (funcall insert-quit-button))
    (add-event-no-capture (doc-get-el-by-id "set-test") "click" switch-test)
    (add-event-no-capture (doc-get-el-by-id "set-train") "click" switch-test))

  insert-quit-button
  (lambda ()
    (let ((form (aref (get-elements-by-tag-name
                       (doc-get-el-by-id "config-left") "form") 0))
          (but1div (parent-node (doc-get-el-by-id "set-test")))
          (quitb (make-button "quit-button" "Quit")))
      (add-event-no-capture quitb "click" cancel-game-play)
      (insert-before form quitb but1div)))

  cancel-game-play 
  (lambda (ev)
    (prevent-default ev) ;;FIXME should call game-stop
    (funcall game-stop)
    (funcall restore-page-state))
  
  add-won
  (lambda ()
    (with-slots (add-score) content-object
      (with-slots (best-time improvements) add-score
        (if best-time true false))))

  sub-won
  (lambda ()
    (with-slots (sub-score) content-object
      (with-slots (best-time improvements) sub-score
        (if best-time true false))))

  add-score 
  (ps:create best-time false
             improvements false
             optotal false)

  sub-score 
  (ps:create best-time false
             improvements false
             optotal false)

  get-addsub-scores 
  (lambda ()
    (with-slots (add-pairs sub-pairs) training-pairs
      (labels ((callback (response)
                 (let* ((parr (parse-json response))
                        (ass (aref parr 0))
                        (bag1 (aref parr 1))
                        (bag2 (aref parr 2))
                        (addsc (aref ass 0))
                        (subsc (aref ass 1)))
                   (setf add-pairs bag1
                         sub-pairs bag2)
                   ;; (if (funcall add-won) ;;HERE
                   ;;     (push (jsn sub-pairs) current-questions)
                   ;;     (push (jsn add-pairs) current-questions))
                   (funcall set-local-add-score addsc)
                   (funcall set-local-sub-score subsc)
                   (if (funcall add-won) ;;HERE
                       (push (jsn sub-pairs) current-questions)
                       (push (jsn add-pairs) current-questions))
                   (funcall insert-game-holder)
                   (funcall insert-trainer-checkboxes))))
        (ajax_get_addsub_scores owner callback))))
  
  voluntary-game
  (lambda (addsc subsc)
    (with-slots (add-pairs sub-pairs mul-pairs div-pairs) training-pairs
      (cond ((eql game-name "Subtraction")
             (funcall set-local-sub-score subsc)
             (funcall set-local-add-score addsc)
             (push (jsn sub-pairs) current-questions)
             (setf add-pairs (funcall gen-add-pairs));;Must set both
             (setf sub-pairs (funcall gen-sub-pairs)))
            ((eql game-name "Addition")
             (funcall set-local-add-score addsc)
             (funcall set-local-sub-score subsc)
             (push (jsn add-pairs) current-questions)
             (setf sub-pairs (funcall gen-sub-pairs))
             (setf add-pairs (funcall gen-add-pairs)))
            ((eql game-name "Multiplication") ;;EXTEND MUL DIV
             (push (jsn mul-pairs) current-question);;Must set both
             (setf mul-pairs (funcall gen-mul-pairs)))
            ((eql game-name "Division")
             (push (jsn div-pairs) current-questions)
             (setf div-pairs (funcall gen-div-pairs))))
      (funcall insert-game-holder)
      (funcall insert-trainer-checkboxes)))
  
  gen-add-pairs
  (lambda ()
    (do ((d1 1 (1+ d1))
         (tmp (ps:array)))
        ((= d1 10) tmp)
      (do ((d2 d1 (1+ d2))
           (tmp2 (ps:array)))
          ((= d2 10) (setf tmp (append tmp2 tmp)))
        (push (list d1 d2) tmp2))))

;; (lambda ()
;;     (loop for d1 from 1 to 9
;;        append (loop for d2 from d1 to 9
;;                  collect (list d1 d2))))


  gen-sub-pairs
  (lambda ()
    (append (funcall gen-teen-sub-pairs)
            (funcall gen-digit-sub-pairs)))

  gen-teen-sub-pairs
  (lambda ()
    (do ((u 8 (1- u)) (tmp (ps:array)))
        ((= u 0) tmp)
      (do ((sub (1+ u) (1+ sub)) (tmp2 (ps:array)))
          ((= sub 10) (setf tmp (append tmp2 tmp)))
        (push (list (+ 10 u) (- sub)) tmp2))))
      
;; (lambda ()
;;     (loop for u from 8 downto 1
;;         append (loop for sub from (1+ u) to 9
;;                  collect (list (+ 10 u) (- sub)))))

  gen-digit-sub-pairs
  (lambda ()
    (do ((d1 9 (1- d1)) (tmp (ps:array)))
        ((= d1 1) tmp)
      (do ((d2 (1- d1) (1- d2))
           (tmp2 (ps:array)))
          ((= d2 1) (setf tmp (append tmp2 tmp)))
        (push (list d1 (- d2)) tmp2))))
  
  ;; (lambda ()
  ;;     (loop for d1 from 9 downto 2
  ;;        append (loop for d2 from (1- d1) downto 1
  ;;                    collect (list d1 (- d2)))))
  
  set-local-add-score
  (lambda (score)
    (with-slots (add-score) content-object
      (with-slots (best-time improvements optotal) add-score
        (set-score-code score))))
  
  set-local-sub-score
  (lambda (score)
    (with-slots (sub-score) content-object
      (with-slots (best-time improvements optotal) sub-score
        (set-score-code score))))
    
  intro-text
  (ps:lisp 
   (who:with-html-output-to-string (str)
     
     ;;(:h1 "How to play.")
     (:ul (:li (:strong "Click start.") "  A sum will appear.")
          (:li (:strong "Write the answer") " with the keyboard.  You
          do not have to press enter.")
          (:li "If you make a mistake, you must "(:strong "correct the
          sum") " to continue.")
          (:li "When you have done all the sums, the ones you
     got wrong will " (:strong "be repeated.")))
     
     (:p "Your first goal is to get all the sums right, with no
     mistakes: First with plus, then minus.")
     (:p "When your first goal has been achieved, you can play the
     games again by pressing the " (:q "games") " button on your
     page.")
     (:p "If you play them again, a timer will be displayed.  Your
     goal can then be to improve your times with no mistakes.")
     (:p "First concentrate on getting them all right.")))
    
  start 
  (lambda (ev)
    (let ((revmenu (doc-get-el-by-id "review-menu")))
      (set-display revmenu "none")
      (if voluntary
          (funcall voluntary-game)
          (funcall get-addsub-scores))))
  
  create-blackboard ;; Slot
  (lambda ()
    (let ((bb (div-id "intro-text-holder"))
          (txd (div-id "intro-text-display"))
          (bdiv (div-id "intro-button-holder")))
      (add-class bb "shift-right")
      (append-child bb txd)
      (append-child bb bdiv)
      bb))

  init-script ;;start-menu
  (lambda ()
    (let ((bb (funcall create-blackboard))
          (butholder (div-id "start-buttons"))
          (stbut (make-button "no-help" "Start" ("click" start)))
          (hbut (make-button "yes-help" "Help" ("click" intro))))
      (append-children butholder stbut hbut)
      (append-child (dots document body) bb)
      (set-display bb "inline")
      (append-child (dgebid "intro-text-display") butholder)))

  intro 
  (lambda () 
    (let ((bb (dgebid "intro-text-holder"))
          (bdiv (dgebid "intro-button-holder"))
          (but (make-button "end-intro" "Continue"))
          (h1 (create-element "h1")))
      (remove-class bb "shift-right")
      ;;(append-child bb h1)
      (insert-before bb h1 (dgebid "intro-text-display"))
      (append-child bdiv but)
      (setf (text-content-of h1) "How to play")
      (add-event-no-capture but "click" start)
      (set-inner-html (doc-get-el-by-id "intro-text-display") intro-text)))

  )
;; END OF SPECIAL CONTENT

  ;; intro 
  ;; (lambda () 
  ;;   (let ((bb (funcall create-blackboard)))
  ;;     (append-child (dots document body) bb)
  ;;     (set-display bb "inline")
  ;;     ;;(funcall (dots drag-drop init-element) bb)
  ;;     (add-event-no-capture (doc-get-el-by-id "end-intro") "click" start)
  ;;     (set-inner-html (doc-get-el-by-id "intro-text-display") intro-text))))



(defun init-mod-digs-nat-lesson-slots ()
  (ps:ps 
    (defun init-mod-digs-nat-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (create-ddsum-cell
                     create-length-line add-lengths-nl discrete-digit-sub-nl
                     place-circle-on-nl-mark
                     init-count-lines sub-lengths-nl add-line-on-nl
                     place-minuend-line-on-nl place-second-term place-first-term
                     remove-circles-from-numberline  place-subtrahend-line-on-nl
                     discrete-digit-sum-nl add-circle-on-numberline 
                     sub-circle-from-numberline second-term 
                     create-lsum-cell  place-circle-in-cell count-lines
                     set-minuend-circles-nl place-circles-n init-circle-counter 
                     count-circles ) ephemeral
          (with-slots (number-from-nlid in-summary
                       remove-default-rl-handlers restore-default-rl-handlers
                       garbage record-garbage remove-garbage
                       current-lesson-variables label-mark 
                       next-lesson-text previous-lesson-text 
                       reset-var use-var init-var ajax-reset-var
                       teach add-or-remove-wrong-click-nl-handlers
                       numberline-id wrong-click-nl random-int
                       from-direction current-module-name) non-ephemeral
            (setf
             
             add-or-remove-wrong-click-nl-handlers ;; Slot
             (lambda (except add)
               (let ((nl (doc-get-el-by-id "nl-holder")))
                 (when nl
                   (let ((lines (get-elements-by-tag-name nl "line")))
                     (dolist (line lines)
                       (when (and (not (string= (id-of line) ""))
                                  (not (string= (id-of line) except)))
                         (if add
                             (add-event-no-capture line "click" wrong-click-nl)
                             (remove-event-no-capture line "click" wrong-click-nl))))))))
             
             ;; wrong-nl-click ;; Slot
             ;; (lambda (ev)
             ;;   (let* ((me (id-of this))
             ;;          (number (aref (split (aref (split me "(") 1) ")") 0))
             ;;          (txt (doc-get-el-by-id "lesson-text-display"))
             ;;          (p (create-element "p"))
             ;;          (tn (strcat "No, that is " number ". Try again.")))
             ;;     (append-child p tn)
             ;;     (append-child txt p)))
             
             init-circle-counter ;; Slot
             (lambda (stop stop-text)
               (cond ((string= from-direction "rtl")
                      (funcall previous-lesson-text))
                     (t
                      (remove-event-no-capture (doc-get-el-by-id "previous-step")
                                               "click" previous-lesson-text)
                      (remove-event-no-capture (doc-get-el-by-id "next-step") 
                                               "click" next-lesson-text)
                      (when (not stop) (setf stop 1))
                      (when (not stop-text) (setf stop-text ""))
                      (with-slots (message limit counter inc dec reset) count-circles
                        (setf counter 0 limit stop message stop-text
                              inc (lambda () 
                                    (incf counter)
                                    (if (= counter limit)
                                        (let ((ret limit))
                                          (funcall reset)
                                          ret)
                                        false))
                              dec (lambda () 
                                    (when (> counter 0) (decf counter))
                                    (if (= counter limit)
                                        (let ((ret limit))
                                          (funcall reset)
                                          ret)
                                        false))
                              reset (lambda () 
                                      (setf counter 0 limit 0)))))))
             second-term ;; Slot
             (lambda (varspec)
               (let* ((val (funcall use-var varspec))
                      (re (ps:regex "/(\\+|\\-)/"))
                      (ts (split val re))
                      (t2 (aref ts 2)))
                 t2))
             
             discrete-digit-sub-nl ;; Slot
             (lambda (id which)
               (let ((already (if (string= from-direction "rtl") true false))
                     (ltd (doc-get-el-by-id "lesson-text-display")))
                 (if already
                     (funcall reset-var which (funcall use-var which))
                     (let* ((val (funcall use-var which))
                            (re (ps:regex "/(\\+|\\-)/"))
                            (ts (split val re))
                            (t1 (parse-int (aref ts 0)))
                            (op (aref ts 1)) 
                            (t2 (parse-int (aref ts 2)))
                            (skip svg-grid-skip) 
                            (height 3)
                            (width 3)
                            (ddsum-holder (div-id-class "ddsum-holder" "svg-container no-left"))
                            (optxt (let ((tn (make-text-node (strcat t1 op))))
                                     (append-child ddsum-holder tn)
                                     tn))
                            (svg1 (standard-svg-grid 
                                   ddsum-holder id 
                                   height width 
                                   create-ddsum-cell id))                      
                            (desc (let* ((p (create-element "p"))
                                         (tn (make-text-node 
                                              (strcat 
                                               t1 " is shown in the numberline.  Subtract " t2 " by clicking on the circles in the square."
                                               "This  represents the sum: "
                                               "$" val "$"))))
                                    (append-child p tn)
                                    (append-child ltd p)
                                    (mj-hub-queue (array "Typeset" mj-hub p))
                                    tn)))
                       (funcall set-minuend-circles-nl 1 t1)
                       (set-attribute svg1 "class" "dsum1")
                       (let ((t2child (last-child svg1)))
                         (funcall 
                          place-circles-n t2 t2child "blue" sub-circle-from-numberline))
                       (append-child ltd ddsum-holder)))))

             add-lengths-nl ;; Slot
             (lambda (id1 id2 which)
               (if (string= from-direction "rtl") (funcall previous-lesson-text)
                   (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                          (val (funcall use-var (strcat which ":q")))
                          (ans (funcall use-var (strcat which ":a")))
                          (re (ps:regex "/(\\+|\\-)/"))
                          (ts (split val re)) (t1 (parse-int (aref ts 0)))
                          (op (aref ts 1))  (t2 (parse-int (aref ts 2)))
                          (skip svg-grid-skip)(height 1) (width1 t1) (width2 t2)
                          (lsum-holder (div-id-class "lsum-holder" "svg-container no-left"))
                          (svg1 (standard-svg-grid lsum-holder id1  height width1 
                                                   create-lsum-cell id1))
                          (bar1  (funcall create-length-line 
                                          svg1 (ems (* skip width1)) "blue" t1))
                          (optxt (let ((tn (make-text-node op)))
                                   (append-child lsum-holder tn) tn))
                          (svg2 (standard-svg-grid lsum-holder id2 height width2 create-lsum-cell id2))
                          (bar2 (funcall create-length-line 
                                         svg2 (ems (* skip width2)) "green" t2))
                          (desc (append-para-text ltd 
                                                  (strcat "The colored bars represent the sum: $" val "$.  Click on the bars to add them in the numberline.") true)))
                     (set-attribute svg1 "class" "lsum1")
                     (set-attribute svg2 "class" "lsum2")
                     ;;EVENT handling from here
                     (funcall init-count-lines (strcat which ":q") 
                              2 (strcat 
                                 "This has shown $" val "=" ans "$
                                     by placing lines of the correct
                                     lengths in the numberline."))
                     (add-event-no-capture bar1 "click" add-line-on-nl)
                     (add-event-no-capture bar2 "click" add-line-on-nl)
                     (append-child ltd lsum-holder))))

             create-length-line ;; Slot
             (lambda (parent width color term-value id)
               (let* ((b1 (svg-create-element "line"))
                      (skip svg-grid-skip)
                      (y1 (ems (/ skip 2)))
                      (y2 y1))
                 (set-attributes b1 "term-value" term-value
                                 "stroke-width" "1.5em"
                                 "stroke" color  "x1" "0em" "y1" y1
                                 "stroke-opacity" "0.3"
                                 "x2" width "y2" y2)
                 (when id (set-attribute b1 "id" id))
                 (append-child parent b1)
                 b1))
             
             sub-lengths-nl ;; Slot
             (lambda (id1 id2 which) 
               (if (string= from-direction "rtl")
                   (let ((vn (strcat which ":q")))
                     (funcall reset-var vn (funcall use-var vn))
                     (funcall previous-lesson-text))
                   (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                          (sum (funcall use-var (strcat which ":q")))
                          (ans (funcall use-var (strcat which ":a")))
                          (re (ps:regex "/(\\+|\\-)/"))
                          (ts (split sum re)) 
                          (t1 (parse-int (aref ts 0)))
                          (op (aref ts 1))  
                          (t2 (parse-int (aref ts 2)))
                          (skip svg-grid-skip) (height 1) (width1 t1) (width2 t2)
                          (lsum-holder (div-id-class "lsum-holder" "svg-container no-left"))
                          (svg1 (standard-svg-grid lsum-holder id1  height width1 
                                                   create-lsum-cell id1))
                          (bar1  (funcall create-length-line svg1 
                                          (ems (* skip width1)) "blue" t1 "minuend"))
                          (optxt (let ((tn (make-text-node op)))
                                   (append-child lsum-holder tn) tn))
                          (svg2 (standard-svg-grid lsum-holder id2 height width2 create-lsum-cell id2))
                          (bar2 (funcall create-length-line svg2 
                                         (ems (* skip width2)) "green" t2 "subtrahend"))
                          (desc (append-para-text ltd 
                                                  (strcat "The colored bars represent the subtraction: $" sum "$.  Click on the first bar.") true)))
                     (set-attribute svg1 "class" "lsum1")
                     (set-attribute svg2 "class" "lsum2")
                     (append-child ltd lsum-holder)
                     ;;Here
                     (remove-event-no-capture (doc-get-el-by-id "previous-step") "click"
                                              previous-lesson-text)
                     (remove-event-no-capture (doc-get-el-by-id "next-step") "click"
                                              next-lesson-text)
                     (add-event-no-capture bar1 "click" place-first-term) 
                     )))       

             count-circles (ps:new (-object)) ;; Var Slot
             count-lines (ps:new (-object)) ;; Var slot
             
             init-count-lines  ;; Slot
             (lambda (val howmany msg)
               (with-slots (current remaining dec reset initialized remove 
                                    remove-handler) 
                   count-lines
                 (let ((rarr (doc-get-el-by-id "next-step"))
                       (larr (doc-get-el-by-id "previous-step"))
                       (tn (make-text-node msg))
                       (p (create-element "p"))
                       (ltd (doc-get-el-by-id "lesson-text-display")))
                   (remove-event-no-capture rarr "click" next-lesson-text)
                   (remove-event-no-capture larr "click" previous-lesson-text)
                   (append-child p tn)
                   (mj-hub-queue (ps:array "Typeset" mj-hub p))
                   (setf current 0 initialized true remaining howmany
                         remove (ps:array)
                         remove-handler 
                         (lambda ()
                           (dolist (r remove)
                             (remove-child (aref r 0) (aref r 1)))
                           (remove-event-no-capture rarr "click"
                                                    remove-handler)
                           (remove-event-no-capture larr "click"
                                                    remove-handler)
                           (add-event-no-capture rarr "click" 
                                                 next-lesson-text)
                           (add-event-no-capture larr "click" 
                                                 previous-lesson-text)
                           (let ((tn (make-text-node "You can
                           continue or go back a step to try again with
                           other numbers."))
                                 (p (create-element "p"))
                                 (ltd (doc-get-el-by-id "lesson-text-display")))
                             (append-child p tn)
                             (append-child ltd p)
                             (funcall reset-var val (funcall use-var val))))
                         dec (lambda (tvalue)
                               (decf remaining)
                               (cond ((= remaining 0)
                                      (add-event-no-capture rarr "click" remove-handler)
                                      (add-event-no-capture larr "click" 
                                                            remove-handler)
                                      (append-child ltd p))
                                     (t (setf current (+ current tvalue))
                                        current)))
                         reset (lambda ()
                                 (setf current 1                                
                                       remove (ps:array)
                                       remaining 0))
                         ))))

             place-first-term ;; Slot
             (lambda (ev)
               (funcall place-minuend-line-on-nl this))

             place-second-term ;; Slot
             (lambda (ev)
               (funcall place-subtrahend-line-on-nl this))

             place-minuend-line-on-nl  ;; Slot
             (lambda (line)
               (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                      (skip svg-grid-skip)
                      (m0 (doc-get-el-by-id "nl-mark(0)"))
                      (mp (parent-node m0))
                      (nl-line (first-child (doc-get-el-by-id "numberline-line")))
                      (x1 (get-attribute m0 "x1"))
                      (tval (get-attribute line "term-value"))
                      (y1 (ems (/ skip 2))) ;;(get-attribute nl-line "y1"))
                      (y2 y1)
                      (x2 (ems (+ (* tval skip) (parse-float x1)))))
                 (set-attributes line
                                 "x1" x1 "y1" y1 "x2" x2 "y2" y2)
                 (remove-event-no-capture line "click" place-first-term)
                 (append-child mp line)
                 (funcall record-garbage mp line)
                 (append-para-text ltd "Click on the second bar." false)
                 (add-event-no-capture (doc-get-el-by-id "subtrahend") 
                                       "click" place-second-term)))

             place-subtrahend-line-on-nl ;; Slot
             (lambda (line)
               (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                      (parent (parent-node line))
                      (m0 (doc-get-el-by-id "nl-mark(0)"))
                      (m0x (parse-float (get-attribute m0 "x1")))
                      (ltd (doc-get-el-by-id "lesson-text-display"))
                      (skip svg-grid-skip)
                      (minuend (doc-get-el-by-id "minuend"))
                      (x2 (get-attribute minuend "x2"))
                      (t1 (parse-float (get-attribute minuend "term-value")))
                      (t2 (parse-float (get-attribute line "term-value")))
                      (diff (- t1 t2))
                      (x1 (ems (+ m0x (* skip diff))))
                      (pm (parent-node (doc-get-el-by-id (funcall numberline-id diff))))
                      (y1 (ems (+ (parse-float (get-attribute (first-child (doc-get-el-by-id "numberline-line")) "y1")) (/ skip 2))))
                      (y2 y1))
                 (remove-child parent line)
                 (set-attributes line "x1" x1 "y1" y1 "x2" x2 "y2" y2)
                 (remove-event-no-capture line "click" place-second-term)
                 (append-child pm line)
                 (funcall record-garbage pm line)
                 (append-para-text ltd (strcat "This has shown $" 
                                               t1 "-" t2 "=" diff 
                                               "$ in the numberline.  You can
                                        see that the first bar is "
                                               diff " longer than the
                                        second.") true)
                 (funcall restore-default-rl-handlers )))

             add-line-on-nl ;; Slot
             (lambda (ev final)
               (with-slots (current dec remove) count-lines
                 (let* ((skip svg-grid-skip)
                        (parent (parent-node this))
                        (tval (parse-float (get-attribute this "term-value")))
                        (curr current)
                        (mark (doc-get-el-by-id (funcall numberline-id curr)))
                        (mark-parent (parent-node mark))
                        (x1 (get-attribute mark "x1"))
                        (x2 (ems (+ (parse-float x1) (* skip tval))))
                        (y1 (ems svg-grid-skip)) (y2 y1)
                        (nl (doc-get-el-by-id "nl-holder")))
                   (remove-event-no-capture this "click" add-line-on-nl)
                   (push (ps:array mark-parent this) remove)
                   (remove-child parent this)
                   (set-attributes this "x1" x1 "y1" y1 "x2" x2 "y2" y2)
                   (append-child mark-parent this)
                   (funcall dec tval))))            
             
             remove-circles-from-numberline ;; Slot
             (lambda (ev)
               (let* ((circles (get-elements-by-tag-name 
                                (doc-get-el-by-id "nl-holder") "circle"))
                      (ln (1- (length circles))))
                 (loop for i from ln downto 0
                    for circ = (aref circles i)
                    for parent = (parent-node circ)
                    do (remove-child parent circ))
                 (remove-event-no-capture this "click" remove-circles-from-numberline)
                 (add-event-no-capture this "click" next-lesson-text)
                 (add-event-no-capture (doc-get-el-by-id "previous-step")
                                       "click" previous-lesson-text)
                 (funcall next-lesson-text)))

             add-circle-on-numberline  ;; Slot
             (lambda (ev)
               (with-slots (counter inc message) count-circles
                 (let* ((circ-parent (parent-node this))
                        (last (funcall inc))
                        (n (if last last counter))
                        (mark-id (funcall numberline-id n))
                        (mark (doc-get-el-by-id mark-id))
                        (mark-parent (parent-node mark))
                        (mark-x1 (get-attribute mark "x1"))
                        (mark-y2 (get-attribute mark "y2"))
                        (newy (ems (+ (parse-float mark-y2) 0.0625))))
                   (remove-event-no-capture this "click" add-circle-on-numberline)
                   (remove-child circ-parent this)
                   (append-child mark-parent this)
                   (set-attributes this "cx" mark-x1 "cy" newy)
                   (when last 
                     (let ((p (create-element "p"))
                           (tn (make-text-node message))
                           (ltd (doc-get-el-by-id "lesson-text-display"))
                           (rarr (doc-get-el-by-id "next-step")))
                       (remove-event-no-capture rarr "click" next-lesson-text)
                       (add-event-no-capture rarr "click" remove-circles-from-numberline)
                       (append-child p tn)
                       (append-child ltd p)
                       (mj-hub-queue (array "Typeset" mj-hub p)))))))

             discrete-digit-sum-nl ;; Slot
             (lambda (id1 id2 which)
               (let ((already (if (string= from-direction "rtl") true false))
                     (ltd (doc-get-el-by-id "lesson-text-display")))
                 (if already
                     (funcall reset-var which (funcall use-var which))
                     (let* 
                         ((val (funcall use-var which)) (re (ps:regex "/(\\+|\\-)/"))
                          (ts (split val re)) (t1 (parse-int (aref ts 0)))
                          (op (aref ts 1))(t2 (parse-int (aref ts 2)))
                          (skip svg-grid-skip) (height 3) (width 3)
                          (ddsum-holder (div-id-class "ddsum-holder" "svg-container no-left"))
                          (svg1 (standard-svg-grid ddsum-holder id1 
                                                   height width 
                                                   create-ddsum-cell id1))
                          (optxt (let ((tn (make-text-node op)))
                                   (append-child ddsum-holder tn)
                                   tn))                     
                          (svg2 (standard-svg-grid 
                                 ddsum-holder id2 
                                 height width 
                                 create-ddsum-cell id2))
                          (desc (append-para-text 
                                 ltd 
                                 (strcat 
                                  "Click on the circles in the squares. "
                                  "The squares represent the sum: "
                                  "$" val "$") true)))
                       (set-attribute svg1 "class" "dsum1")
                       (set-attribute svg2 "class" "dsum2")
                       (let ((t1child (last-child svg1)))
                         (funcall 
                          place-circles-n t1 t1child "blue" add-circle-on-numberline))
                       (let ((t2child (last-child svg2)))
                         (funcall 
                          place-circles-n t2 t2child "green" add-circle-on-numberline))
                       (append-child ltd ddsum-holder)))))             
             
             place-circle-in-cell ;; Slot
             (lambda (cell color handler)
               (let* ((skip svg-grid-skip)
                      (xy-offset (/ skip 2))
                      (radius (/ xy-offset 2))
                      (circ (svg-create-element "circle"))
                      (rect (first-child cell))
                      (rx (parse-float (get-attribute rect "x")))
                      (ry (parse-float (get-attribute rect "y"))))
                 (set-attributes circ "cx" (ems (+ rx xy-offset))
                                 "cy" (ems (+ ry xy-offset))
                                 "r" (ems radius)
                                 "fill" color)
                 (when handler 
                   (add-event-no-capture circ "click" handler))
                 (append-child cell circ)))

             place-circles-n ;; Slot
             (lambda (n cell color handler)
               (cond ((<= n 0) false)
                     (t 
                      (funcall place-circle-in-cell cell color handler)
                      (funcall place-circles-n (1- n) (previous-sibling cell)
                               color handler))))

             place-circle-on-nl-mark ;; Slot
             (lambda (parent cx cy color)
               (let* ((skip svg-grid-skip)
                      (xy-offset (/ skip 2))
                      (radius (/ xy-offset 2))
                      (circ (svg-create-element "circle")))
                 (set-attributes circ "cx" cx "cy" cy "r" (ems radius) "fill" color)
                 (append-child parent circ)))
             create-ddsum-cell ;; Slot
             (define-anonymous-with-cell-variables
                 (set-attributes g  "class" "ddsum-cell")
                 (set-attributes rect "x" xem "y" yem "width" skipem
                                 "height" skipem "fill" "none"
                                 "pointer-events" "all")
               (append-child g rect)
               (append-child svg g))
             create-lsum-cell ;; Slot
             (define-anonymous-with-cell-variables
                 (set-attributes g  "class" "lsum-cell")
                 (set-attributes rect "x" xem "y" yem "width" skipem
                                 "height" skipem "fill" "none"
                                 "pointer-events" "all"
                                 "stroke" "#cdcdcd")
               (append-child g rect)
               (append-child svg g))
             set-minuend-circles-nl ;;slot
             (lambda (count t1)
               (when (< t1 1) false) 
               (if (> count t1) true
                   (let* ((mark (doc-get-el-by-id (funcall numberline-id count)))
                          (g  (parent-node mark))
                          (mark-x1 (get-attribute mark "x1"))
                          (mark-y2 (get-attribute mark "y2"))
                          (newy (ems (+ (parse-float mark-y2) 0.0625))))
                     (funcall place-circle-on-nl-mark g mark-x1 newy "blue") 
                     (funcall set-minuend-circles-nl (1+ count) t1)
                     )))

             sub-circle-from-numberline ;; Slot
             (lambda (ev) 
               (with-slots (counter inc message) count-circles
                 (let* ((circles (get-elements-by-tag-name 
                                  (doc-get-el-by-id "nl-holder")
                                  "circle"))
                        (to-remove (aref circles (1- (length circles))))
                        (parent (parent-node to-remove))
                        (last (funcall inc))
                        (n (if last last counter)))
                   (set-attributes this "fill" "#dfdfdf" "stroke" "#000000")
                   (remove-event-no-capture this "click" sub-circle-from-numberline)
                   (remove-child parent to-remove)
                   (when last 
                     (let ((p (create-element "p"))
                           (tn (make-text-node message))
                           (ltd (doc-get-el-by-id "lesson-text-display"))
                           (rarr (doc-get-el-by-id "next-step")))
                       (remove-event-no-capture rarr "click" next-lesson-text)
                       (add-event-no-capture rarr "click" remove-circles-from-numberline)
                       (append-child p tn)
                       (append-child ltd p)
                       (mj-hub-queue (array "Typeset" mj-hub p)))))))
             )))
        ephemeral
        ))
    (init-mod-digs-nat-lesson-slots)
    ))

(define-lesson mod-digs-nat
    :stepif (special-lesson-module
             ((:p "Welcome to the first lesson in the program.")
              (:p "Usually, when you use this program, each part will start
     with an explanation of something and will continue with exercises
     for you to do.")
   (:p "This part is a little different.")
   (:p "Instead, after this lesson, you will play some learning
     games which can help you to memorise the basic facts of adding
     small numbers from 0 to 9, and subtracting from small numbers
     below 18.")
   (:p "When you have won in the games, you will automatically go
   forward in the program.")
   (:p "Usually you must press the right arrow to continue in the
   lesson.  Sometimes, you will have to do something else to continue
   with a lesson, so it is important either that you read the text, or
   that your teacher or a someone else has read it for you.")))

   :step ((:p "There are two parts to learning some new piece of mathematics:")
           (:ul (:li (:strong "DO") " something new.")
                (:li (:strong "UNDERSTAND") " something new."))
           (:p "Sometimes, learning something new in mathematics requires
     going backwards and forwards between doing something and trying
     to understand it, " (:strong "many") " times.")
          (:p "So do not become discouraged if you do not understand
          something immediately."))
          
     
    :step ( (:p "Very often, a good way of thinking about anything to
    do with most types of numbers is with a numberline.") (:p "Press
    the right arrow to see a numberline."))
    :display (funcall numberline "mod-digs-nat-nl" -20 20 1 "#000000" 0.125)
    :step ( (:p "A numberline is just a picture of how typical numbers
    'fit together'.") (:p "You can see a 'zero' in the middle of the
    numberline."))
    :replace-default-next-step 
    ((handle-nl-click0 "nl-mark(0)")
     "Click the zero mark on the numberline to continue")
    :step ((:p "Good. Each mark after zero shows the next number, so we
    get '0, 1, 2, ...' (The dots mean the numbers continue 'forever',
    and thats why there are arrows on the line)") (:p "The numbers to
    the right of zero are called " (:strong "positive") " numbers.  We
    can also go to the left of zero to get " (:strong "negative") "
    numbers. So we have '..., -2, -1, 0'.  When we say aloud these
    numbers to the left of zero we say 'minus 2', 'minus 1' and so on.
    Sometimes people say, 'negative 1' instead."))
    :globals ((:variable "nlneg1" (funcall numberline-id (funcall random-int -9 -1))))
    :replace-default-next-step
    ((handle-nl-clickneg (funcall use-var "nlneg1"))
     (strcat 
      "Click on the " (funcall number-from-nlid 
                               (funcall use-var "nlneg1")) " mark to continue."))
    :step ((:p "We will come
    back to the negative numbers in a later lesson.  For now, we will
    be concentrating on the " (:strong "positive") " numbers."))
    :globals ((:variable "nlpos1" (funcall numberline-id 
                                           (funcall random-int 1 9))))
    :replace-default-next-step
    ((handle-nl-clickpos (funcall use-var "nlpos1"))
     (strcat "Click on the " 
             (funcall number-from-nlid 
                      (funcall use-var "nlpos1")) " mark to continue."))
    :step ( (:p "When you are counting things, you can imagine pairing
    that thing with its number on the numberline.")  (:p "If you are
    pairing more things with the numbers, then you
    are " (:strong "adding.") "  If you are removing things from the
    numberline, then you are " (:strong "subtracting.")) (:p "Here
    follows an example of adding in this way."))
    :using-q&a-vars
    ((:q&a-var "ddsum1" digadd)
     (progn
       (funcall init-circle-counter (parse-int (funcall use-var "ddsum1:a"))
                (strcat "This has shown $"
                        (funcall use-var "ddsum1:q") "="
                        (funcall use-var "ddsum1:a") "$ in the numberline."))
        (funcall discrete-digit-sum-nl "t1" "t2" "ddsum1:q")))
    :step ( "Here follows an example with subtraction")
    :using-q&a-vars
    ((:q&a-var "ddsub1" digsubn)
     (progn
       (funcall init-circle-counter 
                (parse-int (funcall second-term "ddsub1:q"))
                (strcat "This has shown $"
                        (funcall use-var "ddsub1:q") "="
                        (funcall use-var "ddsub1:a") "$ in the numberline."))
       (funcall discrete-digit-sub-nl "t1" "ddsub1:q")))
    :step ( (:p "Since we do not " (:strong "only") " use numbers to
    count things, another way of thinking with the numberline, is to
    imagine that you are measuring something with it.  This means
    seeing the numberline like a ruler or a tape-measure.") (:p "If
    you think this way, then " (:strong "adding") " numbers is like
    placing lengths represented by those numbers one after the other
    and measuring the whole thing with the numberline to see what you
    get."))
    :step ((:p "Here follows an example of seeing addition as
    placing lengths together."))
    :using-q&a-vars
    ((:q&a-var "mdsum1" digadd-nz)
     (funcall add-lengths-nl "t1" "t2" "mdsum1"))
    :step ((:p "When thinking of addition as placing lengths together,
    it is quite easy to see, that it does not matter which length you
    place first, since the whole length remains the same.") (:p "That
    means that if we have a plus sign between two numbers, we can swap
    the numbers around if we want to.") (:p "Also, If we had 3 numbers
    to add, it would not matter which lengths we placed together
    first") (:p "This means we are free to choose any 2 numbers to add
    first."))
    :step ( (:p "If we want to " (:strong "subtract") " one number
    from another one, this is like finding
    the " (:strong "difference") " in their lengths.") (:p "With
    subtraction we are measuring how much the " (:strong "first") "
    length is longer than the second so it
    is " (:strong "important") " which number comes first (unless the
    numbers are the same, of course, since then there is no
    difference).") (:p "This means that if we have a minus sign
    between 2 numbers, we " (:strong "cannot") " swap them
    around.") (:p "This will make more sense when you learn about
    negative numbers, but for now, just remember that it is
    important."))
    :step ((:p "Here follows an example of seeing a subtraction
    as the difference of two lengths."))
    :using-q&a-vars
    ((:q&a-var "mdsub1" digsubn)
     (funcall sub-lengths-nl "t1" "t2" "mdsub1"))
    :step ( (:p "An important thing about subtraction is that it is in
    some sense the 'reverse' of addition.  This means that you can
    always check a subtraction by doing an addition.") (:p "If you
    want to try this, click the left arrow key to go back in the
    lesson and repeat the last few steps, while asking yourself this
    question: what must I " (:strong "add") " to the second number to
    get the first?"))
    :step (( :h1 "Important information") (:p "You don't have to think of
    the numberline every time you do an addition or subtraction, but
    if a sum seems difficult, it can be very useful to think about it
    with the help of the numberline. You will also find the numberline
    very useful when you learn about negative numbers, fractions and
    other numbers."))

    :stepif (special-lesson-module
             ((:p "That completes the first lesson.  After the next step,
     which is a summary of this lesson, you will come to the games.
     There are two games: One for learning to add and one for learning
     to subtract. The adding game comes first.")
     (:p "There are 2 goals for each game:")
     (:ul (:li (:strong "You must") " get all the questions right with
     no mistakes at least once."))
              (:p "When you have achieved the first goal, you will
     automatically come to the next part of the program.")
              (:ul
               (:li "Once you can get them all right, " (:strong "you
          can choose") " to play again as many times as
          you like, to see how fast you can do it.  A button will
          appear on your page which you can press to play again."))
     
     (:p "You might have to play the games many times before you
     achieve the first goal.")))
    
    :summary ((funcall numberline "mod-digs-nat-nl" -20 20 1 "#000000" 0.125)
              ((:h1 "Summary") (:ul (:li "With addition, you can take
    either number first.  Order doesn't matter") (:li "If you have
    more than 2 numbers to add, you can first add any 2 of them
    together.") (:li "With subtraction, order matters, and you must
    subtract from the first number.") (:li "Subtraction is a kind
    of " (:emph "reverse") " of addition.  You can use this fact to
    check a subtraction")
    (:li "Two basically different ways of using numbers are:"
         (:ul (:li (:strong "Counting") "
         whole " (:strong "things.") " In this case, we can think
         of " (:strong "pairing") " the thing with it's numberline
         number.")
              (:li (:strong "Measuring") " amounts of something. This
              is best thought of as " (:strong "lengths") " or
              distances on the numberline."))))
    (:p "Press the right arrow to end this lesson.")))
    :complete? t)
