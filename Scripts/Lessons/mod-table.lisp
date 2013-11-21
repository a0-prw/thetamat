(in-package :autotutor)
;;FIXME remember to setf ps:*ps-print-pretty* nil

;;score-code is defined in dom-utils.lisp

(define-special-content mod-table 
    (("Multiplication" "Division") #'pupil-basic-mul) "SCRIPT" 
    (start ;;division-pairs
     voluntary-game ;;gen-mul-pairs gen-div-pairs 
     ;;gen-teen-div-pairs gen-digit-div-pairs 
     reset-game-record game-record pupil-record-object
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
     mul-won div-won make-header insert-start-button 
     insert-quit-button intro intro-text create-blackboard 
     complete-game voluntary teacher-demo restore-page-state
     get-muldiv-scores set-local-mul-score set-local-div-score
     mul-score div-score game-won
     special-previous-lesson 
     handle-nl-click0 handle-nl-clickpos handle-nl-clickneg
     )

  teacher-demo
  false

  voluntary 
  false

  ;;division-pairs (get-division-pairs)
  
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
  (with-slots (best-time) mul-score
    (lambda ()
      (if game-name game-name
          (if (not best-time)
              "Multiplication"
              "Division"))))
  
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
  (with-slots (bad-answers grace no-errors) game-record
    (with-slots (best-time improvements optotal) pupil-record
      (with-slots (train test) switch
        (lambda (pupil-record game-name)
          (labels ((callback (response)
                     ;;successful game completion - if both have been
                     ;;completed or game was called voluntarily from
                     ;;user page return to exercises
                     (if (or voluntary
                             (and (funcall mul-won)
                                  (funcall div-won)))
                         (funcall complete-game 0)
                         ;;otherwise empty-stack and change
                         ;;current-questions
                         (let ((name game-name))
                           (cond ((string= name "Multiplication")
                                  (setf current-questions 
                                        (ps:array (jsn div-pairs))))
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
                   (se (text-content-of 
                                (doc-get-el-by-id "errors-field")))
                   (es (parse-int se))
                   (empty-repeats (not-defined (aref repeats 0))))
              (cond ((or (and empty-repeats no-errors)
                         (and empty-repeats (< es 6) (< grace 2)))
                     (let* ((fin (funcall stop-time))
                            (phrase (quantify-noun se "corrected errors"))
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
                               (my-alert (strcat "Well done! " (if (zerop es) "No errors"
                                                                   (strcat "You had " phrase)) "!")
                                         uprc-fun))
                           (let ((msg (if best-time ;;if best-time is
                                                    ;;not false, timer
                                                    ;;is displaying
                                          (strcat "Well done! " (if (zerop es) "No errors"
                                                                   (strcat "You had " phrase)) "!"
                                                                   "Your time was: " 
                                                                   fin " seconds.")
                                          (strcat "Well done! " (if (zerop es) "No errors"
                                                                    ;;(strcat es " corrected errors!")
                                                                    (strcat "You had " phrase)) "!")
                                                                    )))
                             ;;"Well done! No errors!")))
                             (when test (setf best-time fin))
                             (my-alert msg uprc-fun)))))       
                    (empty-repeats
                     (funcall complete-game (parse-int se)))
                    (t 
                     (let* ((phrase1 (quantify-noun se "errors" true))
                            (phrase2 (quantify-noun se "problems")))
                       (my-alert (strcat ;;Game not over there are repeats
                                  "You had " phrase1 " in " phrase2
                                  " , which will now be repeated.")
                                 (lambda ()
                                   (setf stack bad-answers
                                         grace (1+ grace)
                                         bad-answers (ps:array))
                                   (funcall make-question))))))))))))
  
  ;; repeat-or-update
  ;; (with-slots (bad-answers no-errors) game-record
  ;;   (with-slots (best-time improvements optotal) pupil-record
  ;;     (with-slots (train test) switch
  ;;       (lambda (pupil-record game-name)
  ;;         (labels ((callback (response)
  ;;                    ;;successful game completion - if both have been
  ;;                    ;;completed or game was called voluntarily from
  ;;                    ;;user page return to exercises
  ;;                    (if (or voluntary
  ;;                            (and (funcall mul-won)
  ;;                                 (funcall div-won)))
  ;;                        (funcall complete-game 0)
  ;;                        ;;otherwise empty-stack and change
  ;;                        ;;current-questions
  ;;                        (let ((name game-name))
  ;;                          (cond ((string= name "Multiplication")
  ;;                                 (setf current-questions 
  ;;                                       (ps:array (jsn div-pairs))))
  ;;                                ((string= name "Multiplication")
  ;;                                 (setf current-questions 
  ;;                                       (ps:array (jsn div-pairs))))
  ;;                                (t (throw (strcat "Game name error: "
  ;;                                                  name))))
  ;;                          (funcall empty-stack)
  ;;                          (funcall game-stop))))
  ;;                  (uprc-fun ()
  ;;                    (ajax_update_pupil_record owner game-name 
  ;;                                              (stringify-json 
  ;;                                               (list best-time 
  ;;                                                     improvements optotal))
  ;;                                              callback)))
  ;;           (let* ((repeats bad-answers)
  ;;                  (empty-repeats (not-defined (aref repeats 0))))
  ;;             (cond ((and empty-repeats no-errors)
  ;;                    (let* ((fin (funcall stop-time))
  ;;                           (cbt (when test best-time)))
  ;;                      (setf optotal (1+ optotal)) 
  ;;                      (if cbt ;;FIXME do not update best time in train mode.
  ;;                          (if (> (- (parse-float cbt) (parse-float fin)) 0)
  ;;                              (progn 
  ;;                                (setf best-time fin)
  ;;                                (setf improvements
  ;;                                      (if improvements (1+ improvements) 1))
  ;;                                (my-alert (strcat "Well done! New best time: " 
  ;;                                                  fin) uprc-fun))
  ;;                              (my-alert "Well done! No errors!"
  ;;                                        uprc-fun))
  ;;                          (let ((msg (if best-time ;;if best-time is
  ;;                                                   ;;not false, timer
  ;;                                                   ;;is displaying
  ;;                                         (strcat "Well done! No errors!"
  ;;                                                 (ps:lisp #\Newline)
  ;;                                                 "Your time was: " 
  ;;                                                 fin " seconds.")
  ;;                                         "Well done! No errors!")))
  ;;                            (when test (setf best-time fin))
  ;;                            (my-alert msg uprc-fun)))))                          
  ;;                   (empty-repeats
  ;;                    (let ((se (text-content-of 
  ;;                               (doc-get-el-by-id "errors-field"))))
  ;;                      (funcall complete-game se)))
  ;;                   (t 
  ;;                    (let* ((se (text-content-of 
  ;;                                (doc-get-el-by-id "errors-field")))
  ;;                           (phrase1 (quantify-noun se "errors" true))
  ;;                           (phrase2 (quantify-noun se "problems")))
  ;;                      (my-alert (strcat ;;Game not over there are repeats
  ;;                                 "You had " phrase1 " in " phrase2
  ;;                                 " , which will now be repeated.")
  ;;                                (lambda ()
  ;;                                  (setf stack bad-answers
  ;;                                        bad-answers (ps:array))
  ;;                                  (funcall make-question))))))))))))
  
  ;;give-test-response
    
  complete-game
  (with-slots (grace) game-record
    (with-slots (test train) switch
      (lambda (nerrors)
        (labels ((callback (response)
                   (let ((msg (parse-json response)))
                     (funcall restore-page-state)
                     (alert msg)
                     (get-all-tables-and-start))))
          (let ((update (if voluntary false true)))
            (if (or (zerop nerrors)
                    (and (< nerrors 6)
                         (< grace 2)))                    
                (if test
                    (ajax_update_module owner update callback)
                    (funcall game-stop))
                (my-alert (strcat "You completed the game with " 
                                  (quantify-noun nerrors "errors")
                                  ".  Try again!") (lambda ()
                                                     (funcall game-stop)))))))))

  ;; complete-game
  ;; (with-slots (test train) switch
  ;;   (lambda (nerrors)
  ;;     (labels ((callback (response)
  ;;                (funcall restore-page-state)
  ;;                (get-all-tables-and-start)))
  ;;       (let ((update (if voluntary false true)))
  ;;         (if (zerop nerrors) ;;parse-int?
  ;;             (if test
  ;;                 (ajax_update_module owner update callback)
  ;;                 (funcall game-stop))
  ;;             (my-alert (strcat "You completed the game with " 
  ;;                               (quantify-noun nerrors "errors")
  ;;                               ".  Try again!") (lambda ()
  ;;                                                  (funcall game-stop))))))))
  ;; restore-page-state
  ;; (lambda ()
  ;;   (let ((gh (doc-get-el-by-id "game-holder"))
  ;;         (ith (doc-get-el-by-id "intro-text-holder"))
  ;;         (lrm (doc-get-el-by-id "review-menu"))
  ;;         (rm (doc-get-el-by-id "replay-menu"))
  ;;         (rp (doc-get-el-by-id "right"))
  ;;         (pg (doc-get-el-by-id "pager")))
  ;;     (funcall stop-time)
  ;;     (remove-child (dots document body) gh)
  ;;     (when ith (remove-child (dots document body) ith))
  ;;     (set-display lrm "inline-block")
  ;;     (set-display rm "inline-block")
  ;;     (set-display rp "inline-block")
  ;;     (when (> all-tables 1) (set-display pg "block"))
  ;;     (setf content-object (ps:new (-object)))))

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
  ;;(with-slots (bad-answers no-errors) game-record
    (lambda ()
      (let ((triple (pop stack)))
        (if (not-defined triple)
            (let* ((game-name (text-content-of (doc-get-el-by-id "game-field")))
                   (pupil-record (cond ((string= game-name "Multiplication")
                                        mul-score)
                                       ((string= game-name "Division")
                                        div-score)
                                       ((string= game-name "Multiplication")
                                        mul-score)
                                       ((string= game-name "Division")
                                        div-score)
                                       (t (throw 
                                              (strcat 
                                               "gameName error: " game-name))))))
              (funcall repeat-or-update pupil-record game-name))
            (ps:destructuring-bind (op t1 t2)
                triple
              (let* ((ans (if (string= op "*") (* t1 t2) (/ t1 t2)))
                     (sign (if (string= op "*") "\\cdot" "\\div"))
                     (sans (strcat "" ans))
                     (lans (length sans))
                     (inp (make-text-input-element lans lans))
                     (st1 (strcat "" t1))
                     (st2 (strcat "" t2))
                     (same (string= st2 st1))
                     (qh (div-id "current-question"))
                     (tx1 (strcat st1 sign st2 ))
                     (tx2 (if (or same (string= op "/")) false (strcat st2 sign st1)))
                     (place (doc-get-el-by-id "game-interface-holder")))
              (setf cq-handler (funcall make-answer-handler triple tx1 tx2 sans lans))
              (prune-tree-from-nth place 0)
              (append-child place qh)
              (append-para-text qh (mj-bracket tx1) true)
              (when tx2 (append-para-text qh (mj-bracket tx2) true))
              (append-child qh inp)
              (focus inp)
              (add-event-no-capture inp "keydown" cq-handler))))));;)
  
  make-answer-handler
  (with-slots (bad-answers no-errors) game-record
    (lambda (trip tx1 tx2 sans lans)
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
                    (push trip bad-answers))
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
          (timesnames (list (list (jsn 0*) "$0 \\cdot$")
                            (list (jsn 1*) "$1 \\cdot $") 
                            (list (jsn 2*) "$2 \\cdot $")
                            (list (jsn 3*) "$3 \\cdot $")
                            (list (jsn 4*) "$4 \\cdot $")
                            (list (jsn 5*) "$5 \\cdot $")
                            (list (jsn 6*) "$6 \\cdot $")
                            (list (jsn 7*) "$7 \\cdot $")
                            (list (jsn 8*) "$8 \\cdot $")
                            (list (jsn 9*) "$9 \\cdot $")
                            (list (jsn 10*) "$10 \\cdot $")))
          (divnames (list (list (jsn /1) "$\\div 1 $") 
                          (list (jsn /2) "$\\div 2 $")
                          (list (jsn /3) "$\\div 3 $")
                          (list (jsn /4) "$\\div 4 $")
                          (list (jsn /5) "$\\div 5 $")
                          (list (jsn /6) "$\\div 6 $")
                          (list (jsn /7) "$\\div 7 $")
                          (list (jsn /8) "$\\div 8 $")
                          (list (jsn /9) "$\\div 9 $")
                          (list (jsn /10) "$\\div 10 $")

                     )))
      (dolist (type (list timesnames divnames))
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
      (mj-hub-queue (array "Typeset" mj-hub cbxholder))
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
  (ps:create 
   div-pairs (get-muldiv-pairs division)
   mul-pairs (get-muldiv-pairs multiplication)
   "0star" (loop for i from 0 to 10 collect (list "*" 0 i))
   "1star" (loop for i from 0 to 10 collect (list "*" 1 i))
   "2star" (loop for i from 0 to 10 collect (list "*" 2 i))
   "3star" (loop for i from 0 to 10 collect (list "*" 3 i))
   "4star" (loop for i from 0 to 10 collect (list "*" 4 i))
   "5star" (loop for i from 0 to 10 collect (list "*" 5 i))
   "6star" (loop for i from 0 to 10 collect (list "*" 6 i))
   "7star" (loop for i from 0 to 10 collect (list "*" 7 i))
   "8star" (loop for i from 0 to 10 collect (list "*" 8 i))
   "9star" (loop for i from 0 to 10 collect (list "*" 9 i))
   "10star" (loop for i from 0 to 10 collect (list "*" 10 i))
   "slash1" (collect-by-divisor 1)
   "slash2" ( collect-by-divisor 2)
   "slash3" ( collect-by-divisor 3)
   "slash4" ( collect-by-divisor 4)
   "slash5" ( collect-by-divisor 5)
   "slash6" ( collect-by-divisor 6)
   "slash7" ( collect-by-divisor 7)
   "slash8" ( collect-by-divisor 8)
   "slash9" ( collect-by-divisor 9)
   "slash10" ( collect-by-divisor 10))
  

;; Removed because 'use strict' won't allow 'with' which ps generates
;; for many loops

;;teendiv
;; (loop for u from 8 downto 1 
;;                           append (loop for div from (1+ u) to 9 
;;                                 collect (list (+ 10 u) (- div))))

;;digitdiv
;; (loop for d1 from 9 downto 2
;;                            append (loop for d2 from (1- d1) downto 1
;;                                      collect (list d1 (- d2))))
  
  
  switch-test
  (with-slots (train test) switch
    (lambda (ev)
      (cond ((eql (id-of this) "set-test")
             (let ((cbxs (get-elements-by-tag-name 
                          (doc-get-el-by-id "checkbox-holder") "input"))
                   (mul (funcall mul-won)))
               (setf train false
                     test true)
               (funcall toggle-checkbox-visibility)
               (dolist (cbx cbxs)
                 (setf (checked cbx) false))
               (cond ((string= game-name "Multiplication")
                      (setf current-questions (ps:array))
                      (push (jsn mul-pairs) current-questions))
                     ((or mul (string= game-name "Division"))
                      (setf current-questions (array))
                      (push (jsn div-pairs) current-questions))
                     (t 
                      (setf current-questions (array))
                      (push (jsn mul-pairs) current-questions)))))
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
           grace 0
           no-errors true)))

  game-record
  (ps:create 
   bad-answers (ps:array)
   grace 0
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
    (cond ((string= gname "Multiplication")
           mul-score)
          ((string= gname "Division")
           div-score)
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
                    (and (funcall mul-won)
                         (funcall div-won)))
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
  
  mul-won
  (lambda ()
    (with-slots (mul-score) content-object
      (with-slots (best-time improvements) mul-score
        (if best-time true false))))

  div-won
  (lambda ()
    (with-slots (div-score) content-object
      (with-slots (best-time improvements) div-score
        (if best-time true false))))

  mul-score 
  (ps:create best-time false
             improvements false
             optotal false)

  div-score 
  (ps:create best-time false
             improvements false
             optotal false)
  
  get-muldiv-scores 
  (lambda ()
    (with-slots (mul-pairs div-pairs) training-pairs
      (labels ((callback (response)
                 (let* ((parr (parse-json response))
                        (ass (aref parr 0))
                        (bag1 (aref parr 1))
                        (bag2 (aref parr 2))
                        (mulsc (aref ass 0))
                        (divsc (aref ass 1)))
                   (setf mul-pairs bag1
                         div-pairs bag2)
                   ;; (if (funcall mul-won) ;;HERE
                   ;;     (push (jsn div-pairs) current-questions)
                   ;;     (push (jsn mul-pairs) current-questions))
                   (funcall set-local-mul-score mulsc)
                   (funcall set-local-div-score divsc)
                   (if (funcall mul-won) ;;HERE
                       (push (jsn div-pairs) current-questions)
                       (push (jsn mul-pairs) current-questions))
                   (funcall insert-game-holder)
                   (funcall insert-trainer-checkboxes))))
        (ajax_get_muldiv_scores owner callback))))
  
  voluntary-game
  (lambda (mulsc divsc)
    (with-slots (mul-pairs div-pairs) training-pairs
      (cond ((eql game-name "Division")
             (funcall set-local-div-score divsc)
             (funcall set-local-mul-score mulsc)
             (push (jsn div-pairs) current-questions))
            ((eql game-name "Multiplication")
             (funcall set-local-mul-score mulsc)
             (funcall set-local-div-score divsc)
             (push (jsn mul-pairs) current-questions))
            (t (alert "Voluntary game error")))      
      (funcall insert-game-holder)
      (funcall insert-trainer-checkboxes)))

  
  set-local-mul-score
  (lambda (score)
    (with-slots (mul-score) content-object
      (with-slots (best-time improvements optotal) mul-score
        (set-score-code score))))
  
  set-local-div-score
  (lambda (score)
    (with-slots (div-score) content-object
      (with-slots (best-time improvements optotal) div-score
        (set-score-code score))))
    
  intro-text
  (ps:lisp 
   (who:with-html-output-to-string (str)
     
     (:ul (:li (:strong "Click start.") "  A problem will appear.")
          (:li (:strong "Write the answer") ".  Do not press enter.")
          (:li "If you make a mistake, "(:strong "correct
          your answer") " to continue.")
          (:li "When you have done the problems, the ones you
     got wrong will " (:strong "be repeated.")))
     
     (:p "You can practice one or more tables at a time by
     selecting " (:q "practice"))
     (:p "Your first goal is to get all the problems right, with no
     mistakes: First with multiplication, then division.")
     (:p "Although the goal is no mistakes, if you
     make " (:strong "fewer") " than 6 mistakes, and can correct them
     all without extra mistakes after the game, you will have
     succeeded and can proceed.")
     (:p "When your first goal has been achieved, you can play the
     games again by pressing the " (:q "games") " button on your
     page.")
     (:p "If you play them again, a timer will be displayed.  Your
     goal can then be to improve your times.")
     (:p "First concentrate on getting them all right.")))
  
  start 
  (lambda (ev)
    (let ((revmenu (doc-get-el-by-id "review-menu")))
      (set-display revmenu "none")
      (if voluntary
          (funcall voluntary-game)
          (funcall get-muldiv-scores))))
  
  create-blackboard ;; Slot
  (lambda ()
    (let ((bb (div-id "intro-text-holder"))
          (txd (div-id "intro-text-display"))
          (bdiv (div-id "intro-button-holder")))
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
      (add-class bb "shift-right")
      (append-child (dgebid "intro-text-display") butholder)))

  intro 
  (lambda () 
    (let ((bb (dgebid "intro-text-holder"))
          (bdiv (dgebid "intro-button-holder"))
          (but (make-button "end-intro" "Continue"))
          (h1 (create-element "h1")))
      (remove-class bb "shift-right")
      (insert-before bb h1 (dgebid "intro-text-display"))
      (append-child bdiv but)
      (setf (text-content-of h1) "How to play")
      (add-event-no-capture but "click" start)
      (set-inner-html (doc-get-el-by-id "intro-text-display") intro-text)))

  ;; start 
  ;; (lambda (ev)
  ;;   (if voluntary
  ;;       (funcall voluntary-game)
  ;;       (funcall get-muldiv-scores)))

    ;; create-blackboard ;; Slot
  ;; (lambda ()
  ;;   (let ((bb (div-id "intro-text-holder"))
  ;;         (h1 (create-element "h1"))
  ;;         ;;(htext "Introduction")
  ;;         (txd (div-id "intro-text-display"))
  ;;         (bdiv (div-id "intro-button-holder"))
  ;;         (but (make-button "end-intro" "Continue")))
  ;;     (setf (text-content-of h1) "How to play")
  ;;     (append-child bb h1)
  ;;     (append-child bb txd)
  ;;     (append-child bdiv but)
  ;;     (append-child bb bdiv)
  ;;     bb))
  
  ;; init-script ;; Slot
  ;; (lambda ()
  ;;   (funcall intro))
  
  ;; intro 
  ;; (lambda () 
  ;;   (let ((bb (funcall create-blackboard)))
  ;;     (append-child (dots document body) bb)
  ;;     (set-display bb "inline")
  ;;     ;;(funcall (dots drag-drop init-element) bb)
  ;;     (add-event-no-capture (doc-get-el-by-id "end-intro") "click" start)
  ;;     (set-inner-html (doc-get-el-by-id "intro-text-display") intro-text))))

  )
;; END OF SPECIAL CONTENT

(defun init-mod-table-lesson-slots ()
  (ps:ps 
    (defun init-mod-table-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (insert-product-builder
                     make-product-builder-layout create-prodb-cell
                     prodb-loop pb-styles product-cell-mouseover
                     product-cell-mouseout product-cell 
                     mtab display-product
                     display-quotients 
                     
             
                     ) ephemeral
          (with-slots (remove-default-rl-handlers 
                       restore-default-rl-handlers
                       garbage record-garbage remove-garbage
                       current-lesson-variables label-mark 
                       next-lesson-text previous-lesson-text 
                       reset-var use-var init-var ajax-reset-var
                       teach add-or-remove-wrong-click-nl-handlers
                       numberline-id wrong-click-nl random-int
                       from-direction current-module-name) non-ephemeral
            (setf
             
             pb-styles (ps:create
                        zero "stroke:red;
                             stroke-width:0.125%;"
                        multiplicand "stroke:green;
                                     stroke-width:0.125%;"
                        multiplier "stroke:blue;
                                   stroke-width:0.125%;"
                        product-off "stroke:#CDCDCD;"
                        product-on "stroke:black;
                                    stroke-width:0.125%")
             
             product-cell
             (lambda (tspn)
               (parent-node (parent-node tspn)))

             display-product
             (lambda  (x y p)
               (let* ((lprod (dgebid "l-product-display"))
                      (inph (dgebid "l-but-holder"))
                      (f1 (dgebid "factor1"))
                      (f2 (dgebid "factor2"))
                      (pd (strcat "$" x " \\cdot " y "=" p "$")))
                 (remove-children-from-nth lprod 0)
                 (append-para-text lprod pd true)))

             display-quotients
             (lambda (x y p)
               (let* ((quot1 (dgebid "quot1-display"))
                      (quot2 (dgebid "quot2-display"))
                      (sq1) (sq2))
                 (cond ((and (zerop x)
                             (zerop y))
                        (setf sq1 "No quotient!"
                              sq2 sq1))
                       ((zerop y)
                        (setf sq1 (strcat "$0 \\div " x "=0$")
                              sq2 "No quotient!"))
                       ((zerop x)
                        (setf sq2 (strcat "$0 \\div " y "=0$")
                              sq1 "No quotient!"))
                       (t
                        (setf sq1 (strcat "$" p " \\div " x "=" y "$")
                              sq2 (strcat "$" p " \\div " y "=" x "$"))))
                 (remove-children-from-nth quot1 0)
                 (remove-children-from-nth quot2 0)
                 (append-para-text quot1 sq1 true)
                 (append-para-text quot2 sq2 true)))
                       
             mtab
             (ps:create

              rectangle false
             
              initialize
              (lambda ()
                (with-mtab
                    (setf rectangle (ps:create
                                     x false
                                     y false
                                     r false
                                     style "fill: #0000ff;
                                            stroke: #0000ff; 
                                            stroke-width: 1; 
                                            stroke-opacity: 0.3;
                                            fill-opacity: 0.3;")
                          released true)))

              draw-rectangle
              (lambda (x y)
                (with-mtab
                    (setf (rect-value x) x
                          (rect-value y) y)
                    (let* ((r (svg-create-element "rect"))
                           (topl-name (cell-id 1 (rect-value y)))
                           (topl (dgebid topl-name))
                           (cellr (aref (gebtag topl "rect") 0))
                           (toplx (get-attribute cellr "x"))
                           (toply (get-attribute cellr "y"))
                           (toplw (parse-float (get-attribute cellr "width")))
                           (toplh (parse-float (get-attribute cellr "height")))
                           (rw (strcat (* toplw (rect-value x)) "em"))
                           (rh (strcat (* toplh (rect-value y)) "em"))
                           (canvas (dgebid "pb-canvas")))                          
                      (setf (rect-value r) r)
                      (set-attributes r
                                      "pointer-events" "none"
                                      "x" toplx
                                      "y" toply
                                      "width" rw
                                      "height" rh
                                      "style" (rect-value style))
                      
                      
                      (append-child canvas r))))
                                         
              free-rectangle
              (lambda ()
                (with-mtab
                    (remove-child (dgebid "pb-canvas") (rect-value r))
                  (setf (rect-value r) false
                        (rect-value x) false
                        (rect-value y) false)))

              released true
                           
              product-click-handler
              (lambda (ev)
                (with-mtab
                    (if released
                        (funcall handler-grab this)
                        (funcall handler-release this))))
              
              handler-grab
              (lambda (me)
                (let* ((txt (parent-node me))
                       (cell (parent-node txt))
                       (coords (cell-coords (id-of cell)))
                       (x (parse-int (cell-x coords)))
                       (y (parse-int (cell-y coords)))
                       (p (* x y)))
                  (set-attribute txt "data-selected" "true"))
                (with-mtab 
                    (setf released false)
                  (funcall draw-rectangle x y))
                (funcall display-product x y p)
                (funcall display-quotients x y p))
              
              handler-release
              (lambda (me)
                (let* ((txt (parent-node me))
                       (data (get-attribute txt "data-selected"))
                       (selected (string= data "true")))
                  (when selected
                      (with-mtab
                          (funcall free-rectangle) 
                        (setf released true)
                        (set-attribute txt "data-selected" "false")
                        (funcall clear-pqs)))))

              clear-pqs
              (lambda ()
                (let ((lprod (dgebid "l-product-display"))
                      (tquot (dgebid "quot1-display"))
                      (bquot (dgebid "quot2-display")))
                  (remove-children-from-nth lprod 0)
                  (remove-children-from-nth tquot 0)
                  (remove-children-from-nth bquot 0)))

              swap-factors
              (lambda (ev)
                (labels ((click-thing (thing on mouseover)
                           (let ((event (doc-create-event "MouseEvents"))
                                 (text (parent-node thing)))
                             (init-event event "click" false false)
                             (when thing 
                               (dispatch-event thing event)
                               (when mouseover
                                 (if on (funcall product-cell-mouseover false text)
                                     (funcall product-cell-mouseout false text))))
                               ))
                         (get-clickable (x y)
                           (let* ((cn (cell-id x y))
                                  (cell (dgebid cn))
                                  (tsp (aref (gebtag cell "tspan") 0)))
                                 tsp)))
                  (with-mtab
                      (let* ((x (rect-value x))
                             (y (rect-value y))
                             (current (funcall get-clickable x y))
                             (next (funcall get-clickable y x)))
                        (funcall click-thing current false (if (or (zerop x) (zerop y)) false true))
                        (funcall click-thing next true (if (or (zerop x) (zerop y)) false true))))))
                        
                           
              );;end mtab
             
             product-cell-mouseover
             (lambda (ev el)
               (with-pb-styles
                   (with-mtab (if ev (when released
                                       (set-attribute this "style" product-on))
                                  (progn
                                    (set-attribute el "style" product-on)
                                    (setf released false))))))

             product-cell-mouseout
             (lambda (ev el)
               (with-pb-styles 
                   (with-mtab (when released
                                (set-attribute (if ev this el) "style" product-off)))))

             create-prodb-cell
             (define-anonymous-with-cell-variables 
                 (set-attributes g "id" (cell-id xv yv nil)
                                 "class" "scratch-pad-cell"
                                 "partial-answer" "false")
               (set-attributes rect "x" xem "y" yem "width" skipem
                               "height" skipem "fill" "none"
                               "pointer-events" "all"
                               "stroke" (if (or (zerop xv)
                                                (zerop yv)) 
                                            "none"
                                            "#526565") "stroke-opacity" "0.3")
               (append-child g rect)
               (append-child svg g)
               (let* ((cnt  (cond ((and (zerop xv)
                                        (zerop yv)) "0")
                                  ((zerop xv) (strcat "" yv))
                                  ((zerop yv) (strcat "" xv))
                                  (t (strcat "" (* xv yv)))))
                      (len (length cnt))
                      (cellcnt (make-text-node cnt))
                      (svgtxt (svg-create-element "text"))
                      (txtspn (svg-create-element "tspan")))                      
                 (add-event-no-capture txtspn "click" (with-mtab
                                                          product-click-handler))
                 (append-child txtspn cellcnt)
                 (append-child svgtxt txtspn)
                 (set-attributes svgtxt "x" xem "y" yem)
                 (cond ((= len 1)
                        (set-attributes txtspn "dx" "0.4em" "dy" "1.25em" 
                                        "style" "font-size:90%;" ))
                       ((= len 2)
                        (set-attributes txtspn "dx" "0.3em" "dy" "1.25em" 
                                        "style" "font-size:80%;" ))
                       (t (set-attributes txtspn "dx" "0.15em" "dy" "1.40em" 
                                        "style" "font-size:65%;" )))
                 (with-pb-styles
                     (cond ((and (zerop xv)
                                 (zerop yv)) 
                            (set-attributes svgtxt "style" zero))
                           ((zerop xv)
                            (set-attributes svgtxt "style" multiplier))
                           ((zerop yv)
                            (set-attributes svgtxt "style" multiplicand))
                           (t (set-attributes svgtxt "style" product-off)
                              (add-event-no-capture 
                               svgtxt "mouseover" product-cell-mouseover)
                              (add-event-no-capture 
                               svgtxt "mouseout" product-cell-mouseout))))
                 (append-child g svgtxt)                      
                 cellcnt))
             
             prodb-loop
             (lambda (startx starty startyv)
               (lambda (height width columns skip rows skipem svg content-fn id)
                 (loop for y from starty below height by skip
                    for yv from startyv downto 0 by 1
                    do (loop for x from startx to width by skip
                          for xv from startx to columns by 1
                          do (funcall content-fn x y xv yv skipem svg id)))))

                                
             insert-product-builder 
             (lambda ()
               (let ((ltd (dgebid "lesson-text-display"))
                     (pb (funcall make-product-builder-layout))
                     (lfn (funcall prodb-loop 0 1 10)))
                 (with-mtab (funcall initialize))
                 (remove-children-from-nth ltd 0)
                 (append-child ltd pb)
                 (let ((pgrid (custom-svg-grid 
                               "pb-canvas-holder" 
                               "pb-canvas" 
                               12 11
                               create-prodb-cell lfn)))
                   pgrid)))
             
             make-product-builder-layout
             (lambda ()
               (let ((pbuilder-holder (div-id "pbuilder-holder"))
                     (top (div-id "pb-top"))
                     (bottom (div-id "pb-bottom"))
                     (canvas-holder (div-id "pb-canvas-holder"))
                     (info-holder (div-id "pb-info-holder"))
                     (left-info (div-id-class "pb-left-info" "tutor-left"))
                     (right-info (div-id-class  "pb-right-info" 
                                                "tutor-right long-right"))
                     (prod-disp-holder (div-id-class "l-prod-disp-holder" 
                                                     "padded-items"))
                     (product-display (div-id-class "l-product-display" "pb-info"))
                     (but-holder (div-id-class "l-but-holder" "padded-items"))
                     (topq-holder (div-id-class "r-topq-holder" "padded-items"))
                     (qh (make-header "Quotients"))
                     (ph (make-header "Product"))
                     (cdot (make-text-node "$ \\cdot $"))
                     (bottq-holder (div-id-class "r-bottq-holder" "padded-items"))
                     (quot1-display (div-id-class "quot1-display" "pb-info"))
                     (quot2-display (div-id-class "quot2-display" "pb-info"))
                     (but (make-button "swap-factors" "swap"
                                       ("click" (with-mtab swap-factors)))))
                 (add-to-class-list but "pointer-things")
                 (append-children pbuilder-holder top bottom)
                 (append-child top canvas-holder)
                 (append-children bottom left-info right-info)
                 (append-child right-info qh)
                 (append-child left-info ph)
                 (append-children right-info topq-holder bottq-holder)
                 (append-child topq-holder quot1-display)
                 (append-child bottq-holder quot2-display)
                 (append-children left-info prod-disp-holder but-holder)
                 (append-child prod-disp-holder product-display)
                 (append-child but-holder but)
                 pbuilder-holder))
             
             )))
        ephemeral
        ))
    (init-mod-table-lesson-slots)
    ))

(define-lesson mod-table
     
    
    :stepj ( (:p "Along with the paired operations
    of " (:strong "addition") " and
    subtraction, " (:strong "multiplication") " and division are the
    four operations of basic mathematics.")
             
             (:table :border "1" 
                     (:tr (:th "Basic operation") 
                          (:th :colspan "2" (:q "Reversing the operation")))
                     (:tr (:td "$2+3=5$") (:td "$5-3=2$") (:td "$5-2=3$"))
                     (:tr (:td "$7\\cdot 3=21$") (:td "$21\\div 3=7$")
                          (:td "$21\\div 7=3$")))
             (:p "As you can see, we use the dot " (:q "$\\cdot$") "
             for multiplication.  Another sign that is often used
             is " (:q "$\\times$") ".  When we say the expression
             aloud we say " (:q "seven times three.") " You may also
             hear people say " (:q "seven by three") ".  This way of
             speaking is used in carpentry measurements of wood.")

             (:p "When we divide we say " (:q "twenty one divided by
             seven") " or sometimes " (:q "seven into twenty one") ".")

             (:p "On the next page are some examples of situations
    where multiplication can be used in " (:q "real-life") ". After
    that, on the following pages, the lesson explains why these
    examples were chosen."))

    :stepj ((:ol (:li "Someone earns 6 dollars a day.  How much will the
    person earn if they work for 5 days? $6\\cdot 5=30$, so in 5 days
    they earn 30 dollars.")
                (:li "A crate holds 85 bananas.  How many bananas are
                there if there are 3 crates? $85\\cdot 3=255$, so
                there are 255 bananas.")
                (:li "How much of a square field has been mown if a
                square measuring a half by a half of the field is
                mown?"))
           (:p "Actually, that last example is unfair, since you have
           not learned about fractions yet, and " (:q "a half") " is a
           fraction.  However there is a good reason for including it
           ..."))

    :stepj ((:p "The traditional way of " (:q "explaining") " what
            multiplication is, is to say that it is " (:q "repeated
            addition") ". In example 1, on the previous page, it
            is possible to say that since the person earns 6 dollars a
            day and works for 5 days, he must have $6+6+6+6+6$ dollars
            after 5 days.  This way of thinking concludes that this
            means multiplication " (:q "is") " repeated addition.")

            (:p "The reason example 3 is included here, is so
           that you realise that multiplication is " (:strong "not") "
           just repeated addition!  While the idea of multiplication
           as repeated addition works fine with positive, whole
           numbers, it is not a good characterisation of
           multiplication of other types of numbers, such as fractions
           or negative numbers.")

            (:p "The last example will be explained properly in a
           later lesson, after you learn about fractions.  The answer
           is a quarter of the field. In this lesson, we will only be
           looking at multiplication of positive digits."))

    :step ((:p "What " (:strong "is") " multiplication then, if it is
    not repeated addition?") (:p "Multiplication is defined
    mathematically by a multiplication table and rules for how
    multiplication works with different types of
    numbers.") (:p "Multiplication can be used in real life situations
    which " (:q "obey") " or correspond to the rules for
    multiplication.") (:p "Learning multiplication 
    means learning the multiplication table and then learning to use
    the rules correctly for the different types of
    numbers.") (:p "Learning when to use multiplication in real life
    means learning to recognise which situations allow you to use the
    rules."))

    :step ((:h1 "Words for mathematical objects")
           (:p "Just as we can refer to
    an " (:span :style "color:red" "addition") " as
    a " (:span :style "color:red"
               (:q "sum")) ", we can refer to
    a " (:span :style "color:blue" "multiplication") " as
    a " (:span :style "color:blue" (:q "product")) ".")
           (:p "Likewise, we can refer to
    a " (:span :style "color:red" "subtraction") " as
    a " (:span :style "color:red" (:q "difference")) " and
    a " (:span :style "color:blue" "division") " as
    a " (:span :style "color:blue" (:q "quotient")) ".  These names
    will be used from now on, so read them carefully.") (:p "Also, the
    numbers which are multiplied together in a product are
    called " (:span :style "color:blue" "factors") "."))

    :step ((:p "A good way to visualize a product of two positive
    numbers is to see it as the construction of a rectangle which has
    sides with lengths equal to the numbers being
    multiplied.") (:p "In the next page, you can try this for
    yourself.") (:ul (:li "Click on a number in the multiplication
    table to show the rectangle which can represent the product. The
    selected number will be emphasised.") (:li "You can remove the
    product by " (:strong "clicking the selected number
    again") ".") (:li " Notice which numbers appear as the factors of
    the product.  Why do these numbers appear?") (:li "Swap the
    factors by pressing " (:q "swap") ". What happens?") (:li "What
    happens if you click a number off the table?") (:li "What happens
    if you click " (:q "0") "?") (:li "The numbers on the diagonal
    from 0 to 100 are called " (:q "square numbers") ".  Why do you
    think they are called this?")))

    :display (funcall insert-product-builder)

    :step ((:p "Experimenting with the table on the previous page has
    hopefully illustrated some of the rules of
    multiplication.") (:p "When you swap the factors, the product
    remains the same.") (:p "Any digit multiplied by zero is zero, and
    no digit can be divided by zero.") (:p "Any digit multiplied by 1
    is just that digit.")  (:p "You can go back and experiment again
    to test these claims.") (:p "There are two more important rules
    for multiplication: one of them shows how longer numbers than
    digits can be multiplied. The other is that if you have more than
    2 numbers to multiply, you can first multiply any two of
    them.") (:p "We will come back to the first of these rules in a
    later lesson.") (:p "Your job now is to learn the multiplication
    table, and there is a memory game to help you achieve this."))

    :step ((:h1 "Summary") 
              (:ul 
                   (:li "The result of a multiplication is called
          a " (:strong "product") ". The numbers which are multiplied
          in a product are called " (:strong "factors") ".")
                   (:li "When multiplying two numbers, either number
                can be regarded as the multiplier; Order does not
                matter.")
                   (:li "The product of a digit and 0 is 0.")
                   (:li "The product of a digit and 1 is the digit.")
                   (:li "The product of a digit and 10 is written as that
                   digit and a zero.")
                   (:li "If you have more than 2 numbers to multiply, you
    can first multiply any 2 of them together.") 
                   (:li "With division, order matters, and you must
    divide the first number " (:strong "by") " the second number.")
                   (:li "Division is a kind of " (:emph "reverse") " of
                     multiplication.")
                   (:li "The result of a division is called a " (:strong "quotient") ".")
                   (:li "We can visualize the multiplication of two positive
         digits as a rectangle with sides of length equal to the
         digits."))
              
              (:p "Press the right arrow to end this lesson."))
    :complete? t)
   
