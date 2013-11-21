(in-package :autotutor)

(defun scratchpad-script ()

  (ps:ps 

   (defun initialize-scratchpad ()
     
     (with-slots-declare-input-menu-identifiers
         ( initialized
           sp-rtl-arr sp-ltr-arr problem-stack set-problem-display
           input-menu-icon-pixels make-menu-row make-menu-entry-item               
           partial-answer input-menu-event-handlers-set make-basic-sp-menu
           
           basic-scratchpad-input-menu ;;rem in time
           
           make-sp-menu ;; make-appropriate-sp-menu 
           build-menu-for-menu-level
           scratchpad-menu-building-blocks scratchpad-menu
           
           input-direction math-symbols 
           swaph-input-direction  
           swapv-input-direction 
           insert-this-string scratchpad-menu-help scratchpad-keys-help
           insert-decimal-point-in-selected-cell frac-in-frac-msg
           custom-input-menus alpha-content numer-content
           insert-parent-string
           display-help1 display-help2 insert-vline-in-selected-cell
           insert-hline-in-selected-cell 
           insert-fraction-cell-in-selected-cell  
           insert-power-cell-in-selected-cell
           mark-selected-cell-as-partial-answer
           delete-content-of-selected-cell
           delete-content-of-selected-cell-stay
           move-to-adjacent-cell move-to-vertical-neighbour
           multi-char-input-mode remove-selected-cell-from-partial-answer
           send-marked-answer click-on-selected-cell 
           gather-partial-answer
           set-graphics-for-cell add-event-handler-for-cell 
           add-keyevent-handlers-for-scratchpad click-on-id
           scratch-pad-keypress scratch-pad-keydown autofocus spm-vline 
           spm-hline user-clear-svg-grid user-dismiss-scratchpad 
           clear-svg-grid get-bottom-cell get-top-cell 
           nested-or-next-sibling click-next-cell-if-there
           click-vertical-neighbour-if-there
           place-string-in-selected-cell get-input-character-number 
           place-single-digit-string confirm-action-stack
           clear-messages action-nokayed flush-stack action-okayed
           communicate revert-to-single-character-input
           place-multi-character-string dxshift dyshift font-size 
           cell-contains-fraction cell-contains-power 
           click-on-fraction-cell click-on-power-cell 
           delete-string-from-selected-cell destroy-svg-grid
           summon-msg summon-scratch-pad
           ) scratchpad
             
             (setf 
              initialized true ;; Var slot
              input-menu-icon-pixels 16 ;; Var slot
              selected-cell null ;; Var slot
              partial-answer (ps:array) ;; Var slot
              input-menu-event-handlers-set false ;; Var slot
              confirm-action-stack (ps:array (lambda () (funcall clear-messages)))
              sp-ltr-arr (input-menu-arrow "sp-ltr-arr" 0)
              sp-rtl-arr (input-menu-arrow "sp-rtl-arr" 180)

              ;;Remove ? from here
              numer-content ;; Var slot
              (list 
               (list "0" "1" "2" "3" "4")
               (list "5" "6" "7" "8" "9")
               (list "+" "-" "*" ":" "exit"))
              
              alpha-content ;; Var slot
              (list 
               (list "a" "b" "c" "d" "e" "f" "g" "h" "i")
               (list "j" "k" "l" "m" "n" "o" "p" "q" "r")
               (list "s" "t" "u" "v" "w" "x" "y" "z" "exit"))

              custom-input-menus ;; Var Slot
              (ps:new (-object))
              ;; to here ?

              
              ;;selectedh and selectedv are initialized on page load
              input-direction ;; Var slot
              (ps:create left-to-right 0 right-to-left 1 down-to-up 2 up-to-down 3
                         selectedv 2 selectedh 0)
              frac-in-frac-msg ;; Var slot
              (ps:lisp 
               (format nil "You have attempted to insert a fraction or power into a fraction numerator or denominator.~%It is mathematically permissable to do this, but a technical limitation of the scratchpad unfortunately prevents it.~%You could continue your calculation on paper, or insert your fractions or powers around a ratio sign (:)."))
              scratchpad-menu-help ;; Var Slot
              (ps:lisp
               (who:with-html-output-to-string (str)
                 (:html 
                  (:h1 "Menu Help")
                  (:p "Clicking on a square selects a square for input.")
                  (:ul (:li "Grey: " (:q "1D") " selects how many
                  symbols may appear in a square.  " (:q "Clear") "
                  wipes entire scratchpad.")
                           (:li "Green: Write that symbol")
                           (:li "Blue: Movement. Arrows select direction,
                           Key under arrow moves in that
                           direction. " (:q "Nxt") " selects next
                           problem.")
                           (:li "Pink: Delete (with or without movement) or quit.")
                           (:li "Red: Underline selects answer. Enter
                           prepares to send it, Confirm sends it."))
                  (:p (:q "Quit") ", " (:q "Enter") "
                  and " (:q "Clear") " require you to
                  press " (:q "Confirm") ".")
                  (:p "Press help key again for shortcut keys help"))))

              scratchpad-keys-help
              (ps:lisp
               (who:with-html-output-to-string (str)
                 (:html
                  (:h1 "Shortcut Keys Help")
                  (:ul (:li "digits and " (:q "+") " and " (:q "-") "
                  insert into the selected square on the scratchpad.")
                       (:li "Arrow keys select direction and move.")
                       (:li (:q "_") " (underline symbol) selects
                       answer by underlining it.")
                       (:li (:q "delete") " deletes without
                       moving." (:q "backspace") " deletes while
                       moving.")
                       (:li (:q "t") " confirms action.")
                       (:li (:q "Enter") " prepares to send answer.")
                       (:li (:q "c") " prepares to clear scratchpad.")
                       (:li (:q "d") " selects number of symbols to
                       enter in square.")))))

              ;;FIXME remove? be careful its used further on
              math-symbols ;; Var slot
              (ps:create decimal "." times "*")
              ;;FIXME macro
              
              ;; cell-id ;; Slot
              ;; (lambda (x y name)
              ;;   (if name (strcat name "(" x "," y ")") (strcat "cell(" x "," y ")")))

              swaph-input-direction ;;Slot
              (lambda ()
                (funcall action-nokayed)
                (with-slots (selectedh left-to-right right-to-left) input-direction
                  (if (eql selectedh left-to-right)
                      (let ((menuitem (doc-get-el-by-id "spm-swph")))
                        (setf selectedh right-to-left)
                        (replace-child menuitem sp-rtl-arr ;right-to-left-arrow 
                                       (first-child menuitem)))
                      (let ((menuitem (doc-get-el-by-id "spm-swph")))
                        (setf selectedh left-to-right)
                        (replace-child menuitem sp-ltr-arr ;left-to-right-arrow 
                                       (first-child menuitem))))))
              
              swapv-input-direction ;; Slot
              (lambda ()
                (funcall action-nokayed)
                (with-slots (selectedv down-to-up up-to-down) input-direction
                  (if (eql selectedv down-to-up)
                      (let ((menuitem (doc-get-el-by-id "spm-swpv")))
                        (setf selectedv up-to-down)
                        (replace-child menuitem up-to-down-arrow 
                                       (first-child menuitem)))
                      (let ((menuitem (doc-get-el-by-id "spm-swpv")))
                        (setf selectedv down-to-up)
                        (replace-child menuitem down-to-up-arrow 
                                       (first-child menuitem))))))
              
              insert-this-string ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (let ((string (subseq (dots this text-content) 0 1)))
                  (funcall place-string-in-selected-cell string)))
              
              insert-parent-string ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (let ((string (subseq (dots this parent-node text-content) 0 1)))
                  (funcall place-string-in-selected-cell string)))
              
              insert-decimal-point-in-selected-cell ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (funcall place-string-in-selected-cell 
                         (dots math-symbols decimal)))
              
              display-help1 ;; Slot
              (lambda (ev)
                (remove-event-no-capture 
                 (doc-get-el-by-id "spm-?") "click" display-help1)
                (add-event-no-capture 
                 (doc-get-el-by-id "spm-?") "click" display-help2)
                (funcall action-nokayed)
                (funcall communicate scratchpad-menu-help))

              display-help2 ;; Slot
              (lambda (ev)
                (remove-event-no-capture 
                 (doc-get-el-by-id "spm-?") "click" display-help2)
                (add-event-no-capture 
                 (doc-get-el-by-id "spm-?") "click" display-help1)
                (funcall action-nokayed)
                (funcall communicate scratchpad-keys-help))

              insert-vline-in-selected-cell ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (when selected-cell
                  (let* ((g (doc-get-el-by-id selected-cell))
                         (rect (first-child g))
                         (xem (get-attribute rect "x"))
                         (yem (get-attribute rect "y"))
                         (y2 (parse-float yem))
                         (h (parse-float (get-attribute rect "height")))
                         (y2em (strcat (+ h y2) "em"))
                         (line (svg-create-element "line")))
                    (set-attributes line 
                                    "x1" xem "y1" yem
                                    "x2" xem "y2" y2em
                                    "stroke" "#000000"
                                    "stroke-width" "0.125em")
                    (append-child g line))))

              insert-hline-in-selected-cell ;;Slot
              (lambda (ev)
                (funcall action-nokayed)
                (when selected-cell
                  (let* ((g (doc-get-el-by-id selected-cell))
                         (rect (first-child g))
                         (xem (get-attribute rect "x"))
                         (yem (get-attribute rect "y"))
                         (x2 (parse-float xem))
                         (w (parse-float (get-attribute rect "width")))
                         (x2em (strcat (+ w x2) "em"))
                         (line (svg-create-element "line")))
                    (set-attributes line 
                                    "x1" xem "y1" yem
                                    "x2" x2em "y2" yem
                                    "stroke" "#000000"
                                    "stroke-width" "0.125em")
                    (append-child g line))
                  (funcall click-next-cell-if-there g)))
              
              
              insert-fraction-cell-in-selected-cell ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (when selected-cell
                  (let ((type (subseq selected-cell 0 3)))
                    (if (or (string= type "num")
                            (string= type "den"))
                        (alert frac-in-frac-msg)
                        (let* ((flineoffset 0.165)
                               (group (doc-get-el-by-id selected-cell))
                               (rect (first-child group))
                               (xem (get-attribute rect "x"))
                               (x (parse-float (subseq xem 0 -2)))
                               (yem (get-attribute rect "y"))
                               (y (parse-float (subseq yem 0 -2)))
                               (wem (get-attribute rect "width"))
                               (width (parse-float (subseq wem 0 -2)))
                               (hem (get-attribute rect "height"))
                               (height (parse-float (subseq hem 0 -2)))
                               (fheight (* height 0.5))
                               (fhem (strcat fheight "em"))
                               (deny (+ y fheight))
                               (denyem (strcat deny "em"))
                               (gnum (svg-create-element "g"))
                               (num (svg-create-element "rect"))
                               (gden (svg-create-element "g"))
                               (den (svg-create-element "rect"))
                               (fline (svg-create-element "line"))
                               (fline-relative-offset (* flineoffset width))
                               (flinex1 (+ x fline-relative-offset))
                               (flinex1em (strcat flinex1 "em"))
                               (flinex2 (- (+ x width) fline-relative-offset))
                               (flinex2em (strcat flinex2 "em")))
                          (set-attributes gnum
                                          "id" (strcat "num:" selected-cell)
                                          "class" "numerator")
                          (set-attributes gden
                                          "id" (strcat "den:" selected-cell)
                                          "class" "denominator")
                          (set-attributes num "x" xem
                                          "y" yem
                                          "width" wem
                                          "height" fhem
                                          "fill" "none"
                                          "pointer-events" "all"
                                          "stroke" "#cdcdcd")
                          (add-event-no-capture num "click" select-or-deselect-cell)
                          (set-attributes den
                                          "x" xem
                                          "y" denyem
                                          "width" wem
                                          "height" fhem
                                          "fill" "none"
                                          "pointer-events" "all"
                                          "stroke" "#cdcdcd")
                          (add-event-no-capture den "click" select-or-deselect-cell)
                          (set-attributes fline
                                          "x1" flinex1em 
                                          "y1" denyem
                                          "x2" flinex2em 
                                          "y2" denyem
                                          "width" "0.0625em"
                                          "stroke" "black")
                          (append-child gnum num)
                          (append-child group gnum)
                          (append-child gden den)
                          (append-child group gden)              
                          (append-child group fline)
                          group)))))
              
              insert-power-cell-in-selected-cell ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (when selected-cell
                  (let ((type (subseq selected-cell 0 3)))
                    (if (or (string= type "num")
                            (string= type "den"))
                        (alert frac-in-frac-msg)
                        (let* ((group (doc-get-el-by-id selected-cell))
                               (rect (first-child group))
                               (xem (get-attribute rect "x"))
                               (x (parse-float xem))
                               (yem (get-attribute rect "y"))
                               (y (parse-float yem))
                               (wem (get-attribute rect "width"))
                               (width (parse-float wem))
                               (hem (get-attribute rect "height"))
                               (height (parse-float hem))
                               (expheight (* height 0.375))
                               (expwidth expheight)
                               (expem (strcat expheight "em"))
                               (expxoffem (strcat (+ x (* height 0.625)) "em"))
                               (expyoffem yem)
                               (byoffem (strcat (+ y (* height 0.375)) "em"))
                               (bxoffem xem)
                               (bhwem (strcat (* height 0.625) "em"))
                               (gexp (svg-create-element "g"))
                               (exp (svg-create-element "rect"))
                               (gbase (svg-create-element "g"))
                               (base (svg-create-element "rect")))
                          (set-attributes gexp
                                          "id" (strcat "exp:" selected-cell)
                                          "class" "exponent power")
                          (set-attributes gbase
                                          "id" (strcat "base:" selected-cell)
                                          "class" "base power")
                          (set-attributes exp "x" expxoffem
                                          "y" expyoffem
                                          "width" expem
                                          "height" expem
                                          "fill" "none"
                                          "pointer-events" "all"
                                          "stroke" "#cdcdcd")
                          (add-event-no-capture exp "click" select-or-deselect-cell)
                          (set-attributes base
                                          "x" bxoffem
                                          "y" byoffem
                                          "width" bhwem
                                          "height" bhwem
                                          "fill" "none"
                                          "pointer-events" "all"
                                          "stroke" "#cdcdcd")
                          (add-event-no-capture base "click" select-or-deselect-cell)
                          (append-child gexp exp)
                          (append-child group gexp)
                          (append-child gbase base)
                          (append-child group gbase)              
                          group)))))
              
              
              mark-selected-cell-as-partial-answer ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (when selected-cell
                  (let* ((group (doc-get-el-by-id selected-cell))
                         (rect (first-child group))
                         (xem (get-attribute rect "x"))
                         (x (parse-float (subseq xem 0 -2)))
                         (yem (get-attribute rect "y"))
                         (y (parse-float (subseq yem 0 -2)))
                         (wem (get-attribute rect "width"))
                         (width (parse-float (subseq wem 0 -2)))
                         (hem (get-attribute rect "height"))
                         (height (parse-float (subseq hem 0 -2)))
                         (newx (+ x width))
                         (newxem (strcat newx "em"))
                         (newy1 (- (+ y height) 0.0625))
                         (newy1em (strcat newy1 "em"))
                         (newy2 (+ newy1 0.125))
                         (newy2em (strcat newy2 "em")))
                    (set-attribute group "partial-answer" "true")
                    (when (every 
                           (lambda (x) (not (string= x selected-cell)))
                           partial-answer)
                      (push selected-cell partial-answer)
                      (let ((topline (svg-create-element "line"))
                            (bottomline1 (svg-create-element "line"))
                            (bottomline2 (svg-create-element "line")))
                        (set-attributes topline
                                        "x1" xem
                                        "y1" yem
                                        "x2" newxem
                                        "y2" yem
                                        "stroke" "black"
                                        "stroke-width" "0.0625em")
                        (set-attributes bottomline1
                                        "x1" xem
                                        "y1" newy1em
                                        "x2" newxem
                                        "y2" newy1em
                                        "stroke" "black"
                                        "stroke-width" "0.0625em")
                        (set-attributes bottomline2
                                        "x1" xem
                                        "y1" newy2em
                                        "x2" newxem
                                        "y2" newy2em
                                        "stroke" "black"
                                        "stroke-width" "0.0625em")
                        (append-child group topline)
                        (append-child group bottomline1)
                        (append-child group bottomline2)                
                        (funcall click-next-cell-if-there group)
                        group)))))

              delete-content-of-selected-cell ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (when selected-cell
                  (let ((cell (doc-get-el-by-id selected-cell)))
                    (setf (dots  (aref (get-elements-by-tag-name cell "rect") 0)
                                 style fill) "")
                    (funcall remove-selected-cell-from-partial-answer)
                    (prune-tree-from-nth cell 1)
                    (funcall click-next-cell-if-there cell))))

              delete-content-of-selected-cell-stay ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (when selected-cell
                  (let ((cell (doc-get-el-by-id selected-cell)))
                    (setf (dots  (aref (get-elements-by-tag-name cell "rect") 0)
                                 style fill) "")
                    (funcall remove-selected-cell-from-partial-answer)
                    (prune-tree-from-nth cell 1))))

              move-to-adjacent-cell ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (when selected-cell
                  (let ((cell (doc-get-el-by-id selected-cell)))
                    (funcall click-next-cell-if-there cell))))

              move-to-vertical-neighbour ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (when selected-cell
                  (funcall click-vertical-neighbour-if-there selected-cell)))

              multi-char-input-mode ;; Slot
              (lambda (ev)
                (funcall action-nokayed)
                (let ((curr (subseq (text-content-of this) 0 3)))
                  (cond ((string= curr "1 D")
                         (setf (text-content-of this) "2 D"))
                        ((string= curr "2 D")
                         (setf (text-content-of this) "3 D"))
                        (t (setf (text-content-of this) "1 D")))))

              remove-selected-cell-from-partial-answer ;; Slot
              (lambda ()
                (labels ((excise (idx)
                           (if (zerop idx) 
                               (setf partial-answer
                                     (subseq partial-answer 1))
                               (setf partial-answer
                                     (append (subseq partial-answer 0 idx)
                                             (subseq partial-answer (1+ idx)))))))
                  (when selected-cell
                    (set-attribute (doc-get-el-by-id selected-cell)
                                   "partial-answer" "false")
                    (when (aref partial-answer 0)
                      (let ((n -1))
                        (dolist (p partial-answer)
                          (incf n)
                          (when (string= p selected-cell)
                            (return (excise n)))))))))

              send-marked-answer ;; Slot
              (with-slots (ps-next ps-pop empty) problem-stack
                (lambda (ev)
                  (let ((answer (funcall gather-partial-answer)))
                    (when (and answer
                               (> (length answer) 0))
                      (funcall ps-pop) ;;rem current
                      (if empty ;;dismiss 
                          (let* ((scratch (doc-get-el-by-id "scratch"))
                                 (aid (id-to 
                                       (get-attribute 
                                        scratch "data-reply-to-id") "A")))
                            (funcall flush-stack)
                            (funcall destroy-svg-grid)
                            (check-sum aid answer))
                          (let* ((scratch (doc-get-el-by-id "scratch"))
                                 (aid (id-to (get-attribute scratch "data-reply-to-id") "A"))
                                 (sel (doc-get-el-by-id selected-cell))
                                 (selrect (if sel
                                              (aref (get-elements-by-tag-name sel "rect") 0)
                                              null))
                                 (action
                                  (lambda ()
                                    (remove-children 
                                     (doc-get-el-by-id "problem-display"))
                                    (remove-attribute scratch "data-reply-to-id")
                                    (funcall clear-svg-grid)
                                    (when selrect 
                                      (set-attribute selrect "fill" "none"))
                                    (setf selected-cell null)
                                    (check-sum aid answer)))
                                 (msg (strcat "`Confirm` to send: " answer)))
                            (push action confirm-action-stack)
                            (funcall communicate msg)))))))
            
  ;; send-marked-answer ;; Slot
              ;; (lambda (ev)
              ;;   (let ((answer (funcall gather-partial-answer)))
              ;;     (when (and answer
              ;;                (> (length answer) 0))
              ;;       (let* ((scratch (doc-get-el-by-id "scratch"))
              ;;              (aid (id-to (get-attribute scratch "data-reply-to-id") "A"))
              ;;              (sel (doc-get-el-by-id selected-cell))
              ;;              (selrect (if sel
              ;;                           (aref (get-elements-by-tag-name sel "rect") 0)
              ;;                           null))
              ;;              (action
              ;;               (lambda ()
              ;;                 (remove-children (doc-get-el-by-id "problem-display")) ;;FIXME here
              ;;                 (remove-attribute scratch "data-reply-to-id") ;;FIXME and here
              ;;                 (funcall clear-svg-grid)
              ;;                 (when selrect (set-attribute selrect "fill" "none"))
              ;;                 (setf selected-cell null)
              ;;                 (check-sum aid answer)))
              ;;              (msg (strcat "`Confirm` to send: " answer)))
              ;;         (push action confirm-action-stack)
              ;;         (funcall communicate msg)))))
              
              click-on-selected-cell ;; Slot
              (lambda (delayed)
                (if delayed
                    (lambda ()
                      (let ((event (doc-create-event "MouseEvents"))
                            (cell (doc-get-el-by-id selected-cell)))
                        (init-event event "click" false false)
                        (dispatch-event cell event)))
                    (let ((event (doc-create-event "MouseEvents"))
                          (cell (doc-get-el-by-id selected-cell)))
                      (init-event event "click" false false)
                      (dispatch-event cell event))))

              gather-partial-answer ;; Slot
              (lambda ()
                (let* ((answer "")
                       (frac-found false)
                       (scratch (doc-get-el-by-id "scratch")))
                  ;;FIXME for powers
                  (loop for cell in (child-nodes-of scratch)
                     for gs = (get-elements-by-tag-name cell "g")
                     for rects = (get-elements-by-tag-name cell "rect")
                     do (when (string= 
                               (get-attribute cell "partial-answer") "true")
                          (let ((fpart (aref gs 0)))
                            (if fpart
                                (cond (frac-found 
                                       (funcall communicate "Error: 2 fractions in answer.")
                                       (return false))
                                      (t (setf frac-found true)
                                         (let ((ofpart (aref gs 1))
                                               (which (subseq (id-of fpart) 0 3)))
                                           (if (string= which "num")
                                               (setf answer (strcat 
                                                             answer " "
                                                             (text-content-of fpart)
                                                             "/"
                                                             (text-content-of ofpart)))
                                               (setf answer (strcat 
                                                             answer " "
                                                             (text-content-of ofpart)
                                                             "/"
                                                             (text-content-of fpart)))))))
                                (let ((rect (aref rects 0)))
                                  (when rect
                                    (cond (frac-found
                                           (funcall communicate "Error: number after fraction")
                                           (return false))
                                          (t (setf answer (strcat 
                                                           answer
                                                           (text-content-of cell)))))))))))
                  answer))

              set-graphics-for-cell ;; Slot
              (lambda (cell)
                (let ((cid (id-of cell)))
                  (cond ((string= cid "spm-vline")
                         (let ((cc (first-child cell)))
                           (replace-child cell (funcall spm-vline "spm-vline-graphic") cc)))
                        ((string= cid "spm-hline")
                         (let ((cc (first-child cell)))
                           (replace-child cell (funcall spm-hline "spm-hline-graphic") cc)))
                        ((string= cid "spm-swph")
                         (let ((cc (first-child cell)))
                           (replace-child cell sp-ltr-arr cc))) ;;left-to-right-arrow cc)))
                        ((string= cid "spm-swpv")
                         (let ((cc (first-child cell)))
                           (replace-child cell down-to-up-arrow cc)))
                        (t false))))

              add-event-handler-for-cell ;; Slot
              (lambda (cell)
                (with-slots (ps-next) problem-stack
                (let ((cid (id-of cell)))
                  (cond ((string= cid "spm-dec")
                         (add-event-no-capture cell "click"
                                               insert-decimal-point-in-selected-cell))
                        ((string= cid "spm-vline")
                         (add-event-no-capture cell "click"
                                               insert-vline-in-selected-cell))
                        ((string= cid "spm-hline")
                         (add-event-no-capture cell "click"
                                               insert-hline-in-selected-cell))
                        ((string= cid "spm-fl")
                         (add-event-no-capture cell "click" 
                                               insert-fraction-cell-in-selected-cell))
                        ((string= cid "spm-alpha")
                         (add-event-no-capture cell "click"
                                               summon-alpha-input))
                        ((string= cid "spm-pow")
                         (add-event-no-capture cell "click" 
                                               insert-power-cell-in-selected-cell))
                        ((string= cid "spm-?")
                         (add-event-no-capture cell "click" display-help1))
                        ((string= cid "spm-eq")
                         (add-event-no-capture cell "click" 
                                               mark-selected-cell-as-partial-answer))
                        ((string= cid "spm-de")
                         (add-event-no-capture cell "click" 
                                               delete-content-of-selected-cell))
                        ((string= cid "spm-dels")
                         (add-event-no-capture cell "click" 
                                               delete-content-of-selected-cell-stay))
                        ((string= cid "spm-swph")
                         (add-event-no-capture cell "click" swaph-input-direction))
                        ((string= cid "spm-swpv")
                         (add-event-no-capture cell "click" swapv-input-direction))
                        ((string= cid "spm-movh")
                         (add-event-no-capture cell "click" move-to-adjacent-cell))
                        ((string= cid "spm-movv")
                         (add-event-no-capture cell "click" move-to-vertical-neighbour))
                        ((string= cid "spm-str")
                         (add-event-no-capture cell "click" multi-char-input-mode))
                        ((string= cid "spm-clr")
                         (add-event-no-capture cell "click" user-clear-svg-grid))
                        ((string= cid "spm-exit")
                         (add-event-no-capture cell "click" user-dismiss-scratchpad))
                        ((string= cid "spm-enter")
                         (add-event-no-capture cell "click" send-marked-answer))
                        ((string= cid "spm-nxt")
                         (add-event-no-capture cell "click" ps-next))
                        ((string= cid "spm-confirm")
                         (add-event-no-capture cell "click" action-okayed))
                        (t (add-event-no-capture cell "click" insert-this-string))))))
              
              add-keyevent-handlers-for-scratchpad ;; Slot
              (lambda ()
                (add-event-no-capture document "keypress" scratch-pad-keypress)
                (add-event-no-capture document "keydown" scratch-pad-keydown))

              click-on-id ;; Slot
              (lambda (id)
                (let ((event (doc-create-event "MouseEvents"))
                      (cell (doc-get-el-by-id id)))
                  (init-event event "click" false false)
                  (when cell (dispatch-event cell event))))

              scratch-pad-keypress ;; Slot
              (lambda (ev)
                (with-slots (which key-code prevent-default char-code ctrl-key) ev
                  ;;(funcall prevent-default)
                  (when (or (not (eql which undefined))
                            (not (= which  0)))
                    (let ((code  which))
                      (switchcc code 
                                48 "spm-0"
                                49 "spm-1"
                                50 "spm-2"
                                51 "spm-3"
                                52 "spm-4"
                                53 "spm-5"
                                54 "spm-6"
                                55 "spm-7"
                                56 "spm-8"
                                57 "spm-9"
                                
                                42 "spm-*"
                                43 "spm-+"
                                45 "spm--"
                                58 "spm-:"
                                61 "spm-="

                                78 "spm-nxt"
                                
                                40 "spm-("
                                41 "spm-)"
                                46 "spm-dec"
                                47 "spm-fl" ;;/ fraction-line
                                112 "spm-pow" ;; p power-cell

                                118 "spm-vline"
                                104 "spm-hline"

                                95 "spm-eq" ;;_ underline
                                13 "spm-enter" ;;return send
                                116 "spm-confirm" ;;t confirm
                                100 "spm-str" ;;d increment chars
                                99 "spm-clr" ;;c clear scratchpad
                                )))))

              scratch-pad-keydown
              (lambda (ev)
                (with-slots (which key-code char-code prevent-default ctrl-key) ev
                  (when (or (not (eql which undefined)) (not (= which 0)))
                    (let ((code which))
                      (switchcc code
                                46 "spm-dels"
                                8 "spm-de"
                                40 (funcall (lambda () (with-slots 
                                                             (selectedv up-to-down)
                                                           input-direction
                                                         (if (= selectedv up-to-down)
                                                             "spm-movv"
                                                             "spm-swpv"))))
                                37 (funcall (lambda () (with-slots 
                                                             (selectedh right-to-left left-to-right)
                                                           input-direction
                                                         (if (= selectedh right-to-left)
                                                             "spm-movh"
                                                             "spm-swph"))))
                                39 (funcall (lambda () (with-slots 
                                                             (selectedh right-to-left left-to-right)
                                                           input-direction
                                                         (if (= selectedh left-to-right)
                                                             "spm-movh"
                                                             "spm-swph"))))
                                38 (funcall (lambda () (with-slots 
                                                             (selectedv down-to-up)
                                                           input-direction
                                                         (if (= selectedv down-to-up)
                                                             "spm-movv"
                                                             "spm-swpv"))))
                                )))))
              
              autofocus ;; Slot
              (lambda ()
                (setf selected-cell "cell(0,0)")
                (let ((rect (aref (get-elements-by-tag-name 
                                   (doc-get-el-by-id selected-cell)
                                   "rect") 0)))
                  (funcall click-on-selected-cell false)
                  (set-attribute rect "fill" "rgba(139, 203, 138, 0.4)")))
              
              spm-vline ;; Slot
              (lambda (id)
                (let* ((skip svg-grid-skip) (skipem (strcat skip "em"))
                       (svg (svg-create-element "svg"))
                       (bar (svg-create-element "line"))
                       (x1 (* skip 0.5)) (y1 0.0625)
                       (x2 x1) (y2 (- skip y1))
                       (x1em (strcat x1 "em")) (x2em x1em)
                       (y1em (strcat y1 "em")) (y2em (strcat y2 "em")))
                  (set-attributes svg "version" "1.2" "baseProfile" "basic"
                                  "id" id "height" skipem "width" skipem)
                  (set-attributes bar "x1" x1em "y1" y1em "x2" x2em "y2" y2em
                                  "stroke" "#000000")
                  (append-child svg bar)
                  svg))

              spm-hline ;; Slot
              (lambda (id)
                (let* ((skip svg-grid-skip) (skipem (strcat skip "em"))
                       (svg (svg-create-element "svg"))
                       (dash (svg-create-element "line"))
                       (y1 (* skip 0.5)) (x1 0.0625)
                       (y2 y1) (x2 (- skip x1))
                       (x1em (strcat x1 "em")) (x2em (strcat x2 "em"))
                       (y1em (strcat y1 "em")) (y2em (strcat y2 "em")))
                  (set-attributes svg "version" "1.2" "baseProfile" "basic"
                                  "id" id "height" skipem "width" skipem)
                  (set-attributes dash "x1" x1em "y1" y1em "x2" x2em "y2" y2em
                                  "stroke" "#000000")
                  (append-child svg dash)
                  svg))
              
              user-clear-svg-grid ;; Slot
              (lambda (ev)
                (funcall flush-stack)
                (push clear-svg-grid confirm-action-stack)
                (funcall communicate "`Confirm` to clear scratchpad"))
              
              user-dismiss-scratchpad ;; Slot
              (lambda (ev)
                (funcall flush-stack)
                (push 
                 (lambda ()
                   (let ((sim (dgebid "select-input-menu")))
                     (remove-children-from-nth sim 0)
                     (setf input-menu-event-handlers-set false)
                     (funcall destroy-svg-grid)))
                   confirm-action-stack)
                (funcall communicate "`Confirm` to dismiss scratchpad"))

              ;; user-dismiss-scratchpad ;; Slot
              ;; (lambda (ev)
              ;;   (funcall flush-stack)
              ;;   (push destroy-svg-grid confirm-action-stack)
              ;;   (funcall communicate "`Confirm` to dismiss scratchpad"))

              clear-svg-grid ;; Slot
              (lambda ()
                (let ((grid (doc-get-el-by-id "scratch")))
                  (dolist (cell (child-nodes-of grid))
                    (when selected-cell
                      (when (string= (id-of cell) selected-cell)
                        (let ((rects (get-elements-by-tag-name cell "rect")))
                          (dolist (rect rects)
                            (set-attribute rect "fill" "none")))))
                    (set-attribute cell "partial-answer" "false")
                    (prune-tree-from-nth cell 1))
                  (funcall autofocus)
                  (setf partial-answer (ps:array))))

              get-bottom-cell ;; Slot 
              (lambda (gs)
                (let ((retval false))
                  (dolist (g gs)
                    (when (or (string= (subseq (id-of g) 0 3) "den")
                              (string= (subseq (id-of g) 0 3) "bas"))
                      (setf retval g)))
                  (if retval retval (next-sibling (doc-get-el-by-id selected-cell)))))
              
              get-top-cell ;; Slot
              (lambda (gs)
                (let ((retval false))
                  (dolist (g gs)
                    (when (or (string= (subseq (id-of g) 0 3) "num")
                              (string= (subseq (id-of g) 0 3) "exp"))
                      (setf retval g)))
                  (if retval retval (next-sibling (doc-get-el-by-id selected-cell)))))
              
              nested-or-next-sibling ;; Slot
              (lambda (cc)
                (let ((gs (get-elements-by-tag-name cc "g")))
                  (with-slots (selectedh left-to-right right-to-left) input-direction
                    (if (aref gs 0) ;;something there
                        ;;do something
                        (cond ((eql selectedh left-to-right)
                               (funcall get-bottom-cell gs))
                              ((eql selectedh right-to-left)
                               (funcall get-top-cell gs))
                              (t (funcall get-bottom-cell gs)))
                        (cond ((eql selectedh left-to-right)
                               (next-sibling cc))
                              ((eql selectedh right-to-left)
                               (previous-sibling cc))
                              (t (next-sibling cc)))))))      

              click-next-cell-if-there ;; Slot
              (lambda (cc)
                (let ((contents (subseq selected-cell 0 3))
                      (event (doc-create-event "MouseEvents")))
                  (init-event event "click" false false)
                  (with-slots (selectedh left-to-right right-to-left) input-direction
                    (let ((next 
                           (cond ((string= contents "cel")                   
                                  (funcall nested-or-next-sibling cc))
                                 ((or (string= contents "den")
                                      (string= contents "bas"))
                                  (cond ((eql selectedh left-to-right)
                                         (previous-sibling cc))
                                        ((eql selectedh right-to-left)
                                         (previous-sibling (parent-node cc)))
                                        (t (previous-sibling cc))))
                                 ((or (string= contents "num")
                                      (string= contents "exp"))
                                  (cond ((eql selectedh left-to-right)
                                         (or (next-sibling (parent-node cc))
                                             (previous-sibling (parent-node cc))))
                                        ((eql selectedh right-to-left)
                                         (next-sibling cc))
                                        (t (next-sibling cc)))))))
                      (when next
                        (dispatch-event (aref (get-elements-by-tag-name next "rect") 0)
                                        event))))))

              click-vertical-neighbour-if-there ;; Slot
              (lambda (curr-id)
                (let* ((arr1 (split curr-id (ps:regex ":")))
                       (fractype (if (= (length arr1) 2) true false))
                       (realid (if fractype (aref arr1 0) curr-id))
                       (arr2 (split (aref (split 
                                           (aref (split realid (ps:regex "\\(")) 1)
                                           (ps:regex "\\)")) 0)
                                    (ps:regex ",")))
                       (col (parse-int (aref arr2 0)))
                       (row (parse-int (aref arr2 1))))
                  (with-slots (selectedv up-to-down down-to-up) input-direction
                    (let ((next
                           (doc-get-el-by-id 
                            (cond ((eql selectedv up-to-down)
                                   (cell-id col (1+ row) nil))
                                  ((eql selectedv down-to-up)
                                   (cell-id col (1- row) nil))
                                  (t (cell-id col (1+ row) nil))))))
                      (when next
                        (let* ((rect (aref (get-elements-by-tag-name next "rect") 0))
                               (event (doc-create-event "MouseEvents")))
                          (init-event event "click" false false)
                          (dispatch-event rect event)))))))
              
              place-string-in-selected-cell
              (lambda (string)
                (when selected-cell
                  (let* ((cell (doc-get-el-by-id selected-cell))
                         (existing (aref 
                                    (get-elements-by-tag-name cell "text") 0))
                         (txtcnt (if existing (text-content-of existing) false))
                         (types (split selected-cell ":"))
                         (frac false) (pow false)
                         (inchars (funcall get-input-character-number)))
                    (cond ((= (length types) 1)
                           false)
                          ((= (length types) 2)
                           (if (or (string= (aref types 0) "num")
                                   (string= (aref types 0) "den"))
                               (setf frac true)
                               (when (or (string= (aref types 0) "base")
                                         (string= (aref types 0) "exp"))
                                 (setf pow true))))
                          (t (setf pow true ;;FIXME if new types change here
                                   frac true)))
                    (cond ((funcall cell-contains-fraction cell)
                           (funcall click-on-fraction-cell cell)
                           (funcall place-string-in-selected-cell string))
                          ((funcall cell-contains-power cell)
                           (funcall click-on-power-cell cell)
                           (funcall place-string-in-selected-cell string))
                          ((= inchars 1)
                           (funcall place-single-digit-string cell types existing string frac pow))
                          (t (funcall place-multi-character-string 
                                      cell existing txtcnt string frac pow))))))

              ;;FIXME make inline macro
              get-input-character-number ;; Slot
              (lambda ()
                (parse-int (subseq (text-content-of (doc-get-el-by-id "spm-str")) 0 1)))

              place-single-digit-string ;; Slot
              (lambda (cell types existing string frac power)
                (when existing (funcall delete-string-from-selected-cell))
                (let* ((style "") (t1 (svg-create-element "text"))
                       (tspan (svg-create-element "tspan"))
                       (txt (make-text-node string))
                       (rects (get-elements-by-tag-name cell "rect"))
                       (els (length rects))
                       (rect (aref rects (1- els)))
                       (exp)(dxem)(dyem)(num)(den)
                       (xem (get-attribute rect "x"))
                       (yem (get-attribute rect "y")))
                  (cond ((and frac power)
                         (if (string= (aref types 1) "exp")
                             (setf exp true)
                             (setf exp false))
                         (if (string= (subseq (id-of cell) 0 3) "num")
                             (setf num true)
                             (setf den true))
                         (setf dxem (cond ((and exp num) "0.2em")
                                          ((and exp den) "0.2em")
                                          (exp "-0.1em")
                                          (t "0.5em")))
                         (setf dyem (cond ((and exp num) "0.3em")
                                          ((and exp den) "0.85em")
                                          (t "0.7em"))))
                        (power 
                         (if (string= (aref types 0) "exp")
                             (setf exp true)
                             (setf exp false))                 
                         (setf dxem (if exp "-0.1em" "0.1em"))
                         (setf dyem "0.85em"))
                        (frac (setf dxem "0.4em")
                              (setf dyem "0.75em"))
                        (t (setf dxem "0.4em")
                           (setf dyem "1.25em")))          
                  (set-attributes tspan "dx" dxem "dy" dyem)
                  (when (string= string (dots math-symbols decimal)
                                 (set-attribute tspan "font-size" "1.1em")))
                  (if (and frac power)
                      (set-attribute tspan "font-size" "0.6em")
                      (when exp (set-attribute tspan "font-size" "0.8em")))
                  (append-child tspan txt)
                  (set-attributes t1 "style" style "x" xem "y" yem) ;;remove style?
                  (append-child cell t1)
                  (append-child t1 tspan)
                  (funcall click-next-cell-if-there cell)
                  t1))

              clear-messages ;; Slot
              (lambda ()
                (let ((msg (doc-get-el-by-id "messages")))
                  (when msg 
                    (let ((nodes (child-nodes-of msg)))
                      (prune-tree-from-nth msg 2))))) ;;FIXME CHECK if SAFE
              
              ;;FIXME inline-macro
              action-nokayed ;; Slot
              (lambda ()
                (if (= (length confirm-action-stack) 1)
                    (funcall (aref confirm-action-stack 0))
                    (let ((discard (pop confirm-action-stack)))
                      (funcall action-nokayed))))

              flush-stack ;; Slot
              (lambda ()
                (setf confirm-action-stack (ps:array clear-messages))
                (funcall clear-messages))
              
              action-okayed ;; Slot
              (lambda (ev)
                (let ((ln (length confirm-action-stack)))
                  (if (= ln 1)
                      (funcall (aref confirm-action-stack 0))
                      (let ((action (pop confirm-action-stack)))
                        (funcall action)
                        (funcall action-okayed)))))

              communicate ;; Slot
              (lambda (message contains-math)
                (let* ((tn (div-class "scratchpad-message"))
                       (msg (doc-get-el-by-id "messages")))
                  (set-inner-html tn message)
                  (append-child msg tn) 
                  (when contains-math
                    (mj-hub-queue (array "Typeset" mj-hub msg)))))

              revert-to-single-character-input ;; Slot
              (lambda ()
                (setf (text-content-of (doc-get-el-by-id "spm-str")) "1 D"))

              place-multi-character-string ;; Slot
              (lambda (cell existing txtcnt string frac power)
                (with-slots (selectedh left-to-right right-to-left) input-direction
                  (let* ((newtxt (if (= selectedh left-to-right)
                                     (strcat (if txtcnt txtcnt "") string)
                                     (strcat string (if txtcnt txtcnt ""))))
                         (newln (length newtxt))
                         (max (funcall get-input-character-number)))
                    (if (> newln max)
                        (funcall communicate (strcat "Text too long: " newtxt) false)
                        (if existing
                            (let ((tspan (aref 
                                          (get-elements-by-tag-name existing "tspan") 0)))
                              (set-attributes tspan "dx" (funcall dxshift max)
                                              "style" (funcall font-size max power))
                              (cond ((= newln max)
                                     (setf (text-content-of tspan) newtxt)
                                     (funcall revert-to-single-character-input)
                                     (funcall click-next-cell-if-there cell))
                                    (t (setf (text-content-of tspan) newtxt))))
                            (let* ((style "") (t1 (svg-create-element "text"))
                                   (tspan (svg-create-element "tspan"))
                                   (txt (make-text-node string)) (dxem (funcall dxshift max))
                                   (dyem (if frac "0.75em" (if power "1em" "1.6em")))
                                   (rects (get-elements-by-tag-name cell "rect"))
                                   (els (length rects))
                                   (rect (aref rects (1- els)))
                                   ;;(rect (last-child cell))
                                   (xem (get-attribute rect "x"))
                                   (yem (get-attribute rect "y")))
                              (set-attributes tspan "dx" dxem "dy" dyem
                                              "style" (funcall font-size max power))
                              (append-child tspan txt)
                              (set-attributes t1 "style" style "x" xem "y" yem)
                              (append-child cell t1)
                              (append-child t1 tspan)))))))

              dxshift ;; Slot
              (lambda (incharn)
                (cond ((= incharn 1) "0.4em")
                      ((= incharn 2) "0.25em")
                      (t "0.1em")))
              
              dyshift ;; Slot FIXME REMOVE
              (lambda (max frac) ;;
                (if frac "0.75em" 
                    (if (= max 1) "1.25em" "1.75em")))

              ;;FIXME inline macro
              font-size ;; Slot
              (lambda (incharn power)
                (cond ((= incharn 1) "font-size:95%")
                      ((= incharn 2) "font-size:80%")
                      (t (if power "font-size:70%" "font-size:80%"))))
              
              cell-contains-fraction ;; Slot
              (define-anonymous-cell-type-test fraction)
              
              cell-contains-power ;; Slot
              (define-anonymous-cell-type-test power)

              click-on-fraction-cell ;; Slot
              (define-anonymous-click-cell-type-function fraction)

              click-on-power-cell
              (define-anonymous-click-cell-type-function power)
              
              delete-string-from-selected-cell ;; Slot
              (lambda ()
                (when selected-cell
                  (let ((cell (doc-get-el-by-id selected-cell)))
                    (remove-child cell (last-child cell)))));;FIXME remove-children-from-nth        
              
              destroy-svg-grid
              (lambda ()
                (let* ((canvas (doc-get-el-by-id "svg-canvas"))
                       (inpmenu-holder (doc-get-el-by-id "input-menu-holder"))
                       (grid (last-child canvas))
                       (alpha (doc-get-el-by-id "alpha-input"))
                       (msg (doc-get-el-by-id "messages")))
                  (remove-event-no-capture document "keypress" scratch-pad-keypress)
                  (remove-event-no-capture document "keydown" scratch-pad-keydown)
                  (setf selected-cell null)
                  (setf partial-answer (ps:array))
                  (setf (dots canvas style display) "")
                  (setf (dots inpmenu-holder style display) "none")
                  (when alpha (remove-child (dots document body) alpha))
                  (remove-child (dots document body) msg)
                  (remove-child canvas grid)
                  (remove-grey-out)))

              summon-msg ;; Slot
              (lambda ()
                (let* ((msg (div-id-class "messages" "scratch-pad-info"))
                       (h1 (create-element "h1")))
                  (set-attribute msg "data-ignore-banner" true)
                  (setf (text-content-of h1) "ScratchPad messages")
                  (append-child msg h1)
                  (setf (dots msg style display) "inline")
                  (append-child (dots document body) msg)
                  msg))
              
              summon-scratch-pad ;; Slot
              (with-slots (init) problem-stack
                (lambda (event id)
                  (let ((ml (get-attribute (dgebid (table-id current-table))
                                   "data-spmenu-level")))
                    (funcall make-sp-menu ml))
                  
                  (cancel-bubble event)
                  (funcall init (get-elements-by-tag-name
                                 (doc-get-el-by-id (table-id current-table))
                                 "input") id)
                  (funcall add-keyevent-handlers-for-scratchpad)
                  (grey-out)
                  (let* ((msg (or (doc-get-el-by-id "messages")
                                  (let ((msg (funcall summon-msg)))
                                    (funcall (dots drag-drop init-element) msg)
                                    msg)))
                         (inpmenu (doc-get-el-by-id "select-input-menu"))
                         (inpmenu-holder (doc-get-el-by-id "input-menu-holder"))
                         (mrows (children-of inpmenu)))
                    (when (not input-menu-event-handlers-set)
                      (setf input-menu-event-handlers-set true)
                      (dolist (row mrows) (dolist (inp (children-of row))
                                            (funcall add-event-handler-for-cell inp)
                                            (funcall set-graphics-for-cell inp))))
                    (set-display inpmenu-holder "table")
                    (let* ((scratch (doc-get-el-by-id "scratch"))
                           (disp (doc-get-el-by-id "problem-display"))
                           (in-use (aref (child-nodes-of disp) 0)))
                      (when (or (and scratch (not in-use))(not scratch))
                        (funcall set-problem-display id inpmenu-holder disp))
                      (let ((svgholder (doc-get-el-by-id "svg-canvas"))
                            (svg (if scratch scratch 
                                     (standard-svg-grid "svg-canvas" 
                                                        "scratch" 
                                                        20 15 
                                                        create-scratchpad-cell)))
                            (layer (1+ grey-out-z-index)))
                        (setf partial-answer (ps:array)
                              (class-name svgholder) "svg-container")
                        (set-attribute svg "data-reply-to-id" id) 
                        (set-z-index (if scratch scratch svg) layer)
                        (set-z-index inpmenu-holder layer)
                        (set-z-index msg layer)
                        (set-z-index svgholder layer) ;;
                        (funcall autofocus)
                        (mj-hub-queue (array "Typeset" mj-hub disp)))))))

              ;;  summon-scratch-pad ;; Slot
              ;; (with-slots (init) problem-stack
              ;;   (lambda (event id)
              ;;     (cancel-bubble event)
              ;;     (funcall init (get-elements-by-tag-name
              ;;                    (doc-get-el-by-id (table-id current-table))
              ;;                    "input") id)
              ;;     (funcall add-keyevent-handlers-for-scratchpad)
              ;;     (grey-out)
              ;;     (let* ((msg (or (doc-get-el-by-id "messages")
              ;;                     (let ((msg (funcall summon-msg)))
              ;;                       (funcall (dots drag-drop init-element) msg)
              ;;                       msg)))
              ;;            (inpmenu (doc-get-el-by-id "select-input-menu"))
              ;;            (inpmenu-holder (doc-get-el-by-id "input-menu-holder"))
              ;;            (mrows (children-of inpmenu)))
              ;;       (when (not input-menu-event-handlers-set)
              ;;         (setf input-menu-event-handlers-set true)
              ;;         (dolist (row mrows) (dolist (inp (children-of row))
              ;;                               (funcall add-event-handler-for-cell inp)
              ;;                               (funcall set-graphics-for-cell inp))))
              ;;       (set-display inpmenu-holder "table")
              ;;       (let* ((scratch (doc-get-el-by-id "scratch"))
              ;;              (disp (doc-get-el-by-id "problem-display"))
              ;;              (in-use (aref (child-nodes-of disp) 0)))
              ;;         (when (or (and scratch (not in-use))(not scratch))
              ;;           (funcall set-problem-display id inpmenu-holder disp))
              ;;         (let ((svgholder (doc-get-el-by-id "svg-canvas"))
              ;;               (svg (if scratch scratch 
              ;;                        (standard-svg-grid "svg-canvas" 
              ;;                                           "scratch" 
              ;;                                           20 15 
              ;;                                           create-scratchpad-cell)))
              ;;               (layer (1+ grey-out-z-index)))
              ;;           (setf partial-answer (ps:array)
              ;;                 (class-name svgholder) "svg-container")
              ;;           (set-attribute svg "data-reply-to-id" id) 
              ;;           (set-z-index (if scratch scratch svg) layer)
              ;;           (set-z-index inpmenu-holder layer)
              ;;           (set-z-index msg layer)
              ;;           (set-z-index svgholder layer) ;;
              ;;           (funcall autofocus)
              ;;           (mj-hub-queue (array "Typeset" mj-hub disp)))))))
              
              set-problem-display 
              (lambda (id inpmenu-holder disp)
                (let* ((orig (mj-original-text 
                              (aref (mj-get-all-jax (id-to id "Q")) 0)))
                       (last ((dots orig slice) -1))
                       (gnew (if (string= last "=") (subseq orig 0 -1) orig))
                       (text (strcat "$" gnew "$")))
                  ;;(disp (doc-get-el-by-id "problem-display")))
                  ;; (set-left inpmenu-holder "30%")
                  ;; (set-top inpmenu-holder "-28px")
                  (setf (text-content-of disp) text)))
              
              make-menu-row 
             (lambda ()
               (let ((menurow (div-class "menu-row")))
                 menurow))
             
             make-menu-entry-item 
             (lambda (txt id glass)
               (let* ((did (strcat "spm-" id))
                      (cllist (let ((ret ""))
                                (dolist (tx (split glass " ") ret)
                                  (setf ret (strcat "spm-" tx " " ret)))))
                      (ei (div-id-class did cllist)))
                 (append-text ei txt)
                 ei))

             problem-stack
             (ps:create stack (ps:array)
                        sc 0
                        ln 0
                        empty true
                        rotate-idx 
                        (with-slots (sc ln) problem-stack
                          (lambda ()
                            (setf sc (rem (1+ sc) ln))
                            sc))
                        ps-pop (with-slots (stack sc ln empty) problem-stack
                                 (lambda ()
                                   (if (not-defined (aref stack 0))
                                       'false
                                       (let ((it (aref stack sc)))
                                         (array-remove-idx sc stack)
                                         (setf ln (1- ln))
                                         (when (zerop ln)
                                           (setf empty true))
                                         it))))
                        init
                        (with-slots (stack ln empty sc) problem-stack
                          (lambda (coll id)
                            (let ((tmp (ps:array))
                                  (count 0))
                              (dolist (c coll)
                                (setf (aref tmp count) (id-of c))
                                (incf count))
                              (setf stack tmp
                                    sc 0
                                    ln count
                                    empty (if (zerop ln) true false)))))
                        ps-top
                        (with-slots (stack sc) problem-stack
                          (lambda ()
                            (let* ((iid (aref stack sc))
                                   (qid (id-to iid "Q"))
                                   (orig (mj-original-text
                                          (aref (mj-get-all-jax qid) 0)))
                                   (last ((dots orig slice) -1))
                                   (gnew (if (string= last "=")
                                             (subseq orig 0 -1) orig))
                                   (text (strcat "$" gnew "$"))
                                   (scratch (doc-get-el-by-id "scratch"))
                                   (probdisp (doc-get-el-by-id "problem-display")))
                              ;;(remove-children prob-disp)
                              (set-attribute scratch "data-reply-to-id" iid)
                              (setf (text-content-of probdisp) text)
                              (mj-hub-queue (array "Typeset" mj-hub probdisp)))))
                            
                        ps-next
                        (with-slots (stack ps-pop rotate-idx ps-top) problem-stack
                          (lambda ()
                            (funcall rotate-idx)
                            (funcall ps-top))))
                                                
             ;;input-menu-modules
             ;;(ps:create "MOD-BLAH-BLAH" '((("foo" "bar" "baz"))))
             
             scratchpad-menu-building-blocks
             (ps:create 
              config 
              (ps:create
               0 '((("1 D" "str" "even text config")
                    ("clear" "clr" "odd text config")
                    ("?" "?" "x config")))
               
               )
              self-insert
              (ps:create
               0 '((("0" "0" "even self-insert")
                    ("1" "1" "odd self-insert")
                    ("2" "2" "x self-insert"))
                   (("3" "3" "even self-insert")
                    ("4" "4" "odd self-insert")
                    ("5" "5" "x self-insert"))
                   (("6" "6" "even self-insert")
                    ("7" "7" "odd self-insert")
                    ("8" "8" "x self-insert"))
                   (("9" "9" "even self-insert")
                    ("+" "+" "odd self-insert")
                    ("-" "-" "x self-insert")))
               1 '((("=" "=" "even self-insert")
                    ("(" "(" "odd self-insert")
                    (")" ")" "x self-insert"))
                   (("*" "*" "even self-insert")
                    ("hline" "hline" "odd self-insert")
                    ("vline" "vline" "x self-insert")))
               ;; 1 '((("*" "*" "even self-insert")
               ;;      ("hline" "hline" "odd self-insert")
               ;;      ("vline" "vline" "x self-insert")))
               2 '((("" "nop" "even self-insert")
                    ("/" "fl" "odd self-insert")
                    ("." "." "x self-insert"))
                   
                   )
               )
              movement
              (ps:create
               0 '((("-->" "swph" "even movement")
                    ("^" "swpv" "odd movement")
                    ("Nxt" "nxt" "x movement"))
                   (("MV H" "movh" "even movement")
                    ("Mv V" "movv" "odd movement")
                    ("" "undef" "x movement")))

              
               )
              delquit
              (ps:create
               0 '((("Del Mv" "de" "even text delete")
                    ("Del" "dels" "odd text delete")
                    ("Quit" "exit" "x text delete")))


               )
              entry
              (ps:create
               0 '((("Underline" "eq" "even text entry")
                    ("Enter" "enter" "odd text entry")
                    ("Confirm" "confirm" "x text entry ")))



               )
              );;end menu building blocks

             scratchpad-menu false
             
             basic-scratchpad-input-menu
               '(;;config and help
                 (("1 D" "str" "even text config")
                  ("clear" "clr" "odd text config")
                  ("?" "?" "x config"))
                 ;;basic self-insert keys
                 (("0" "0" "even self-insert")
                  ("1" "1" "odd self-insert")
                  ("2" "2" "x self-insert"))
                 (("3" "3" "even self-insert")
                  ("4" "4" "odd self-insert")
                  ("5" "5" "x self-insert"))
                 (("6" "6" "even self-insert")
                  ("7" "7" "odd self-insert")
                  ("8" "8" "x self-insert"))
                 (("9" "9" "even self-insert")
                  ("+" "+" "odd self-insert")
                  ("-" "-" "x self-insert"))
                 ;;movement and paging exercises
                 (("-->" "swph" "even movement")
                  ("^" "swpv" "odd movement")
                  ("Nxt" "nxt" "x movement"))
                 (("MV H" "movh" "even movement")
                  ("Mv V" "movv" "odd movement")
                  ("" "undef" "x movement"))
                 ;;deletions and quitting
                 (("Del Mv" "de" "even text delete")
                  ("Del" "dels" "odd text delete")
                  ("Quit" "exit" "x text delete"))
                 ;;confirmation and entry
                 (("Underline" "eq" "even text entry")
                  ("Enter" "enter" "odd text entry")
                  ("Confirm" "confirm" "x text entry ")))

               build-menu-for-menu-level
               (lambda (menu-level)
                 (let ((spec (ps:array))
                       (max (1+ menu-level)))
                   (dolist (access '(config self-insert movement delquit entry))
                     (let ((partspec (ps:getprop scratchpad-menu-building-blocks
                                                 access))
                           (tmp (ps:array)))
                       (dotimes (n max)
                         (let ((found (ps:getprop partspec n)))
                           (if found
                               (setf tmp (append found tmp))
                               ps:break)))
                       (setf spec (append tmp spec))))
                 ;;; FIXME
                   (setf scratchpad-menu (nreverse spec))))
               
               ;; build-menu-for-menu-level
               ;; (lambda (menu-level)
               ;;   (let ((spec (ps:array))
               ;;         (max (1+ menu-level)))
               ;;     (dolist (access '(config self-insert movement delquit entry))
               ;;       (let ((partspec (ps:getprop scratchpad-menu-building-blocks
               ;;                                   access))
               ;;             (tmp (ps:array)))
               ;;         (dotimes (n max)
               ;;           (setf tmp (append (ps:getprop partspec n) tmp)))
               ;;         (setf spec (append tmp spec))))
               ;;   ;;; FIXME
               ;;     (setf scratchpad-menu (nreverse spec))))
               
               ;; make-appropriate-sp-menu
               ;; (lambda (menu-level)
               ;;   (funcall build-menu-for-menu-level menu-level)
               ;;   (funcall make-sp-menu))

               make-sp-menu
               (lambda (menu-level)
                 (let ((menu (doc-get-el-by-id "select-input-menu")))
                   (funcall build-menu-for-menu-level menu-level)
                   (when menu
                     ;;(unless (> (length (children-of menu)) 0) ...) FIXME HERE TO PREVENT EXTRA MENUS
                     (dolist (mr scratchpad-menu)
                       (let ((menurow (funcall make-menu-row)))
                         (dolist (item mr)
                           (append-child menurow 
                                         (funcall make-menu-entry-item 
                                                  (aref item 0)
                                                  (aref item 1)
                                                  (aref item 2))))
                         (append-child menu menurow))))))
               
               ;; make-basic-sp-menu ;;FIXME should this be removed?
               ;; (lambda ()
               ;;   (let ((menu (doc-get-el-by-id "select-input-menu")))
               ;;     (when menu
               ;;       (dolist (mr basic-scratchpad-input-menu)
               ;;         (let ((menurow (funcall make-menu-row)))
               ;;           (dolist (item mr)
               ;;             (append-child menurow 
               ;;                           (funcall make-menu-entry-item (aref item 0)
               ;;                                    (aref item 1)
               ;;                                    (aref item 2))))
               ;;           (append-child menu menurow))))))
               )
             
             (define-basic-input-menu-lambdas alpha 3 9)
             (define-basic-input-menu-lambdas numer 3 5)
             true
             ))
   (initialize-scratchpad)
   ))

(defvar *scratchpad-script*  (scratchpad-script))

