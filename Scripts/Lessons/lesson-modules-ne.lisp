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

(defun get-special-content (module)
  (let ((found (get module 'special-content)))
    (if found found
        (list "SCRIPT"
              (ps:ps (let ((obj (ps:new (-object))))
                       (with-slots (init-script) obj
                         (setf init-script
                               (lambda ()
                                 (alert (strcat 
                                         "SPECIAL-CONTENT for " 
                                         (ps:lisp (symbol-name module))
                                         " not yet implemented."))))
                         obj)))))))

(defmacro define-special-content (label poacc type declarations &body parenscript)
  (destructuring-bind (names access-function)
      poacc
    `(setf (get ',label 'special-content) 
           (list ,type
                 (ps:ps (with-slots (init-script ,@declarations) content-object
                          (setf ,@(ps:lisp parenscript)))))
           (get ',label 'pupil-object-special-content-data)
           (list ',names ,access-function))))

;; (define-special-content mod-digs-nat "SCRIPT" (foo)
;;   init-script ;; Slot
;;   (lambda ()
;;     (funcall foo "Hidy Hody"))
;;   foo (lambda (str) (alert str)))

;; NB:  This macro only needs to define references to vars which are
;; referenced in the actual lessons, not necessary if they are only in
;; the lesson-module.
(ps:defpsmacro with-lesson-module-slots (&body body)
  `(with-slots (non-ephemeral ephemeral) lesson-module
     (with-slots (create-blackboard 
                  set-variable-from-script
                  y-or-n
                  quit-early show-summary in-summary
                  remove-default-rl-handlers restore-default-rl-handlers
                  reset-var use-var init-var ajax-reset-var
                  numberline from-direction current-lesson
                  teach teaching-elements last-lesson-text
                  next-lesson-text previous-lesson-text
                  stop-teaching cleanup-after-lesson label-mark
                  complete-lesson current-lesson-text random-int
                  wrong-click-nl current-lesson-variables
                  current-lesson-title  ;;special-previous-lesson ;;this
                  get-digit get-inline-term continue-next
                  add-or-remove-wrong-click-nl-handlers
                  garbage record-garbage remove-garbage
                  numberline-id   ;;FIXME numberline-id should be a macro
                  current-module-name number-from-nlid) non-ephemeral

       (with-slots (;;maybe unnecessary BUT CHECK WITH TEST
                    special-previous-lesson 
                    handle-nl-click0 handle-nl-clickpos handle-nl-clickneg
                    sum-with-carry
                    
                    ;;in mod2-div-int
                    make-choose-level-screen
                    
                    ;;in mod1-mult
                    insert-practice

                    ;;in mod-table and mod1-mult
                    insert-product-builder

                    ;;in mod-basic-int
                    swap-min-sub show-add-negative
                    
                    ;;in mod-position-sys1-nat
                    summon-tutor make-tutor-input
                    current-tutor-input
                    summon-tutor-for-problem

                    ;;in mod-basic-sub-nat
                    ;;y-or-n 
                    minus-no-borrow
                    
                    ;;in mod-basic-add-nat
                    prepare-qnr-responses 
                    one-times-ten-to-the-nth-number-names
                    multiple-choice-questions
                    sum-no-carry

                    ;;in mod-digs-nat-lesson
                    init-circle-counter discrete-digit-sum-nl
                    second-term discrete-digit-sub-nl
                    add-lengths-nl sub-lengths-nl
                    
                    ;;mod-digs-nat
                    create-ddsum-cell
                    create-length-line 
                    place-circle-on-nl-mark
                    init-count-lines  add-line-on-nl
                    place-minuend-line-on-nl place-second-term place-first-term
                    remove-circles-from-numberline  place-subtrahend-line-on-nl
                    add-circle-on-numberline 
                    sub-circle-from-numberline  
                    create-lsum-cell  place-circle-in-cell count-lines
                    set-minuend-circles-nl place-circles-n 
                    count-circles ) ephemeral
       ;;(ps:lisp 
       ,@body))))
      
(defun lesson-module-ne ()
  (ps:ps 
    (defun init-non-ephemeral-lesson-slots ()
      (with-slots (non-ephemeral ephemeral) lesson-module
        (with-slots (create-blackboard set-variable-from-script
                     quit-early show-summary in-summary
                     garbage record-garbage remove-garbage
                     reset-var use-var init-var ajax-reset-var
                     remove-default-rl-handlers restore-default-rl-handlers
                     numberline from-direction current-lesson
                     current-lesson-title grab-focus 
                     make-refresh-handler insert-refresh-button
                     get-digit get-inline-term continue-next
                     teach teaching-elements last-lesson-text
                     next-lesson-text previous-lesson-text
                     stop-teaching cleanup-after-lesson label-mark
                     complete-lesson current-lesson-text random-int
                     wrong-click-nl current-lesson-variables 
                     numberline-id  y-or-n ;;FIXME numberline-id should be a macro
                     current-module-name number-from-nlid) non-ephemeral
          (setf 

           y-or-n
             (lambda (control-name yes-txt no-txt next typeset)
               (if (and (string= from-direction "rtl")
                        (not (= 0 current-lesson-text)))
                   (funcall previous-lesson-text)
                   (labels ((make-y-or-n-handler (txt)
                              (lambda (ev)
                                (remove-event-no-capture y "click" yhandler)
                                (remove-event-no-capture n "click" nhandler)
                            (funcall restore-default-rl-handlers)
                            (append-para-text ltd txt)
                            (append-para-text ltd next)
                            (when typeset 
                              (mj-hub-queue (array "Typeset" mj-hub ltd)))
                            )))
                 (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                        (form (create-element "form"))
                        (ydiv (plain-div))
                        (ndiv (plain-div))
                        (y (make-radio-input-element control-name))
                        (n (make-radio-input-element control-name))
                        (yhandler (make-y-or-n-handler yes-txt))
                        (nhandler (make-y-or-n-handler no-txt)))
                   (append-child ydiv y)
                   (append-text ydiv "yes")
                   (append-child ndiv n)
                   (append-text ndiv "no")
                   (append-children form ydiv ndiv)
                   (funcall remove-default-rl-handlers)
                   (add-event-no-capture y "click" yhandler)
                   (add-event-no-capture n "click" nhandler)
                   (append-child ltd form)))))
                        
           set-variable-from-script
           (lambda (sname valq vala)
             (labels ((mkvobj (v)
                        (ps:create 
                         genfn v
                         value v)))
               (let ((aname (strcat sname ":a"))
                     (qname (strcat sname ":q")))
                 (setf (ps:getprop current-lesson-variables aname)
                       (funcall mkvobj vala)
                       (ps:getprop current-lesson-variables qname)
                       (funcall mkvobj valq))))
             sname)
               

           continue-next
           (lambda ()
             (cond ((string= from-direction "ltr")
                    (funcall next-lesson-text))
                   ((string= from-direction "rtl")
                    (funcall previous-lesson-text))
                   (t false)))
           
           insert-refresh-button ;; Slot
           (lambda (varname x) ;;NB remember to append :q from
             ;;calling function if the varname is
             ;;to be reset by an ajax call
             (let ((ltd (doc-get-el-by-id "lesson-text-display"))
                   (butt (make-button (if x (strcat "refresh-" x)
                                          "refresh") "New")))
               (add-event-no-capture butt "click" (funcall make-refresh-handler
                                                           varname))
               (append-child ltd butt)))
           
           make-refresh-handler ;; Slot
           (lambda (varname)
             (lambda (ev)
               (funcall reset-var varname (funcall use-var varname) true)))   
           
           grab-focus ;; Slot
           (lambda (ev on)
             (let ((target (if on on this)))
               (funcall (dots target focus))
               (let ((val (value target)))
                 (setf (value target) ""
                       (value target) val))))
           
           get-inline-term ;; Var slot
           (lambda (strexp n)
             (aref (split strexp " ") n))
           
           get-digit ;; Slot
           (lambda (nstring power)
             (let* ((lastidx (1- (length nstring)))
                    (start (- lastidx power))
                    (end (1+ start))
                    (found (subseq nstring start end)))
               (if (string= found "") "0" found)))
           
           remove-default-rl-handlers ;; Slot
           (lambda ()
             (remove-event-no-capture 
              (doc-get-el-by-id "previous-step")
              "click" previous-lesson-text)
             (remove-event-no-capture 
              (doc-get-el-by-id "next-step")
              "click" next-lesson-text))
           
           restore-default-rl-handlers ;; Slot
           (lambda (obj1 fn1 obj2 fn2)
             (when (and obj1 fn1) (remove-event-no-capture obj1 "click" fn1))
             (when (and obj2 fn2) (remove-event-no-capture obj2 "click" fn2))
             (add-event-no-capture 
              (doc-get-el-by-id "previous-step")
              "click" previous-lesson-text)
             (add-event-no-capture 
              (doc-get-el-by-id "next-step")
              "click" next-lesson-text))
           
           ajax-reset-var ;; Slot
           (lambda (name currval recall)
             (labels ((callback (response)
                        (let ((p (parse-json response)))
                          (dolist (part p)
                            (funcall init-var (aref part 0) (aref part 1)))
                          (when recall
                            (funcall (aref current-lesson current-lesson-text))))))
               (ajax_get_new_qna_values name current-module-name currval callback)))
           
           use-var ;; Slot
           (lambda (varstring)
             (with-slots (genfn value) obj                 
                 (let ((obj (ps:getprop current-lesson-variables varstring)))    
                   value)))
           
           reset-var ;; Slot
           (lambda (varstring currval recall)
             (let* ((arr (split varstring ":"))
                    (nm (aref arr 0))
                    (ajax (aref arr 1)))
               (let ((obj (ps:getprop current-lesson-variables varstring)))
                 (with-slots (genfn value) obj
                   (if ajax
                       (funcall ajax-reset-var nm currval recall)
                       (setf value (funcall genfn)))))))

           init-var ;; Slot
           (lambda (varstring fn)
             (if (dots varstring map) ;;its an array
                 (dolist (pair varstring)
                   (funcall init-var (aref pair 0) (aref pair 1)))
                 (let ((variable (ps:create genfn fn
                                            value (if (string= (typeof fn) "function")
                                                      (funcall fn)
                                                      fn))))
                   (setf (ps:getprop current-lesson-variables varstring)
                         variable))))           

           numberline-id ;; Slot
           (lambda (n)
             ;;FIXME macro
             (strcat "nl-mark(" n ")"))
           random-int ;; Slot 
           (lambda (min max)
             (+ min (funcall (dots -math floor)
                             (* (funcall (dots -math random)) (1+ (- max min))))))
           
           number-from-nlid ;; Slot FIXME macro
           (lambda (nlid)
             (aref (split (aref (split nlid "(") 1) ")") 0))

           wrong-click-nl ;; Slot
           (lambda (ev)
             (let* ((id (id-of this))
                    (n (aref (split (aref (split id "(") 1) ")") 0))
                    (tn (make-text-node (strcat "No, that was " n ". Try again.")))
                    (p (create-element "p"))
                    (ltd (doc-get-el-by-id "lesson-text-display")))
               (append-child p tn)
               (append-child ltd p)))
           
           random-int ;; Slot
           (lambda (min max)
             (+ min (funcall (dots -math floor)
                             (* (funcall (dots -math random)) (1+ (- max min))))))
           
           complete-lesson ;; Slot
           (lambda ()
             (labels ((teacher-quit () ;;ajax_complete_lesson is only
                                       ;;defined in the pupil script,
                                       ;;so if it doesn't exist, we're
                                       ;;in a teacher page and
                                       ;;shouldn't call it.
                        (funcall cleanup-after-lesson)
                        (funcall stop-teaching))
                      (callback (response)
                        ;;ignore response
                        ;; (dolist (el teaching-elements)
                        ;;   (remove-child (dots document body) el))
                        (if work-on-screen
                            (replace-off-screen-answer-handlers)
                            (restore-off-screen-answer-handlers))
                        (funcall cleanup-after-lesson)
                        (funcall stop-teaching)))
               ;;If the cycle is already 2, the lesson was started
               ;;voluntarily by user - so no need to call server to update
               ;;cycle and no need to add the lesson to viewed - just remove
               (ps:try ;;an exception will be thrown before anything
                       ;;actually gets done if we are in a teacher
                       ;;page.  This is important and intentional.
                       (let ((table0 (doc-get-el-by-id "table_0")))
                         (if table0
                             (cond ((equal 
                                     (get-attribute table0 "cycle") 1)
                                    (loop for tidx from 0 below all-tables
                                       do (set-attribute 
                                           (doc-get-el-by-id (table-id tidx)) "cycle" 2))
                                    (ajax_complete_lesson 
                                     owner (get-attribute table0 "module") callback))
                                   (t 
                                    (if work-on-screen
                                        (replace-off-screen-answer-handlers)
                                        (restore-off-screen-answer-handlers))
                                    (funcall cleanup-after-lesson)
                                    (funcall stop-teaching)))
                             (progn ;; (funcall cleanup-after-lesson)
                               ;; (funcall stop-teaching)
                               (mj-hub-queue 
                                (array ajax_complete_lesson 
                                       owner special-lesson-module callback) 
                             get-all-tables-and-start))))
                       (:catch (err)
                         (teacher-quit)))))
           
           cleanup-after-lesson ;; Slot
           (lambda ()
             (setf svg-grid-skip 1.5)
             ;; (let ((ltd (doc-get-el-by-id "lesson-text-holder")))
             ;;   (when ltd (remove-child (dots document body) ltd)))
             (setf special-lesson-module false)
             (setf can-quit-lesson false)
             (setf from-direction nil)
             (remove-grey-out)
             (setf current-module-name nil)
             (setf last-lesson-text 0)
             (setf current-lesson-variables (ps:new (-object)))
             (setf ephemeral (ps:new (-object))))

           stop-teaching ; Slot 
           (lambda ()
             (dolist (el teaching-elements)
               (remove-child (dots document body) el))
             (setf teaching-elements (ps:array)))

           next-lesson-text ;; Slot
           (lambda (ev)
             (funcall remove-garbage)
             (setf from-direction "ltr")
             (setf last-lesson-text current-lesson-text)
             (incf current-lesson-text)
             (let ((fn (aref current-lesson current-lesson-text)));; HERE aref
               (if fn (fn) (decf current-lesson-text))))

           previous-lesson-text ;;Slot
           (lambda (ev)
             (unless (= current-lesson-text 0)
             (when can-quit-lesson
               (when in-summary
                 (add-event-no-capture (doc-get-el-by-id "lesson-summary")
                                       "click" show-summary)
                 (setf in-summary false)))
             (funcall remove-garbage)
             (setf from-direction "rtl")
             (setf last-lesson-text current-lesson-text)
             (decf current-lesson-text)
             (if (< current-lesson-text 0)
                 (setf current-lesson-text 0)
                 (let ((fn (aref current-lesson current-lesson-text))) ;; HERE aref
                   (if fn (fn) (incf current-lesson-text))))))

           create-blackboard ;; Slot
           (lambda ()
             (let ((bb (div-id "lesson-text-holder"))
                   (h1 (create-element "h1"))
                   (htext current-lesson-title)
                    ;;(get-attribute (doc-get-el-by-id "table_0") "description"))
                   (txd (div-id "lesson-text-display"))
                   (arrs (div-id "lesson-pager"))
                   (rarr (div-id "next-step"))
                   (larr (div-id "previous-step")))
               (set-attribute bb "data-ignore-banner" true)
               (set-z-index bb (1+ grey-out-z-index))
               (setf (text-content-of h1) 
                     (if (and htext (not (string= htext "")))
                         (strcat "Lesson: " htext) "Lesson: "))
               (append-child rarr left-to-right-arrow) ;; HERE
               (append-child larr right-to-left-arrow)  ;; HERE
               (append-child arrs larr)
               (append-child arrs rarr)
               (append-child bb arrs)
               (append-child bb h1)
               (append-child bb txd)
               (when can-quit-lesson
                 (let ((nav (div-id "lesson-nav"))
                       (qb (make-button "lesson-quit" "Exit"))
                       (sb (make-button "lesson-summary" "Summary")))
                   (append-child nav qb) 
                   (append-child nav sb)
                   (append-child bb nav)))
               bb))

           quit-early ;; Slot
           (lambda (ev)
             (funcall complete-lesson))

           show-summary ;; Slot
           (lambda (ev replaced)
             (unless replaced
               (funcall restore-default-rl-handlers)
               (remove-event-no-capture this "click" show-summary))
             (setf in-summary true)
             (let ((pos (- (length current-lesson) 2)))
               (funcall (aref current-lesson pos)) ;;HERE aref
               (setf current-lesson-text pos
                     last-lesson-text (1- pos))))

           teach ;; Slot
           (lambda (string typeset)
             (let ((bb (doc-get-el-by-id "lesson-text-holder")))
               
               (unless bb
                 (setf bb (funcall create-blackboard))
                 (ps:try (save-and-block-default-answer-handlers) ;; HERE
                         (:catch (error)
                           ;; just ignore, it is neither needed nor 
                           ;; defined in teacher script
                           ))
                 (funcall (dots drag-drop init-element) bb)
                 (append-child (dots document body) bb)
                 
                 (push bb teaching-elements) ;;CHECK
                 (let ((qb (doc-get-el-by-id "lesson-quit"))
                       (sb (doc-get-el-by-id "lesson-summary")) ;; HERE
                       (rarr (doc-get-el-by-id "next-step"))
                       (larr (doc-get-el-by-id "previous-step")))
                   (when qb
                     (add-event-no-capture qb "click" quit-early))
                   (when sb
                     (add-event-no-capture sb "click" show-summary))
                   (add-event-no-capture rarr "click" next-lesson-text)
                   (add-event-no-capture larr "click" previous-lesson-text))
                 (setf (dots bb style display) "inline"))
               (let ((display (doc-get-el-by-id "lesson-text-display")))
                 (set-inner-html display string)
                 (when typeset 
                   (mj-hub-queue (array "Typeset" mj-hub display))))))
           
;; (lambda (string)
;;              (let ((bb (doc-get-el-by-id "lesson-text-holder")))
;;                (unless bb
;;                  (setf bb (funcall create-blackboard))
;;                  (save-and-block-default-answer-handlers) ;; HERE
;;                  (funcall (dots drag-drop init-element) bb)
;;                  (append-child (dots document body) bb)
;;                  (push bb teaching-elements) ;;CHECK
;;                  (let ((rarr (doc-get-el-by-id "next-step"))
;;                        (larr (doc-get-el-by-id "previous-step")))
;;                    (add-event-no-capture rarr "click" next-lesson-text)
;;                    (add-event-no-capture larr "click" previous-lesson-text))
;;                  (setf (dots bb style display) "inline"))
;;                (let ((display (doc-get-el-by-id "lesson-text-display")))
;;                  (set-inner-html display string))))
           
           numberline ;; Slot
           (lambda (id start end units color width)
             (let ((already (doc-get-el-by-id "nl-holder"))
                   (lth (dgebid "lesson-text-holder")))
               (add-to-class-list lth "with-nl")
               (if already
                   (if (not in-summary)
                       (progn (remove-child (dots document body) already)
                              (array-remove already teaching-elements)
                              (funcall previous-lesson-text))
                       false)
                   (let* ((skip svg-grid-skip)
                          (height 2)
                          (padding 4)
                          (nlx (* skip (/ padding 2)))
                          (nl-holder (div-id-class "nl-holder" "svg-container no-left")))
                     (funcall (dots drag-drop init-element) nl-holder)
                     (set-z-index nl-holder (1+ grey-out-z-index))
                     (let* ((nl-length (+ (abs start) (abs end)))
                            (svg (standard-svg-grid 
                                  nl-holder id 
                                  height (+ nl-length padding)
                                  create-scratchpad-cell
                                  ))
                            (x1 nlx)
                            (y1 (* skip (/ height 2)))
                            (x2 (+ nlx (* skip nl-length)))
                            (y2 y1)
                            (nl (svg-draw-line svg x1 y1 x2 y2 color 
                                               (ems width) nil nil)))
                       (set-attribute nl "id" "numberline-line")
                       (set-attribute svg "class" "numberline")
                       (draw-left-arrowhead svg skip x1 y1 color (ems width))
                       (draw-right-arrowhead svg skip x2 y2 color (ems width))
                       (draw-unit-markers 
                         svg skip x1 y1 color "0.225em" start nl-length units t)
                       (push nl-holder teaching-elements)
                       (append-child (dots document body) nl-holder)
                       (when (not in-summary)
                         (funcall next-lesson-text)))))))
           
           label-mark ;; Slot
           (lambda (g idn)
             (let* ((mark (first-child g)) (sidn (funcall (dots idn to-string)))
                    (lsidn (length sidn)) (neg (string= (subseq sidn 0 1) "-"))
                    (lshift) (fsize) (rat (split sidn "/")) (frac (< 1 (length rat)))
                    (decarr (when (not frac) (split sidn ".")))
                    (dec (when (and decarr (< 1 (length decarr)))))
                    (x (get-attribute mark "x1")) (y (get-attribute mark "y1"))
                    (svgtxt (svg-create-element "text"))
                    (tspan (svg-create-element "tspan"))
                    (tn (make-text-node sidn)))
               (cond ((> lsidn 2) (setf lshift "-0.8em") (setf fsize "0.7em"))
                     ((> lsidn 1) (setf lshift "-0.5em") (setf fsize "0.7em"))
                     (t  (setf lshift "-0.3em") (setf fsize "0.7em")))
               (set-attributes svgtxt "x" x "y" y)
               (set-attributes tspan "dx" lshift "dy" "-0.1em" "font-size" fsize
                               "class" "nl-unit")
               (append-child tspan tn)
               (append-child svgtxt tspan)
               (append-child g svgtxt)))
           
           record-garbage ;; Slot
           (lambda (p e)
             (let ((garbage-item (ps:new (-object))))
               (with-slots (parent element) garbage-item
                 (setf parent p
                       element e)
                 (push garbage-item garbage)
                 garbage-item)))

           remove-garbage ;; Slot
           (lambda ()
             (with-slots (parent element) garbage-item
               (dolist (garbage-item garbage)
                 (remove-child parent element))
               (setf garbage (ps:array))
               garbage))
           
           from-direction nil ;; Var slot
           in-summary false ;; Var slot
           garbage (ps:array) ;; Var slot
           ;;garbage-item (ps:new (-object)) ;; Var slot
           current-lesson-variables (ps:new (-object)) ;; Var slot
           current-lesson false ;; Var slot
           teaching-elements (ps:array) ;; Var slot
           last-lesson-text null ;Var slot
           current-lesson-text 0 ; Var slot
           current-module-name nil ;; Var slot
           ))
        non-ephemeral
        ))
    (init-non-ephemeral-lesson-slots)
    ))



