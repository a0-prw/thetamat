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
;;FIXME remember to setf ps:*ps-print-pretty* nil

(defun init-mod-sub-position-sys-nat-lesson-slots ()
  (ps:ps 
    (defun init-mod-sub-position-sys-nat-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (test-set-vars ;;REMOVE
                     make-operand 
                     tutor-next-step
                     make-tutor-input
                     create-input-grid-for-sum-tio ;;create-input-grid-for-plus
                     ;; from scratchpad
                     tio-grid-keydown tio-grid-keypress input-direction 
                     click-on-id add-keyevent-handlers-for-tio-grid
                     set-graphics-for-cell add-event-handler-for-cell
                     math-symbols
                     ;;;;; handlers for menu and keypress
                     tutor-input-next 
                     insert-this-string
                     insert-decimal-point-in-selected-cell 
                     insert-vline-in-selected-cell 
                     insert-hline-in-selected-cell 
                     delete-content-of-selected-cell
                     delete-content-of-selected-cell-stay 
                     move-to-adjacent-cell
                     move-to-vertical-neighbour
                     ;;;;; end handlers
                     get-input-character-number
                     place-string-in-selected-cell place-single-digit-string
                     delete-string-from-selected-cell place-multi-digit-string 
                     focus input-chars dxshift font-size 
                     click-on-selected-cell
                     tio-grid-menu make-tio-grid-menu build-menu-for-menu-level
                     make-menu-row make-menu-entry-item 
                     tio-grid-menu-building-blocks
                     click-next-cell-if-there 
                     click-vertical-neighbour-if-there swaph-input-direction
                     swapv-input-direction 
                     tio-ltr-arr
                     tio-rtl-arr
                     ;;end from scratchpad
                     toggle-hints make-hint-function
                     make-no-hint make-noncomm-start-arc-action
                     make-in-plus-sum-hint make-in-minus-sum-hint
                     make-regroup-hint-function make-borrow-hint-function
                     make-minus-regroup-hint-function 
                     make-answer-hint-function hints-on
                     make-operands-hint-function
                     make-nyi-hint make-completed-hint
                     make-plusmachine-start-hint make-minusmachine-start-hint
                     summon-tutor-for-problem
                     summon-tutor initialize-tutor
                     tutor-new-problem
                     tutor-state tutor-show-summary tutor-lesson-quit remove-tutor 
                     clear-tio-grid clear-old-input-menu
                     make-tutor-display make-hints
                     insert-tutor-display
                     emote be-neutral svg-canvas
                     neutral-mouth make-face
                     place-carry-digit-in-cell 
                     insert-answer-line
                     
                     enter-regroup-mode exit-regroup-mode
                     defplusmachine make-fsm-for-tio
                     make-fsm-node-name make-writer-action make-have-operand-action
                     make-answer-fsm-node-name make-next-answer-name
                     make-next-answer-spec
                     defarcs defalt defanswer
                     do-unique-idx-c get-commutative-arc-path-specs 
                     get-noncomm-arc-path-specs defminusmachine
                     get-plus-answer-specs get-minus-answer-specs
                     get-tutor-base
                     get-cell-coords-string get-cell-coords 
                     do-idx-c get-arc-path-specs 
                     make-self-insert-input-for-tutor 
                     feed-self-insert-input-to-tutor
                     node-inputs node-outputs initialize-ephemeral) ephemeral
          (with-slots (get-inline-term show-summary quit-early
                       next-lesson-text previous-lesson-text
                       remove-default-rl-handlers restore-default-rl-handlers
                       reset-var use-var from-direction) non-ephemeral 
            (setf

             hints-on
             (lambda ()
               (let ((but (dgebid "hints-on")))
                 (checked but)))
                          
             input-chars 1
             
             tutor-state
             (ps:create
              reset (lambda ()
                      (setf selected-cell "cell(0,0)")
                      (with-input-direction
                          (funcall reset))
                      (with-tutor-state
                          (setf current-tutor-input false
                                hint false
                                grid-clear true
                                initialized false))
                      (with-hint
                          (setf cell0 false
                                cell1 false
                                text-element false))
                      (with-tutor-fsm
                          (setf 
                           commutative true
                           current-node false
                           in-answer false 
                           initialized false
                           prefix false
                           regroup-mode false
                           arcs (ps:array)
                           alternative-states (ps:new (-object))
                           nodes (ps:new (-object))
                           start-nodes (ps:new (-object)))))
              selected-cell-color "rgba(139, 203, 138, 0.4)"
              plain-color "#ffffff"
              fsm (ps:create
                   initialized false
                   a-prefix "a_"
                   b-prefix "b_"
                   prefix false
                   start-nodes (ps:new (-object))
                   alternative-states (ps:new (-object))
                   current-node false
                   in-answer false
                   regroup-mode false
                   commutative true
                   current-hint (ps:create text-element false
                                           colors (ps:create
                                                   color0-color "green"
                                                   color0-name "green"
                                                   color1-color "red"
                                                   color1-name "red")
                                           cell0 false
                                           cell1 false
                                           words (ps:create
                                                  writea "Write a "
                                                  inthe " in the " 
                                                  ora " or a " 
                                                  anda " and a "
                                                  square " square"
                                                  period ".")
                                           display (lambda (visible)
                                                     (with-hint
                                                         (when text-element
                                                           (let ((scr (dgebid "tutor-msg-screen")))
                                                             (if visible
                                                                 (progn
                                                                   (append-child scr text-element)
                                                                   (funcall paint))
                                                                 (progn
                                                                   (remove-children-from-nth scr 0)
                                                                   (funcall clear-color cell0)
                                                                   (funcall clear-color cell1)))))))
                                           paint (lambda ()
                                                   (with-hint
                                                       (labels ((colorise (celln col)
                                                                  (let* ((cobj (dgebid celln))
                                                                         (rect (aref (gebtag cobj "rect") 0)))
                                                                    (set-attribute rect "fill" col))))
                                                         (when cell0 (funcall colorise cell0 (with-colors color0-color)))
                                                         (when cell1 (funcall colorise cell1 (with-colors color1-color))))))
                                           clear-color (lambda (cellname)
                                                         (with-tutor-state
                                                             (when cellname
                                                               (let* ((cobj (dgebid cellname))
                                                                      (rect (aref (gebtag cobj "rect") 0)))
                                                                 (set-attribute rect "fill"
                                                                                (if (string= cellname selected-cell)
                                                                                    selected-cell-color
                                                                                    plain-color))))))
                                           remove (lambda ()
                                                    (with-hint
                                                        (let ((scr (dgebid "tutor-msg-screen")))
                                                          (remove-children-from-nth scr 0)
                                                          (funcall clear-color cell0)
                                                          (funcall clear-color cell1)
                                                          (setf text-element false
                                                                cell0 false
                                                                cell1 false)))))
                   handle-completion
                   (lambda ()
                     (let ((scr (dgebid "tutor-msg-screen")))
                       (remove-children-from-nth scr 0)
                       (append-para-text scr "Sum finished.")))
                   
                   borrower-target false

                   handle-non-commutative
                   (lambda (input rpress)
                     (with-tutor-fsm 
                         (if (not regroup-mode)
                             (let ((arc (ps:getprop (node-outputs current-node) input)))
                               (if (or (not arc)
                                       rpress);;HERE
                                   (funcall emote false)
                                   (let ((gnew (arc-to arc)))
                                     (funcall emote true)
                                     (let ((atstart (string= (fsm-node-name current-node)
                                                             "start")))
                                       (funcall (arc-action arc))
                                       (unless atstart
                                         (funcall set-current-node gnew))))))
                                         ;;(setf current-node gnew))))))
                             (let* ((myname (fsm-node-name current-node))
                                    (narr (split myname "_"))
                                    (len (length narr)))                        
                               (cond ((= len 2)
                                      (funcall emote false)
                                      (setf regroup-mode false))
                                     (t (funcall emote true)
                                        (cond ((string= (aref narr 0) "b") ;;we got sent here from an answer cell
                                               (let* ((nxtn (node-next-name current-node))
                                                      (nxtfocus (mkcn (aref (split nxtn "_") 1))))
                                                 (ps:destructuring-bind (col row)
                                                     (split (aref narr 1) ",")
                                                 (setf borrower-target (strcat col "," (- (parse-int row) 2))))
                                                 ;;(setf current-node (funcall find-node nxtn))
                                                 (funcall set-current-node (funcall find-node nxtn))
                                                 (funcall focus nxtfocus)))
                                              
                                              (t (funcall handle-borrowing (strcat "r_" input))))))))))
                   
                   handle-borrowing 
                   (lambda (input)
                     (with-tutor-fsm
                         (ps:destructuring-bind (pref currcoords digtoplace)
                             (split input "_")
                           (let* ((cnnam (fsm-node-name current-node))
                                  (cnarr (split cnnam "_"))
                                  (len (length cnarr)))
                             ;;(when (= 3 len) ;;HERE just setf input-chars to length!
                             (ps:destructuring-bind (cnpref cncurrcoords cndigits)
                                 cnarr
                               (let ((len (length cndigits))) ;;FIXME remove input-chars
                                 ;; (if (> len 1) 
                                 ;;     (setf input-chars len)
                                 ;;     (setf input-chars 1)))
                                 (if (= len 1) ;;(= input-chars 1)
                                     (let* ((arcout (ps:getprop (node-outputs current-node) input))
                                            (tonode (when arcout (arc-to arcout)))
                                            (toname (when tonode (fsm-node-name tonode))))
                                       (if (not arcout)
                                           (funcall emote false)
                                           (let ((narr (split toname "_")))
                                             (funcall emote true)
                                             (cond ((= (length narr) 2)
                                                    (ps:destructuring-bind (coords digs)
                                                        narr
                                                      (funcall place-string-in-selected-cell digtoplace true)
                                                      ;;(setf current-node tonode)
                                                      (funcall set-current-node tonode)
                                                      (funcall focus (mkcn coords))
                                                      (setf regroup-mode false)))
                                                   (t (ps:destructuring-bind (r coords digs)
                                                          narr
                                                        (funcall place-string-in-selected-cell digtoplace true)
                                                        ;;(setf current-node tonode)
                                                        (funcall set-current-node tonode)
                                                        (funcall focus (mkcn coords))))))))
                                     (let ((success (funcall handle-multichar-input cndigits digtoplace)))
                                       (when success 
                                         (let* ((arc (ps:getprop (node-outputs current-node) cnnam))
                                                (gnew (arc-to arc)))
                                           (setf regroup-mode false)
                                           (funcall (arc-action arc))
                                           ;;(setf current-node gnew)
                                           (funcall set-current-node gnew)
                                           ))))
                                 ))))))
                   
                   multichar-count 0

                   handle-multichar-input
                   (lambda (expected entry)
                     (with-tutor-fsm
                         (cond ((zerop multichar-count) ;;first entry
                                (cond ((string= entry (subseq expected 0 1))
                                       (funcall emote true)
                                       (setf multichar-count 1)
                                       (funcall place-string-in-selected-cell entry true)
                                       false)
                                      (t (funcall emote false)
                                         false)))
                               (t (let* ((cell (dgebid selected-cell)) ;;HERE
                                         (existing (aref (get-elements-by-tag-name cell "text") 0))
                                         (txtcnt (if existing (text-content-of existing) false))
                                         (gnu (strcat txtcnt entry)))
                                    (cond ((string= gnu expected)
                                           (setf multichar-count 0)
                                           gnu)
                                          (t (funcall emote false)
                                             false)))))))
                       
                   ;; Commutative works by maintaining 2 fsms until
                   ;; input is sufficient to distinguish between
                   ;; operands: works like this: 1) look for start
                   ;; node in A and B. 2) if found both places,
                   ;; maintain 2 states until and (if) ...  3) if found
                   ;; only in one, set prefix to that 4) profit
                   ;; (...... naaaaht)
                   handle-prefix-set
                   (lambda (input )
                     (with-tutor-state
                         (with-fsm
                             (let* ((input (strcat prefix input))
                                    (arc (ps:getprop (node-outputs
                                                      current-node)
                                                     input)))
                               (if (not arc)
                                   (funcall emote false)
                                   (let ((gnew (arc-to arc)))
                                     (funcall emote true)
                                     (funcall (arc-action arc))
                                     (funcall set-current-node gnew)))))))
                   ;; handle-prefix-notset
                   ;; (lambda (input)
                   ;;   (with-tutor-state
                   ;;       (with-fsm
                   ;;           (let* ((inpa (strcat a-prefix input))
                   ;;                  (inpb (strcat b-prefix input))
                   ;;                  (currenta (ps:getprop alternative-states "A"))
                   ;;                  (currentb (ps:getprop alternative-states "B"))
                   ;;                  (arca (ps:getprop (node-outputs currenta) inpa))
                   ;;                  (arcb (ps:getprop (node-outputs currentb) inpb)))
                   ;;             (cond ((and arca arcb)
                   ;;                    (let ((newa (arc-to arca))
                   ;;                          (newb (arc-to arcb)))
                   ;;                      (setf (ps:getprop alternative-states "A") newa
                   ;;                            (ps:getprop alternative-states "B") newb)
                   ;;                      ;;next call is ok: they have same side (printing) effect
                   ;;                      (funcall emote true)
                   ;;                      (funcall (arc-action arca))))
                   ;;                   (arca (setf prefix a-prefix
                   ;;                               current-node (arc-to arca))
                   ;;                         (funcall emote true)
                   ;;                         (funcall (arc-action arca)))
                   ;;                   (arcb (setf prefix b-prefix
                   ;;                               current-node (arc-to arcb))
                   ;;                         (funcall emote true)
                   ;;                         (funcall (arc-action arcb)))
                   ;;                   (t (funcall emote false)))))))
                   handle-prefix-notset
                   (lambda (input)
                     (with-tutor-state
                         (with-fsm
                             (let* ((inpa (strcat a-prefix input))
                                    (inpb (strcat b-prefix input))
                                    (currenta (ps:getprop alternative-states "A"))
                                    (currentb (ps:getprop alternative-states "B"))
                                    (arca (ps:getprop (node-outputs currenta) inpa))
                                    (arcb (ps:getprop (node-outputs currentb) inpb)))
                               (cond ((and arca arcb)
                                      (let ((newa (arc-to arca))
                                            (newb (arc-to arcb)))
                                        (setf (ps:getprop alternative-states "A") newa
                                              (ps:getprop alternative-states "B") newb)
                                        ;;next call is ok: they have same side (printing) effect
                                        (funcall emote true)
                                        (funcall (arc-action arca))
                                        (when (funcall hints-on)
                                          (let ((hint1 (funcall (node-hint-function newa) true))
                                                (hint2 (string-downcase (funcall (node-hint-function newb) true)))
                                                (disp (dgebid "tutor-msg-screen")))
                                            (remove-children-from-nth disp 0)
                                            (append-para-text disp (strcat hint1 " or " hint2 "."))))
                                            ))
                                     (arca (let ((newa (arc-to arca)))
                                             (setf prefix a-prefix ;;HERE call set-current-node instead?
                                                   current-node newa)
                                             (funcall emote true)
                                             (funcall (arc-action arca))
                                             (when (funcall hints-on)
                                               (let ((hnt (funcall (node-hint-function newa) true))
                                                     (disp (dgebid "tutor-msg-screen")))
                                                 (remove-children-from-nth disp 0)
                                                 (append-para-text disp (strcat hnt "."))))))
                                     (arcb (let ((newb (arc-to arcb)))
                                             (setf prefix b-prefix ;;HERE call set-current-node instead?
                                                   current-node newb)
                                             (funcall emote true)
                                             (funcall (arc-action arcb))
                                             (when (funcall hints-on)
                                               (let ((hnt (funcall (node-hint-function newb) true))
                                                     (disp (dgebid "tutor-msg-screen")))
                                                 (remove-children-from-nth disp 0)
                                                 (append-para-text disp (strcat hnt "."))))))
                                     (t (funcall emote false)))))))


                   handle-alternative-states
                   (lambda (input)
                     (with-tutor-state
                         (with-fsm
                             (let* ((inpa (strcat a-prefix input))
                                    (inpb (strcat b-prefix input))
                                    (founda (funcall find-node inpa)) 
                                    (foundb (funcall find-node inpb))) 
                               (cond ((and founda foundb (identical-operands))
                                      (setf prefix a-prefix
                                            current-node founda
                                            initialized true)
                                      (funcall transition input))                                      
                                     ((and founda foundb)
                                      (setf (ps:getprop alternative-states "A") founda
                                            (ps:getprop alternative-states "B") foundb
                                            initialized true)
                                      (funcall transition input))
                                     (founda (setf prefix a-prefix
                                                   current-node founda
                                                   initialized true)
                                             (funcall transition input))                                                    
                                     (foundb (setf prefix b-prefix
                                                   current-node foundb
                                                   initialized true)
                                             (funcall transition input))
                                     (t (funcall emote false)))))))
                   handle-regroup 
                   ;;in-answer
                   (lambda (input )
                     (with-tutor-fsm
                             (labels ((arc-error (i)
                                        (funcall emote false))
                                      (arc-new (a stage2)
                                        (if (not stage2)
                                            (funcall exit-regroup-mode)
                                            (setf regroup-mode stage2))
                                        (funcall emote true)
                                        (funcall (arc-action a))
                                        (funcall set-current-node (arc-to a))))
                               (if (= regroup-mode 1)
                                   (let* ((arc (ps:getprop (node-outputs
                                                            current-node)
                                                           input)))
                                     (if (not arc)
                                         (funcall arc-error input)
                                         (funcall arc-new arc 2)))
                                   (let* ((input (strcat "R_" input))
                                          (arc (ps:getprop (node-outputs
                                                            current-node)
                                                           input)))
                                     (if (not arc)
                                         (funcall arc-error input)
                                         (funcall arc-new arc false)))))))

                   handle-no-regroup
                   ;;in-answer, but not regrouping
                   (lambda (input)
                     (with-tutor-state
                         (with-fsm
                             (let ((arc (ps:getprop (node-outputs
                                                     current-node)
                                                    input)))
                               (if (not arc)
                                   (funcall emote false)
                                   (let* ((gnew (arc-to arc))
                                          (name (dots gnew fsm-node-name)))
                                     (if (string= (subseq name 0 1) "R")
                                         ;; the next node is a "carry"
                                         ;; node, but the user has not
                                         ;; pressed "R", otherwise we
                                         ;; would be in handle-regroup
                                         (funcall emote false)
                                         (progn 
                                           (funcall emote true)
                                           (funcall (arc-action arc))
                                           (funcall set-current-node gnew)))))))))
                   transition (lambda (input rpress)
                                (with-tutor-state
                                    (with-fsm
                                        (if commutative
                                            (if initialized
                                                (cond (in-answer
                                                       (if (string= (fsm-node-name current-node)
                                                                    "completed")
                                                           (funcall handle-completion)
                                                           (if regroup-mode 
                                                               (funcall handle-regroup input)
                                                               (funcall handle-no-regroup input))))
                                                      (prefix (funcall handle-prefix-set input))
                                                      (t (funcall handle-prefix-notset input)))
                                                (funcall handle-alternative-states input))
                                           (funcall handle-non-commutative input rpress)))))
                   add-node (lambda (name)
                              (with-tutor-state 
                                  (with-fsm
                                      (let ((node (ps:create fsm-node-name name
                                                             hint-function false
                                                             inputs (ps:new (-object))
                                                             outputs (ps:new (-object)))))
                                        (setf (ps:getprop nodes name) node)
                                        node))))
                   find-node (lambda (name)
                               (with-tutor-state
                                   (with-fsm
                                       (ps:getprop nodes name))))
                   add-arc (lambda (from-name label to-name action)
                             (with-tutor-state
                                 (with-fsm
                                     (let* ((from (funcall find-node from-name))
                                            (to (funcall find-node to-name)))
                                       (let ((arc (ps:create
                                                   from from
                                                   to to
                                                   label label
                                                   action action)))
                                         (push arc arcs)
                                         (setf (ps:getprop from "next") to-name)
                                         (setf (ps:getprop (node-outputs from) 
                                                           from-name)
                                               ;;this is correct ^^^
                                               ;;because of how we
                                               ;;construct the node
                                               ;;from the user input
                                               arc)
                                         (setf (ps:getprop (node-inputs to) 
                                                           from-name)
                                               arc)
                                         arc)))))
                   set-current-node (lambda (node)
                                      (with-tutor-fsm
                                          (setf current-node node)
                                        (funcall (node-hint-function node)))
                                      (with-hint
                                          (funcall display false)
                                          (when hint
                                            (funcall display true))))
                   arcs (ps:array) 
                   nodes (ps:new (-object)))
              initialized false
              hint false
                                                   
              grid-clear true
              current-tutor-input false)

             tutor-next-step
             (lambda (ev)
               (cancel-bubble ev)
               (let ((dir (id-of this)))
                 (funcall restore-default-rl-handlers
                          (dgebid "previous-step") tutor-next-step
                          (dgebid "next-step") tutor-next-step)
                 (funcall remove-tutor)
                 (if (string= dir "next-step")
                     (funcall next-lesson-text)
                     (funcall previous-lesson-text))))                 

             defarcs ;;returns an array of: digit, current name, current node
             (lambda (op-rst isa)
               (labels ((defarcs-rec (ds)
                          (let* ((curr (aref ds 0))
                                 (next (aref ds 1))
                                 (currname (funcall make-fsm-node-name curr))
                                 (nextname (when next (funcall make-fsm-node-name next))))
                            (if (not-defined next) (progn (defnode currname)
                                                          (when isa (def-completed-arc curr currname))
                                                          (ps:array (aref curr 2) currname curr))
                                (progn
                                  (defnode currname)
                                  (defnode nextname)
                                  (if isa 
                                      (def-answer-arc curr currname next nextname)
                                      (def-printer-arc curr currname next nextname))
                                  (funcall defarcs-rec (subseq ds 1)))))))
                 (funcall defarcs-rec op-rst true)))
             
             defalt
             (lambda (op0 op1)
               (let* ((op0-fst (aref op0 0))
                      (name (funcall make-fsm-node-name op0-fst)))
                 (ps:destructuring-bind (pref (ccol crow) dig)
                     op0-fst
                   (ps:destructuring-bind (ds currname curr)
                       (funcall defarcs op0)
                     (def-start-arc pref name)
                     (with-tutor-state
                         (let* ((opr (dots current-tutor-input operator))
                                (opnode (ps:array pref (ps:array 0 1) opr))
                                (opname (strcat pref "0,1_" opr))
                                (next (aref op1 0))
                                (nextname (funcall make-fsm-node-name next)))
                           (defnode nextname)
                           (defnode opname)
                           (def-printer-arc curr currname opnode opname)
                           (def-printer-arc opnode opname next nextname)
                           (ps:destructuring-bind (lastd lastname last)
                               (funcall defarcs op1)
                             (ps:array last lastname lastd))))))))

             make-noncomm-start-arc-action
             (lambda (to-name)
               (with-tutor-fsm
                   (ps:destructuring-bind (coords dig)
                       (split to-name "_")
                     (ps:destructuring-bind (col row)
                         (split coords ",")
                       (let* ((cell (cell-id col row))
                              (node (funcall find-node to-name))
                              (next (node-next-name node))
                              (nextnode (funcall find-node next)))
                         (lambda ()
                           (funcall place-string-in-selected-cell dig)
                           (funcall  set-current-node nextnode)))))))
                           
                       ;;
                       ;;(funcall focus tocell)))))
             
             make-next-answer-name
             (lambda (spec)
               (ps:destructuring-bind (pref (col row) d)
                   spec
                 (strcat pref col "," row "_" d)))

             make-next-answer-spec
             (lambda (col row d0 d1 a na)
               (let ((test (+ d0 d1)))
                 (cond ((not-defined na) false)
                       ((= test a)
                        (ps:array "" (ps:array (1- col) row) na))
                       ((= a (mod test 10))
                        (ps:array "R_" (ps:array (1- col) (- row 2)) 1))
                       (t (ps:array "" (ps:array col row) na)))))

             defanswer
             (lambda (aspecs)
               (let* ((first (aref aspecs 0))
                      (firstname (funcall make-fsm-node-name first)))
                 (funcall defarcs aspecs true)
                 (ps:array first firstname)))
               
             defplusmachine
             (lambda (opsarr ansarr)
               (ps:destructuring-bind ((n00 n01) (n10 n11))
                   opsarr
                 (defnode "start")
                 ;;(defnode "last-operand-digit")
                 (ps:destructuring-bind (last0 lastname0 lastd0)
                     (funcall defalt  n00 n01)
                   (ps:destructuring-bind (last1 lastname1 lastd1)
                       (funcall defalt  n10 n11)
                     (ps:destructuring-bind (first firstname)
                         (funcall defanswer ansarr)
                       (def-have-operands-arc lastd0 lastname0 firstname)
                       (def-have-operands-arc lastd1 lastname1 firstname)
                       (with-tutor-fsm
                           ;;this is inefficient, but it is easier to
                           ;;do it here, as make-hint-function needs
                           ;;info about tos and froms which are 'only'
                           ;;(ie, 'most easily') available after the
                           ;;nodes have been defined.  If there are
                           ;;(non-network!)  performance problems,
                           ;;fixing this might be a reasonable
                           ;;priority.  A one-pass solution can
                           ;;obviously be done, but why add complexity
                           ;;unless it is really necessary?
                           (ps:for-in 
                            (key nodes)
                            (let ((node (ps:getprop nodes key)))
                              (setf (node-hint-function node)
                                    (funcall make-hint-function node))))
                         (funcall set-current-node 
                               (funcall find-node "start"))))))))

             do-unique-idx-c
             (lambda (digits rows start)
               (with-tutor-fsm
                   (labels ((coll (string-prefix)
                              (let ((tmp (ps:array))
                                    (pref string-prefix))
                                (dolist (row rows tmp)
                                  (let ((idx start)
                                        (collection (ps:array)))
                                    (dolist (d digits 
                                             (progn (unless (string= pref "")
                                                      (setf pref b-prefix))
                                                    (push collection tmp)))
                                      (push (ps:array pref (ps:array idx row) d) 
                                            collection)
                                      (incf idx)))))))
                     (if commutative
                         (funcall coll a-prefix)
                         (funcall coll "")))))                              

             get-commutative-arc-path-specs 
             (lambda (n1 n2) ;;these are string reps of integers
               (let* ((len1 (length n1))
                      (len2 (length n2)))
                 (if (= len1 len2)
                     (ps:destructuring-bind (a0 a1)
                         (funcall do-unique-idx-c n1 (ps:array 0 1) 1)
                       (ps:destructuring-bind (b0 b1)
                           (funcall do-unique-idx-c n2 (ps:array 1 0) 1)
                         (ps:array (ps:array a0 b0)
                                   (ps:array b1 a1))))
                       (let ((longer (if (> len1 len2) n1 n2))
                             (shorter (if (> len1 len2) n2 n1))
                             (gl (max len1 len2))
                             (sl (min len1 len2)))
                         (ps:destructuring-bind (a0 a1)
                             (funcall do-unique-idx-c longer (ps:array 0 1) 1)
                           (ps:destructuring-bind (b0 b1)
                               (funcall do-unique-idx-c shorter 
                                        (ps:array 1 0) 
                                        (1+ (- gl sl)))
                             (ps:array (ps:array a0 b0)
                                       (ps:array b1 a1)))))))) 

             test-set-vars ;;FIXME remove
             (lambda (q a)
               (setf (dots (ps:getprop (dots lesson-module non-ephemeral current-lesson-variables) "sub1:q") value) q
                     (dots (ps:getprop (dots lesson-module non-ephemeral current-lesson-variables) "sub1:q") genfn) q
                     (dots (ps:getprop (dots lesson-module non-ephemeral current-lesson-variables) "sub1:a") value) a
                     (dots (ps:getprop (dots lesson-module non-ephemeral current-lesson-variables) "sub1:a") genfn) a))
                          
             get-minus-answer-specs
             (lambda (op0cfs op1cfs anscfs)
               ;;first build minuend object to record borrowing information
               (let* ((len (length op0cfs))
                      (minuend (ps:new (-array len)))
                      (idx (1- len)))
                      ;;(acc (ps:array)))
                 (loop for i from 0 to idx
                    do (let ((ov (aref op0cfs i)))
                         (setf (aref minuend i)
                               (ps:create ov ov ;;ov = original value,
                                                ;;cv = current value
                                          cv ov))))
                 (let ((minue (nreverse minuend))
                       (sub (nreverse (split op1cfs "")))
                       (ans (nreverse (split anscfs "")))
                       (row (with-grid-dim (1- rows)))
                       (maxcol (with-grid-dim (1- columns))))
                   (labels ((borrow (cidx bidx min bacc)
                              (if (zerop bidx) 
                                  (let* ((targ (aref min bidx))
                                         (cv (ps:getprop targ "cv"))
                                         (cvi (parse-int cv))
                                         (ab (strcat "" (+ 10 cvi))))
                                    (setf (ps:getprop targ "cv") ab)
                                    (push (ps:array "r_" (ps:array cidx (- row 2)) ab) bacc)
                                    bacc)
                                  (let* ((targ (aref min bidx))
                                         (cv (ps:getprop targ "cv"))
                                         (cvi (parse-int cv)))
                                    (cond ((zerop cvi)
                                           (setf (ps:getprop targ "cv") "9")
                                           (push (ps:array "r_" (ps:array (- cidx bidx) (- row 2)) "9") bacc)
                                           (funcall borrow cidx (1- bidx) min bacc))
                                          (t (let ((ab (strcat "" (1- cvi))))
                                               (setf (ps:getprop targ "cv") ab)
                                               (push (ps:array "r_" (ps:array (- cidx bidx) (- row 2)) ab) bacc)
                                               (funcall borrow cidx (1- bidx) min bacc)))))))
                            (spec-rec (col min sub ans acc)
                              (let* ((as (aref ans 0))
                                     (a (if as (parse-int as) false)))
                                (if (not-defined as)
                                    acc
                                    (let* ((m0 (aref min 0))
                                           (mcv (ps:getprop m0 "cv"))
                                           (s0 (aref sub 0))
                                           (test (- (parse-int mcv) (if s0 (parse-int s0) 0))))
                                      (cond ((= test a) ;;must be first
                                             (let ((nc (1- col)))
                                               (push  (ps:array "" (ps:array col row) a) acc)
                                               (funcall spec-rec nc (subseq min 1) (subseq sub 1) (subseq ans 1) acc)))
                                            (t ;;if test != a, we have to borrow
                                             (let ((nc (1- col))
                                                   (btarg (loop for i from 1 to col
                                                             until (not (string= "0" (ps:getprop (aref min i) "cv")))
                                                             finally (return i))))             
                                               (setf acc (append acc (funcall borrow col btarg min (ps:array (ps:array "b_" (ps:array col row) "r") ))))
                                               (funcall spec-rec col min sub ans acc)))))))))
                            (funcall spec-rec maxcol minue sub ans (ps:array))))))
             
             get-plus-answer-specs
             (lambda (op0cfs op1cfs anscfs)
               (let* ((op0 (nreverse (split op0cfs "")))
                      (op1 (nreverse (split op1cfs "")))
                      (ans (nreverse (split anscfs "")))
                      (row (with-grid-dim (1- rows)))
                      (maxcol (with-grid-dim (1- columns))))
                 (labels ((spec-rec (col ds0 ds1 das acc)
                            (let* ((as (aref das 0))
                                   (a (if as (parse-int as) false))
                                   (d0 (aref ds0 0))
                                   (d1 (aref ds1 0))
                                   (test (+ (if d0 (parse-int d0) 0)  (if d1 (parse-int d1) 0))))
                              (cond ((not-defined as) acc)
                                    ((= test a) ;;must be first
                                     (let ((nc (1- col)))
                                       (push  (ps:array "" (ps:array col row) a) acc)
                                       (funcall spec-rec nc (subseq ds0 1) (subseq ds1 1) (subseq das 1) acc)))
                                    ((= a (mod test 10))
                                     (let ((nc (1- col)))
                                       (setf acc (append acc (ps:array (ps:array "" (ps:array col row) a)
                                                                       (ps:array "R_" (ps:array nc (- row 2)) 1))))
                                       (funcall spec-rec nc (subseq ds0 1) (subseq ds1 1) (subseq das 1) acc)))
                                    (t (if (>= test 9) ;;do another carry
                                           (let ((nc (1- col)))
                                             (setf acc (append acc (ps:array (ps:array "" (ps:array col row) a)
                                                                         (ps:array "R_" (ps:array nc (- row 2)) 1))))
                                             (funcall spec-rec nc (subseq ds0 1) (subseq ds1 1) (subseq das 1) acc))
                                           (let ((nc (1- col)))
                                             (push (ps:array "" (ps:array col row) a)
                                                   acc)
                                             (funcall spec-rec nc (subseq ds0 1) (subseq ds1 1) (subseq das 1) acc))))))))
                   (funcall spec-rec maxcol op0 op1 ans (ps:array)))))

             get-noncomm-arc-path-specs
             (lambda (n1 n2)
               (let ((len1 (length n1))
                     (len2 (length n2)))
                 (if (=  len2 len1)
                     (ps:destructuring-bind (a0 a1)
                         (funcall do-unique-idx-c n1 (ps:array 0) 1)
                       (ps:destructuring-bind (b0 b1)
                           (funcall do-unique-idx-c n2 (ps:array 1) 1)
                         (ps:array a0 b0)))
                     ;;len2 is < len1 (if len2 > len1 we have lost ...
                     ;;but that can't happen! really! honestly!)
                     (ps:destructuring-bind (a0 a1)
                         (funcall do-unique-idx-c n1 (ps:array 0) 1)
                       (ps:destructuring-bind (b0 b1)
                           (funcall do-unique-idx-c n2
                                    (ps:array 1) 
                                    (1+ (- len1 len2)))
                         (ps:array a0 b0))))))
                                   
             defminusmachine
             (lambda (opsarr ansarr)
               (defnode "start")
               (ps:destructuring-bind (op0 op1)
                   opsarr
                 (ps:destructuring-bind (last lastname lastd)
                     (funcall defalt op0 op1))
                 (ps:destructuring-bind (first firstname)
                     (funcall defanswer ansarr)
                   (def-have-operands-arc lastd lastname firstname)))
               (with-tutor-fsm
                   (ps:for-in 
                    (key nodes)
                    (let ((node (ps:getprop nodes key)))
                      (setf (node-hint-function node)
                            (funcall make-hint-function node))))
                 (funcall set-current-node 
                          (funcall find-node "start"))))

             make-fsm-node-name
             (lambda (spec)
               (ps:destructuring-bind (pref (x y) d)
                   spec
                 (strcat pref  x "," y "_" d)))
             
             make-fsm-for-tio 
             (lambda (tio)
               (with-tutor-state
                   (with-fsm
                       (let ((opd1 (operand-coefficients (operands-opd1 tio)))
                             (opd2 (operand-coefficients (operands-opd2 tio)))
                             (ans (dots current-tutor-input answer coefficients))
                             (opr (tio-operator tio)))
                         (cond ((string= opr "+")
                                (funcall defplusmachine 
                                         (funcall get-commutative-arc-path-specs 
                                                  opd1 opd2)
                                         (funcall get-plus-answer-specs 
                                                  opd1 opd2 ans)))
                               ((string= opr "-")
                                (setf commutative false)
                                (funcall defminusmachine
                                         (funcall get-noncomm-arc-path-specs
                                                  opd1 opd2)
                                         (funcall get-minus-answer-specs
                                                  opd1 opd2 ans))) ;;HERE
                               (t (alert (strcat "NYI: " opr))))))))
             
             get-tutor-base
             (lambda ()
               (with-slots (current-tutor-input) tutor-state
                 (with-slots (base) current-tutor-input
                   base)))
                 
             math-symbols ;; Var slot
             (ps:create decimal "." times "*")

             make-operand ;;opd == operand
             (lambda (number cs)
               (let* ((len (length cs)))
                 (ps:create
                  decimal-value number
                  coefficients cs
                  length len)))
             
             get-cell-coords-string
             (lambda (cellid)
               (subseq cellid 5 (1- (length cellid))))

             get-cell-coords
             (lambda (cellid)
               (split (funcall get-cell-coords-string cellid) ","))

             make-tutor-input ;;tio == tutor input object 
             (lambda (operation num1 num2 ans base) 
               ;;num1, num2, answer  must be strings in decimal base, base must
               ;;be a number in decimal base               
               (with-slots (current-tutor-input) tutor-state
                 (let* ((b (if base base 10))
                        (n1 (parse-int num1)) ;;FIXME why parse to int??
                        (cs1 (to-string n1 b))
                        (n2 (parse-int num2))
                        (cs2 (to-string n2 b))
                        (nans (parse-int ans))
                        (cans (to-string nans b)))
                   (let ((cti 
                          (ps:create
                           identical-operands (if (string= num1 num2) true false)
                           base b
                           answer (funcall make-operand nans cans)
                           operands (ps:create opd1 (funcall make-operand n1 cs1)
                                               opd2 (funcall make-operand n2 cs2))
                           operator (case operation
                                      ("plus" "+")
                                      ("+" "+")
                                      ("minus" "-")
                                      ("-" "-")
                                      (t (alert (strcat "NYI: " operation)))))))
                     (setf  current-tutor-input cti)
                     cti))))

             create-input-grid-for-sum-tio
             (lambda (tio)
               (let* ((op (tio-operator tio))
                      (opdlen (max (operand-length (operands-opd1 tio))
                                         (operand-length (operands-opd2 tio))))
                      (side (1+ opdlen))
                      (tio-grid (standard-svg-grid "tutor-canvas" 
                                                   "tutor-input-object" 
                                                   3 side  
                                                   create-scratchpad-cell))
                      (holder (parent-node tio-grid))
                      (pdisp (dgebid "tutor-problem-display")))
                 (setf (text-content-of pdisp) 
                       (strcat "$"
                               (operand-coefficients (operands-opd1 tio))
                               op
                               (operand-coefficients (operands-opd2 tio))
                               "$")
                       (ps:getprop tio "dimensions") (ps:create rows 3
                                                                columns side))
                 (mj-hub-queue (array "Typeset" mj-hub pdisp))
                 tio-grid))
                          
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; from scratchpad

             input-direction ;; Var slot done
             (ps:create left-to-right 0 right-to-left 1 down-to-up 2 up-to-down 3
                        selectedv 2 selectedh 0
                        reset (lambda ()
                                (with-input-direction 
                                    (setf left-to-right 0 right-to-left 1 down-to-up 2 up-to-down 3
                                          selectedv 2 selectedh 0))))

             tio-ltr-arr (input-menu-arrow "tio-ltr-arr" 0)
             tio-rtl-arr (input-menu-arrow "tio-rtl-arr" 180)

             set-graphics-for-cell ;; Slot done
             (lambda (cell)
               (let ((cid (id-of cell)))
                 (cond ((string= cid "tio-vline")
                        (let ((cc (first-child cell)))
                          (replace-child cell (funcall tio-vline "tio-vline-graphic") cc)))
                       ((string= cid "tio-hline")
                        (let ((cc (first-child cell)))
                          (replace-child cell (funcall tio-hline "tio-hline-graphic") cc)))
                       ((string= cid "tio-swph")
                        (let ((cc (first-child cell)))
                          (replace-child cell tio-ltr-arr cc))) ;;left-to-right-arrow cc)))
                       ((string= cid "tio-swpv")
                        (let ((cc (first-child cell)))
                          (replace-child cell down-to-up-arrow cc)))
                       (t false))))
             
             tio-grid-keypress ;; Slot done
              (lambda (ev)
                (with-slots (which key-code prevent-default char-code ctrl-key) ev
                  (when (or (not (eql which undefined))
                            (not (= which  0)))
                    (let ((code  which))
                      (switchcc code 
                                48 "tio-0"
                                49 "tio-1"
                                50 "tio-2"
                                51 "tio-3"
                                52 "tio-4"
                                53 "tio-5"
                                54 "tio-6"
                                55 "tio-7"
                                56 "tio-8"
                                57 "tio-9"
                                
                                42 "tio-*"
                                43 "tio-+"
                                45 "tio--"
                                58 "tio-:"
                                
                                46 "tio-dec"

                                114 "tio-str"
                                
                                118 "tio-vline"
                                104 "tio-hline"

                                95 "tio-eq" ;;_ underline
                                13 "tio-enter" ;;return send
                                116 "tio-confirm" ;;t confirm
                                ))))) ;;FIXME need 'q' for quit

              tio-grid-keydown ;; done
              (lambda (ev)
                (with-slots (which key-code char-code prevent-default ctrl-key) ev
                  (when (or (not (eql which undefined)) (not (= which 0)))
                    (let ((code which))
                      (switchcc code
                                46 "tio-dels"
                                8 "tio-de"
                                40 (funcall (lambda () (with-slots 
                                                             (selectedv up-to-down)
                                                           input-direction
                                                         (if (= selectedv up-to-down)
                                                             "tio-movv"
                                                             "tio-swpv"))))
                                37 (funcall (lambda () (with-slots 
                                                             (selectedh right-to-left left-to-right)
                                                           input-direction
                                                         (if (= selectedh right-to-left)
                                                             "tio-movh"
                                                             "tio-swph"))))
                                39 (funcall (lambda () (with-slots 
                                                             (selectedh right-to-left left-to-right)
                                                           input-direction
                                                         (if (= selectedh left-to-right)
                                                             "tio-movh"
                                                             "tio-swph"))))
                                38 (funcall (lambda () (with-slots 
                                                             (selectedv down-to-up)
                                                           input-direction
                                                         (if (= selectedv down-to-up)
                                                             "tio-movv"
                                                             "tio-swpv"))))
                                )))))
              click-on-id ;; Slot done
              (lambda (id)
                (let ((event (doc-create-event "MouseEvents"))
                      (cell (doc-get-el-by-id id)))
                  (init-event event "click" false false)
                  (when cell (dispatch-event cell event))))

              add-keyevent-handlers-for-tio-grid ;; Slot done
              (lambda ()
                (add-event-no-capture document "keypress" tio-grid-keypress)
                (add-event-no-capture document "keydown" tio-grid-keydown))

              insert-decimal-point-in-selected-cell ;;done
              (lambda (ev)
                (funcall place-string-in-selected-cell 
                         (dots math-symbols decimal)))
              
              insert-vline-in-selected-cell ;;done
              (lambda (ev)
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

              insert-hline-in-selected-cell ;;done
              (lambda (ev)
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

              swaph-input-direction ;; done
              (lambda ()
                (with-slots (selectedh left-to-right right-to-left) input-direction
                  (if (eql selectedh left-to-right)
                      (let ((menuitem (doc-get-el-by-id "tio-swph")))
                        (setf selectedh right-to-left)
                        (replace-child menuitem tio-rtl-arr ;right-to-left-arrow 
                                       (first-child menuitem)))
                      (let ((menuitem (doc-get-el-by-id "tio-swph")))
                        (setf selectedh left-to-right)
                        (replace-child menuitem tio-ltr-arr ;left-to-right-arrow 
                                       (first-child menuitem))))))
              
              swapv-input-direction ;; done
              (lambda ()
                (with-slots (selectedv down-to-up up-to-down) input-direction
                  (if (eql selectedv down-to-up)
                      (let ((menuitem (doc-get-el-by-id "tio-swpv")))
                        (setf selectedv up-to-down)
                        (replace-child menuitem up-to-down-arrow 
                                       (first-child menuitem)))
                      (let ((menuitem (doc-get-el-by-id "tio-swpv")))
                        (setf selectedv down-to-up)
                        (replace-child menuitem down-to-up-arrow 
                                       (first-child menuitem))))))
              
              insert-this-string ;; done
              (lambda (ev)
                (let ((string (subseq (text-content-of this) 0 1)))
                  (funcall place-string-in-selected-cell string)))

              feed-self-insert-input-to-tutor
              (lambda (input)
                (with-tutor-state
                    (with-fsm
                        (with-slots (selectedh left-to-right right-to-left) 
                            input-direction
                          (when (eql selectedh (if in-answer 
                                                   left-to-right 
                                                   right-to-left))
                            (funcall swaph-input-direction)))
                      (funcall transition input))))

              make-self-insert-input-for-tutor ;; done
              (lambda (ev)
                (with-tutor-state
                    (with-fsm
                        (let* ((string (subseq (text-content-of this) 0 1))
                               (sc selected-cell)
                               (coords (funcall get-cell-coords sc))
                               (cx (cell-x coords))
                               (cy (cell-y coords))
                               (input (strcat cx "," cy "_" string)))
                          (funcall feed-self-insert-input-to-tutor input)))))

              insert-answer-line
              (lambda ()
                (let* ((arow (with-grid-dim (1- rows)))
                       (cols (with-grid-dim columns))
                       (g0 (doc-get-el-by-id (strcat "cell(" 0 "," arow ")")))
                       (rect0 (first-child g0))
                       (xem (get-attribute rect0 "x"))
                       (yem (get-attribute rect0 "y"))
                       (x2 (parse-float xem))
                       (w (parse-float (get-attribute rect0 "width")))
                       (x2em (strcat (+ (* w cols) x2) "em"))
                       (line (svg-create-element "line")))
                  (set-attributes line 
                                  "x1" xem "y1" yem
                                  "x2" x2em "y2" yem
                                  "stroke" "#000000"
                                  "stroke-width" "0.125em")
                  (append-child g0 line)))

              place-carry-digit-in-cell 
              (lambda (carrystr cell)
                ;;should cell be string or object?? Object for now 
                (let* ((style "") 
                       (t1 (svg-create-element "text"))
                       (tspan (svg-create-element "tspan"))
                       (txt (make-text-node carrystr)) 
                       (dxem "0.05em")
                       (dyem "0.9em")
                       (rects (get-elements-by-tag-name cell "rect"))
                       (els (length rects))
                       (rect (aref rects (1- els)))
                       (xem (get-attribute rect "x"))
                       (yem (get-attribute rect "y")))
                  (set-attributes tspan "dx" dxem "dy" dyem
                                  "style" "font-size:70%")
                  (append-child tspan txt)
                  (set-attributes t1 "style" style "x" xem "y" yem)
                  (append-child cell t1)
                  (append-child t1 tspan)))
  
              place-string-in-selected-cell
              (lambda (string stay)
                (with-slots (grid-clear) tutor-state
                  (setf grid-clear false)
                  (let* ((cell (dgebid selected-cell)) ;;HERE
                         (existing (aref (get-elements-by-tag-name cell "text") 0))
                         (txtcnt (if existing (text-content-of existing) false)))
                    (cond ((= input-chars 1)
                           (funcall place-single-digit-string 
                                    cell existing string stay))
                          (t (funcall place-multi-digit-string 
                                      cell existing txtcnt string stay))))))

              place-multi-digit-string 
              (lambda (cell existing txtcnt string stay)
                (with-slots (selectedh left-to-right right-to-left) input-direction
                  (let* ((newtxt (if (= selectedh left-to-right)
                                     (strcat (if txtcnt txtcnt "") string)
                                     (strcat string (if txtcnt txtcnt ""))))
                         (newln (length newtxt))
                         (max input-chars))
                    (if existing
                        (let ((tspan (aref 
                                      (get-elements-by-tag-name existing "tspan") 0)))
                          (set-attributes tspan "dx" (funcall dxshift max)
                                          "style" (funcall font-size max false))
                          (cond ((= newln max)
                                 (setf (text-content-of tspan) newtxt
                                       input-chars 1)
                                 (unless stay
                                   (funcall click-next-cell-if-there cell)))
                                (t (setf (text-content-of tspan) newtxt))))
                        (let* ((style "") (t1 (svg-create-element "text"))
                               (tspan (svg-create-element "tspan"))
                               (txt (make-text-node string)) (dxem (funcall dxshift max))
                               (dyem "1.6em")
                               (rects (get-elements-by-tag-name cell "rect"))
                               (els (length rects))
                               (rect (aref rects (1- els)))
                               (xem (get-attribute rect "x"))
                               (yem (get-attribute rect "y")))
                          (set-attributes tspan "dx" dxem "dy" dyem
                                          "style" (funcall font-size max false))
                          (append-child tspan txt)
                          (set-attributes t1 "style" style "x" xem "y" yem)
                          (append-child cell t1)
                          (append-child t1 tspan))))))

              dxshift ;; Slot
              (lambda (incharn)
                (cond ((= incharn 1) "0.4em")
                      ((= incharn 2) "0.25em")
                      (t "0.1em")))
              
              font-size ;; Slot
              (lambda (incharn power)
                (cond ((= incharn 1) "font-size:95%")
                      ((= incharn 2) "font-size:80%")
                      (t (if power "font-size:70%" "font-size:80%"))))
                            
              place-single-digit-string
              (lambda (cell existing string stay)
                (when existing 
                  ;;in tutor: trying to overwrite/delete an input
                  ;;which is good! deny
                  (funcall delete-string-from-selected-cell))
                (let* ((t1 (svg-create-element "text"))
                       (tspan (svg-create-element "tspan"))
                       (txt (make-text-node string))
                       (rects (get-elements-by-tag-name cell "rect"))
                       (els (length rects))
                       (rect (aref rects (1- els)))
                       (xem (get-attribute rect "x"))
                       (yem (get-attribute rect "y"))
                       (carry (> (length string) 1))
                       (dxem (if carry "0.5em" "0.4em")) (dyem (if carry "1.4em" "1.25em")))
                  (set-attributes tspan "dx" dxem "dy" dyem)
                  (when carry (set-attribute tspan "style" "font-size:80%"))
                  (when (string= string (dots math-symbols decimal)
                                 (set-attribute tspan "font-size" "1.1em")))
                  (append-child tspan txt)
                  (set-attributes t1 "x" xem "y" yem)
                  (append-child cell t1)
                  (append-child t1 tspan)
                  (unless stay
                    (funcall click-next-cell-if-there cell))
                  t1))       
              
              delete-string-from-selected-cell ;; Slot
              (lambda ()
                (when selected-cell
                  (let ((cell (doc-get-el-by-id selected-cell)))
                    (remove-child cell (last-child cell)))))
              ;;FIXME remove-children-from-nth     
              
              click-next-cell-if-there ;; done
              (lambda (cc)
                (let ((event (doc-create-event "MouseEvents")))
                  (init-event event "click" false false)
                  (with-slots (selectedh left-to-right right-to-left) 
                      input-direction
                    (let ((next (if (eql selectedh left-to-right)
                                    (next-sibling cc)
                                    (previous-sibling cc))))
                      (when next
                        (dispatch-event (aref (get-elements-by-tag-name next "rect") 0)
                                        event))))))
              
              click-vertical-neighbour-if-there ;; done
              (lambda (id)
                (let* ((arr2 (split (aref (split 
                                           (aref (split id (ps:regex "\\(")) 1)
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
              

              delete-content-of-selected-cell ;; done
              (lambda (ev)
                (when selected-cell
                  (let ((cell (doc-get-el-by-id selected-cell)))
                    (setf (dots  (aref (get-elements-by-tag-name cell "rect") 0)
                                 style fill) "")
                    (prune-tree-from-nth cell 1)
                    (funcall click-next-cell-if-there cell))))

              delete-content-of-selected-cell-stay ;; done
              (lambda (ev)
                (when selected-cell
                  (let ((cell (doc-get-el-by-id selected-cell)))
                    (setf (dots  (aref (get-elements-by-tag-name cell "rect") 0)
                                 style fill) "")
                    (prune-tree-from-nth cell 1))))

              move-to-adjacent-cell ;; done
              (lambda (ev)
                (when selected-cell
                  (let ((cell (doc-get-el-by-id selected-cell)))
                    (funcall click-next-cell-if-there cell))))

              move-to-vertical-neighbour ;; done
              (lambda (ev)
                (when selected-cell
                  (funcall click-vertical-neighbour-if-there selected-cell)))

              enter-regroup-mode
              (lambda (ev)
                (with-tutor-state
                    (with-fsm
                        (when in-answer
                          (setf regroup-mode 1))
                      (when (not commutative)
                        (funcall transition (fsm-node-name current-node) true)))))

              exit-regroup-mode
              (lambda ()
                (with-tutor-state
                    (with-fsm
                        (when in-answer
                          (setf regroup-mode false)))))
              
              get-input-character-number ;; Slot
              (lambda ()
                (let ((n (subseq (text-content-of 
                                  (doc-get-el-by-id "tio-str")) 1 2)))
                  (if n 
                      (parse-int n)
                      false)))

              add-event-handler-for-cell ;; Slot done
              (lambda (cell)
                (with-slots (ps-next) problem-stack
                (let ((cid (id-of cell)))
                  (cond ((string= cid "tio-nop") false)
                        ((string= cid "tio-dec") (add-event-no-capture cell "click" insert-decimal-point-in-selected-cell))
                        ((string= cid "tio-vline") (add-event-no-capture cell "click" insert-vline-in-selected-cell))
                        ((string= cid "tio-hline") (add-event-no-capture cell "click" insert-hline-in-selected-cell))
                        ((string= cid "tio-de") (add-event-no-capture cell "click" delete-content-of-selected-cell))
                        ((string= cid "tio-dels") (add-event-no-capture cell "click" delete-content-of-selected-cell-stay))
                        ((string= cid "tio-nxt") (add-event-no-capture cell "click" tutor-input-next))
                        ((string= cid "tio-swph") (add-event-no-capture cell "click" swaph-input-direction))
                        ((string= cid "tio-swpv") (add-event-no-capture cell "click" swapv-input-direction))
                        ((string= cid "tio-movh") (add-event-no-capture cell "click" move-to-adjacent-cell))
                        ((string= cid "tio-movv") (add-event-no-capture cell "click" move-to-vertical-neighbour))
                        ((string= cid "tio-str") (add-event-no-capture cell "click" enter-regroup-mode))
                        (t (add-event-no-capture 
                            cell "click"
                            make-self-insert-input-for-tutor))))))
              
              tutor-input-next
              (lambda (ev)
                (alert (strcat "NYI: " (id-of this))))
              
              make-menu-row ;;done
              (lambda ()
                (let ((menurow (div-class "menu-row")))
                  menurow))
              
              make-menu-entry-item ;;done
              (lambda (txt id glass)
                (let* ((did (strcat "tio-" id))
                       (cllist (let ((ret ""))
                                 (dolist (tx (split glass " ") ret)
                                   (setf ret (strcat "tio-" tx " " ret)))))
                       (ei (div-id-class did cllist)))
                  (append-text ei txt)
                  ei))

              tio-grid-menu false ;;done
              
              make-tio-grid-menu ;;done
               (lambda (menu-level)
                 (let ((menu (doc-get-el-by-id "tutor-input-menu")))
                   (funcall build-menu-for-menu-level menu-level)
                   (when menu
                     (dolist (mr tio-grid-menu)
                       (let ((menurow (funcall make-menu-row)))
                         (dolist (item mr)
                           (append-child menurow 
                                         (funcall make-menu-entry-item 
                                                  (aref item 0)
                                                  (aref item 1)
                                                  (aref item 2))))
                         (append-child menu menurow))))))

               build-menu-for-menu-level ;;done
               (lambda (menu-level)
                 (let ((spec (ps:array))
                       (max (1+ menu-level)))
                   (dolist (access '(config movement self-insert))
                     (let ((partspec (ps:getprop tio-grid-menu-building-blocks
                                                 access))
                           (tmp (ps:array)))
                       (dotimes (n max)
                         (let ((found (ps:getprop partspec n)))
                           (if found
                               (setf tmp (append found tmp))
                               ps:break)))
                       (setf spec (append tmp spec))))
                   (setf tio-grid-menu (nreverse spec))))
               
               tio-grid-menu-building-blocks ;;done
               menu-elements

               ;; (ps:create 
               ;;  config 
               ;;  (ps:create
               ;;   0 '((("R" "str" "even text config")
               ;;        ("" "" "odd text config")
               ;;        ("" "" "x config"))))
               ;;  ;;       ("clear" "clr" "odd text config")
               ;;  ;;       ("?" "?" "x config")))
                 
               ;;  ;;  )
               ;;  self-insert
               ;;  (ps:create
               ;;   0 '((("0" "0" "even self-insert")
               ;;        ("1" "1" "odd self-insert")
               ;;        ("2" "2" "x self-insert"))
               ;;       (("3" "3" "even self-insert")
               ;;        ("4" "4" "odd self-insert")
               ;;        ("5" "5" "x self-insert"))
               ;;       (("6" "6" "even self-insert")
               ;;        ("7" "7" "odd self-insert")
               ;;        ("8" "8" "x self-insert"))
               ;;       (("9" "9" "even self-insert")
               ;;        ("+" "+" "odd self-insert")
               ;;        ("-" "-" "x self-insert")))
               ;;   1 '((("*" "*" "even self-insert")
               ;;        ("hline" "hline" "odd self-insert")
               ;;        ("." "." "x self-insert")))
               ;;   )
               ;;  movement
               ;;  (ps:create
               ;;   0 '((("-->" "swph" "even movement")
               ;;        ("^" "swpv" "odd movement")
               ;;        ("Nxt" "nxt" "x movement"))
               ;;       (("MV H" "movh" "even movement")
               ;;        ("Mv V" "movv" "odd movement")
               ;;        ("" "nop" "x movement")))
                 
                 
               ;;   )
               ;;  delquit
               ;;  (ps:create
               ;;   0 '((("Del Mv" "de" "even text delete")
               ;;        ("Del" "dels" "odd text delete")
               ;;        ("" "nop" "x text delete")))
                 
                 
               ;;   )
               ;;  ;; entry
               ;;  ;; (ps:create
               ;;  ;;  0 '((("Underline" "eq" "even text entry")
               ;;  ;;       ("Enter" "enter" "odd text entry")
               ;;  ;;       ("Confirm" "confirm" "x text entry ")))
                 
                 
                 
               ;;  ;;  )
               ;;  )
               
               
              ;;;;;;;;;;;;;;;;;;;;;;;;;; end from scratchpad
               
               make-tutor-display
               (lambda ()
                 (let ((tutd (div-id "tutor-display"))
                       (ttop (div-id-class "tutor-top" "tutor-top"))
                       (ic (div-id-class "tutor-ic" "tutor-top-item tutor-right"))
                       (icnb (div-id "tutor-ic-new"))
                       (icsum (div-id "tutor-ic-sum"))
                       (ich (div-id "tutor-ic-hints"))
                       (hints (funcall make-hints))
                       
                       (faceh (div-id-class "tutor-face-holder" "tutor-left"))
                       (face (funcall make-face))
                                              
                       (msg (div-id-class "tutor-msg-holder" "tutor-bottom"))
                       (msgscr (div-id-class "tutor-msg-screen" 
                                             "tutor-bottom-item")))
                   (append-child ich hints)
                   (append-children ic icnb icsum ich)
                   (append-child faceh face)
                   (append-children ttop ic faceh)
                   (append-child msg msgscr)

                   (append-children tutd ttop msg)
                   tutd))

               make-hints
               (lambda ()
                 (let ((hintsdiv (div-id "select-hint"))
                       (nodiv (div-id "no-hints"))
                       (offbutt (create-radiobutton "hints-off"
                                                    :handler ("click" toggle-hints)
                                                    :control-name "selhint"))
                       (ondiv (div-id "yes-hints"))
                       (onbutt (create-radiobutton "hints-on"
                                                   :handler ("click" toggle-hints)
                                                   :control-name "selhint")))                       
                   (setf (checked offbutt) true)
                   (append-text nodiv "Hints Off")
                   (append-text ondiv "Hints On")
                   (append-child nodiv offbutt)
                   (append-child ondiv onbutt)
                   (append-children hintsdiv nodiv ondiv)
                   hintsdiv))
               
               insert-tutor-display
               (lambda ()
                 (let ((ltd (dgebid "lesson-text-display"))
                        (tutd (funcall make-tutor-display)))                       
                   (remove-children-from-nth ltd 0)
                   (append-child ltd tutd)))

               svg-canvas
               (ps:create ;;heightem "9em" widthem "9em"
                height 9 width 9
                canvas-height 12 canvas-width 12)
               
               make-face
               (lambda ()
                 (with-slots (width height canvas-height canvas-width) svg-canvas
                   (let* ((canvas (svg-create-element "svg"))
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
                     canvas)))
               
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
               
               be-neutral
               (lambda ()
                 (let* ((old-mouth (dgebid "mouth"))
                        (parent (parent-node old-mouth))
                        (new-mouth (funcall neutral-mouth)))
                   (replace-child parent new-mouth old-mouth)))
               
               emote
               (lambda (happy)
                 (with-slots (width height canvas-width canvas-height) svg-canvas
                   (let* ((mouth (doc-get-el-by-id "mouth"))
                          (parent (parent-node mouth))
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
                     smile)))

               initialize-tutor
               (lambda (tio)
                 (with-slots (initialized grid-clear current-tutor-input)
                     tutor-state
                   (prepare-input-menu-holder "tutor-menu-holder" "tutor-input-menu"
                                              "tutor-menu" "Tutor Menu")
                   (prepare-calced-canvas "tutor-canvas" "svg-container tutor-grid"
                                          "tutor-msg" "scratch-pad-info tutor-info"
                                          "tutor-problem-display" "scratch-pad-info")
                   (funcall add-keyevent-handlers-for-tio-grid)
                   (funcall make-tio-grid-menu 0) 
                   (setf current-tutor-input tio)
                   (funcall remove-default-rl-handlers)
                   (add-event-no-capture (dgebid "next-step") "click" tutor-next-step)
                   (add-event-no-capture (dgebid "previous-step") "click" tutor-next-step)
                   (let* ((inpmenu (doc-get-el-by-id "tutor-input-menu"))
                          (inpmenu-holder (doc-get-el-by-id  "tutor-menu-holder"))
                          (pdisp (dgebid "tutor-problem-display"))
                          (mrows (children-of inpmenu))
                          (layer (1+ grey-out-z-index)))
                     (add-to-class-list "svg-canvas" "tutor-grid")
                     (dolist (row mrows) 
                       (dolist (inp (children-of row))
                         (funcall add-event-handler-for-cell inp)
                         (funcall set-graphics-for-cell inp)))
                     (set-display inpmenu-holder "table")
                     (funcall create-input-grid-for-sum-tio tio)
                     (funcall make-fsm-for-tio tio) ;;FSM constructed here
                     (let* ((tio-grid (dgebid "tutor-input-object"))
                            (tio-grid-holder (parent-node tio-grid)))
                       (set-z-index inpmenu-holder layer)
                       (set-z-index pdisp layer)
                       (set-z-index tio-grid layer)
                       (set-z-index tio-grid-holder layer)
                       (funcall focus "cell(0,0)")
                       (setf initialized true)))))
               
               summon-tutor
               (lambda (tio)
                 (with-slots (initialized grid-clear current-tutor-input)
                     tutor-state
                   (if (not initialized)
                       (funcall initialize-tutor tio)
                     (progn
                       (setf current-tutor-input tio)
                       (when (not grid-clear)
                         (funcall clear-tio-grid))))))

               clear-tio-grid
               (lambda ()
                 (with-slots (grid-clear) tutor-state
                   (let* ((tg (dgebid "tutor-input-object"))
                          (texts (html-collection-to-array
                                  (gebtag tg "text"))))
                     (dolist (txt texts)
                       (let ((parent (parent-node txt)))
                         (remove-child parent txt))))
                     (setf grid-clear true)))                     

               tutor-show-summary
               (lambda (ev)
                 (let ((found (dgebid "tutor-menu-holder")))
                   (when found (funcall remove-tutor))
                   (funcall show-summary false true)))

               tutor-lesson-quit
               (lambda (ev)
                 (let ((found (dgebid "tutor-menu-holder")))
                   (when found (funcall remove-tutor))
                   (funcall quit-early)))
               
               remove-tutor
               (lambda ()
                 (let ((mholder (dgebid "tutor-menu-holder")))
                   (when mholder
                     (let* ((mparent (parent-node mholder))
                            (canvas (dgebid "tutor-canvas"))
                            (cparent (parent-node canvas)))
                       (remove-child mparent mholder)
                       (remove-child cparent canvas)
                       (remove-event-no-capture document "keypress" tio-grid-keypress)
                       (remove-event-no-capture document "keydown" tio-grid-keydown))
                     (with-tutor-state (funcall reset))
                     true)))

               focus ;; Slot
               (lambda (onme)
                 (labels ((colorise (fstr)
                            (let ((rect (aref (get-elements-by-tag-name 
                                               (doc-get-el-by-id (if selected-cell
                                                                     selected-cell
                                                                     onme))
                                               "rect") 0)))
                              (set-attribute rect "fill" fstr))))
                   (funcall colorise (with-tutor-state plain-color))
                   (setf selected-cell onme)
                   (funcall click-on-selected-cell false)
                   (funcall colorise (with-tutor-state selected-cell-color))))
                   
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
               
               make-minusmachine-start-hint
               (lambda (node)
                   ;;only one node to find so this is ok
                 (let ((outs (node-outputs node))
                       (name))
                   (ps:for-in (key outs)
                              (setf name (fsm-node-name
                                          (arc-to 
                                           (ps:getprop outs key)))))
                   (ps:destructuring-bind (coords dig)
                       (split name "_")
                     (let* ((color0 (with-hint (with-colors color0-color)))
                            (color0-name (with-hint (with-colors color0-name)))
                            (txt-el (with-words (create-para-text writea)))
                            (span0 (create-span-text dig))
                            (fintxt (with-words 
                                        (strcat inthe color0-name 
                                                square period))))
                       (set-color span0 color0)
                       (append-child txt-el span0) 
                       (append-text txt-el fintxt)
                       (lambda () (with-hint (setf text-element txt-el
                                                   cell0 (mkcn coords))))))))
                       
                   
                   ;;"NYI: minusmachine-start-hint hint"))

               make-plusmachine-start-hint
               (lambda (node)
                 (ps:destructuring-bind (name0 name1)
                     (let ((names (ps:array))
                           (outs (dots node outputs)))
                       (ps:for-in (key outs)
                                  (push (fsm-node-name
                                         (arc-to 
                                          (ps:getprop outs key))) names))
                       names)
                   (ps:destructuring-bind (ig co0 d0)
                       (split name0 "_")
                     (ps:destructuring-bind (ig co1 d1)
                         (split name1 "_")
                       (let* ((color0 (with-hint (with-colors color0-color)))
                              (color0-name (with-hint (with-colors color0-name)))
                              (color1 (with-hint (with-colors color1-color)))
                              (color1-name (with-hint (with-colors color1-name)))
                              (txt-el (with-words (create-para-text writea)))
                              (span0 (create-span-text d0))
                              (span1 (create-span-text d1)))
                         (set-color span0 color0)
                         (set-color span1 color1)
                         (append-child txt-el span0)
                         (cond ((string= co0 co1)
                                (let ((fintxt (with-words 
                                                  (strcat inthe color0-name 
                                                          square period))))
                                  (with-words 
                                      (when (not (string= d0 d1))
                                        (append-text txt-el ora)
                                        (append-child txt-el span1))
                                    (append-text txt-el fintxt)
                                  (lambda () (with-hint (setf text-element txt-el
                                                              cell0 (mkcn co0))))
                                  )))
                               (t (let ((col0txt (with-words (strcat inthe color0 
                                                                     square ora)))
                                        (fintxt (with-words (strcat inthe color1-name 
                                                                    square period))))
                                    (append-text txt-el col0txt)
                                    (append-child txt-el span1)
                                    (append-text txt-el fintxt)
                                    (lambda () (with-hint (setf text-element txt-el
                                                                cell0 (mkcn co0)
                                                                cell1 (mkcn co1))))
                                    ))))))))

               make-completed-hint
               (lambda (node) ;;don't use node yet
                 (let* ((ans (with-tutor-state
                                (dots current-tutor-input
                                      answer coefficients)))
                       (p (create-para-text (strcat "Sum correct! " ans))))
                   (lambda ()
                     (with-hint (setf text-element p
                                      cell0 false
                                      cell1 false)
                       true))))

               make-regroup-hint-function 
               (lambda (narr)
                  (ps:destructuring-bind (pref coords d)
                      narr
                    (let ((txt-el (create-para-text 
                                   (strcat "Write the tens digit of the sum you did: "
                                           d))));;FIXME
                      (lambda ()
                        (with-hint (setf cell0 false
                                         cell1 false
                                         text-element txt-el))))))

               make-minus-regroup-hint-function 
               (lambda (node narr)
                  (ps:destructuring-bind (pref coords d)
                      narr
                    (let ((name)
                          (cell (mkcn coords))
                          (borrow false)
                          (ins (node-inputs node)))
                      (ps:for-in (key ins)
                                 (setf name (fsm-node-name
                                             (arc-from
                                              (ps:getprop ins key)))))
                      (when (string= (subseq name 0 1) "b")
                        (setf borrow true))
                      (lambda ()
                        (let* ((cc (dgebid cell))
                               (txtcnt (text-content-of (aref (get-elements-by-tag-name cc "text") 0)))
                               (txt-el (if borrow 
                                           (create-para-text 
                                            (strcat "Regroup by reducing " 
                                                    txtcnt 
                                                    " by 1, so that you can add 10 to the next digit. Write " d "."))
                                           (create-para-text
                                            (with-tutor-fsm 
                                                (if (string= txtcnt "0")
                                                    (if (string= coords borrower-target)
                                                        ;;"If you are finished with regrouping, write 10. If you are not finished, write 9, so you can add 10 to the next digit."
                                                        "You are finished with regrouping, write 10."
                                                        "You are not finished with regrouping.  You add 10 to 0 and get 10, but regroup by reducing it to 9, so you can add 10 to the next digit. Write 9.")
                                                (strcat "You are finished with regrouping. Add 10 to " txtcnt ". Write " d ".")))))))
                          (with-hint (setf cell0 false
                                           cell1 false
                                           text-element txt-el)))))))
               
               make-in-plus-sum-hint
               (lambda (node nodename)
                 (let ((narr (split nodename "_")))
                   (cond ((= (length narr) 2)
                          (if (string= (aref narr 1) "start")
                              (funcall make-no-hint node narr)
                              (funcall make-answer-hint-function node narr)))
                         ((string= (aref narr 0) "R")
                          (funcall make-regroup-hint-function narr))
                         (t
                           (funcall make-operands-hint-function narr)))))

               make-in-minus-sum-hint
               (lambda (node nodename)
                 (let ((narr (split nodename "_")))
                   (cond ((= (length narr) 2)
                          (if (string= (aref narr 1) "start")
                              (funcall make-no-hint node narr)
                              (funcall make-operands-hint-function narr)))
                         ((string= (aref narr 0) "r") ;;FIXME
                          (funcall make-minus-regroup-hint-function node narr))
                         ((string= (aref narr 0) "b")
                          (funcall make-borrow-hint-function node narr))
                         (t
                          (funcall make-operands-hint-function narr)))))
               
               make-borrow-hint-function
               (lambda (node narr)
                 (ps:destructuring-bind (pref coords d)
                     narr
                   (let ((txt-el (create-para-text 
                                  (strcat "You need to regroup, so press: "
                                          d "."))))
                     (lambda ()
                       (with-hint (setf cell0 false
                                        cell1 false
                                        text-element txt-el))))))
               
               make-no-hint
               (lambda (node narr)
                 (let ((txt-el (create-para-text "No hint for start node")))
                   (lambda ()
                     (with-hint (setf cell0 false
                                      cell1 false
                                      text-element txt-el)))))
               
               make-answer-hint-function
               (lambda (node narr)
                 (ps:destructuring-bind (coords d)
                     narr
                   (let* ((cn (mkcn coords))
                          ;(tonode (arc-to (node-outputs node)))
                          (to-r (string= (aref (split (node-next-name node) "_")
                                               0)
                                         "R"))
                          (txt-el (create-para-text
                                   (strcat "Add the digits above your position." 
                                           (if to-r "  Press 'r' and write the units digit of the sum." 
                                               "  Write the sum.")))))
                     (lambda ()
                       (with-hint 
                           (setf cell0 false
                                 cell1 false
                                 text-element txt-el))))))

               make-operands-hint-function
               (lambda (narr)
                 (with-tutor-fsm
                     (if commutative
                         ;; (ps:destructuring-bind (pref coords d)
                         ;;     narr
                         ;;   (let ((txt-el (create-para-text 
                         ;;                  (strcat "Write "
                         ;;                          d "."))));;FIXME
                         ;;     (lambda ()
                         ;;       (with-hint (setf cell0 false
                         ;;                        cell1 false
                         ;;                        text-element txt-el)))))
                         (ps:destructuring-bind (pref coords d)
                             narr
                           (let* ((txt (strcat "Write " d))
                                  (txt-el (create-para-text 
                                           (strcat txt "."))))
                             (lambda (return-string)
                               (if return-string txt
                                   (with-hint (setf cell0 false
                                                    cell1 false
                                                    text-element txt-el))))))
                         (ps:destructuring-bind (coords d)
                             narr
                           (let ((atxt-el (create-para-text 
                                           (strcat "Do a subtraction of the digits above your position: first digit - second digit. Write "
                                                      d ".")))
                                 (txt-el (create-para-text 
                                          (strcat "Write "
                                                  d "."))))
                             (lambda ()
                               (with-hint (setf cell0 false
                                                cell1 false
                                                text-element (if (with-tutor-fsm in-answer) atxt-el txt-el)))))))))
               make-hint-function
               (lambda (node)
                 (let* ((opr (with-tutor-state
                                 (tio-operator current-tutor-input)))
                        (nodename (fsm-node-name node)))
                   (cond ((string=  opr "+")
                          (cond ((string= nodename "start")
                                 (funcall make-plusmachine-start-hint node))
                                ((string= nodename "completed")
                                 (funcall make-completed-hint node))
                                (t (funcall make-in-plus-sum-hint node nodename))))
                         ((string= opr "-")
                          (cond ((string= nodename "start")
                                 (funcall make-minusmachine-start-hint node))
                                ((string= nodename "completed")
                                 (funcall make-completed-hint node))
                                (t (funcall make-in-minus-sum-hint node nodename))))
                         (t (alert (strcat "NYI: hints for opr: " opr))))))

               make-nyi-hint
               (lambda (node)
                 (let ((txt-el (create-para-text (strcat "No hint for node: "
                                                         (fsm-node-name node)))))
                   (lambda ()
                     (with-hint (setf cell0 false
                                      cell1 false
                                      text-element txt-el)))))
               
               toggle-hints
               (lambda (ev)
                 (with-hint
                     (funcall display false)
                   (if (string= (id-of this)
                                "hints-on")
                       (setf hint true)
                       (setf hint false))
                   (when hint (funcall display true))))
               
               tutor-new-problem
               (lambda (ev)
                 (let* ((opr (with-tutor-state (tio-operator current-tutor-input)))
                        (vn (cond ((string= opr "+") "plus1:q")
                                  ((string= opr "-") "sub1:q")
                                  (t (alert (strcat "NYI: operator: " opr))))))
                   (funcall remove-tutor)
                   (funcall reset-var vn (funcall use-var vn) true)))

                 ;; (funcall summon-tutor-for-problem 
                 ;;          (funcall use-var "plus1:q")
                 ;;          (funcall use-var "plus1:a"))
                 ;; )
                 
               summon-tutor-for-problem
               (lambda (q a)
                 (let* ((op (funcall get-inline-term q 1))
                        (n1 (funcall get-inline-term q 0))
                        (n2 (funcall get-inline-term q 2)))
                   (funcall insert-tutor-display)
                   (let ((sd (dgebid "tutor-ic-sum")))
                     (append-para-text 
                      sd (strcat "$" 
                                 (cond ((string= op "+")
                                        (funcall use-var "plus1:q"))
                                       ((string= op "-")
                                        (funcall use-var "sub1:q"))
                                       (t "#"))
                                 "$") true))
                   (funcall summon-tutor (funcall make-tutor-input op n1 n2 a))))
               
               initialize-ephemeral
               (lambda ()
                 (let ((summ (dgebid "lesson-summary"))
                       (lq (dgebid "lesson-quit")))
                   (remove-event-no-capture summ "click" show-summary)
                   (remove-event-no-capture lq "click" quit-early)
                   (add-event-no-capture summ "click" tutor-show-summary)
                   (add-event-no-capture lq "click" tutor-lesson-quit)))
               
               );;end of setf

            ))
        ephemeral
        ))
    (init-mod-sub-position-sys-nat-lesson-slots)
    
    ))


(define-lesson mod-sub-position-sys-nat
    :step ((:p "In this lesson, you can first practice some slightly
    longer addition sums, and can then learn about regrouping with
    minus."))
    
    :using-q&a-vars
    ((:q&a-var "plus1" addnat-lt1000)
     (with-lesson-module-slots
         (funcall summon-tutor-for-problem 
                  (funcall use-var "plus1:q")
                  (funcall use-var "plus1:a"))))

    :step ((:p "We saw in the very first lesson, that the number line
    contains both positive and negative numbers.  Even though we have
    been subtracting, we have not yet worked with negative numbers,
    and will first do so properly in a later lesson. Negative numbers
    can arise from subtracting a " (:strong "larger") " number from a
    " (:strong "smaller") " one. They are completely legitimate
    numbers, and that is why they are mentioned now, so that you
    understand this from the start, and do not develop
    the " (:strong "incorrect") " idea that they are somehow not
    really numbers."))

    :step ((:p "When we subtract a " (:strong "smaller") " positive
    number " (:strong "from") " a " (:strong "larger") " positive
    number, we always get a positive number as a result.  However,
    when we are subtracting the digits of the smaller number from the
    larger number's digits, in some cases, the actual digit of the
    smaller number might be larger than the corresponding digit of the
    larger number."))  

    :stepj ((:p "As an example: in the sum $21-12$, obviously $21$ is
    bigger than $12$, but if want to subtract the units digits, we
    have the digit sum $1-2$, which gives a " (:strong "negative") "
    number as the unit subtraction answer!") (:p "We do not normally
    want negative digits in our answers, as that is not the
    conventional way numbers are written in our tradition.  To deal
    with this, we "(:strong "regroup") " the $21$ which is 2 tens and
    1 unit as " (:q "1 ten and 1 ten and 1 unit") ". We say 1 ten and
    1 unit is $11$, and then we can do the units subtraction $11-2$
    with a positive result of 9.  The remaining ten minus the ten from
    $12$ give 0, so the final answer is 9.") (:p "When all this is
    explained in words, it might seem a bit complicated, but after
    practicing it for a while, you will become used to it, and
    understand the system. As before, try it with hints turned on, and
    then practice until you can do many subtractions without hints."))

    :using-q&a-vars
    ((:q&a-var "sub1" small-b-minus)
     (with-lesson-module-slots
         (funcall summon-tutor-for-problem 
                  (funcall use-var "sub1:q")
                  (funcall use-var "sub1:a"))))

    :step ((:h1 "Summary")
           (:p "If we are subtracting a lesser number from a larger
           number by hand, an efficient way to arrange our work when
           adding or subtracting longer numbers is to:"
           (:ul (:li "Write down the first number.")
                (:li "Write the second number under the first, lining
                up the corresponding digits of the numbers, so units
                are under units, tens under tens, and so on.")
                (:li "Starting with the units digits, add or subtract
                the corresponding digits as required, remembering to
                regroup if necessary.")))
           (:p "If you have a situation with a greater digit being
           subtracted from a lesser digit, then you must regroup by
           reducing a higher power of ten by one.")
           (:p "In a later lesson, we will see what to do when the
           digit which counts the next higher power of ten is zero.")
           (:p "Press the right arrow to complete this lesson."))
    :complete? t)
