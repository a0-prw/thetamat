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

(defun init-mod-basic-add-nat-lesson-slots ()
  (ps:ps 
    (defun init-mod-basic-add-nat-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (prepare-qnr-responses sum-2digit
                     one-times-ten-to-the-nth-number-names
                     sum-no-carry digit-plus-input-div make-digit-plus-handler
                     find-number-name sum-with-carry
                     find-number-name-handler
                     make-qnr-handler ;;grab-focus
                     make-multiple-choice-part process-mc-button
                     make-mc-handler qnr-tmp refresh-mcq
                     multiple-choice-questions) ephemeral
          (with-slots (number-from-nlid
                       make-refresh-handler insert-refresh-button
                       remove-default-rl-handlers restore-default-rl-handlers
                       garbage record-garbage remove-garbage
                       get-digit get-inline-term grab-focus
                       current-lesson-text current-lesson
                       current-lesson-variables label-mark 
                       next-lesson-text previous-lesson-text 
                       reset-var use-var init-var ajax-reset-var
                       teach add-or-remove-wrong-click-nl-handlers
                       numberline-id wrong-click-nl random-int
                       from-direction current-module-name) non-ephemeral
            (setf
             
             qnr-tmp ;; Var slot
             (ps:create mcq (ps:new (-object)))

             sum-with-carry ;; Slot
             (lambda (vname)
              (funcall sum-2digit vname true))
             

             sum-no-carry ;; Slot
             (lambda (vname)
               (funcall sum-2digit vname false))

             sum-2digit ;; Slot
             (lambda (vname carry)
               (with-slots (snc) qnr-tmp
                 (with-slots (tot curr eh) snc
                   (cond ((string= from-direction "rtl")
                          (let ((varname (strcat vname ":q")))
                            (funcall reset-var varname (funcall use-var varname)))
                          (funcall previous-lesson-text))
                         (t
                          (funcall remove-default-rl-handlers)
                   (setf snc (ps:new (-object))
                         tot 2
                         curr 0
                         eh (ps:new (-object)))
                   (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                          (qname (strcat vname ":q"))
                          (ques (funcall use-var qname))
                          (ans (funcall use-var (strcat vname ":a")))
                          (jq (strcat #.(lang '((american-english "The sum is $")
                                                (danish "Summen er $")))ques "$."))
                          (t1 (funcall get-inline-term ques 0))
                          (t2 (funcall get-inline-term ques 2))
                          (t1units (funcall get-digit t1 0))
                          (t1tens (funcall get-digit t1 1))
                          (t2units (funcall get-digit t2 0))
                          (t2tens (funcall get-digit t2 1))
                          (aunits (if carry
                                      (strcat "" (+ (parse-int t1units)
                                                    (parse-int t2units)))
                                      (funcall get-digit ans 0)))
                          (atens (funcall get-digit ans 1))
                          (qholder (plain-div)))
                     (append-para-text qholder jq true)
                     (funcall digit-plus-input-div 
                              carry qname qholder t1 t2 aunits #.(lang '((american-english "units")
                                                                         (danish "etterne"))))
                     (funcall digit-plus-input-div 
                              carry qname qholder t1 t2 atens #.(lang '((american-english "tens")
                                                                        (danish "tierne"))))
                     (funcall digit-plus-input-div 
                              carry qname qholder t1 t2 ans)
                     (prune-tree-from-nth ltd 0)
                     (append-child ltd qholder)))))))
             
             digit-plus-input-div ;; Slot
             (with-slots (snc) qnr-tmp
               (with-slots (tot curr eh) snc
                 (lambda (carry vname holder t1 t2  ans placename)
                   (let* ((div (div-class "digit-plus-input"))
                          (id (if placename
                                  (strcat "digit-plus-" placename)
                                  "digit-plus-result"))
                          (max (length ans)) (sz max)
                          (inp (make-text-input-element max sz))
                          (txt1 (if placename
                                    (strcat #.(lang '((american-english "What is the result of adding the ")
                                                      (danish "Hvad er resultatet af, at addere ")))
                                            placename #.(lang '((american-english " digits of the sum")
                                                                (danish " af summen")))
                                            (if (and carry
                                                     (string= placename #.(lang '((american-english "tens")
                                                                                  (danish "tierne")))))
                                                #.(lang '((american-english ", remembering to add
the extra ten which came from adding the units?")
                                                          (danish ". Husk at addere den ekstra tier som kom fra addition af etterne.")))
                                                "?"))
                                    #.(lang '((american-english "What is the final result of the
                                               sum?")
                                              (danish "Hvad er den endelige resultat af summen?")))))
                          ;;(txt2 (strcat " " t1 " and " t2 "?  "))
                          (handler (funcall make-digit-plus-handler 
                                            vname
                                            ans 
                                            (if placename false true)
                                            placename)))
                     (setf (ps:getprop eh id) handler)
                     (set-attribute inp "id" id)
                     (add-event-no-capture inp "keydown" 
                                           handler)
                     (add-event-no-capture inp "click" grab-focus)
                     (append-para-text div txt1)
                     ;;(append-para-text div txt2)
                     (append-child div inp)
                     (append-child holder div)
                     holder))))

             make-digit-plus-handler ;; Slot
             (with-slots (snc) qnr-tmp
               (with-slots (tot curr eh) snc
                 (lambda (vname ans isresult placename)
                   (lambda (ev)
                     (prevent-default ev)
                     (when (numeric-input ev)
                         (let* ((lna (length ans))
                                (input (strcat (value this) (key-char ev)))
                                (lni (length input)))
                           (if (= lna lni)
                               (if (string= input ans)
                                   (let ((pn (parent-node this))
                                         (txtdiv (plain-div)))
                                     (when (not isresult)
                                       (incf curr))
                                     (setf (value this) input)
                                     ;;(prevent-default ev)
                                     (prune-tree-from-nth pn 2)
                                     (append-text txtdiv 
                                                  (if isresult 
                                                      #.(lang '((american-english "Correct! Well done.")
                                                                (danish "Korrekt! Flot.")))
                                                      (strcat #.(lang '((american-english "Correct!
                                                      So the ")
                                                                        (danish "Korrekt! Så")))
                                                      placename #.(lang '((american-english "
                                                      digit in the
                                                      final answer
                                                      is ")
                                                                          (danish " i den endelige resultat er ")))(funcall
                                                      get-digit input
                                                      0) ".")))
                                     (append-child pn txtdiv)
                                     (when isresult
                                       (funcall insert-refresh-button vname))
                                     (remove-event-no-capture 
                                      this "keydown" 
                                      (ps:getprop eh (id-of this)))
                                     (add-event-no-capture 
                                      this "keydown" 
                                      (lambda (ev) (prevent-default ev)))
                                     (when isresult
                                       (funcall restore-default-rl-handlers)
                                       (setf eh null)))
                                   (let ((pn (parent-node this))
                                         (txtdiv (plain-div)))
                                     (prune-tree-from-nth pn 2)
                                     (append-text txtdiv (strcat 
                                                          #.(lang '((american-english "No, ")
                                                                    (danish "Nej, ")))
                                                          input 
                                                          #.(lang '((american-english " is wrong. Try again!")
                                                                    (danish " er forkert. Prøv igen!")))))
                                     (append-child pn txtdiv)
                                     (setf (value this) "")))
                               (setf (value this) input) )))))))
               
             one-times-ten-to-the-nth-number-names ;; Slot
             (lambda ()
               (if (string= from-direction "rtl")
                   (funcall previous-lesson-text)
                   (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                          (inpholder (plain-div))
                          (inp (make-text-input-element 9 10)))
                     (append-child inpholder inp)
                     (setf (value inp) "1")
                     (add-event-no-capture inp "keydown" find-number-name-handler)
                     (add-event-no-capture inp "click" grab-focus)
                     (append-text inpholder "one")
                     (append-para-text ltd #.(lang '((american-english "You can try this now.
                     Insert or remove zeros in the box below to write
                     bigger or smaller numbers from 1 to a billion.")
                                                     (danish "Du kan prøve det dette nu.  Indsæt eller fjern nullere i kassen nedenunder for at skrive større eller mindre tal fra 1 til en milliard."))))
                     (append-child ltd inpholder)
                     (focus inp)
                     (setf (selection-end inp) (length (value inp))
                           (selection-start inp) (selection-end inp)
                           (style-color inpholder) "red")
                     )))

             find-number-name ;; Slot
             (lambda (parent input)
               (let* ((names
                      (array #.(lang '((american-english "one")
                                       (danish "en"))) 
                             #.(lang '((american-english "ten")
                                       (danish "ti")))
                             #.(lang '((american-english "one hundred")
                                       (danish "et hundred")))
                             #.(lang '((american-english "one thousand")
                                       (danish "en tusind")))
                             #.(lang '((american-english "ten thousand")
                                       (danish "ti tusind")))
                             #.(lang '((american-english "one hundred thousand")
                                       (danish "en hundred tusind")))
                             #.(lang '((american-english "one million")
                                       (danish "en million")))
                             #.(lang '((american-english "ten million")
                                       (danish "ti million")))
                             #.(lang '((american-english "one hundred million")
                                       (danish "en hundred million")))
                             #.(lang '((american-english "one billion")
                                       (danish "en milliard")))))
                      (old (last-child parent))
                      (gnew (make-text-node 
                             (aref names (1- (length (value input)))))))
                 (replace-child parent gnew old)))               

             find-number-name-handler ;; Slot
             (lambda (ev)
               (let ((current (value this))
                     (lnc (length (value this)))
                     (mult false)
                     (div false)
                     (pn (parent-node this)))
                 (funcall (dots ev prevent-default)) ;;
                 (when (= (dots ev key-code) 48)
                   (setf mult true))
                 (when (= (dots ev key-code) 8)
                   (setf div true))
                 (cond (mult
                        (when (not (= 10 lnc))
                          (setf (value this) (strcat current "0")))
                        (funcall find-number-name pn this))
                       (div
                        (when (not (= 1 lnc))
                          (setf (value this) 
                                (subseq current 0 (1- (length current)))))
                        (funcall find-number-name pn this))
                       (t false))))

             multiple-choice-questions ;; Slot
             (lambda (msg mcarr)
               (with-slots (qnr-tmp ) ephemeral
                 (with-slots (eh mcq total correct) qnr-tmp
                   (if (ps:getprop mcq current-lesson-text)
                        
                         (funcall refresh-mcq msg 2)
                         
                       (if (string= from-direction "rtl")
                           (funcall previous-lesson-text)
                           (let ((ltd (doc-get-el-by-id "lesson-text-display"))
                                 (container-div (div-class "multiple-choice"))
                                 (form (create-element "form"))
                                 (count 0))
                             (funcall remove-default-rl-handlers)
                             (prune-tree-from-nth ltd 0)
                             ;;  (setf qnr-tmp (ps:create total 0 correct 0))
                             (setf  total 0 correct 0)
                             (setf eh (ps:new (-object)))
                             (append-para-text container-div msg)
                             (dolist (part mcarr)
                               (funcall make-multiple-choice-part form part count)
                               (incf count))
                             (append-child container-div form)
                             (append-child ltd container-div)))))))

             refresh-mcq ;; Slot
             (lambda (msg n)
               (with-slots (mcq) qnr-tmp
                 (labels ((callback (response)
                            (let ((newarr (parse-json response)))
                              (setf (ps:getprop mcq "spec")  newarr
                                    (ps:getprop mcq current-lesson-text) false
                                    (aref current-lesson current-lesson-text)
                                    (lambda ()
                                      (funcall multiple-choice-questions msg
                                               (ps:getprop mcq "spec"))))
                              (funcall (aref current-lesson current-lesson-text)))))
                            (ajax_refresh_mcq n callback))))

             make-multiple-choice-part ;; Slot
             (lambda (form parray control)
               (with-slots (qnr-tmp) ephemeral
                 (with-slots (total) qnr-tmp
                   (incf total)
                   (let* ((bag (array))
                          (ques (aref parray 0))
                          (rightarr (aref parray 1))
                          (none false)
                          (id-count 0)
                          (rightans (aref rightarr 0))
                          (cresp (aref rightarr 1))
                          (wrongs (aref parray 2)))
                     (append-para-text form ques)
                     (when (not-defined rightans)
                       (setf none true
                             rightans #.(lang '((american-english "None of the other choices:")
                                                (danish "Ingen af de andre valgmuligheder:")))
                             cresp #.(lang '((american-english "Yes, none of the other choices were correct.")
                                             (danish "Korrekt. Ingen af de andre valgmuligheder er rigtige.")))))
                     (let ((div (funcall process-mc-button rightans cresp control true id-count)))
                       (push div bag)
                       (dolist (wrong wrongs)
                         (incf id-count)
                         (let* ((choice (aref wrong 0))
                                (resp (aref wrong 1))
                                (div (funcall process-mc-button choice resp control false id-count)))
                           (push div bag)))
                       (when (not none)
                         (incf id-count)
                         (let ((div 
                                (funcall 
                                 process-mc-button
                                 #.(lang '((american-english "None of the other choices:")
                                           (danish "Ingen af de andre valgmuligheder:")))
                                 #.(lang '((american-english "No, one of the other choices is correct.")
                                           (danish "Nej. En af de andre valgmuligheder er korrekt.")))
                                 control false id-count)))
                           (push div bag)))
                       (let ((partholder (div-class "mc-part"))
                             (ln (length bag)))
                         (dotimes (n ln)
                           (let ((obj (random-remove bag)))
                             (append-child partholder obj)))
                         (append-child form partholder))
                       )
                       ))))

             process-mc-button
             (lambda (choice responce control true-or-false id-count)
               (with-slots (qnr-tmp) ephemeral
                 (with-slots (eh) qnr-tmp
                   (let* ((div (div-class "mc-button-holder"))
                          (id (strcat "mc-button-" control id-count))
                          (cont  (strcat "mc-control-" control))
                          (buthandler 
                           (funcall make-mc-handler true-or-false responce))
                          (but (make-radio-input-element cont)))
                     (set-attribute but "id" id)
                     (setf (ps:getprop eh id) buthandler)
                     (add-event-no-capture but "click" buthandler)
                     (append-child div but)
                     (append-text div choice)
                     div))))

             make-mc-handler ;; Slot
             (lambda (right-choice response)
               (with-slots (qnr-tmp) ephemeral
                 (with-slots (total correct eh mcq) qnr-tmp
                   (lambda (ev)
                     (when right-choice
                       (incf correct)
                       (if (= total correct)
                           (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                                  (pn (parent-node this))
                                  (pholder (parent-node pn))
                                  (buts (get-elements-by-tag-name 
                                         pholder "input")))
                             (dolist (but buts)
                               (remove-event-no-capture 
                                but "click" 
                                (ps:getprop eh (id-of but)))
                               (add-event-no-capture 
                                but "click" (lambda (ev) (prevent-default ev))))
                             (setf (ps:getprop mcq current-lesson-text) true)
                             (append-para-text ltd #.(lang '((american-english "Well done")
                                                             (danish "Flot"))))
                             (funcall restore-default-rl-handlers))
                           (let* ((pn (parent-node this))
                                  (pholder (parent-node pn))
                                  (buts (get-elements-by-tag-name 
                                         pholder "input")))
                             (dolist (but buts)
                               (remove-event-no-capture 
                                but "click" 
                                (ps:getprop eh (id-of but)))
                               (add-event-no-capture 
                                but "click" (lambda (ev) (prevent-default ev)))))))
                     (let* ((pn (parent-node this))
                            (pholder (parent-node pn))
                            (mcdivs (get-elements-by-tag-name pholder "div")))
                       (dolist (mc mcdivs)
                         (remove-child-nodes-from-nth mc 2))
                       (append-text pn (strcat " - " response)))))))
                                   
             prepare-qnr-responses ;;Slot
             (lambda (msg arrs)
               (with-slots (qnr-tmp) ephemeral
                 (with-slots (eh answers taken) qnr-tmp
               (if (string= from-direction "rtl")
                   (funcall previous-lesson-text)                 
                   (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                        (pdiv (create-element "div"))
                        (p (create-element "p"))
                        (tmp (ps:array))
                        (tn (make-text-node msg)))
                 (append-child p tn)
                 (append-child pdiv p)
                 ;;(setf qnr-tmp (ps:new (-object)))
                 (setf eh (ps:new (-object)))
                 (dolist (arr arrs)
                   (let* (
                          (vark (aref arr 0))
                          (id (strcat vark "-response"))
                          (div (div-id-class id "qnr-response"))
                          (correct (aref arr 1))
                          (lncorr (length correct))
                          (maxln (1+ (parse-int lncorr)))
                          (good-resp (aref arr 2))
                          (bad-resp (aref arr 3))
                          (inp (make-text-input-element 
                                maxln (1+ maxln))))
                     (set-attributes inp "value" "" "name" vark)
                     (push correct tmp)
                     (setf (ps:getprop eh id)  (funcall make-qnr-handler good-resp bad-resp))
                     (add-event-no-capture inp "keydown" (ps:getprop eh id))
                     (add-event-no-capture inp "click" grab-focus)
                     (append-child div inp)
                     (append-child pdiv div)))
                 (setf answers tmp taken (ps:array))
                 (funcall remove-default-rl-handlers)
                 (prune-tree-from-nth ltd 0)
                 (append-child ltd pdiv)  
                 (funcall (dots (aref (get-elements-by-tag-name ltd "input") 0)
                                focus)))))))

             make-qnr-handler ;; Slot
             (lambda (gr br)
               (with-slots (qnr-tmp) ephemeral
                 (with-slots (taken answers eh) qnr-tmp
                   (lambda (ev)
                     (let ((kc (dots ev key-code)))
                       (funcall (dots ev prevent-default))
                       (when (and (>= kc 48)
                                  (<= kc 57))
                         (setf (value this) 
                               (funcall (dots -string from-char-code) kc)) ;;FIXME
                                                                           ;;have
                                                                           ;;macro
                                                                           ;;for
                                                                           ;;this
                         (let* ((id (id-of (parent-node this)))
                                (div (doc-get-el-by-id id))
                                (input (aref (get-elements-by-tag-name 
                                              div "input") 0))
                                (entry (value input))
                                (is-ans (index-of entry answers))
                                (is-taken (index-of entry taken)))
                           (cond ((and (not (= is-ans -1)) ;is answer
                                       (= is-taken -1)) ;not taken yet
                                  ;;(array-remove entry answers)
                                  (push entry taken)
                                  (when (= (length answers) (length taken))
                                    (funcall restore-default-rl-handlers)
                                    (append-para-text 
                                     (doc-get-el-by-id "lesson-text-display")
                                     #.(lang '((american-english "Well done.")
                                               (danish "Flot.")))))
                                  (remove-event-no-capture this "keydown" (ps:getprop eh id))                                
                                  (add-event-no-capture this "keydown" 
                                                        (lambda (ev) 
                                                          (funcall (dots ev prevent-default))))
                                  (remove-child-nodes-from-nth div 1)
                                  (append-text div (strcat #.(lang '((american-english "    Yes, ")
                                                                     (danish "    Ja, ")))entry gr))
                                  (let ((nex (next-sibling div)))
                                    (when (not (not-defined nex)) 
                                      (funcall (dots (first-child nex) focus)))))
                                 ((and (not (= is-ans -1)) ;is answer
                                     (not (= is-taken -1))) ;but has been taken
                                  (remove-child-nodes-from-nth div 1) ;;here
                                  (append-text div (strcat #.(lang '((american-english "    Yes, but you have already chosen ")
                                                                     (danish "    Ja, men du har allerede valgt ")))entry))
                                (setf (value input) ""))
                                 (t (remove-child-nodes-from-nth div 1)
                                    (append-text div (strcat #.(lang '((american-english "    No, ")
                                                                       (danish "    Nej, ")))entry br))
                                    (setf (value input) "")))
                         )))))))
             ) ;;ends setf slots
            ))
        ephemeral
        ))
    (init-mod-basic-add-nat-lesson-slots)
    ))


(define-lesson mod-basic-add-nat
    
    :step ((:p #.(lang '((american-english "The numbers 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9 are
    called ")
                         (danish "Tallene 0, 1, 2, 3, 4, 5, 6, 7, 8, og 9 hedder ")))
               (:strong #.(lang '((american-english "digits.")
                                  (danish "cifre.")))) 
               #.(lang '((american-english " There are ten digits in our number
    system.  ")
                         (danish " Der er ti cifre i vores talsystem.  ")))
               (:strong #.(lang '((american-english "Digit")
                             (danish "")))) 
               #.(lang '((american-english " can also mean
    ")
                         (danish ""))) (:strong #.(lang '((american-english "finger")
                                                          (danish ""))))
                         #.(lang '((american-english ", so it is easy to remember.")
                                   (danish "")))
                         (:p #.(lang '((american-english "The digits are
    the numbers we can type on the keyboard with just one keypress.
    They are just one symbol each.  Most numbers need more than 1
    digit to write.")
                                       (danish "Cifrene er all de tal,
                                       som vi kan skrive på et
                                       keyboard med kun et tryk.  De
                                       består af kun et symbol.  De
                                       fleste tal kræver flere end et
                                       ciffer at skrive."))))
                         (:p #.(lang '((american-english "When we are writing numbers, and reach 9,
    we have to start ")
                                       (danish "Når vi skriver tal og når 9, er vi nødt til at ")))
                             (:q #.(lang '((american-english "putting digits together")
                                           (danish "sammensætte cifre")))) 
                             #.(lang '((american-english " to write
    bigger numbers.  The way we ")
                                       (danish " for at skrive større tal.  Måden vi ")))
                             (:q #.(lang '((american-english "put digits together")
                                           (danish "sammensætter cifre")))) 
                             #.(lang '((american-english " is called the ")
                                       (danish " hedder ")))
                             (:strong #.(lang '((american-english "decimal position system.")
                                                (danish "titalssystemet.")))))))

    :step ((:p (:q #.(lang '((american-english "Decimal")
                             (danish "Ti")))) 
               #.(lang '((american-english " in ")
                         (danish " i ")))
               (:strong #.(lang '((american-english "decimal position
    system")
                                  (danish "titalssystemet"))))  
               #.(lang '((american-english " means that it is based on the number 10, which is the
    first number that we need more than one digit to
    write.")
                         (danish " betyder at det er baseret på tallet ti, som er det første tal, som vi behøver flere end et ciffer at skrive.")))) 
           (:p (:strong #.(lang '((american-english "Position")
                                  (danish "Titalssystemet")))) 
               #.(lang '((american-english " in ")
                         (danish " er også et ")))
               (:strong #.(lang '((american-english "decimal position
    system")
                                  (danish "positionssystem")))) 
               #.(lang '((american-english " means that what the different digits in
    a ")
                         (danish " og det betyder at hvad ")))
               (:q #.(lang '((american-english "put-together")
                             (danish "sammensatte"))))
               #.(lang '((american-english " number ")
                         (danish " tal ")))(:strong #.(lang '((american-english "mean")
                                                              (danish "betyder")))) 
                         #.(lang '((american-english " depend
    on ")
                                   (danish " afhænger af ")))
                         (:strong #.(lang '((american-english "where")
                                            (danish "hvor")))) 
                         #.(lang '((american-english " the digits are in the number")
                                   (danish " ciffrene befinder sig i tallet")))))

    :step ((:p #.(lang '((american-english "Before we go on to look at how to
    add ")
                         (danish "Før vi ser på hvordan man kan addere ")))
               (:q #.(lang '((american-english "put-together-numbers")
                             (danish "sammensatte tal")))) 
               #.(lang '((american-english " we need to be sure of 2
    things:")
                         (danish " er vi nødt til at være sikre på to ting:")))
               (:ul (:li #.(lang '((american-english "Do we know what the digits in a number
    are?")
                                   (danish "Ved vi hvad ciffrene i et tal er?")))) 
                    (:li #.(lang '((american-english "Do we know what the digits pposition in the number
    mean?")
                                   (danish "Ved vi hvad ciffrenes position i tallet betyder?"))))) 
               (:p #.(lang '((american-english "In the next few steps, you will be practising the
    first thing we need to know about.  You will be asked a question
    about digits in a number.")
                             (danish "Så i de næste skærme, vil du
    øve den første ting du skal ved.  Du vil blive stillet et
    spørgsmål om ciffrene i et tal")))) 
               
               (:p #.(lang '((american-english "Use the keyboard to write
    the digit.")
                             (danish "Brug dit keyboard for at skrive cifret.")))) 
               (:p #.(lang '((american-english "You must get them all right before you can
    continue.  If you make a mistake you can correct it by just
    writing a new digit.")
                             (danish "Du skal have alle rigtige før du
                             kan fortsætte.  Hvis du lave en fejl,
                             skal du bare rette den, ved at skrive et
                             nyt ciffer."))))))

    :display (funcall 
              prepare-qnr-responses
              #.(lang '((american-english "What are the digits in 10?")
                        (danish "Hvad er ciffrene i 10?")))
              (array
               (array "qnr1" "1" #.(lang '((american-english " is a digit in 10.")
                                           (danish " er et ciffer i 10.")))
                      #.(lang '((american-english " is not a digit in 10.")
                                (danish " er ikke et ciffer i 10."))))
               (array "qnr2" "0" #.(lang '((american-english " is a digit in 10.")
                                           (danish " er et ciffer i 10.")))
                      #.(lang '((american-english " is not a digit in 10.")
                                (danish " er ikke et ciffer i 10."))))))
    :display (funcall 
              prepare-qnr-responses
              "What are the digits in 18?"
              (array
               (array "qnr1" "1" #.(lang '((american-english " is a digit in 18.")
                                           (danish " er et ciffer i 18.")))
                      #.(lang '((american-english " is not a digit in 18.")
                                (danish " er ikke et ciffer i 18."))))
               (array "qnr2" "8" #.(lang '((american-english " is a digit in 18.")
                                           (danish " er et ciffer i 18.")))
                      #.(lang '((american-english " is not a digit in 18.")
                                (danish " er ikke et ciffer i 18."))))))
    :display (funcall 
              prepare-qnr-responses
              #.(lang '((american-english "What are the digits in 7258?")
                        (danish "Hvad er ciffrene i 7258")))
              (array
               (array "qnr1" "7" #.(lang '((american-english " is a digit in 7258.")
                                           (danish " er et ciffer i 7258")))
                      #.(lang '((american-english " is not a digit in 7258.")
                                (danish " er ikke et ciffer i 7258."))))
               (array "qnr2" "2" #.(lang '((american-english " is a digit in 7258.")
                                           (danish " er et ciffer i 7258")))
                      #.(lang '((american-english " is not a digit in 7258.")
                                (danish " er ikke et ciffer i 7258."))))
               (array "qnr3" "5" #.(lang '((american-english " is a digit in 7258.")
                                           (danish " er et ciffer i 7258")))
                      #.(lang '((american-english " is not a digit in 7258.")
                                (danish " er ikke et ciffer i 7258."))))
               (array "qnr4" "8" #.(lang '((american-english " is a digit in 7258.")
                                           (danish " er et ciffer i 7258")))
                      #.(lang '((american-english " is not a digit in 7258.")
                                (danish " er ikke et ciffer i 7258."))))
               ))

    :step ((:p #.(lang '((american-english "You know what the digits are now, in any number that
    you see.  But what about what they mean?  We said before that the
    digit's ")
                         (danish "Nu ved du hvad ciffrene i et hvilket som helst tal er.  Men hvad betyder de? Vi sagde før at ciffrenes ")))
               #.(lang '((american-english (:strong "meaning")
                          (danish "betydning"))))
               #.(lang '((american-english " depends on ")
                         (danish " afhænger af ")))
               (:strong #.(lang '((american-english "where")
                                  (danish "hvor")))) 
               #.(lang '((american-english " it is placed in the ")
                         (danish " den er placeret i det ")))
               (:q #.(lang '((american-english "put-together-number")
                             (danish "sammensatte tal")))) ".") 
           (:p #.(lang '((american-english "The next pages explain what we mean by this?")
                         (danish "De næste skærme forklarer hvad vi mener med dette?")))))

    :step ((:p "You already know that a single digit by itself just
    means " (:strong "that") " many units, or that place on the
    numberline. The secret to understanding the digit's meaning
    in " (:q "put-together-numbers") " is to know that each extra
    digit put in front of the digit says how many tens, hundreds,
    thousands, and so on, there are in the number.") (:p "Here follows
    an example of what this means."))

    :step ((:p "The digit " (:q "3") " by itself just means 3 units.")
    (:p "If we write another digit in front of the 3, that is how many
    tens there are in the number.") (:p "For example in the
    number " (:q "13") " there is 1 ten and 3 units.") (:p "In the
    number " (:q "23") " there are 2 tens and 3 units.") (:p "In the
    number " (:q "43") " there are 4 tens and 3 units.") (:p "If we
    write another digit in front of the " (:q "tens place") " then
    that digit says how many hundreds there are in the
    number") (:p "So, in the number " (:q "143") " there is 1 hundred, 4
    tens, and 3 units."))
    
    :step ((:p "Ten is a very special number in the decimal number
    system.") (:p "It is easy to write bigger numbers with one and
    zeros.  Just write a " (:q "1") " followed by a " (:q "0") " to
    write ten.") (:p "If you write another " (:q "0") "you have written
    one hundred.  You can continue in this way to write thousands,
    tens of thousands, hundreds of thousands, millions, tens of
    millions and so on.") (:p (:strong "In each new group, you have 10
    times as many as you had in the previous group")))
           
    :display (funcall one-times-ten-to-the-nth-number-names)
    
    :stepj ((:p "To repeat what we said before in a different way: A
    digit in a number says how many of " (:strong "that") " group
    there are in the whole number.")  (:p "The correct name for
    these " (:q "groups") " is " (:q "powers of ten") ".") (:p "Here
    are some examples:") (:p "In the number " (:q "12") " there
    is " (:strong "1") " ten and " (:strong "2") "
    units.  " (:strong "We can also write this as $10 + 2 =
    12$")) (:p "In the number " (:q "22") " there
    are " (:strong "2") " tens and " (:strong "2") "
    units.  " (:strong "We can also write this as $20 + 2 =
    22$")) (:p "In the number " (:q "32") " there
    are " (:strong "3") " tens and " (:strong "2") "
    units.  " (:strong "We can also write this as $30 + 2 =
    32$")) (:p "In the number " (:q "124") " there
    is " (:strong "1") " hundred and there are " (:strong "2") " tens
    and " (:strong "4") " units.  " (:strong "We can also write this
    as $100 + 20 + 4 = 124$")))
    
    :step ((:p "Here follow a few questions where you can practise
    what some of the digits in numbers mean."))
               
    :display (funcall 
              prepare-qnr-responses
              "How many tens are there in 13"
              (array
               (array "qnr1" "1" " there is 1 ten in 13." 
                      " does not say how many tens are in 13.")))
    :display (funcall 
              prepare-qnr-responses
              "How many tens are there in 27"
              (array
               (array "qnr1" "2" " there are 2 tens in 27." 
                      " does not say how many tens are in 27.")))
    :display (funcall 
              prepare-qnr-responses
              "How many tens are there in 99"
              (array
               (array "qnr1" "9" " there are 9 tens in 99." 
                      " does not say how many tens are in 99.")))
    :display (funcall 
              prepare-qnr-responses
              "How many hundreds are there in 127"
              (array
               (array "qnr1" "1" " there is 1 hundred in 127." 
                      " does not say how many hundreds are in 127.")))

    :step ((:p "All this has a very nice consequence.  If we want to
    add numbers, we can just add the digits which say how many of a
    group there are in each number." (:strong " And you already know
    how to add digits")) (:p "Before we see how to use this to
    add " (:q "put-together-numbers") " here follows another question
    where you must decide the meaning of the digits."))

    :display (funcall
              multiple-choice-questions
              "Select the correct meanings of the digits:"
              (array 
               (array  
                "The digit 3 in 34 means" ;;question first part
                (array "3 tens:" "Yes, 3 in 34 means 3 tens.") 
                ;; correct, response an empty array means "None of the above"
                (array ;;incorrct choices, responses 
                 (array "3 hundreds:" "No, there are no hundreds in 34.")
                 (array "3 units:" "No, try again.")))
               (array 
                
                "The digit 4 in 34 means" ;;optional nth (here second) part
                (array "4 units:" "Yes, 4 in 34 means 4 units")
                (array
                 (array "4 hundreds:" "No, there are no hundreds in 34.")
                 (array "4 tens:" "No, 4 in 34 does not mean 4 tens, try again.")))))
    
    :stepj ((:p "In a previous lesson, we established that when we are
    adding it does not matter which number we take first.  We can use
    this when we are
    adding " (:q "put-together-numbers") ": " (:ul (:li "We add the
    units digits of the numbers to find how many units there are in
    the result, and ...") (:li "we add the tens digits to find how
    many tens there are in the result, and ...") (:li "We add the
    hundreds digits together to find how many hundreds there are in
    the result, and ...") (:li "We continue in this way, until there
    are no more powers of ten to add."))))

    :stepj ((:p "Here is an example: Let's say we want to add 15 and
    13." (:p "15 is 1 ten and 5 units.  We can write it mathematically
    as $15=10+5$.") (:p "13 is 1 ten and 3 units.  We can write it
    mathematically as $13=10+3$.") (:p "To add 15 and 13 we can first
     " (:strong "add the units") ", which are 5 in 15 and 3 in 13 to
    get 8.  So we know that there are 8 units in our
    result.") (:p "What about the tens.  There is 1 ten in 13 and 1
    ten in 15.  Adding 1 and 1 gives 2, so there are 2 tens in our
    result.") (:p "Writing the tens and units down we get
    28.") (:p "Writing the sum mathematically, we have: $15+13=28$")))

    :stepj ((:p "Sometimes when we do this, we get a number which is
    too big for the group.  When this happens, we just add the extra
    digit to the next higher group of ten.") (:p "For example, we just
    did the sum: $15+13=28$.  If we had instead done $15+16$, when we
    add the digits which are 5 and 6, we get 11.  Now this means that
    there is 1 unit and 1 ten in our result, " (:strong "but we are
    not finished") " since we have not yet added the tens from our
    numbers.") (:p "So we must remember to add the extra ten from
    $5+6=11$ when we add the tens.") (:p "We add 1 ten from 15 and 1
    ten from 16 to get 2 and to this we add the extra ten from 11 to
    get 3 tens in the final result: $15+16=31$"))

    :step ((:p "Here follows a sum for you, where there
    are " (:strong "no extra") " tens from adding the units.") (:p "
    When you have completed it, a button will appear.  At that point,
    you can either continue with the lesson or, if you want to try
    another sum, click on the button that appears to get a new one."))

    :using-q&a-vars
    ((:q&a-var "nocarry1" tiny-nc-plus)
     (funcall (lambda () (funcall sum-no-carry "nocarry1"))))

    :step ((:p "Here follows a sum for you, where there
    is " (:strong "an extra") " ten from adding the units.  You must
    remember to also add this to the tens."))

    :using-q&a-vars
    ((:q&a-var "carry1" %tiny-c-plus)
     (funcall (lambda () (funcall sum-with-carry "carry1"))))

    :step ((:h1 "Summary")
           (:ul (:li "What digits mean depends on where they are
           written in a number.")
                (:li "To add together 2 numbers, we can add the digits
                in the same positions in the numbers.")
                (:li "When we add digits in the same position and the
                result is bigger than 9, we must remember to add to
                the next higher position."))
           
           (:p "Press the right arrow to complete this lesson."))
    :complete? t)


;; (:p "In the next few steps, you
;;     will be asked a question about digits in a number.") (:p "Use the
;;     keyboard to write the digit.") (:p "You must get them all right
;;     before you can continue.  If you make a mistake you can correct it
;;     by just writing a new digit."))
;;     :display (funcall 
;;               prepare-qnr-responses
;;               "What are the digits in 10?"
;;               (array
;;                (array "qnr1" "1" " is a digit in 10." " is not a digit in 10.")
;;                (array "qnr2" "0" " is a digit in 10." " is not a digit in 10.")))
;;     :display (funcall 
;;               prepare-qnr-responses
;;               "What are the digits in 18?"
;;               (array
;;                (array "qnr1" "1" " is a digit in 18." " is not a digit in 18.")
;;                (array "qnr2" "8" " is a digit in 18." " is not a digit in 18.")))
;;     :display (funcall 
;;               prepare-qnr-responses
;;               "What are the digits in 7258?"
;;               (array
;;                (array "qnr1" "7" " is a digit in 7258." " is not a digit in 7258.")
;;                (array "qnr2" "2" " is a digit in 7258." " is not a digit in 7258.")
;;                (array "qnr3" "5" " is a digit in 7258." " is not a digit in 7258.")
;;                (array "qnr4" "8" " is a digit in 7258." " is not a digit in 7258.")
;;                ))
;;     :step ((:p "You know what the digits are now, in any number that
;;     you see.  But what about what they mean?  We said before that the
;;     digit's " (:strong "meaning") " depends on " (:strong "where") "
;;     it is placed in the " (:q "put-together-number") ".") (:p "What
;;     do we mean by this?"))
;;     :step ((:p "You already know that a single digit by itself just
;;     means " (:strong "that") " many units, or that place on the
;;     numberline. The secret to understanding the digit's meaning
;;     in " (:q "put-together-numbers") " is to know that each extra
;;     digit put in front of a number says how many tens, hundreds,
;;     thousands, and so on, there are in the number.") (:p "Here follows
;;     an example of what this means."))

;;     :step ((:p "The digit " (:q "3") " by itself just means 3 units.")
;;     (:p "If we write another digit in front of the 3, that is how many
;;     tens there are in the number.") (:p "For example in the
;;     number " (:q "13") " there is 1 ten and 3 units.") (:p "In the
;;     number " (:q "23") " there are 2 tens and 3 units.") (:p "In the
;;     number " (:q "43") " there are 4 tens and 3 units.") (:p "If we
;;     write another digit in front of the " (:q "tens place") " then
;;     that digit says how many hundreds there are in the
;;     number") (:p "So, in the number " (:q "143") " there is 1 hundred, 4
;;     tens, and 3 units."))

;;     :display (funcall 
;;               prepare-qnr-responses
;;               "How many tens are there in 13"
;;               (array
;;                (array "qnr1" "1" " there is 1 ten in 13." 
;;                       " does not say how many tens are in 13.")))
;;     :display (funcall 
;;               prepare-qnr-responses
;;               "How many tens are there in 27"
;;               (array
;;                (array "qnr1" "2" " there are 2 tens in 27." 
;;                       " does not say how many tens are in 27.")))
;;     :display (funcall 
;;               prepare-qnr-responses
;;               "How many tens are there in 99"
;;               (array
;;                (array "qnr1" "9" " there are 9 tens in 99." 
;;                       " does not say how many tens are in 99.")))
;;     :display (funcall 
;;               prepare-qnr-responses
;;               "How many hundreds are there in 127"
;;               (array
;;                (array "qnr1" "1" " there is 1 hundred in 127." 
;;                       " does not say how many hundreds are in 127.")))

;;     :step ((:p "All this has a very nice consequence.  If we want to
;;     add numbers, we can just add the digits which say how many of a
;;     group there are in each number." (:strong " And you already know
;;     how to add digits")) (:p "Before we see how to use this to
;;     add " (:q "put-together-numbers") " here follows another question
;;     where you must decide the meaning of the digits."))

;;     :display (funcall
;;               multiple-choice-questions
;;               "Select the correct meanings of the digits:"
;;               (array 
;;                (array  
;;                 "The digit 3 in 34 means" ;;question first part
;;                 (array "3 tens:" "Yes, 3 in 34 means 3 tens.") 
;;                 ;; correct, response an empty array means "None of the above"
;;                 (array ;;incorrct choices, responses 
;;                  (array "3 hundreds:" "No, there are no hundreds in 34.")
;;                  (array "3 units:" "No, try again.")))
;;                (array 
                
;;                 "The digit 4 in 34 means" ;;optional nth (here second) part
;;                 (array "4 units:" "Yes, 4 in 34 means 4 units")
;;                 (array
;;                  (array "4 hundreds:" "No, there are no hundreds in 34.")
;;                  (array "4 tens:" "No, 4 in 34 does not mean 4 tens, try again.")))))
;;     :stepj ((:p "We can add " (:q "put-together-numbers") " by adding
;;     the units digits of the numbers to find how many units there are
;;     in the result, and adding the tens digits together to find how
;;     many tens there are in the result, and adding the hundreds digits
;;     together to find how many hundreds there are in the result, and so
;;     on.") (:p "For example: If we want to add 15 and 13, we add the
;;     units, which are 5 in 15 and 3 in 13 to get 8.  So we know that
;;     there are 8 units in our result.  What about the tens.  There is 1
;;     ten in 13 and 1 ten in 15.  Adding 1 and 1 gives 2, so there are 2
;;     tens in our result.  Writing the tens and units down we get
;;     28.") (:p "Writing the sum mathematically, we have: $15+13=28$"))

;;     :stepj ((:p "Sometimes when we do this, we get a number which is
;;     too big for the group.  When this happens, we just add the extra
;;     digit to the next higher group of ten.") (:p "For example, we just
;;     did the sum: $15+13=28$.  If we had instead done $15+16$, when we
;;     add the digits which are 5 and 6, we get 11.  Now this means that
;;     there is 1 unit and 1 ten in our result, " (:strong "but we are
;;     not finished") " since we have not yet added the tens from our
;;     numbers.") (:p "So we must remember to add the extra ten from
;;     $5+6=11$ when we add the tens.") (:p "We add 1 ten from 15 and 1
;;     ten from 16 to get 2 and to this we add the extra ten from 11 to
;;     get 3 tens in the final result: $15+16=31$"))

;;     :step ((:p "Here follows a sum for you, where there
;;     are " (:strong "no extra") " tens from adding the units.") (:p "
;;     When you have completed it, a button will appear.  At that point,
;;     you can either continue with the lesson or, if you want to try
;;     another sum, click on the button that appears to get a new one."))

;;     :using-q&a-vars
;;     ((:q&a-var "nocarry1" tiny-nc-plus)
;;      (funcall (lambda () (funcall sum-no-carry "nocarry1"))))

;;                 ;; (let ((ltd (doc-get-el-by-id "lesson-text-display"))
;;                 ;;       (ques (funcall use-var "nocarry1:q"))
;;                 ;;       (ans (funcall use-var "nocarry1:a")))
;;                 ;;   (append-para-text ltd ques)
;;                 ;;   (append-para-text ltd (funcall get-digit ans 0))))))
 
