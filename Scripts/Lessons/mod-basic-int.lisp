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

(defun init-mod-basic-int-lesson-slots ()
  (ps:ps 
    (defun init-mod-basic-int-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (create-ddsum-cell
                     swap-min-sub canvas-spec draw-minus draw-plus
                     create-length-line add-neglengths-nl 
                     place-subtrahend-line-on-nl
                     init-count-lines sub-lengths-nl add-line-on-nl add-negline-on-nl
                     place-minuend-line-on-nl place-second-term place-first-term
                     second-term count-lines
                     create-lsum-cell show-add-negative
                     ) ephemeral
          (with-slots (number-from-nlid in-summary set-variable-from-script
                       remove-default-rl-handlers restore-default-rl-handlers
                       garbage record-garbage remove-garbage
                       current-lesson-variables label-mark 
                       next-lesson-text previous-lesson-text 
                       reset-var use-var init-var ajax-reset-var
                       teach add-or-remove-wrong-click-nl-handlers
                       numberline-id wrong-click-nl random-int continue-next
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
             
             second-term ;; Slot
             (lambda (varspec)
               (let* ((val (funcall use-var varspec))
                      (re (ps:regex "/(\\+|\\-)/"))
                      (ts (split val re))
                      (t2 (aref ts 2)))
                 t2))
             
             ;; svg-canvas
             ;; (ps:create ;;heightem "9em" widthem "9em"
             ;;  height 9 width 9
             ;;  canvas-height 12 canvas-width 12
             ;;  correct-arrow false
             ;;  incorrect-arrow false)
               
             draw-minus
             (lambda (obj id)
               (with-slots (canvas-height canvas-width width height) obj
                 (labels ((down (y) (strcat " -" y))
                          (back (x) (strcat " -" x))
                          (up (y) (strcat " " y))
                          (forward (x) (strcat " " x))
                          (draw (x y) (strcat " l" x " " y))
                          (at (x) (strcat " M " x)))
                   (let* ((unit input-menu-icon-pixels)
                          (half (* unit 0.5))
                          (quarter (* unit 0.25))
                          (no-y " 0 ")
                          (no-x " 0 ") 
                          (start (if correct 
                                     (strcat 0  " " (* unit (/ canvas-height 2)))
                                     (strcat (* unit canvas-width) " " 
                                             (* unit (/ canvas-height 2)))))
                          (minus (svg-create-element "path")))
                     (set-attributes minus "id" id
                                     "stroke" "black" "fill" "black"
                                     "d" (strcat (at start) 
                                                 (draw (up quarter) (forward unit))
                                                 (draw (down half) (back unit))
                                                 (draw (up quarter))))
                     minus))))

             canvas-spec
             (lambda (ch cw h w)
               (ps:create
                canvas-height ch
                canvas-width cw
                height h
                width w))
                     
  ;; svg-canvas
  ;;            (ps:create ;;heightem "9em" widthem "9em"
  ;;             height 9 width 9
  ;;             canvas-height 12 canvas-width 12
  ;;             correct-arrow false
  ;;             incorrect-arrow false)
  
             
  ;;            draw-arrow
  ;;            (lambda (correct)
  ;;              (with-slots (canvas-height canvas-width width height
  ;;                                         correct-arrow incorrect-arrow) svg-canvas
  ;;                (labels ((down (y) (strcat " -" y))
  ;;                         (back (x) (strcat " -" x))
  ;;                         (up (y) (strcat " " y))
  ;;                         (forward (x) (strcat " " x))
  ;;                         (draw (x y) (strcat " l" x " " y))
  ;;                         (at (x) (strcat " M " x)))
  ;;                  (let* ((unit input-menu-icon-pixels)
  ;;                         (half (* unit 0.5))
  ;;                         (no-y " 0 ")
  ;;                         (no-x " 0 ") 
  ;;                         (start (if correct 
  ;;                                    (strcat 0  " " (* unit (/ canvas-height 2)))
  ;;                                    (strcat (* unit canvas-width) " " 
  ;;                                            (* unit (/ canvas-height 2)))))
  ;;                         (arrow (svg-create-element "path")))
  ;;                    (set-attributes arrow "id" (if correct "correct-arrow" "incorrect-arrow")
  ;;                                    "stroke" "black" "fill" (if correct "green" "red")
  ;;                                    "d"
  ;;                                    (if correct
  ;;                                        (strcat (at start) 
  ;;                                                (draw (forward unit) (up unit))
  ;;                                                (draw no-x (down half)) 
  ;;                                                (draw (forward half) no-y)
  ;;                                                (draw no-x (down unit))
  ;;                                                (draw (back half) no-y)
  ;;                                                (draw no-x (down half))
  ;;                                                (draw (back unit) (up unit)))
  ;;                                        (strcat (at start)
  ;;                                                (draw (back unit) (down unit))
  ;;                                                (draw no-x (up half))
  ;;                                                (draw (back half) no-y)
  ;;                                                (draw no-x (up unit))
  ;;                                                (draw (forward half) no-y)
  ;;                                                (draw no-x (up half))
  ;;                                                (draw (forward unit) (down unit)))))
  ;;                    arrow))))
             
             add-neglengths-nl ;; Slot
             (lambda (id1 id2 which)
               (if (string= from-direction "rtl") (funcall previous-lesson-text)
                   (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
                          (val (funcall use-var (strcat which ":q")))
                          (ans (funcall use-var (strcat which ":a")))
                          (ts (split val "+")))
                     (ps:destructuring-bind (s1 s2) ts
                       (let* ((t1 (parse-int (string-trim s1)))
                              (abst1 (* -1 t1))
                              (t2 (parse-int (string-trim s2)))
                              (abst2 (* -1 t2))(op " + ") (optn (make-text-node op)) (sign " - ")
                              (svgmin1 (svg-create-element "rect"))
                              ;;(svgplus (svg-create-element "polyline"))
                              (signtn1 (make-text-node sign))
                              (signtn2 (make-text-node sign))
                              (skip svg-grid-skip)(height 1) (width1 abst1) (width2 abst2)
                              (lsum-holder (div-id-class "lsum-holder" "svg-container no-left")))
                         (append-child lsum-holder signtn1)
                         (let* ((svg1 (standard-svg-grid lsum-holder id1  height width1 
                                                         create-lsum-cell id1))
                                (bar1  (funcall create-length-line 
                                                svg1 (ems (* skip width1)) "blue" abst1)))
                           (append-children lsum-holder optn signtn2)
                           (let* ((svg2 (standard-svg-grid lsum-holder id2 height width2 create-lsum-cell id2))
                                  (bar2 (funcall create-length-line 
                                                 svg2 (ems (* skip width2)) "green" abst2))
                                  (desc (append-para-text ltd 
                                                          (strcat "The colored bars represent the sum: $" 
                                                                  val "$.  Click on the bars to add them in the numberline.") true)))
                             (set-attribute svg1 "class" "lsum1")
                             (set-attribute svg2 "class" "lsum2")
                             ;;EVENT handling from here
                             (funcall init-count-lines (strcat which ":q") 
                                      2 (strcat "This has shown $" val "=" ans "$ by placing lines of the correct lengths in the numberline."))
                             (add-event-no-capture bar1 "click" (lambda (ev) (let ((pn (parent-node this)))
                                                                               (funcall add-negline-on-nl this pn true))))
                             (add-event-no-capture bar2 "click"  (lambda (ev) (let ((pn (parent-node this)))
                                                                               (funcall add-negline-on-nl this pn true))))
                             (append-child ltd lsum-holder)
                             )))))))             
             
             create-length-line ;; Slot
             (lambda (parent width color data-term-value id)
               (let* ((b1 (svg-create-element "line"))
                      (skip svg-grid-skip)
                      (y1 (ems (/ skip 2)))
                      (y2 y1))
                 (set-attributes b1 "data-term-value" data-term-value
                                 "stroke-width" "1.5em"
                                 "stroke" color  "x1" "0em" "y1" y1
                                 "stroke-opacity" "0.3"
                                 "x2" width "y2" y2)
                 (when id (set-attribute b1 "id" id))
                 (append-child parent b1)
                 b1))
             
             swap-min-sub 
             (lambda (v)
               (let* ((cv (split v ":"))
                      (root (aref cv 0))
                      (cvq (ps:getprop current-lesson-variables (strcat root ":q") ))
                      (cva (ps:getprop current-lesson-variables (strcat root ":a") ))
                      (nv)(na)(nas))
                 (with-current-variable cvq
                   (let ((ts (split value " ")))
                     (ps:destructuring-bind (t1 op t2)
                         ts
                       (setf t1 (parse-int t1)
                             t2 (parse-int t2)
                             nv (strcat t2 " - " t1)
                             na (- t2 t1)
                             nas (strcat "" na)
                             genfn nv
                             value nv))))
                 (with-current-variable cva
                   (setf genfn nas
                         value nas)))
               (funcall continue-next))             
             
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
                      (tval (get-attribute line "data-term-value"))
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
                      (t1 (parse-float (get-attribute minuend "data-term-value")))
                      (t2 (parse-float (get-attribute line "data-term-value")))
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
                                               (if (> 0 diff)
                                                   (abs diff)
                                                   diff)
                                               (if (> 0 diff)
                                                   " shorter"
                                                   " longer")
                                               " than the second.") true)
                 (funcall restore-default-rl-handlers )))

             add-line-on-nl ;; Slot
             (lambda (ev final)
               (with-slots (current dec remove) count-lines
                 (let* ((skip svg-grid-skip)
                        (parent (parent-node this))
                        (tval (parse-float (get-attribute this "data-term-value")))
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

             add-negline-on-nl ;; Slot
             (lambda (svgobj parent-node negative)
               (with-slots (current dec remove) count-lines
                 (let* ((skip svg-grid-skip)
                        (parent parent-node)
                        (tval (parse-float (get-attribute svgobj "data-term-value")))
                        (ival (- (parse-int (get-attribute svgobj "data-term-value"))))
                        (curr  (if negative (if (zerop current) ival (+ ival current)) current))
                        (mark (doc-get-el-by-id (funcall numberline-id curr)))
                        (mark-parent (parent-node mark))
                        (x1 (get-attribute mark "x1"))
                        (x2 (ems (+ (parse-float x1) (* skip tval))))
                        (y1 (ems svg-grid-skip)) (y2 y1)
                        (nl (doc-get-el-by-id "nl-holder")))
                   (remove-event-no-capture svgobj "click" add-negline-on-nl)
                   (push (ps:array mark-parent svgobj) remove)
                   (remove-child parent svgobj)
                   (set-attributes svgobj "x1" x1 "y1" y1 "x2" x2 "y2" y2)
                   (append-child mark-parent svgobj)
                   (funcall dec (if negative ival tval)))))
                          

             create-lsum-cell ;; Slot
             (define-anonymous-with-cell-variables
                 (set-attributes g  "class" "lsum-cell")
                 (set-attributes rect "x" xem "y" yem "width" skipem
                                 "height" skipem "fill" "none"
                                 "pointer-events" "all"
                                 "stroke" "#cdcdcd")
               (append-child g rect)
               (append-child svg g))
             
             show-add-negative
             (lambda ()
               (let* ((n1 (- (funcall random-int 1 9)))
                      (n2 (- (funcall random-int 1 9)))
                      (a (+ n1 n2))
                      (q (strcat n1 " + " n2))
                      (name "addneg1"))
                 (funcall set-variable-from-script name q a)
                 (funcall add-neglengths-nl "n1" "n2" "addneg1")))
             
             )))
        ephemeral
        ))
    (init-mod-basic-int-lesson-slots)
    ))

(define-lesson mod-basic-int
    
    :step ((:p "Negative numbers are numbers which are less than zero.
    They can be visualized on the numberline to the left of
    zero.") (:p "Negative numbers only seem strange if you
    incorrectly think that numbers can only be used for counting
    physical objects.") (:p "If we use numbers as places on a measure,
    then there are many situations in the real world which can be
    described with the help of negative numbers.  Before we go onto
    the next page with some examples of these situations, you might
    like to try to think of some yourself."))

    :step ((:h1 "Examples of uses of negative numbers:")
           (:ul
            (:li "Distances above and below sea-level.  If we consider
                    sea-level to be zero, and all distances above
                    sea-level as positive numbers, then depths under
                    the sea can be described with negative numbers.")
                    (:li "Amounts of money.  If you have money in the
                    bank, you have a positive amount of money.  If you
                    have borrowed money from the bank, and you owe
                    more than you have, then your " (:q "balance") "
                    is negative.  If you have no money, but you owe
                    $100, then you can say you have " (:q "minus 100
                    dollars") ".")
                    (:li "Temperatures.  Zero degrees centigrade is
                    the temperature water freezes to ice.  If it gets
                    even colder, then the temperature is described by
                    a negative number.")
                    (:li "Time. If we choose some event to be
                    represented by zero, then the times after that
                    event are positive, and the times before are
                    negative. Although we usually do
                    not " (:strong "speak") " of negative times, but
                    use some other words.")))

    :step ((:p "Apart from these practical uses of negative numbers,
    negative numbers play an important role in mathematics.  For
    example, negative numbers and addition can replace subtraction,
    and doing this makes mathematics simpler and clearer.") (:p "For
    all these reasons, it is necessary to accept negative numbers as
    having " (:q "equal rights") ", and in fact other types of numbers
    also exist, as we will see in later lessons") (:p "In the next few
    pages, we will see a way to produce negative numbers."))                    
    
    :step ((:p "Press the right arrow to see a numberline."))
    
    :display (funcall numberline "mod-basic-int-nl" -20 20 1 "#000000" 0.125)
    
    :step ((:p "Here we first repeat an earlier lesson's example of
    seeing a subtraction as the difference of two lengths.  We are
    subtracting here, but still not producing or using negative
    numbers."))
    :using-q&a-vars
    ((:q&a-var "mdsub1" digsubn)
     (funcall sub-lengths-nl "t1" "t2" "mdsub1"))

    :step ((:p "Now see what happens if we swap the numbers.  Press
    the right arrow to continue."))

    :display (funcall swap-min-sub "mdsub1:q")
    
    :using-q&a-vars
    ((:q&a-var "mdsub1" digsubn)
     (funcall sub-lengths-nl "t1" "t2" "mdsub1"))

    :step ((:p "An important and useful fact is illustrated by the
    previous examples.  Although we may not swap numbers around a
    minus sign, because doing so gives us a different answer, the
    answer is nevertheless identical " (:strong "except for the sign")"!")
    (:p "This has thus illustrated how to calculate subtracting a
    greater positive number from a lesser positive number: Write a
    minus sign before your answer and then do the reverse
    subtraction!") (:p "You should go back and repeat the previous
    examples until you are convinced that this justification is
    plausible.") (:p "When you have learned about multiplication in a
    later lesson, a mathematically more precise justification can be
    given."))

    :step ((:p "How about adding negative numbers?") (:p "This is like
    adding positive numbers, but on the other side of zero and in the
    negative direction.") (:p "Because you are " (:strong "adding") "
    you can take either number first, so the rule about being able to
    swap numbers around a plus sign also works with negative
    numbers."))

    :display (funcall show-add-negative)

    :stepj ((:p "Finally, what about adding a negative number to a
    positive number?  This is " (:q "like doing a subtraction") ".  In
    fact, we could " (:strong "define") " the negative of a positive
    number as " (:strong "exactly that") " number which
    when " (:strong "added to") " the positive number is equal to
    zero") (:p "So answering the question: " (:q "What is $-1$?") " We
    can say, " (:q "It is the number which when added to 1 is equal to
    0.")) (:p (:q "What is $-2$?") (:q " It is the number which when
    added to 2 is equal to 0.")) (:p (:q "What is $-3$?") (:q " It is
    the number which when added to 3 is equal to 0.")) " ... and so
    on, for all whole numbers.")

    :summary ((funcall numberline "mod-basic-int-nl" -20 20 1 "#000000" 0.125)
              ((:h1 "Summary") 
                (:p "This lesson has shown you how to produce negative
    numbers, by subtracting a greater positive number from a lesser
    positive number.") (:p "It has also shown you how to add two
    negative numbers, and how to add a positive and negative
    number.")  (:p "In a later lesson, after you have learned about
    multiplication, subtracting negative numbers will be
    explained.") (:ul (:li "To subtract a greater positive number from
    a lesser positive number, write a minus sign before your answer
    and then do the reverse subtraction.") (:li "To add two negative
    numbers, do just as you would with positive numbers, but in the
    negative direction."))
               (:p "Press the right arrow to end this lesson.")))
    :complete? t)
