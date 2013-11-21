(in-package :autotutor)

(defun init-mod2-int-lesson-slots ()
  (ps:ps 
    (defun init-mod2-int-lesson-slots ()
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
    (init-mod2-int-lesson-slots)
    ))

(define-lesson mod2-int
    
    :step ((:p "This lesson will explain the rules for multiplication
    of negative numbers. There are two pieces of knowledge which are
    important for understanding this lesson:" (:ol (:li "The rule for
    multiplying a factor by a sum (the distributive law)") (:li "How
    to add negative numbers."))) (:p "These subjects are explained in
    the lessons titled " (:q "Multiplication - 1") "
    and " (:q "Negative Numbers - 1") ".  If you need to, you can go
    back to these lessons and review them.") (:p "You should also
    remember that multiplication is always done before
    addition (unless the addition is in
    brackets).") (:p (:strong "What we already have") " are rules for
    adding and multiplying positive whole numbers, and adding negative
    whole numbers. " (:strong "What we want") " are rules for how to
    multiply and subtract negative whole numbers.") (:p "An important
    principle that we will use is: " (:strong "any new rules must not
    contradict the rules we already have") ".") (:p "If we allowed new
    rules to break our old rules, things would start to get very
    complicated and mathematics would become harder than it is.  We
    want simplicity, as far as possible."))
                   
    :stepj ((:p "This next line is pretty simple, and well known to
    you:") (:p "$1\\cdot 1=1$.") (:p "Now write " (:strong "the same
    thing") " as a factor multiplied by a sum like
    this:") (:p "$1\\cdot(2 + -1)$") (:p "Because the expression
    inside the brackets is equal to $1$, we have written the same
    thing as $1\\cdot 1$ , just in another way.") (:p "Now use the
    distributive law:") (:p "$1\\cdot(2 + -1)=1\\cdot 2 + 1\\cdot
    -1$") (:p "Look carefully at that last line until you can see that
    we have just used the distributive law."))

    :stepj ((:p "Our last line was:") (:p "$1\\cdot(2 + -1)=1\\cdot 2
    + 1\\cdot -1$") (:p "We know that $1\\cdot 2 = 2$, so we
    have:") (:p "$2+ 1\\cdot -1=1$") (:p "We know it must equal 1,
    since it is the same thing as $1\\cdot 1$") (:p "So $2+$ (whatever
    $1\\cdot -1$ is equal to) must be 1.") (:p "The only number which
    added to 2 gives 1 is $-1$, and that suggests $1\\cdot -1=-1$ is
    the only possibility, " (:strong "if we want to keep the
    distributive law.")))

    :stepj ((:p "We can use the same procedure to find the result of 1
    multiplied by any negative number.  For example, we could do the
    same thing with $1\\cdot 1=1=1\\cdot (3+ -2)$ to discover that
    $1\\cdot -2=-2$") (:p "What about all the numbers greater than 1.
    What do they give when multiplied by a negative
    number?") (:p "Well, try it out with 2.") (:p "$2\\cdot
    2=4=2\\cdot (4 + -2)=2\\cdot 4 + 2\\cdot -2$") (:p "The last
    expression gives:") (:p "$8 + 2\\cdot -2=4$") (:p "So $2\\cdot -2$
    must be $-4$ for the final result to be consistent with what we
    already knew."))

    :step ((:p "In fact, we could use the procedure with any positive
    numbers to show that a sensible rule is:") (:p "Multiplying a
    number
    " (:strong "without a negative sign") " by a
    number " (:strong "with a negative sign") " is equal to the
    negative of the result of multiplying those numbers without
    signs.") (:p "Since we already have a rule saying we can swap
    numbers around a multiplication sign, we have also automatically
    got the answer to what to do if you have to multiply a negative by
    a positive number."))

    :stepj ((:p "Now we want to work out a sensible rule for
    multiplying 2 negative whole numbers.  We already have the rule
    that any number multiplied by zero is zero, so to keep the new
    rules consistent with the ones we already have, we
    say:") (:p "$-1\\cdot 0=0$") (:p "Now write the " (:strong "same
    thing") " but expressing zero as
    $1+-1$:") (:p "$-1\\cdot (1+-1)=0$") (:p "Use the distributive
    law:") (:p "$-1\\cdot 1 + -1\\cdot -1=0$") (:p "We know what the
    first multiplication is, since we have just decided that $-1\\cdot
    1=-1$, so we have:") (:p "$-1+ -1\\cdot -1=0$") (:p "The only
    number which gives zero when added to " (:q "$-1$") " is 1.  This
    suggests that $-1\\cdot -1=1$ is the only
    possibility, " (:strong "if we want to keep the distributive
    law.")))

    :step ((:p "Just as before, we could repeat this with other
    numbers, to suggest the rule:") (:p "A number with a negative
    sign times another number with a negative sign is equal to the
    same result as multiplying those numbers without the negative
    signs."))

    :stepj ((:p "In a previous lesson (titled " (:q "Negative Numbers
    - 1") ") we explained how to calculate the subtraction of a
    greater positive number from a lesser positive number, by saying
    you could write a negative sign and then do the reverse
    subtraction. We said that after you had learned about
    multiplication of negative numbers, we could give a better
    justification for why the procedure works") (:p "Here it is, as a
    single example: ") (:p "$2-3=2+ -3=-1\\cdot -2 + -1\\cdot
    3$") (:p "$=-1\\cdot(-2 + 3)=-1\\cdot (3+
    -2)=-1\\cdot (3-2)$.") (:p "You might want to work through it,
    finding which laws/rules were used to transform each expression
    into the next.") (:p "You might also want to find other examples
    yourself using other numbers."))

    :stepj ((:p "The rules we have just established also provide us
    with a way to understand subtraction of a negative number from a
    negative number.") (:p "An example: $-1--1$.") (:p "This
    expression is considered to be equivalent to
    $-1-(-1)$.") (:p "Which is equivalent to
    $-1+ (-1)(-1)$.") (:p "Which is equivalent to $-1+1=0$."))

    :step ((:h1 "Summary")
           (:p "This lesson has explained the rules for multiplying
    negative numbers, and has explained that those rules were chosen
    by mathematicians because any other rules would contradict the
    distributive law, or other existing laws." (:ul (:li "A number
    with a negative sign times another number with a negative sign is
    equal to the same result as multiplying those numbers without the
    negative signs.") (:li "A number without a negative sign times a
    number with a negative sign is equal to the negative of the result
    of multiplying those numbers without the negative signs.")))
               (:p "Press the right arrow to end this lesson."))
    :complete? t)
