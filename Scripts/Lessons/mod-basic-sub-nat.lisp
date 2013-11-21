(in-package :autotutor)
;;FIXME remember to setf ps:*ps-print-pretty* nil

(defun init-mod-basic-sub-nat-lesson-slots ()
  (ps:ps 
    (defun init-mod-basic-sub-nat-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (;;y-or-n 
                     minus-no-borrow
                     minus-2digit qnr-tmp digit-minus-input-div
                     make-digit-minus-handler
                     ) ephemeral
          (with-slots (number-from-nlid
                       y-or-n
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

             minus-no-borrow
             (lambda (vname)
               (funcall minus-2digit vname false))

             minus-2digit
             (lambda (vname borrow)
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
                          (jq (strcat "The sum is $" ques "$."))
                          (t1 (funcall get-inline-term ques 0))
                          (t2 (funcall get-inline-term ques 2))
                          (t1units (funcall get-digit t1 0))
                          (t1tens (funcall get-digit t1 1))
                          (t2units (funcall get-digit t2 0))
                          (t2tens (funcall get-digit t2 1))
                          (aunits (if borrow
                                      (strcat "" (+ (parse-int t1units)
                                                    (parse-int t2units)))
                                      (funcall get-digit ans 0)))
                          (atens (funcall get-digit ans 1))
                          (qholder (plain-div)))
                     (append-para-text qholder jq true)
                     (funcall digit-minus-input-div 
                              borrow qname qholder t1 t2 aunits "units")
                     (funcall digit-minus-input-div 
                              borrow qname qholder t1 t2 atens "tens")
                     (funcall digit-minus-input-div 
                              borrow qname qholder t1 t2 ans)
                     (prune-tree-from-nth ltd 0)
                     (append-child ltd qholder)))))))

             digit-minus-input-div ;; Slot
             (with-slots (snc) qnr-tmp
               (with-slots (tot curr eh) snc
                 (lambda (borrow vname holder t1 t2  ans placename)
                   (let* ((div (div-class "digit-minus-input"))
                          (id (if placename
                                  (strcat "digit-minus-" placename)
                                  "digit-minus-result"))
                          (max (length ans)) (sz max)
                          (inp (make-text-input-element max sz))
                          (txt1 (if placename
                                    (strcat "What is the result of subtracting the "
                                            placename " digits of the sum"
                                            (if (and borrow
                                                     (string= placename "tens"))
                                                ", remembering to exchange a higher power of ten?"
                                                "?"))
                                    "What is the final result of the
                                               sum?"))
                          ;;(txt2 (strcat " " t1 " and " t2 "?  "))
                          (handler (funcall make-digit-minus-handler 
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

             make-digit-minus-handler ;; Slot
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
                                                      "Correct! Well done." 
                                                      (strcat "Correct!
                                                      So the "
                                                      placename "
                                                      digit in the
                                                      final answer
                                                      is " (funcall
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
                                                          "No, " 
                                                          input 
                                                          " is wrong. Try again!"))
                                     (append-child pn txtdiv)
                                     (setf (value this) "")))
                               (setf (value this) input) )))))))

             ;; y-or-n
             ;; (lambda (control-name yes-txt no-txt)
             ;;   (if (and (string= from-direction "rtl")
             ;;            (not (= 0 current-lesson-text)))
             ;;       (funcall previous-lesson-text)
             ;;       (labels ((make-y-or-n-handler (txt)
             ;;                  (lambda (ev)
             ;;                    (remove-event-no-capture y "click" yhandler)
             ;;                    (remove-event-no-capture n "click" nhandler)
             ;;                (funcall restore-default-rl-handlers)
             ;;                (append-para-text ltd txt)
             ;;                (append-para-text 
             ;;                 ltd "Click the right arrow to see why."))))
             ;;     (let* ((ltd (doc-get-el-by-id "lesson-text-display"))
             ;;            (form (create-element "form"))
             ;;            (ydiv (plain-div))
             ;;            (ndiv (plain-div))
             ;;            (y (make-radio-input-element control-name))
             ;;            (n (make-radio-input-element control-name))
             ;;            (yhandler (make-y-or-n-handler yes-txt))
             ;;            (nhandler (make-y-or-n-handler no-txt)))
             ;;       (append-child ydiv y)
             ;;       (append-text ydiv "yes")
             ;;       (append-child ndiv n)
             ;;       (append-text ndiv "no")
             ;;       (append-children form ydiv ndiv)
             ;;       (funcall remove-default-rl-handlers)
             ;;       (add-event-no-capture y "click" yhandler)
             ;;       (add-event-no-capture n "click" nhandler)
             ;;       (append-child ltd form)))))
                          
             );;end of setf
            ))
        ephemeral
        ))
    (init-mod-basic-sub-nat-lesson-slots)
    ))


(define-lesson mod-basic-sub-nat


    :y-or-n (("riddle_q1" "Yes, thats right!" 
                          "No, surprisingly, he can!" 
                          "Press the right arrow to see how.")
             ((:p "Before we go on with subtraction, Here is a little
    riddle which shows how the decimal position system works:") (:p "A
    man has to count a big pile of small stones, far more than 10, but
    he can only count to 10.  Can he count all the stones?")))

    ;;:display (funcall y-or-n 

    ;; :step ((:p "Before we go on with subtraction, Here is a little
    ;; riddle which shows how the decimal position system works:") (:p "A
    ;; man has to count a big pile of small stones, far more than 10, but
    ;; he can only count to 10.  Can he count all the stones?"))

    ;; :display (funcall y-or-n "riddle_q1" "Yes, thats right!" "No, surprisingly, he can!")

    :step ((:p "The man does this:") (:p "He counts 10 stones and puts
    them in a bag, marked " (:q "10") ". Then he counts another 10
    stones and puts " (:strong "them") " in another bag
    marked " (:q "10") ".  He does this until he has made 10 bags,
    each filled with 10 stones.") (:p "Now, first he thinks he has a
    problem, because there are still lots of stones to count, but then
    he realises he can do the same thing again!") (:p "So he puts the
    10 bags, each with 10 stones in them, into
    a " (:strong "bigger") " bag which he marks " (:q "10 times
    ten") " because thats what is inside it, 10 bags, each with 10
    stones in them."));; (:p "Then he decides to mark
    ;; it " (:q "hundreds") "."))

    :y-or-n (("riddle_q2" "Yes, he can!." 
                          "No, his idea is good enough!"
                          "Press the right arrow to see why.")
             ((:p "Can he count " (:strong "all") " the stones if he
    continues in this way, with the same idea?")))

    ;;:display (funcall y-or-n 

    :step ((:p "In fact, whenever he has counted 10 of something, he
    just puts them into a bag which he then knows contains 10 times
    more than the last thing he was counting. When 10 of the bigger
    bags are full, he puts " (:strong "them") " into an even bigger
    bag, and so on.") (:p "When he is finished organizing the stones
    and bags of stones, and bags of bags of stones ... , if there were
    7395 stones to start with, he can say something like, " (:q "There
    are 7 bags of " (:q "10 times 10 times 10") ", 3 bags of " (:q "10
    times 10") ", 9 bags of tens and 5 stones not in a bag: 7
    thousands, 3 hundreds, 9 tens and 5")) (:p "He
    has " (:q "counted") " all the stones without ever directly using
    a number bigger then 10!"))

    :step ((:p "Maybe you think, " (:q "But that's cheating, he's
    not " (:strong "really counting") "!")) (:p "Maybe, but that's how
    the decimal position system works: it " (:strong "organises") "
    whatever we are counting into units, groups of tens, groups of
    hundreds, groups of thousands, and so on.") (:p "It does this,
    because doing so allows us to add and subtract (and do other
    things) with the numbers which we make in this way.") (:p "What
    might seem a pointless idea, just organising the things we are
    counting, turns out to be a very powerful
    idea."))

    :step ((:p "In this story, the man is counting " (:q "stones") "
    but in fact, it could be anything he is counting, or just a number
    which does " (:strong "not") " refer to anything, which can be
    organised in this way.") (:p "Later, you will learn about numbers
    which can not be exactly represented by this system."))

    :step ((:p "The " (:strong "names") " which we give to the bigger
    and bigger " (:q "bags") " do not really matter, except that we
    need to call them something. The important thing about them is
    that each is ten times more than the one that went
    before.") (:p "In everyday language we call them tens, hundreds,
    thousands, and so on.  You already know how to write them
    mathematically (with a 1 and zeroes). Later on you will learn
    about a way to name them mathematically, and another way to write
    them.") (:p "But now, lets get back to subtracting."))

    :step ((:p "From a previous lesson, we know:" (:ul (:li "what the
    digits in a number " (:strong "are.")) (:li "what
    they " (:strong "mean") " depends on " (:strong "where") " they
    are in the number.") (:li "If we add 2 or
    more " (:q "put-together-numbers") " we can add the digits which
    have the same positions in the numbers, because these digits
    tell " (:strong "how many") " of that group there are in each
    number.")) (:p "We can use the same idea when we
    subtract " (:q "put-together-numbers"))))

    :stepj ((:p "Here is an example:") (:p "The sum " (:q "$19-13$") "
    means we must subtract 10 and 3 " (:strong "from") " 10 and
    9.") (:p "We can just do $9-3=6$, to discover that there are 6
    units in the final answer and $1-1=0$ to discover that there are
    no tens in the final answer.") (:p "Adding the tens and units
    gives us $0+6=6$."))

    :step ((:p "As you know, sometimes, when we
    are " (:strong "adding") ", if the digits added together are
    greater than 9, we have to remember to add the extra digit in to
    the next higher power of ten.")
    (:p "In subtraction, there can also be a situation where we
     have to do something extra."))

    :step ((:p "If the digit we are subtracting from in the first
    number is smaller than the one we are taking away, we have to do
    something extra to find the right answer.") (:p "We will learn
    about it in a later lesson, but if you think carefully about the
    story of organising the stones, you might be able to think of what
    to do in the following situation:") (:p "You have 2 bags of 10
    stones and 3 stones not in a bag, and you must subtract 7 stones.
    What do you do?") (:p "The exercises you will do after this lesson
    will not require that you do something special, you can just
    subtract from the digit in the same position in the first
    number."))

    :step ((:p "Here follows an example for you to try. It should be
    pretty easy for you, and it is really just to help you get used to
    thinking about subtracting digits in matching positions. When you
    have completed it, a button will appear.  At that point, you can
    either continue with the lesson or, if you want to try another
    sum, click on the button that appears to get a new one."))

    :using-q&a-vars
    ((:q&a-var "noborrow1" teen-sub-nb)
     (funcall (lambda () (funcall minus-no-borrow "noborrow1"))))
    
    :step ((:h1 "Summary")
           (:p "In a subtraction, we subtract the second number from
           the first.  To do this:")
           (:ul (:li "We subtract the digits in matching positions in
           the second number from the digits in those postions in the
           first number ") (:li "If the digit we are
           subtracting " (:strong "from") " in the first number
           is " (:strong "smaller") " than the one in the second
           number, we must do something special, which we will learn
           about in a later lesson"))
           
           (:p "Press the right arrow to complete this lesson."))
    :complete? t)


