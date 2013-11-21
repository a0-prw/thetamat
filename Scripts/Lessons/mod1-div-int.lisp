(in-package :autotutor)
;;FIXME remember to setf ps:*ps-print-pretty* nil

(defun init-mod1-div-int-lesson-slots ()
  (ps:ps 
    (defun init-mod1-div-int-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (dummy initialize-ephemeral
                     ) ephemeral
          (with-slots ( get-inline-term remove-default-rl-handlers 
                       restore-default-rl-handlers show-summary quit-early
                       garbage record-garbage remove-garbage
                       current-lesson-variables label-mark 
                       next-lesson-text previous-lesson-text 
                       reset-var use-var init-var ajax-reset-var
                       teach add-or-remove-wrong-click-nl-handlers
                       numberline-id wrong-click-nl random-int
                       from-direction current-module-name) non-ephemeral 
            (setf
             dummy false               
             initialize-ephemeral
             (lambda ()
               (setf dummy true))
                              
             );;end of setf

            ))
        ephemeral
        ))
    (init-mod1-div-int-lesson-slots)
    
    ))


(define-lesson mod1-div-int
    :stepj ((:p "There are many " (:em "very") " different situations
    in " (:q "real life") " where division or an idea using division
    is useful.") (:p "Mathematically, things are simpler: division and
    multiplication are like two sides of one coin." (:strong " It will
    help you a lot ") " if you learn to think of them as just two
    different ways of looking at " (:q "one
    thing") ".") (:p "A " (:em "mathematical") " answer to the
    question, " (:q "What " (:em "is") " division? ") " is this:"
    ) (:p "We say what division is by using multiplication like this:
    Two numbers are " (:strong "factors") " in
    a " (:strong "product") " " (:em "if and only if") "
    the " (:strong "product") " " (:strong "divided") " by either one
    of the numbers is equal to the other number."))

    :stepj ((:p "If we call the factors in this definition $A$ and
    $B$, and the product $C$, then we can write the previous
    definition more clearly as follows:") (:p "$C \\div B = A \\iff A
    \\cdot B = C$.")(:p "The double-sided arrow is a mathematical way
    of writing " (:q "if and only if") ". A statement using this
    double arrow is only true if the things on both sides of the
    double arrow are true.  Using it as a " (:q "definition") " means
    you are stating that both sides are true.")  (:p "As an example,
    since it is true that $23\\cdot 17=391$, then by definition,
    $391\\div 17=23$ is also true.") (:p "One useful consequence of
    this is that a division can always be checked by doing a
    multiplication, as shown on the next few pages."))

    ;; :y-or-n (("riddle_q1" "Yes, thats right!" "No, surprisingly, he can!")
    ;;          ((:p "Before we go on with subtraction, Here is a little
    ;; riddle which shows how the decimal position system works:") (:p "A
    ;; man has to count a big pile of small stones, far more than 10, but
    ;; he can only count to 10.  Can he count all the stones?")))

    :y-or-nj (("false_yn1" 
               "No, it can not true, since $11\\cdot 7 \\neq 87$." 
               "Well done. It can not be true, since $11\\cdot 7 \\neq 87$."
               "Press the right arrow to try another.") 
              
              ((:p "If we want to know if $117\\div 13=9$ is
    correct, we can multiply the number after the division sign ($13$)
    by the number after the equal-sign ($9$) and if the product is the
    same as the number before the division sign ($117$), then the
    division was correct.  In this case, $13\\cdot 9=117$, so the
    division was correct.") (:p "Here is one for you to try: Is it
    true that $87\\div 11=7$?")))

    :y-or-nj (("true_yn2"
               "That's right, since $17\\cdot 8=136$."
               "No, it must be true, because $17\\cdot 8=136$"
               "Press the right arrow to try another one.")
              (:p "Is it true that $136\\div 8=17$?"))

    :y-or-nj (("true_yn3"
               "Yes, since $15\\cdot 15=225$."
               "No, it must be true, because $15\\cdot 15=225$"
               "Press the right arrow to try continue.")
              (:p "Is it true that $225\\div 15=15$?"))
    
    :stepj ((:p "Another nice consequence, apart from being able to
    check a division with a multiplication, is that the rules for
    dividing with negative numbers automatically correspond to the
    rules for multiplication with negative numbers.") (:p "For
    example: is it true that $128\\div -8=-16$?") (:p "Yes, since
    $-16\\cdot -8=128$."))

    :y-or-nj (("false_yn4" 
               "No, it can not true, since $-12\\cdot 6 \\neq 72$." 
               "Well done. It can not be true, since $-12\\cdot 6 \\neq 72$."
               "Press the right arrow to try another.")
              (:p "Is it true that $72\\div 6=-12$?"))

    :y-or-n-j (("true_yn5"
                "Correct! Because $-17\\cdot -12=204$."
                "No, in fact it is true since $-17\\cdot -12=204$."
                "Press the right arrow to try a final example.")
               (:p "Is it true that $204\\div -12=-17$?"))

    :y-or-nj (("false_yn6" 
               "No, it can not true, and we do not even need to
calculate, since $-658\\cdot -123$ can never equal the negative number
$-80934$."
               "Well done. It can not be true, and we do not even need
to calculate to discover this, since $-658\\cdot -123$ can never equal
the negative number $-80934$."
               "Press the right arrow to continue with the lesson.")
              (:p "Is it true that $-80934\\div -658=-123$?"))

    :stepj ((:p "The mathematical definition of division also explains
    why no number can be divided by zero.") (:p "Let us see why we can
    not make sense of dividing some number by zero: let $A$ be any
    number and consider $A \\div 0=x$.") (:p "We want to know what $x$
    could possibly be?  In order to answer this according to our
    definition of division, we have to find a number $x$ which when
    multiplied by zero gives another number $A$.  But we already know
    that " (:strong (:em "all")) " numbers multiplied by zero give a
    result of zero.") (:p "Mathematicians decided to deal with this
    special case be deciding that division by zero is undefined.  It
    simply has " (:em "no meaning") " in ordinary mathematics.  Try
    getting a pocket calculator or a computer to calculate a number
    divided by zero and see what it says."))

    :step ((:p "We already mentioned in an earlier lesson that the
    result of a division is called a quotient.  The number you are
    dividing is called a " (:strong "dividend") " and the number you
    are dividing by is called a " (:strong "divisor") ". They have
    different names, because you cannot swap their positions around
    the division sign.  In contrast, when you multiply, either
    factor " (:em "could") " come first and still give the same
    result, so in that case, we do not need a special name for each
    factor.")(:p "Calculating divisions yourself involves
    a " (:q "trial-and-error") " process, unless you happen to know
    the answer already.  By " (:q "trial and error") " we mean that
    you try a number to multiply with the divisor, and if it is
    not " (:q "close enough") ", you try another. We mean something
    special by " (:q "close enough")". This " (:q "trial and error") "
    could also be called " (:q "guess and test") ".") (:p "All the
    numbers we have seen divided so far have had an exact result.
    That means that when we multiply the result by the divisor, we get
    the dividend exactly.  In fact, with most divisions, that does not
    happen, and there is no exact result.") (:p "On the next page is
    an example."))

    :stepj ((:p "An easy example, with an exact result, is $12\\div
    6=2$.  Of course, this is exact and correct, because $2\\cdot
    6=12$.") (:p "How about $13\\div 6$?") (:p "None of the types of
    numbers we have discussed in the lessons up to now can give the
    exact result of $13$ when multiplied by $6$ or $2$!  To have an
    exact result, we will have to learn about a new type of number
    which has only briefly been mentioned before.") (:p "When this
    situation arises, there is a " (:strong "difference") " between
    the dividend and the closest you can get by multiplying whole
    numbers.  This difference is called
    a " (:strong "remainder") ". In the division $13\\div 6$, the
    remainder is $1$, because $13= (6\\cdot 2)+1$."))

    :y-or-nj (("true_yn7" 
               "Yes, the remainder of $23\\div 7$ is $2$ because
$23=(7\\cdot 3)+2$."
               "No, the remainder of $23\\div 7$ is $2$ because
$23=(7\\cdot 3)+2$."
               "Click the right arrow to continue.")
              (:p "Yes or no? $2$ is the " (:strong "remainder") " of
              $23\\div 7$."))

    :y-or-nj (("true_yn8" 
               "Yes, the remainder of $30\\div 6$ is $0$ because
$30=(6\\cdot 5)+0$."
               "No, the remainder of $30\\div 6$ is $0$ because
$30=(6\\cdot 5)+0$."
               "Click the right arrow to continue.")
              (:p "Yes or no? $0$ is the " (:strong "remainder") " of
              $30\\div 6$."))

    :y-or-nj (("true_yn9" 
               "Yes, the remainder of $72\\div 7$ is $2$ because
$72=(7\\cdot 10)+2$."
               "No, the remainder of $72\\div 7$ is $2$ because
$72=(7\\cdot 10)+2$."
               "Click the right arrow to continue.")
              (:p "Yes or no? $2$ is the " (:strong "remainder") " of
              $72\\div 7$."))

    :step ((:p "A very important use of division is now
    introduced:") (:p "In the very first lesson in this program, it
    was stated that two ways of using numbers are counting and
    measuring.  When we measure things, the thing we are measuring
    usually does not neatly correspond to a number on our measure, but
    falls somewhere between 2 of our numberline marks.  In order to
    make more accurate measurements, the spaces between our numberline
    marks can be " (:strong "divided") " into smaller, regular
    distances than a whole unit.") (:p "When we do this, we
    are " (:strong "dividing") ", and doing so, we create a type of
    number which is sometimes called a fraction, when we mean some
    whole number (perhaps zero) " (:strong "and") " a part.") (:p "We
    would also like to have a single number as an exact result for any
    division and fractions are included in that type of
    number.") (:p "Fractions and how to calculate with them will be
    discussed in later lessons.  For now, you should remember that
    they can be visualized as points on the numberline found by
    regularly dividing up the space between the numberline marks."))
    
    :step ((:h1 "Summary")
           (:ul (:li "Two numbers are factors in a product " (:em "if
    and only if") " the product divided by one of the numbers is equal
    to the other number.")
                (:li "The rules for division involving negative
                numbers correspond to the rules for multiplying
                negative numbers.")
                (:li "One important use of division is to create
                fractions, which can be visualized as numbers marking
                numberline positions created by dividing a unit into
                smaller, regular intervals."))
           (:p "Press the right arrow to complete this lesson."))
    :complete? t)
