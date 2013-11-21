(in-package :autotutor)

(Xdefine-lesson mod-digs-nat in-mod-digs-nat)

;;(in-lesson mod-digs-nat in-mod-digs-nat)
(in-lesson mod-digs-nat)

(define-ifstep 0mod-digs-nat
    :switch special-lesson-module
    :html ((:p "Welcome to the first lesson in the program.")
           (:p "Usually, when you use this program, each part will
     start with an explanation of something and will continue with
     exercises for you to do.")
           (:p "This part is a little different.")
   (:p "Instead, after this lesson, you will play some learning games
     which can help you to memorise the basic facts of adding small
     numbers from 0 to 9, and subtracting from small numbers below
     18.")
   (:p "When you have won in the games, you will automatically go
   forward in the program.")
   (:p "Usually you must press the right arrow to continue in the
   lesson.  Sometimes, you will have to do something else to continue
   with a lesson, so it is important either that you read the text, or
   that your teacher or a someone else has read it for you.")))

(define-step 1mod-digs-nat
    :html ((:p "Very often, a good way of thinking about anything to
    do with most types of numbers is with a numberline.") (:p "Press
    the right arrow to see a numberline.")))

(define-display 2mod-digs-nat
    :code (funcall numberline "mod-digs-nat-nl" -20 20 1 "#000000" 0.125))

(define-step 3mod-digs-nat
    :html ( (:p "A numberline is just a picture of how typical numbers
    'fit together'.") (:p "You can see a 'zero' in the middle of the
    numberline.")))

(define-replace-default-next-step 4mod-digs-nat
    :event-handler-id (handle-nl-click0 "nl-mark(0)")
    :message-text "Click the zero mark on the numberline to continue")

(define-step 5mod-digs-nat
    :html ((:p "Good. Each mark after zero shows the next number, so we
    get '0, 1, 2, ...' (The dots mean the numbers continue 'forever',
    and thats why there are arrows on the line)") (:p "The numbers to
    the right of zero are called " (:strong "positive") " numbers.  We
    can also go to the left of zero to get " (:strong "negative") "
    numbers. So we have '..., -2, -1, 0'.  When we say aloud these
    numbers to the left of zero we say 'minus 2', 'minus 1' and so on.
    Sometimes people say, 'negative 1' instead.")))

(define-globals nlneg1
    :name-setter-pairs 
  (("nlneg1" (funcall numberline-id (funcall random-int -9 -1)))))

(define-replace-default-next-step 5mod-digs-nat
    :event-handler-id (handle-nl-clickneg (funcall use-var "nlneg1"))
    :message-text (strcat "Click on the " 
                          (funcall number-from-nlid 
                                   (funcall use-var "nlneg1")) 
                          " mark to continue."))

(define-step 6mod-digs-nat
    :html ((:p "We will come back to the negative numbers in a later
    lesson.  For now, we will be concentrating on
    the " (:strong "positive") " numbers.")))

(define-globals nlpos1
    :name-setter-pairs
  (("nlpos1" (funcall numberline-id  (funcall random-int 1 9)))))

(define-replace-default-next-step 7mod-digs-nat
    :event-handler-id (handle-nl-clickpos (funcall use-var "nlpos1"))
    :message-text (strcat "Click on the " 
                          (funcall number-from-nlid 
                                   (funcall use-var "nlpos1")) 
                          " mark to continue."))

(define-step 8mod-digs-nat
    :html ((:p "When you are counting things, you can imagine pairing
    that thing with its number on the numberline.")  (:p "If you are
    pairing more things with the numbers, then you
    are " (:strong "adding.") "  If you are removing things from the
    numberline, then you are " (:strong "subtracting.")) (:p "Here
    follows an example of adding in this way.")))


(define-using-q&a-variables 9mod-digs-nat
    :name-didactic-element ("ddsum1" digadd)
    :code  (progn
             (funcall init-circle-counter (parse-int (funcall use-var "ddsum1:a"))
                      (strcat "This has shown $"
                              (funcall use-var "ddsum1:q") "="
                              (funcall use-var "ddsum1:a") "$ in the numberline."))
             (funcall discrete-digit-sum-nl "t1" "t2" "ddsum1:q")))

(define-step 10mod-digs-nat
    :html ("Here follows an example with subtraction"))

(define-using-q&a-variables 11mod-digs-nat
    :name-didactic-element ("ddsub1" digsubn)
    :code (progn
            (funcall init-circle-counter 
                     (parse-int (funcall second-term "ddsub1:q"))
                     (strcat "This has shown $"
                             (funcall use-var "ddsub1:q") "="
                             (funcall use-var "ddsub1:a") "$ in the numberline."))
            (funcall discrete-digit-sub-nl "t1" "ddsub1:q")))

(define-step 12mod-digs-nat
    :html ((:p "Since we do not " (:strong "only") " use numbers to
    count things, another way of thinking with the numberline, is to
    imagine that you are measuring something with it.  This means
    seeing the numberline like a ruler or a tape-measure.") (:p "If
    you think this way, then " (:strong "adding") " numbers is like
    placing lengths represented by those numbers one after the other
    and measuring the whole thing with the numberline to see what you
    get.")))

(define-step 13mod-digs-nat
    :html ((:p "Here follows an example of seeing addition as
    placing lengths together.")))

(define-using-q&a-variables 14mod-digs-nat
    :name-didactic-element ("mdsum1" digadd-nz)
    :code (funcall add-lengths-nl "t1" "t2" "mdsum1"))

(define-step 15mod-digs-nat
    :html ((:p "When thinking of addition as placing lengths together,
    it is quite easy to see, that it does not matter which length you
    place first, since the whole length remains the same.") (:p "That
    means that if we have a plus sign between two numbers, we can swap
    the numbers around if we want to.") (:p "Also, If we had 3 numbers
    to add, it would not matter which lengths we placed together
    first") (:p "This means we are free to choose any 2 numbers to add
    first.")))

(define-step 16mod-digs-nat
    :html ((:p "If we want to " (:strong "subtract") " one number
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
    important.")))

(define-step 17mod-digs-nat
    :html ((:p "Here follows an example of seeing a subtraction
    as the difference of two lengths.")))

(define-using-q&a-variables 18mod-digs-nat
    :name-didactic-element ( "mdsub1" digsubn)
    :code (funcall sub-lengths-nl "t1" "t2" "mdsub1"))

(define-step 19mod-digs-nat
    :html ((:p "An important thing about subtraction is that it is in
    some sense the 'reverse' of addition.  This means that you can
    always check a subtraction by doing an addition.") (:p "If you
    want to try this, click the left arrow key to go back in the
    lesson and repeat the last few steps, while asking yourself this
    question: what must I " (:strong "add") " to the second number to
    get the first?")))

(define-step 20mod-digs-nat
    :html ((:h1 "Important information") (:p "You don't have to think of
    the numberline every time you do an addition or subtraction, but
    if a sum seems difficult, it can be very useful to think about it
    with the help of the numberline. You will also find the numberline
    very useful when you learn about negative numbers, fractions and
    other numbers.")))

(define-ifstep 21mod-digs-nat
    :switch special-lesson-module
    :html ((:p "That completes the first lesson.  After the next step,
     which is a summary of this lesson, you will come to the games.
     There are two games: One for learning to add and one for learning
     to subtract. The adding game comes first.")
           (:p "There are 2 goals for each game:")
           (:ul (:li (:strong "You must") " get all the questions
     right with no mistakes at least once."))
           (:p "When you have achieved the first goal, you will
     automatically come to the next part of the program.")
   (:ul
    (:li "Once you can get them all right, " (:strong "you
          can choose") " to play again as many times as
          you like, to see how fast you can do it.  A button will
          appear on your page which you can press to play again."))
           (:p "You might have to play the games many times before you
     achieve the first goal.")))

(define-summary 22mod-digs-nat
    :first-do (funcall numberline "mod-digs-nat-nl" -20 20 1 "#000000" 0.125)
    :html ((:h1 "Summary") (:ul (:li "With addition, you can take
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

(define-lesson-end end-mod-digs-nat)

(exit-current-lesson)
