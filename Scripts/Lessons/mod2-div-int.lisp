(in-package :autotutor)
;;FIXME remember to setf ps:*ps-print-pretty* nil

(defun init-mod2-div-int-lesson-slots ()
  (ps:ps 
    (defun init-mod2-div-int-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (operand-generator tutor-current-problem
                     generate-operands narray-to-string
                     make-level-div make-choose-level-screen
                     make-level-example level-butt-handler
                     make-problem-page build-layout-table
                     get-table-cell table-cell-coords insert-initial-elements
                     draw-division-marks place-dsor-inputs place-dend-inputs
                     colorise
                     tutor collect-tc-array 
                     make-user-input-dispatcher ;;handle-opd-input 
                     insert-new-remainder-elements
                     good-difference
                     place-difference-inputs
                     insert-difference-elements
                     good-partprod
                     good-res-input
                     ;;handle-res-input
                     ;;place-res-inputs
                     problem-solved
                     insert-result-element
                     place-new-remainder-inputs
                     insert-tmp-remainder-elements
                     good-input report-bad
                     initialize-ephemeral 
                                       
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

             tutor
             (ps:create
              live-input-elements false
              live-result-elements false
              
              get-subtrahend
              (lambda ()
                (in-tutor-cp
                 (let ((y (- tmp-y-offset 2)))
                   (join (loop for x from 0
                            for tc = (funcall get-table-cell x y)
                            for content = (if tc (or (text-content-of tc) "") "")
                            collect content
                            until (not tc)) ""))))
                    

              get-power-name 
              (lambda (x)
                (in-cp
                 (let* ((total-length (1- (+ (length divisor-string) (length dividend-string))))
                        (key (- total-length x)))
                   (cond
                     ((= key 3) "thousands")
                     ((= key 2) "hundreds")
                     ((= key 1) "tens")
                     ((= key 0) "units")
                     (t (alert (strcat "Unknown power key: " x)))))))
               
              initialize
              (lambda ()
                (in-tutor
                 (setf live-input-elements false
                       live-result-elements false)))

              instruct-pupil
              (lambda ()
                (in-tutor-cp
                 (let ((mh (dgebid "message-holder"))
                       (inp-done (zerop (length live-input-elements)))
                       (nresels (length live-result-elements)))
                   (remove-children-from-nth mh 0)
                   (if inp-done
                       (let ((cinp (last live-result-elements)))
                         (when cinp
                           (destructuring-bind (x y)
                               (funcall table-cell-coords (id-of (parent-node cinp)))
                             (let ((fst (zerop (- (parse-int dividend-string) tmp-remainder))))
                               (cond ((zerop y)
                                      (append-para-text 
                                       mh 
                                       (strcat "By trial-and-error, try to determine a digit (perhaps zero) to write in the quotient which when multiplied by the divisor gives a product that leaves the smallest possible non-negative remainder when subtracted from the "
                                               (if  fst "dividend." "current remainder."))))
                                     ((evenp y)
                                      (append-para-text 
                                       mh 
                                       (strcat "Multiply the digit you wrote in the quotient with the divisor, remembering to take it's position into account, and write it under the "
                                               (if fst "dividend" "current remainder."))))
                                     (t 
                                      (append-para-text 
                                       mh 
                                       (strcat "Subtract the product you found from the "
                                               (if (= 3 tmp-y-offset) "dividend" "current remainder") 
                                               " to find the new remainder."))))))))
                         (append-para-text mh "Write down the problem.")))))
                       
              explain-pupil
              (lambda ()
                (in-tutor-cp
                 (let ((mh (dgebid "message-holder"))
                       (inp-done (zerop (length live-input-elements))))
                   (remove-children-from-nth mh 0)
                   (if inp-done
                       (let ((cinp (last live-result-elements)))
                         (when cinp
                           (destructuring-bind (x y)
                               (funcall table-cell-coords (id-of (parent-node cinp)))
                             (let ((fst (zerop (- (parse-int dividend-string) tmp-remainder))))
                               (cond ((zerop y)
                                      (let ((pname (funcall get-power-name x)))
                                        (append-para-text mh (strcat "Write the " pname " digit which multiplied by " divisor-string " gives a product which leaves the smallest possible non-negative remainder when subtracted from " tmp-remainder-string "."))))
                                     ((evenp y)
                                      (let* ((pow (1- tmp-x-offset))
                                             (dig (text-content-of (funcall get-table-cell pow 0)))
                                             (pname (funcall get-power-name pow)))
                                        (append-para-text 
                                         mh (strcat "Multiply the '" 
                                                    dig 
                                                    "' "
                                                    pname
                                                    ", which you wrote in the quotient with the divisor, '" 
                                                    divisor-string
                                                    "' to find the product which will be subtracted from the "
                                                    (if fst 
                                                        (strcat "dividend '" (parse-int dividend-string) "' ")
                                                        (strcat "current remainder '" tmp-remainder-string "' "))
                                                    " in the next step."))))
                                     (t 
                                      (let ((is-dividend (= tmp-y-offset 3)))
                                        (append-para-text mh (strcat "Subtract the product you found, '"
                                                                   tmp-partial-product 
                                                                   "', from the "
                                                                   (if  is-dividend "dividend, '" "current remainder, '")
                                                                   (if is-dividend dividend-string (funcall get-subtrahend))
                                                                   "', to find the new remainder.")))))))))
                       
                       (append-para-text 
                        mh
                        (strcat 
                         "Write the divisor, which is '"
                         divisor-string
                         "', to the left of the dividing lines."
                         "  Write the dividend, which is '" 
                         dividend-string 
                         "', under the dividing lines."))))))
        
              show-pupil
              (lambda ()
                (in-tutor-cp
                 (let* ((mh (dgebid "message-holder"))
                        (live-els live-input-elements)
                        (len (length live-els))
                        (inp-done (zerop len)))
                   (remove-children-from-nth mh 0)
                   (if inp-done
                       (let ((cinp (last live-result-elements)))
                         (when cinp
                           (destructuring-bind (x y)
                               (funcall table-cell-coords (id-of (parent-node cinp)))
                             (let* ((tc (parent-node cinp))
                                    (uin (get-attribute tc "data-digit")))
                               (cond ((zerop y) 
                                      (funcall good-res-input uin tc cinp))
                                     (t (cond ((evenp (mod tmp-y-offset 2))
                                               (funcall good-partprod uin tc cinp))
                                              (t (funcall good-difference uin tc cinp)))))))))                                     
                       (dotimes (n len)
                         (let* ((inp (aref live-els 0))
                                (tc (parent-node inp))
                                (cd (get-attribute tc "data-digit")))
                           (funcall good-input cd tc inp))))))
                false))
              
             tutor-current-problem
             (ps:create
              current-problem
              (ps:create 
               divisor false
               dividend-array false
               result false
               remainder false
               result-string false
               result-array false
               remainder-string false
               tmp-y-offset 2
               tmp-x-offset false
               tmp-remainder false
               tmp-remainder-string false
               tmp-partial-product false
               dividend-string false
               divisor-string false
               problem-string false)
              initialize
              (lambda ()
                (in-cp
                 (setf divisor false
                       dividend-array false
                       result false
                       remainder false
                       result-string false
                       result-array false
                       remainder-string false
                       tmp-y-offset 2
                       tmp-x-offset false
                       tmp-remainder false
                       tmp-remainder-string false
                       tmp-partial-product false
                       dividend-string false
                       divisor-string false
                       problem-string false))))
             
             generate-operands 
             (lambda (dl)
               (let ((fn (ps:getprop 
                          (ps:getprop operand-generator dl) "function")))
                 (when (and fn (string= (typeof fn) "function"))
                   (funcall fn))))
             
             operand-generator
             (ps:create
              basic-digit
              (lambda (n)
                (case n
                  (2 (* 2 (funcall random-int 1 4)))
                  (3 (* 3 (funcall random-int 1 3)))
                  (4 (* 4 (funcall random-int 1 2)))))
              second-digit 
               (lambda (n)
                 (case n
                   (2 (* 2 (funcall random-int 0 4)))
                   (3 (1- (* 3 (funcall random-int 1 3))))
                   (4 (aref (ps:array 2 6) (funcall random-int 0 1)))
                   (5 (aref (ps:array 5 0) (funcall random-int 0 1)))
                   (6 (aref (ps:array 2 8) (funcall random-int 0 1)))
                   (7 4)
                   (8 6)))
              ;;difficulty level 0 ; table-division
              0 (ps:create
                 function (lambda () (let ((divisor (funcall random-int 4 9))
                                           (factor (funcall random-int 4 9)))
                             (ps:array divisor (map (split (strcat "" (* divisor factor)) "") 
                                                    (lambda (v i a) (parse-int v))))))
                 description "Simple table division of single digit into a 10-by-10 table product.")
              ;;difficulty level 1 ; no remainders, 1 digit divisor
              1 (ps:create
                 function (lambda () (with-divops 
                               (let* ((divisor (funcall random-int 2 4))
                                      (dividend (ps:array)))
                                 (dotimes (x 3 (ps:array divisor dividend))
                                   (push (funcall basic-digit divisor) dividend)))))
                 description "1-digit divisor exactly divides all digits of 3-digit dividend.")
              ;;difficulty level 2 ; 1 remainder , 1 digit divisor
              2 (ps:create 
                 function (lambda () (with-divops 
                                         (let* ((divisor (funcall random-int 2 8))
                                                (fd (if (< divisor 5) (1+ (funcall basic-digit divisor))
                                                        (1+ divisor)))
                                                (sd (funcall second-digit divisor))
                                                (td (if (< divisor 5) (funcall basic-digit divisor)
                                                        divisor)))
                                           (when (= fd 10) (setf fd 7))
                                           (ps:array divisor (ps:array fd sd td)))))
                 description "Up to 2 remainders with 1-digit divisor into 3-digit dividend.")
              ;;difficulty level 3 ; up to 3 remainders , 1 digit divisor
              3 (ps:create 
                 function (lambda () (let* ((divisor (funcall random-int 2 9))
                                            (low (ceiling (/ 999 divisor)))
                                            (hi (floor (/ 9999 divisor)))
                                            (dividend (* divisor (funcall random-int low hi))))
                                       (ps:array divisor (map (split (strcat "" dividend) "")
                                                              (lambda (v i a) (parse-int v))))))
                 description "Up to 3 remainders with 1-digit divisor into 4-digit dividend.")
              4 (ps:create
                 function (lambda () (let* ((divisor (funcall random-int 11 99))
                                            (low (ceiling (/ 999 divisor)))
                                            (hi (floor (/ 9999 divisor)))
                                            (dividend (* divisor (funcall random-int low hi))))
                                       (ps:array divisor (map (split (strcat "" dividend) "")
                                                              (lambda (v i a) (parse-int v))))))
                 description "2-digit divisor into 4-digit dividend."))
             
             colorise
             (lambda (ev)
               (in-tutor-cp
                (labels ((get-divisor-cells ()
                           (let ((len (length divisor-string)))
                             (loop for x from 0 below len
                                  collect (funcall get-table-cell x 1))))
                         (collect-cells (y)
                           (let ((offs (length divisor-string))
                                 (len (length dividend-string)))
                             (loop for x from offs to (1- (+ offs len))
                                  collect (funcall get-table-cell x y))))
                         (get-dividend-cells ()
                           (funcall collect-cells 1))
                         (get-quotient-cells ()
                           (funcall collect-cells 0)))
                  (let ((quotch (dgebid "quotient-key-holder"))
                        (dsorch (dgebid "divisor-key-holder"))
                        (dendch (dgebid "dividend-key-holder")))
                    (dolist (el (ps:array quotch dsorch dendch))
                      (toggle-class el "dt-invisible-text"))
                    (toggle-class quotch "dt-quotient-color")
                    (toggle-class dsorch "dt-divisor-color")
                    (toggle-class dendch "dt-dividend-color"))
                  (dolist (tc (funcall get-divisor-cells))
                    (toggle-class tc "dt-divisor-color"))
                  (dolist (tc (funcall get-dividend-cells))
                    (toggle-class tc "dt-dividend-color"))
                  (dolist (tc (funcall get-quotient-cells))
                    (toggle-class tc "dt-quotient-color")))))
             
             narray-to-string
             (lambda (na)
               (labels ((string-plus (s1 s2)
                          (strcat s1 s2)))
               (array-reduce na string-plus ""))) ;;Use join

             make-level-example
             (lambda (operands)
               (in-tutor (funcall initialize))
               (with-tcp (funcall initialize))
               (in-cp
                (destructuring-bind (t1 t2)
                    operands
                  (setf divisor t1 dividend-array t2
                        divisor-string (strcat "" t1)
                        dividend-string (funcall narray-to-string t2)
                        tmp-remainder (parse-int dividend-string)
                        tmp-remainder-string dividend-string
                        result (/ (parse-int dividend-string) (parse-int divisor-string))
                        remainder (mod (parse-int dividend-string) (parse-int divisor-string))
                        result-string (strcat "" result)

                        ;;decimal-split (split result-string ".")
                        remainder-string (strcat "" remainder)
                        problem-string (strcat "$" dividend-string "\\div " divisor-string "$")))
                (let ((zeros (let ((tmp (ps:array)))
                               (dotimes (x (- (length dividend-string) (length result-string)) tmp)
                                 (push "0" tmp)))))
                  (setf result-array (append zeros (split result-string ""))))
                (let ((ltd (dgebid "lesson-text-display")))
                  (remove-children-from-nth ltd 0)
                  (append-child ltd (funcall make-problem-page))
                  (append-para-text (dgebid "current-display") problem-string true)
                  (funcall insert-initial-elements))
                false))
             
             get-table-cell
             (lambda (x y)
               (dgebid (strcat "table-cell_" x "," y)))

             table-cell-coords
             (lambda (tc)
               (map (split (aref (split tc "_") 1) ",")
                    (lambda (v i a) (parse-int v))))               
             
             draw-division-marks
             (lambda (tc-arr)
               (dotimes (x (length tc-arr))
                 (let ((curr (aref tc-arr x)))
                   (if (zerop x)
                       (progn 
                         (remove-class curr "dt-bordered-cell")
                         (add-class curr "dt-vert-div-mark"))
                       (progn
                         (remove-class curr "dt-bordered-cell")
                         (add-class curr "dt-horiz-div-mark"))))))

             collect-tc-array
             (lambda (xoffs str)
               (in-tutor-cp
               (loop for digit in (split str "")
                  for x from xoffs
                  collect (let ((tc (funcall get-table-cell 
                                             x tmp-y-offset)))
                            (set-attribute tc "data-digit" digit)
                            tc))))
             
             make-user-input-dispatcher
             (lambda (good bad)
               (lambda (ev)
                 (prevent-default ev)
               (when (numeric-input ev)
                 (let ((uin (strcat "" (key-char ev)))
                       (inp this)
                       (tc (parent-node this)))
                   (if (string= uin (get-attribute tc "data-digit"))
                       (funcall good uin tc inp)
                       (funcall bad inp uin))))))

             good-difference
             (lambda (uin tc inp)
               ;; ;;if live-result-elements is empty, we are finished with input
               ;; ;;do the next result digit element
               (let* ((tn (make-text-node uin))
                      (mh (dgebid "message-holder")))
                 (remove-children-from-nth mh 0)
                 (remove-child tc inp)
                 (append-child tc tn))
               (in-tutor-cp 
                (cond ((zerop (length live-result-elements)) ;;finished
                       (incf tmp-y-offset)
                       (funcall insert-result-element tmp-x-offset))
                      (t (focus (last live-result-elements))))))

             place-difference-inputs
             (lambda (tc-arr)
               (let ((handler (funcall make-user-input-dispatcher 
                                       good-difference report-bad)))
                 (dolist (tc tc-arr)
                   (let ((inp (make-text-input-element 1 1)))
                     (add-event-no-capture inp "click" (lambda (ev)
                                                         (focus this)))
                     (add-event-no-capture inp "keydown" handler)
                     (append-child tc inp))))
               (in-tutor-cp (focus (last live-result-elements))))

             insert-difference-elements
             (lambda ()
               (in-tutor-cp
                (let* ((diff (- tmp-remainder tmp-partial-product))
                       (diffstr (strcat "" diff))
                       (diffslen (length diffstr))
                       (xoff (+ (length divisor-string)
                                (- (length dividend-string)
                                   diffslen))))
                  (setf tmp-remainder diff
                        tmp-remainder-string diffstr)
                  (let ((tc-arr (funcall collect-tc-array xoff diffstr)))
                    (funcall place-difference-inputs tc-arr))))
               false)
             
             good-partprod
             (lambda (uin tc inp)
               ;;if live-result-elements is empty, we are finished with input
               ;;do the difference elements
               (let* ((tn (make-text-node uin))
                      (mh (dgebid "message-holder")))
                 (remove-children-from-nth mh 0)
                 (remove-child tc inp)
                 (append-child tc tn))
               (in-tutor-cp 
                (cond ((zerop (length live-result-elements)) ;;finished
                       (incf tmp-y-offset)
                       (destructuring-bind (x y)
                           (funcall table-cell-coords (id-of tc))
                         (let ((times (length (strcat "" tmp-partial-product)))
                               (ntn (make-text-node "-"))
                               (ntc (funcall get-table-cell (1- x) y)))
                           (append-child ntc ntn)
                           (loop for time from 0 below times
                                for n from x
                                with y = tmp-y-offset
                                do (let ((linecell (funcall get-table-cell n y)))
                                     (add-class linecell "dt-horiz-div-mark")))))
                       (funcall insert-difference-elements))
                      (t (focus (last live-result-elements))))))
                          
             place-new-remainder-inputs
             (lambda (tc-arr)
               (let ((handler (funcall make-user-input-dispatcher 
                                       good-partprod report-bad)))
                 (dolist (tc tc-arr)
                   (let ((inp (make-text-input-element 1 1)))
                     (add-event-no-capture inp "click" (lambda (ev)
                                                         (focus this)))
                     (add-event-no-capture inp "keydown" handler)
                     (append-child tc inp))))
               (in-tutor-cp (focus (last live-result-elements))))
             
             insert-new-remainder-elements
             (lambda (partprod newrem)
               (in-tutor-cp
                (setf tmp-partial-product partprod)
                (let* ((str (strcat "" tmp-partial-product))
                       (xoff (+ (length divisor-string)
                                (- (length dividend-string)
                                   (length str))))
                       (tc-arr (funcall collect-tc-array xoff str)))
                  (funcall place-new-remainder-inputs tc-arr))))
             
             good-res-input
             (lambda (uin tc inpel)
               (in-tutor-cp
                (let* ((tn (make-text-node uin))
                       (mh (dgebid "message-holder")))
                  (destructuring-bind (x y)
                      (funcall table-cell-coords (id-of tc))
                    (let* (;(x (parse-int x))
                           (zeros (- (length result-array) (length result-string)))
                           (divlen (length divisor-string))
                           (offs (+ zeros divlen)))
                      (remove-children-from-nth mh 0)
                      (remove-child tc inpel)
                      (when (>= x offs)
                        (append-child tc tn))
                      (let* ((ui (parse-int uin))
                             (next (1+ x))
                             (power (+ (1- divlen) (- (length dividend-string) x)))
                             (partprod (* (pow 10 power) ui divisor))
                             (newrem (- tmp-remainder partprod))
                             (change (- tmp-remainder newrem)))
                        (if (zerop change)
                            (funcall insert-result-element next (< next offs))
                            (progn
                              (setf tmp-x-offset next)
                              (funcall insert-new-remainder-elements 
                                     partprod newrem))
                               ))))))
               false)
             
             ;; handle-res-input
             ;; (lambda (ev)
             ;;   (prevent-default ev)
             ;;   (when (numeric-input ev)
             ;;     (let ((uin (strcat "" (key-char ev)))
             ;;           (inp this)
             ;;           (tc (parent-node this)))
             ;;       (if (string= uin (get-attribute tc "data-digit"))
             ;;           (funcall good-res-input uin tc inp)
             ;;           (funcall report-bad inp uin)))))
             problem-solved
             (lambda ()
               (in-cp
                (let ((mh (dgebid "message-holder"))
                      (trailing-zero (zerop (mod result 10))))
                  (remove-children-from-nth mh 0)
                  (when trailing-zero 
                    (let ((tn (make-text-node "0"))
                          (tc (funcall get-table-cell tmp-x-offset 0)))
                      (append-child tc tn)))
                 (append-para-text 
                  mh 
                  (strcat "'" 
                          result-string 
                          "' is the correct quotient.  Go back to choose a level if you want to do another or click the right-arrow to continue with the lesson.")))))

             ;;TODO handlers for tell show etc. Also, don't print
             ;;message if show did it

             insert-result-element
             (lambda (&optional (next false) (initial-zero false))
               (in-cp
                (if (zerop tmp-remainder)
                    (funcall problem-solved)
                    (let ((handler (funcall make-user-input-dispatcher
                                            good-res-input report-bad)))
                      (labels ((make-it-so (tc cd inp zeros)
                                 (set-attribute tc "data-digit" cd)
                                 (setf (id-of inp) (strcat "res_" zeros))
                                 (append-child tc inp)
                                 (focus inp)
                                 (add-event-no-capture inp "keydown" handler)
                                 ;;handle-res-input)
                                 (add-event-no-capture inp "click" (lambda (ev)
                                                                     (focus this)))))
                        (if (not next)
                            (let* ((ntmp (length tmp-remainder-string))
                                   (ndend (length dividend-string))
                                   (ndsor (length divisor-string))
                                   (zeros (- ndend ntmp))
                                   (x (+ ndsor zeros))
                                   (rdarr result-array)
                                   (cd (aref rdarr zeros))
                                   (tc (funcall get-table-cell x 0))
                                   (inp (make-text-input-element 1 1)))
                              (funcall make-it-so tc cd inp zeros))
                            ;; (if (not initial-zero)
                            ;;     (funcall 
                            (let* ((x next)
                                   (ndsor (length divisor-string))
                                   (zeros (- x ndsor))
                                   (cd (aref result-array 
                                             (- x ndsor)))
                                   (tc (funcall get-table-cell x 0))
                                   (inp (make-text-input-element 1 1)))
                              (funcall make-it-so tc cd inp zeros))))
                      (in-tutor
                       (setf live-result-elements 
                             (gebtag (dgebid "current-layout-table") "input")))
                      false))))

             insert-tmp-remainder-elements
             (lambda ()
               (in-tutor
                (when (not live-result-elements)
                  (funcall insert-result-element))
               false))

             good-input
             (lambda (uin tc inpel)
               (in-tutor
                (let ((tn (make-text-node uin))
                      (mh (dgebid "message-holder")))
                  (remove-children-from-nth mh 0)
                  (remove-child tc inpel)
                  (append-child tc tn)
                  (let ((next (aref live-input-elements 0)))
                    (when next (focus next)))
                  (let ((inp-done (zerop (length live-input-elements))))
                    (when inp-done 
                      ;;this next setf is necessary because otherwise new
                      ;;input-elements will register here as well
                      (setf live-input-elements (ps:array))
                      (funcall insert-tmp-remainder-elements))))))
                        
             report-bad 
             (lambda (inpel uin)
               (let ((mh (dgebid "message-holder")))
                 (remove-children-from-nth mh 0)
                 (append-para-text mh (strcat 
                                       "No, '" uin "' is incorrect! Try again."))
                 (setf (value inpel) "")
                 false)) ;;CHECK fup with hint-level??             

             ;; handle-opd-input
             ;; (lambda (ev)
             ;;   (prevent-default ev)
             ;;   (when (numeric-input ev)
             ;;     (let ((uin (strcat "" (key-char ev)))
             ;;           (inp this)
             ;;           (tc (parent-node this)))
             ;;       (if (string= uin (get-attribute tc "data-digit"))
             ;;           (funcall good-input uin tc inp)
             ;;           (funcall report-bad inp uin)))))

;;                   (funcall report-bad inp uin))))

             place-dsor-inputs
             (lambda (tc-arr)
               (let ((ds (length tc-arr))
                     (handler (funcall make-user-input-dispatcher 
                                       good-input report-bad)))
                 (dotimes (d ds)
                   (let ((inp (make-text-input-element 1 1)))
                     (setf (id-of inp) (strcat "dsor_" d))
                     (append-child (aref tc-arr d) inp)
                     (add-event-no-capture inp "keydown" handler)
                     ;;handle-opd-input)
                     (add-event-no-capture inp "click" (lambda (ev)
                                                         (focus this))))))
               false)

             place-dend-inputs
             (lambda (tc-arr)
               (let ((ds (length tc-arr))
                     (handler (funcall make-user-input-dispatcher
                                       good-input report-bad)))
                 (dotimes (d ds)
                   (let ((inp (make-text-input-element 1 1)))
                     (setf (id-of inp) (strcat "dend_" d))
                     (append-child (aref tc-arr d) inp)
                     (add-event-no-capture inp "keydown" handler)
                     ;;handle-opd-input)
                     (add-event-no-capture inp "click" (lambda (ev)
                                                         (focus this))))))
               false)             
             
             insert-initial-elements
             (lambda ()
               (in-cp 
                (let* ((ndsors (length divisor-string))
                       (ndends (length dividend-string))
                       (dsor-cells (ps:array))
                       (dend-cells (ps:array)))
                  (dotimes (x ndsors)
                    (let ((tc (funcall get-table-cell x 1)))
                      (set-attribute tc "data-digit" (aref divisor-string x))
                      (push tc dsor-cells)))
                  (dotimes (x ndends)
                    (let ((tc (funcall get-table-cell (+ ndsors x) 1)))
                      (set-attribute tc "data-digit" (aref dividend-string x))
                      (push tc dend-cells)))
                  (funcall draw-division-marks dend-cells)
                  (funcall place-dsor-inputs dsor-cells)
                  (funcall place-dend-inputs dend-cells)
                  (in-tutor (setf live-input-elements (gebtag (dgebid "current-layout-table") "input")))
                  (ps:array dsor-cells dend-cells))))

             build-layout-table
             (lambda (cols rows)
               (let ((tab (div-id-class "current-layout-table" "dt-table dt-collapsed-borders")))
                 (loop for row from 0 to rows
                      do (let ((tr (div-id-class (strcat "table-row_" row) "dt-table-row")))
                           (loop for col from 0 to cols 
                              do (let ((tc (div-id-class (strcat "table-cell_" col "," row) "dt-table-cell dt-bordered-cell")))
                                   (setf (get-style tc "height") (strcat svg-grid-skip "em")
                                         (get-style tc "width") (strcat svg-grid-skip "em"))
                                   (append-child tr tc)))
                           (append-child tab tr)))
                 tab))
                     
             make-problem-page
             (lambda ()
               (let ((ppageh (div-id "problem-page-holder"))
                     (pbannerh (div-id-class "problem-banner-holder" ""))
                     (pconth (div-id-class "problem-content-holder" "dt-table"))
                     (backh (div-id-class "backbutt-holder" "dt-table-cell"))
                     (backbutt (make-button "back-button" "Back" 
                                            ("click" make-choose-level-screen)))
                     (probdisph (div-id-class "problem-display-holder" "dt-table-cell"))
                     (currentdisp (div-id "current-display"))
                     (instrh (div-id-class "instructbutt-holder" "dt-table-cell"))
                     (instrbutt (make-button "instruct-button" "Instruct"))
                     (explh (div-id-class "explainbutt-holder" "dt-table-cell"))
                     (explbutt (make-button "explain-button" "Explain"))
                     (showh (div-id-class "showbutt-holder" "dt-table-cell"))
                     (showbutt (make-button "show-button" "Show"))
                     (controw1 (div-id-class "content-row1" "dt-table-row"))
                     (problayout (div-id-class "problem-layout" "dt-table-cell"))
                     (colorisebutt (make-button "colorise-current" "Colorise"))
                     (qtn (make-text-node "Quotient"))
                     (dsortn (make-text-node "Divisor"))
                     (dendtn (make-text-node "Dividend"))
                     (quotch (div-id-class "quotient-key-holder" "dt-table-cell dt-invisible-text"))
                     (dsorch (div-id-class "divisor-key-holder" "dt-table-cell dt-invisible-text"))
                     (dendch (div-id-class "dividend-key-holder" "dt-table-cell dt-invisible-text"))
                     (controw2 (div-id-class "content-row2" "dt-table-row"))
                     (controw3 (div-id-class "content-row3" "dt-row-table"))
                     (msgh (div-id-class "message-holder" "dt-table-cell")))
                 (in-tutor
                  (add-event-no-capture colorisebutt "click" colorise)
                  (add-event-no-capture instrbutt "click" instruct-pupil)
                  (add-event-no-capture explbutt "click" explain-pupil)
                  (add-event-no-capture showbutt "click" show-pupil))
                 (append-child quotch qtn)
                 (append-child dsorch dsortn)
                 (append-child dendch dendtn)                 
                 (append-children ppageh pbannerh pconth)
                 (append-children pbannerh backh probdisph instrh explh showh)
                 (append-children pconth controw1 controw2 controw3)
                 (append-child controw1 problayout)
                 (append-child controw2 msgh)
                 (append-children controw3 colorisebutt quotch dsorch dendch)
                 (append-child problayout (funcall build-layout-table 10 10))
                 (append-child backh backbutt)
                 (append-child probdisph currentdisp)
                 (append-child instrh instrbutt)
                 (append-child explh explbutt)
                 (append-child showh showbutt)
                 ppageh))
             
             
             level-butt-handler
             (lambda (ev)
               (funcall make-level-example (funcall generate-operands 
                                                    (parse-int (aref (split (id-of this) "_") 1)))))                 

             make-level-div
             (lambda (level)
               (let* ((lev (div-id-class (strcat "level_" level) "division-level"))
                      (butth (div-id-class (strcat "button-level_" level) "level-left"))
                      (txth (div-id-class (strcat "text-level_" level) "level-right"))
                      (desc (ps:getprop (ps:getprop operand-generator level) "description"))
                      (rad (make-input-type (strcat "button_" level)
                                            "level-buttons"
                                            "radio")))
                 (add-event-no-capture butth "click" level-butt-handler)
                 (append-text butth level)
                 (append-text txth desc)
                 (append-child butth rad)
                 (append-children lev butth txth)
                 lev))
             
             make-choose-level-screen
             (lambda ()
               (let ((ltd (dgebid "lesson-text-display")))
                 (remove-children-from-nth ltd 0)
                 (dotimes (n 5)
                   (append-child ltd (funcall make-level-div n)))
                 ltd))
                     
                     
             
             initialize-ephemeral
             (lambda ()
              false)
                              
             );;end of setf

            ))
        ephemeral
        ))
    (init-mod2-div-int-lesson-slots)
    
    ))


(define-lesson mod2-div-int
    :step ((:p "In a "(:em "later lesson") ", we will be looking at
    the new type of numbers which can be made from dividing whole
    numbers, but before that, " (:em "this lesson") " will do two
    things:" (:ul (:li "It will introduce some mathematical names for
    some of the types of numbers, so it is easier for us to speak
    correctly about them.") (:li "It will also show a way of setting
    out your working when you calculate a division yourself."))))

    :step ((:p "As you have seen, there are different types of
    numbers, and this program has occasionally spoken
    about " (:q "whole numbers") " without saying why they
    are " (:q "whole") " or what a " (:q "broken") " number
    is.") (:p "This program has also often just spoken
    about " (:q "numbers") ". This use of language is not
    mathematically accurate, since the type of number we are dealing
    with affects what we can do with it and what it is, but sometimes
    it is easier for a learner if they can ignore some details when
    they are learning a new thing, and that is why we just spoke
    of " (:q "numbers") " previously."))

    :stepj ((:p "When mathematicians in the past have given names to
    types of numbers, they have " (:strong "not") " generally invented
    new words, but have reused existing words, which then have a
    special meaning in mathematical language.") (:p "This has one
    unfortunate result, which is that people learning mathematics
    sometimes think that in mathematics the name means what it did
    outside of mathematics.") (:p "For example, the non-negative
    numbers $0,1,2,3,...$ are called " (:q (:strong "natural
    numbers")) ", but you should " (:strong "not") " think this means
    they are more or less natural than any other type of number, even
    if some mathematicians back in history might have believed
    this.") (:p "The numbers $...,-3,-2,-1,0,1,2,3,...$ are
    called " (:q (:strong "integers")) ". These are the numbers we
    have called " (:q "whole numbers") ", and as you can see, they
    contain the natural numbers.") (:p "The type of numbers we are
    going to be learning about in the next lessons are
    called " (:q (:strong "rational numbers")) ". They are all the
    numbers which can be formed by dividing two integers (except
    dividing by zero - that has no meaning in mathematics). Rational
    numbers include the integers (and therefore, of course, also the
    natural numbers)."))

    :step ((:p "All these types of numbers can be visualized as points
    on the numberline. The rational numbers include fractions (maybe
    the name comes from " (:q "fractured") ", meaning broken) which are
    numbers whose position is determined by " (:strong "dividing") "
    or " (:q "breaking") " the space between 2 integers into a regular
    number of divisions.") (:p "Practically speaking, if we are
    measuring something in the real world, we can make more and more
    accurate measurements by making our regular divisions smaller and
    smaller.") (:p "We will come back to these rational numbers in a
    later lesson, but now a way of setting out your work when you are
    dividing 2 integers will be shown.  The result of all the examples
    in this lesson and the exercises afterwards are all themselves
    integers, so we are not producing fractions yet."))

    :step ((:p "This lesson will now show you a way of arranging your
    work when you have to do a division yourself, to calculate a
    quotient.") (:p "First the method will be explained in words, and
    then you can try it yourself.  Probably, the explanation will only
    begin to make sense when you have tried the method yourself a
    number of times.") (:p (:em "Remember that sometimes, it is
    necessary to repeat something many times before you can master it.
    You might not understand this method the first time you see it
    demonstrated, and maybe not the second or third time, either.
    This method is both more complicated and differs in several ways
    from the methods for adding, subtracting and multiplying which
    have been shown in earlier lessons, so the way it is demonstrated
    is also different.")))

    :step ((:p "In all the other methods which have been shown, for
    subtracting, adding and multiplying, we started by finding the
    units digit of the result, then moved from right to left and found
    tens, hundreds, ... and so on.") (:p "In this method for division,
    the digits of the result are written from " (:strong "left to
    right") " starting with the " (:strong "highest") " power of
    ten.") (:p "This is because when we divide, we do not usually know
    in advance if units are the smallest part of the answer (even
    though all the examples in this lesson have been chosen to give
    exact integer quotients).  Later lessons will show ways of dealing
    with remainders which are smaller than 1.") (:p "Another way in
    which this method differs, is that the digits of the result are
    written " (:strong "above") " the calculation."))

    :step ((:p "The method works by finding progressively smaller and
    smaller remainders for each digit of the
    result " (:strong "multiplied by the divisor.") (:em " It thus
    uses the mathematical definition of division given in the lesson
    titled " (:q "Division - 1"))) (:p "Remember that we use the
    decimal " (:strong "position") " system, so when you multiply by a
    digit, you must be aware of it's position in the result, as this
    determines wether it specifies the number of units, tens,
    hundreds, ...") (:p "At each step, you must choose a digit to
    write into the result which gives the smallest possible
    non-negative remainder when the product of the digit and the
    divisor is subtracted from your current
    remainder.") (:p "The " (:q "current remainder") " at each step is
    the " (:strong "remainder of the previous step") ", starting with
    the dividend in the first step. The " (:q "current
    remainders") "are calculated and written
    " (:strong "underneath") " the problem."))

    :step ((:p "In the next page, you can choose a difficulty
    level. Level zero is easiest, and just shows you how
    to " (:strong "arrange") " a division " (:strong "which you
    already know") " in the way which is used in this method.  The
    purpose is so that you see where the dividend, divisor and
    resulting quotient are written without worrying about the
    calculation.") (:p "When you have chosen a level, the program will
    give you an example page with a division calculation.  On this
    page you can practice doing a division yourself. Click
    on " (:q "Instruct") "for the program to tell you what to
    do. Click on " (:q "Explain") " for a more detailed
    explanation.") (:p "If you need the program to do a step for you,
    you can click " (:q "Show") ", and if you need help remembering
    which is the divisor, quotient and dividend, you can
    click " (:q "colorise") " to highlight these
    items.") (:p "Clicking " (:q "Back") " takes you back to the page
    where you can choose a level."))

    :display (funcall make-choose-level-screen)

    :step ((:p "All the example calculations were with positive
    integers.") (:p "If you want to calculate a division involving
    negative numbers then " (:strong "first") " use your knowledge of
    the rules for multiplying integers to decide what sign the
    quotient must have, then calculate the division as if both
    dividend and divisor were positive, and if necessary, write a
    negative sign in front of your final answer."))
    
    :stepj ((:h1 "Summary")
           (:ul (:li "The numbers $0,1,2,3,...$ are
           called " (:strong "natural numbers") ".")
                (:li "The numbers $...,-3,-2,-1,0,1,2,3,...$ are called " (:strong "integers") ". They " (:em "include") " the natural numbers.")
                (:li "All the numbers which can be made by dividing
                integers by integers (except by zero) are
                called " (:strong "rational numbers") ".  The
                rationals " (:em "include") " the integers (and
                natural numbers) since any integer divided by $1$ is
                just that integer.")
                (:li "A division problem can be calculated by using
                trial-and-error (" (:q "best guess and test") ") to
                determine the digits of the quotient which give
                products that result in smaller and smaller
                non-negative remainders when subtracted from the
                previous remainder.  One way of setting out the
                working of a division calculation has been
                demonstrated."))
           (:p "Press the right arrow to complete this lesson."))
    :complete? t)
