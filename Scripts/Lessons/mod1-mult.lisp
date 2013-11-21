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

(defun init-mod1-mult-lesson-slots ()
  (ps:ps 
    (defun init-mod1-mult-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (insert-product-builder
                     make-product-builder-layout create-prodb-cell
                     prodb-loop dis-styles  product-cell 
                     distab get-unums set-axis-terms
                     sides-ready redraw-product local-svg-grid-skip
                     compute-term-coords insert-practice
                     make-practice-layout ;;generate-practice
                     practice-problem
                     
                     ) ephemeral
          (with-slots (remove-default-rl-handlers 
                       restore-default-rl-handlers
                       garbage record-garbage remove-garbage
                       current-lesson-variables label-mark 
                       next-lesson-text previous-lesson-text 
                       reset-var use-var init-var ajax-reset-var
                       teach add-or-remove-wrong-click-nl-handlers
                       numberline-id wrong-click-nl random-int
                       from-direction current-module-name) non-ephemeral 
            (setf

             svg-grid-skip 0.8
             
             dis-styles (ps:create
                         colors 7
                         0 "stroke:Red;
                            fill:Red;
                            fill-opacity: 0.3;
                            stroke-width:0.125%;"
                         1 "stroke:Green;
                            fill:Green;
                            fill-opacity: 0.3;
                            stroke-width:0.125%;"
                         2 "stroke:Blue;
                            fill:Blue;
                            fill-opacity: 0.3;
                            stroke-width:0.125%;"
                         3 "stroke:Fuschia;
                            fill:Fuschia;
                            fill-opacity: 0.3;
                            stroke-width:0.125%"
                         4 "stroke:Olive;
                            fill:Olive;
                            fill-opacity: 0.3;
                            stroke-width:0.125%"
                         5 "stroke:Aqua;
                            fill:Aqua;
                            fill-opacity: 0.3;
                            stroke-width:0.125%"
                         6 "stroke:Maroon;
                            fill:Maroon;
                            fill-opacity: 0.3;
                            stroke-width:0.125%")
             
             product-cell
             (lambda (tspn)
               (parent-node (parent-node tspn)))
             
             distab
             (ps:create

              rectangle false
                           
              initialize
              (lambda ()
                (with-distab
                    (setf rectangle (ps:create
                                     labels (ps:array)
                                     x false
                                     axis1-sum 0
                                     axis1-terms (ps:array)
                                     axis2-sum 0
                                     axis2-terms (ps:array)
                                     y false
                                     r (ps:array)
                                     style 0
                                     )
                          released true)))
             
              enter-factors
              (lambda (ev)
                (when (not-defined (with-distab (aref (rect-value r) 0)))
                  (let* ((fac1-holder (dgebid "infac1-input"))
                         (fac2-holder (dgebid "infac2-input")))
                    (dispatch-event fac1-holder 
                                    (standard-event "auto-event"
                                                    (ps:create
                                                     evfun get-unums
                                                     element fac1-holder)))
                    (dispatch-event fac2-holder 
                                    (standard-event "auto-event"
                                                    (ps:create
                                                     evfun get-unums
                                                     element fac2-holder))))))
              
              auto
              (lambda (ev)
                (with-distab (funcall free-rectangles))
                (let* ((fac1 (funcall random-int 1 10))
                       (fac1-holder (dgebid "infac1-input"))
                       (t1 (funcall random-int 1 10))
                       (t2 (funcall random-int 1 10))
                       (fac2-holder (dgebid "infac2-input")))
                  (setf (value fac1-holder) "")
                  (dispatch-event fac1-holder 
                                  (standard-event "auto-event"
                                                  (ps:create
                                                   evfun get-unums
                                                   element fac1-holder)))
                  (setf (value fac2-holder) "")
                  (dispatch-event fac2-holder 
                                  (standard-event "auto-event"
                                                  (ps:create
                                                   evfun get-unums
                                                   element fac2-holder)))
                  (setf (value fac1-holder) (strcat "" fac1))
                  (dispatch-event fac1-holder 
                                  (standard-event "auto-event"
                                                  (ps:create
                                                   evfun get-unums
                                                   element fac1-holder)))
                  (setf (value fac2-holder) (strcat t1 "+" t2))
                  (dispatch-event fac2-holder 
                                  (standard-event "auto-event"
                                                  (ps:create
                                                   evfun get-unums
                                                   element fac2-holder)))))

              write-minor-label
              (lambda (xstart xstop ystart ystop on-x-axis)
                (let* ((cstop (if on-x-axis ystop xstop))
                       (interval (if on-x-axis 
                                     (- xstop xstart)
                                     (- ystop ystart)))
                       (mid (floor (/ interval 2)))
                       (rem (mod interval 2))
                       (cother (+ (if on-x-axis
                                      xstart ystart)
                                  (if (zerop rem) (1+ mid) mid)))
                       (cnt (strcat "" interval))
                       (len (length cnt))
                       (label (make-text-node cnt))
                       (cid (cell-id (if on-x-axis cother cstop)
                                     (if on-x-axis cstop cother)))
                       (cell (dgebid cid))
                       (rect (aref (gebtag cell "rect") 0))
                       (xem (get-attribute rect "x"))
                       (yem (get-attribute rect "y"))
                       (svgtxt (svg-create-element "text"))
                       (txtspn (svg-create-element "tspan"))
                       (canvas (dgebid "pb-canvas")))
                  (append-child txtspn label)
                  (append-child svgtxt txtspn)
                  (set-attributes svgtxt "x" xem "y" yem)
                  (cond ((= len 1)
                         (set-attributes txtspn 
                                         "dx" (if on-x-axis (if (zerop rem) "-1.5em" "0.25em") "0.25em")
                                         "dy"  (if (zerop rem) (if on-x-axis "-0.15em" "1.5em") (if (= interval 1) "-0.2em" "-0.15em"))
                                         "style" "font-size:70%;" ))
                        (t 
                         (set-attributes txtspn 
                                         "dx" (if on-x-axis (if (zerop rem) "-1.5em" "0.25em") "0.25em")
                                         "dy"  (if (zerop rem) (if on-x-axis "-0.15em" "1.5em") "-0.15em")
                                         "style" "font-size:65%;" )))
                  (append-child canvas svgtxt)
                  (push svgtxt (with-distab (rect-value labels)))))
  
              write-label
              (lambda (axis)
                (with-distab 
                    (let ((coords) (label) (id))
                      (cond ((= axis 1)
                             (let* ((side (rect-value axis1-sum))
                                    (mid (floor (/ side 2)))
                                    (rm (mod side 2)))
                               (setf coords (ps:array (if (and (zerop rm)
                                                               (not (= side 2))) mid (1+ mid)) 0)
                                     label (strcat "" (rect-value axis1-sum))
                                     id "x-label")))                            
                            (t 
                             (let* ((side (rect-value axis2-sum))
                                    (mid (floor (/ side 2)))
                                    (rm (mod side 2)))
                             (setf coords (ps:array 0 (if (and (zerop rm)
                                                               (not (= side 2))) mid (1+ mid)))
                                   label (strcat "" (rect-value axis2-sum))
                                   id "y-label"))))
                      (ps:destructuring-bind (x y)
                          coords
                        (let* ((cell (dgebid (cell-id x y)))
                               (rect (aref (gebtag cell "rect") 0))
                               (xem (get-attribute rect "x"))
                               (yem (get-attribute rect "y"))
                               (len (length label))
                               (cell-label (make-text-node label))
                               (svgtxt (svg-create-element "text"))
                               (txtspn (svg-create-element "tspan")))
                          (append-child txtspn cell-label)
                          (append-child svgtxt txtspn)
                          (set-attributes svgtxt "x" xem "y" yem "id" id)
                          (cond ((= len 1)
                                 (set-attributes txtspn 
                                                 "dx" "0em"
                                                 "dy"  "1em" ;;(if (= axis 2) "1em" "0.25em")
                                                 "style" "font-size:70%;" ))
                                (t 
                                 (set-attributes txtspn 
                                                 "dx" "-0.1em" 
                                                 "dy"  "1em" ;;(if (= axis 2) "1em" "1em")
                                                 "style" "font-size:65%;" )))
                          (push svgtxt (rect-value labels))
                          (append-child (dgebid "pb-canvas")
                                        svgtxt))))))

              write-area-label
              (lambda (xstart xstop ystart ystop)
                (let* ((xint (- xstop xstart))
                       (yint (- ystop ystart))
                       (cx (+ xstart (floor (/ xint 2))))
                       (cy (+ ystart (floor (/ yint 2))))
                       (cid (cell-id cx cy))
                       (area (* xint yint))
                       (ltxt (strcat "" area))
                       (len (length ltxt))
                       (label (make-text-node ltxt))
                       (cell (dgebid cid))
                       (rect (aref (gebtag cell "rect") 0))
                       (xem (get-attribute rect "x"))
                       (yem (get-attribute rect "y"))
                       (svgtxt (svg-create-element "text"))
                       (txtspn (svg-create-element "tspan"))
                       (canvas (dgebid "pb-canvas")))
                  (append-child txtspn label)
                  (append-child svgtxt txtspn)
                  (set-attributes svgtxt "x" xem "y" yem)
                  (cond ((= len 1)
                         (set-attributes txtspn 
                                         "dx" "0.2em"
                                         "dy"  "-0.2em"
                                         "style" "font-size:70%;" ))
                        (t 
                         (set-attributes txtspn 
                                         "dx" "0em"
                                         "dy"  "-0.2em"
                                         "style" "font-size:65%;" )))
                  (append-child canvas svgtxt)
                  (push svgtxt (with-distab (rect-value labels)))))
                       
              draw-rectangle
              (lambda (yc xc)
                (destructuring-bind (ystart ystop)
                    yc
                  (destructuring-bind (xstart xstop)
                      xc
                    (with-distab
                        (when (= xstop (1+ (rect-value axis1-sum)))
                          (funcall write-minor-label xstart xstop ystart ystop false))
                      (when (= ystop (rect-value axis2-sum))
                        (funcall write-minor-label xstart xstop ystart ystop true))
                      (funcall write-area-label xstart xstop ystart ystop)
                        (let* ((r (svg-create-element "rect"))
                               (topl-name (cell-id xstart ystop))
                               (topl (dgebid topl-name))
                               (cellr (aref (gebtag topl "rect") 0))
                               (toplx (get-attribute cellr "x"))
                               (toply (get-attribute cellr "y"))
                               (toplw (parse-float (get-attribute cellr "width")))
                               (toplh (parse-float (get-attribute cellr "height")))
                               (rw (strcat (* toplw (- xstop xstart)) "em"))
                               (rh (strcat (* toplh (- ystop ystart)) "em"))
                               (stylen (rect-value style))
                               (style (ps:getprop dis-styles stylen))
                               (canvas (dgebid "pb-canvas")))                    
                          (push r (rect-value r))
                          (setf (rect-value style) (mod (1+ stylen)
                                                        (ps:getprop dis-styles 
                                                                    "colors")))
                          (set-attributes r
                                          "pointer-events" "none"
                                          "x" toplx
                                          "y" toply
                                          "width" rw
                                          "height" rh
                                          "style" style)
                          (append-child canvas r))))))

              free-rectangles
              (lambda ()
                (with-distab
                    (let ((canvas (dgebid "pb-canvas")))
                      (labels ((rem (arr)
                                 (let ((obj (aref arr 0)))
                                   (if (not-defined obj)
                                       true
                                       (progn (remove-child canvas obj)
                                              (funcall rem (subseq arr 1)))))))
                        (funcall rem (rect-value r))
                        (funcall rem (rect-value labels))
                        (setf (rect-value r) (ps:array))
                        (setf (rect-value labels) (ps:array))))))
          
              clear-all
              (lambda ()
                (with-distab
                    (funcall free-rectangles)
                  (funcall initialize)
                  (setf (value (dgebid "infac1-input")) ""
                        (value (dgebid "infac2-input")) "")
                  (let ((disp (dgebid "l-product-display")))
                    (remove-children-from-nth disp 0)
                    (append-para-text disp "$x\\cdot y=xy$" true))))
                        
              draw-all-rectangles
              (lambda ()
                (with-distab
                    (let ((ycs (funcall compute-term-coords 
                                        (rect-value axis2-terms)))
                          (xcs (funcall compute-term-coords 
                                        (rect-value axis1-terms) true)))
                      (setf released false)
                      (loop for yc in ycs
                         do (loop for xc in xcs
                               do (funcall draw-rectangle yc xc))))))
              released true
                                         
              );;end distab

             practice-problem
             (ps:create digit-factor false
                        sum-factor false
                        user-term1 false
                        user-term2 false
                        user-partial-product1 false
                        user-partial-product2 false
                        user-total-sum false
                        initialize
                        (lambda ()
                          (in-practice
                           (let ((buth (dgebid "emptyr"))
                                 (msg (dgebid "pmessage"))
                                 (pdisp (dgebid "pdisplay"))
                                 (ent1 (dgebid "entry1"))
                                 (resp1 (dgebid "response1"))
                                 (ent2 (dgebid "entry2"))
                                 (resp2 (dgebid "response2"))
                                 (ent3 (dgebid "entry3"))
                                 (resp3 (dgebid "response3")))
                             (dolist (el (ps:array buth msg pdisp ent1 resp1 
                                                   ent2 resp2 ent3 resp3))
                               (remove-children-from-nth el 0))
                             (setf digit-factor false
                                   sum-factor false
                                   user-term1 false
                                   user-term2 false
                                   user-partial-product1 false
                                   user-partial-product2 false
                                   user-total-sum false))))

                        clear-message
                        (lambda ()
                          (let ((md (dgebid "pmessage")))
                            (remove-children-from-nth md 0)))
                        
                        error-response2
                        (lambda (el1 el2)
                          (in-practice
                           (funcall clear-message)
                             (let ((txt (strcat "No. Each entry should equal the digit multiplied by a term of the sum.")))
                               (append-para-text (dgebid "pmessage") txt)
                               (setf (value el1) ""
                                     (value el2) ""))))
                        
                        error-response1
                        (lambda (el1 e1 el2 e2 should-be)
                          (in-practice
                           (funcall clear-message)
                             (let ((txt (strcat "No. $" e1 "+" e2 "\\neq " should-be "$. Try again.")))
                               (append-para-text (dgebid "pmessage") txt true)
                               (setf (value el1) ""
                                     (value el2) ""))))

                        error-response3
                        (lambda (el1 en1)
                          (in-practice
                           (funcall clear-message)
                             (let ((txt (strcat "No. $" en1 "\\neq " user-partial-product1  "+" user-partial-product2 "$. Try again.")))
                               (append-para-text (dgebid "pmessage") txt true)
                               (setf (value el1) ""))))

                        continue-message
                        (lambda ()
                          (in-practice
                           (funcall clear-message)
                             (let ((oldd (dgebid "entry3"))
                                   (txt (strcat "Well done."))
                                   (newbut (dgebid "new-practice")))
                               (remove-children-from-nth oldd 0)
                               (append-child oldd (make-text-node (strcat "$=" user-total-sum "$")))
                               (mj-hub-queue (ps:array "Typeset" mj-hub oldd))
                               (add-event-no-capture newbut "click" generate-practice)
                               (append-para-text (dgebid "pmessage") txt false))))

                        handle-response3
                        (lambda (ev)
                          (in-practice
                           (let* ((el1 (dgebid "pe5"))
                                  (e1 (value el1)))

                             (if (string= e1 "")
                                 (setf  (value e1) ""
                                        (value e2) "")
                                 (let* ((en1 (parse-int e1)))
                                        
                                   (if (not (is-na-n en1))
                                       (if (not (= en1 (* digit-factor sum-factor)))
                                           (funcall error-response3 el1 en1)
                                           (progn (setf user-total-sum en1)
                                                  (funcall continue-message)))
                                       (funcall error-response3 el1 en1)))))))                    

                        generate-practice-stage3
                        (lambda (rb)
                          (in-practice
                           (remove-event-no-capture rb "click" handle-response1)
                           (funcall clear-message)
                           (let ((pr (dgebid "response3"))
                                 (oldd (dgebid "entry2"))
                                 (ud (dgebid "entry3"))
                                 (ue5 (make-text-input-element 4 4))
                                 (ptxt (make-text-node "$=\\quad$"))
                                 (cb (make-button "pcheck3" "check")))
                             (remove-children-from-nth oldd 0)
                             (append-child oldd (make-text-node 
                                                 (strcat "$=" user-partial-product1 "+" user-partial-product2 "$")))
                             (mj-hub-queue (ps:array "Typeset" mj-hub oldd))
                             (add-event-no-capture ue5 "click" (lambda (ev)
                                                                 (focus this)))
                             (add-event-no-capture cb "click" handle-response3)
                             (set-attribute ue5 "id" "pe5")
                             (append-children ud ptxt ue5)
                             (append-child pr cb)
                             (focus ue5)
                             (mj-hub-queue (ps:array "Typeset" mj-hub ud)))))
                        
                        handle-response2
                        (lambda (ev)
                          (in-practice
                           (let* ((el1 (dgebid "pe3"))
                                  (e1 (value el1))
                                  (el2 (dgebid "pe4"))
                                  (e2 (value el2)))
                             (if (or (string= e1 "")
                                     (string= e2 ""))
                                 (setf  (value e1) ""
                                        (value e2) "")
                                 (let* ((en1 (parse-int e1))
                                        (en2 (parse-int e2)))
                                   (if (and (not (is-na-n en1))
                                            (not (is-na-n en2)))
                                       (let ((p1 (* digit-factor user-term1))
                                             (p2 (* digit-factor user-term2)))
                                         (cond  ((and (= p1 en1)
                                                      (= p2 en2))
                                                 (setf user-partial-product1 p1
                                                       user-partial-product2 p2)
                                                 (funcall generate-practice-stage3 this))
                                                ((and (= p1 en2)
                                                      (= p2 en1))
                                                 (setf user-partial-product1 p2
                                                       user-partial-product2 p1)
                                                 (funcall generate-practice-stage3 this))
                                                (t (funcall error-response2 el1 el2))))
                                       (funcall error-response2 el1 el2)))))))
                        
                        generate-practice-stage2
                        (lambda (rb)
                          (in-practice
                           (remove-event-no-capture rb "click" handle-response1)
                           (funcall clear-message)
                           (let ((oldd (dgebid "entry1"))
                                 (pr (dgebid "response2"))
                                 (ud (dgebid "entry2"))
                                 (ue3 (make-text-input-element 3 3))
                                 (ue4 (make-text-input-element 3 3))
                                 (ptxt (make-text-node "$=\\quad$"))
                                 (uto (make-text-node "$+$"))
                                 (cb (make-button "pcheck2" "check")))
                             (remove-children-from-nth oldd 0)
                             (append-child oldd (make-text-node (strcat "$=" digit-factor 
                                                                        "\\cdot (" user-term1 
                                                                        "+" user-term2 ")$")))
                             (mj-hub-queue (ps:array "Typeset" mj-hub oldd))
                             (add-event-no-capture ue3 "click" (lambda (ev)
                                                                 (focus this)))
                             (add-event-no-capture ue4 "click" (lambda (ev)
                                                                 (focus this)))
                             (add-event-no-capture cb "click" handle-response2)
                             (set-attribute ue3 "id" "pe3")
                             (set-attribute ue4 "id" "pe4")
                             (append-children ud ptxt ue3 uto ue4)
                             (append-child pr cb)
                             (focus ue3)
                             (mj-hub-queue (ps:array "Typeset" mj-hub ud)))))
                        
;; generate-practice-stage2
;;                         (lambda (rb)
;;                           (in-practice
;;                            (remove-event-no-capture rb "click" handle-response1)
;;                            (funcall clear-message)
;;                            (let (
;;                                  (pr (dgebid "response2"))
;;                                  (ud (dgebid "entry2"))
;;                                  (ue3 (make-text-input-element 3 3))
;;                                  (ue4 (make-text-input-element 3 3))
;;                                  (ptxt (make-text-node "$=\\quad$"))
;;                                  (uto (make-text-node "$+$"))
;;                                  (cb (make-button "pcheck2" "check")))
;;                              (add-event-no-capture ue3 "click" (lambda (ev)
;;                                                                  (focus this)))
;;                              (add-event-no-capture ue4 "click" (lambda (ev)
;;                                                                  (focus this)))
;;                              (add-event-no-capture cb "click" handle-response2)
;;                              (set-attribute ue3 "id" "pe3")
;;                              (set-attribute ue4 "id" "pe4")
;;                              (append-children ud ptxt ue3 uto ue4)
;;                              (append-child pr cb)
;;                              (focus ue3)
;;                              (mj-hub-queue (ps:array "Typeset" mj-hub ud)))))
                           
                        
                        handle-response1
                        (lambda (ev)
                          (in-practice
                           (let* ((el1 (dgebid "pe1"))
                                  (e1 (value el1))
                                  (el2 (dgebid "pe2"))
                                  (e2 (value el2)))
                             (if (or (string= e1 "")
                                     (string= e2 ""))
                                 (setf  (value e1) ""
                                        (value e2) "")
                                 (let* ((en1 (parse-int e1))
                                        (en2 (parse-int e2)))
                                   (if (and (not (is-na-n en1))
                                            (not (is-na-n en2)))
                                       (if (not (= (+ en1 en2) sum-factor))
                                           (funcall error-response1 el1 e1 el2 e2 sum-factor)
                                           (progn (setf user-term1 en1
                                                        user-term2 en2)
                                           (funcall generate-practice-stage2 this)))
                                       (funcall error-response1 el1 e1 el2 e2 sum-factor)))))))

                        ;; (lambda (el1 e1 el2 e2 should-be1 should-be2)
                          
                        
                        ;; make-response-handler
                        ;; (lambda (id1 id2 should-be1 should-be2)
                        ;;   (lambda (ev)
                        ;;     (in-practice
                        ;;      (let* ((el1 (dgebid id1))
                        ;;             (e1 (value el1))
                        ;;             (el2 (dgebid id2))
                        ;;             (e2 (value el2)))
                        ;;        (if (or (string= e1 "")
                        ;;                (string= e2 ""))
                        ;;            (setf  (value e1) ""
                        ;;                   (value e2) "")
                        ;;            (let* ((en1 (parse-int e1))
                        ;;                   (en2 (parse-int e2)))
                        ;;              (if (and (not (is-na-n en1))
                        ;;                       (not (is-na-n en2)))
                        ;;                  (funcall inputs-are-numbers el1 e1 el2 e2 should-be
                            
                        
                        generate-practice
                        (lambda ()
                          (in-practice
                           (funcall initialize)
                           (setf digit-factor (funcall random-int 2 9)
                                 sum-factor (funcall random-int 10 99))
                           (funcall display-practice-start)))

                        display-practice-start
                        (lambda ()
                          (in-practice
                           (let ((pd (dgebid "pdisplay"))
                                 (pr (dgebid "response1"))
                                 (buth (dgebid "emptyr"))
                                 (newb (make-button "new-practice" "new"))
                                 (ud (dgebid "entry1"))
                                 (ptxt (strcat "$" digit-factor 
                                              "\\cdot" sum-factor "$"))
                                 (ue1 (make-text-input-element 2 2))
                                 (ue2 (make-text-input-element 2 2))
                                 (ut1 (make-text-node (strcat "$=" digit-factor
                                                              "\\cdot ($")))
                                 (uto (make-text-node "$+$"))
                                 (ut2 (make-text-node "$)$"))
                                 (cb (make-button "pcheck1" "check")))
                             (add-event-no-capture ue1 "click" (lambda (ev)
                                                                 (focus this)))
                             (add-event-no-capture ue2 "click" (lambda (ev)
                                                                 (focus this)))
                             (add-event-no-capture cb "click" handle-response1)
                             (set-attribute ue1 "id" "pe1")
                             (set-attribute ue2 "id" "pe2")
                             (append-child buth newb)
                             (append-para-text pd ptxt true)
                             (append-children ud ut1 ue1 uto ue2 ut2)
                             (append-child pr cb)
                             (focus ue1)
                             (mj-hub-queue (ps:array "Typeset" mj-hub ud)))))
                        
                        );;end practice-problem
                          
             create-prodb-cell
             (define-anonymous-with-cell-variables 
                 (set-attributes g "id" (cell-id xv yv nil)
                                 "class" "scratch-pad-cell"
                                 "partial-answer" "false")
               (set-attributes rect "x" xem "y" yem "width" skipem
                               "height" skipem "fill" "none"
                               "pointer-events" "all"
                               "stroke" (if (or (zerop xv)
                                                (zerop yv)) 
                                            "none"
                                            "#526565") "stroke-opacity" "0.3")
               (append-child g rect)
               (append-child svg g))
             
             prodb-loop
             (lambda (startx starty startyv)
               (lambda (height width columns skip rows skipem svg content-fn id)
                 (loop for y from starty below height by skip
                    for yv from startyv downto 0 by 1
                    do (loop for x from startx to width by skip
                          for xv from startx to columns by 1
                          do (funcall content-fn x y xv yv skipem svg id)))))

             insert-product-builder 
             (lambda ()
               (let ((ltd (dgebid "lesson-text-display"))
                     (pb (funcall make-product-builder-layout))
                     (lfn (funcall prodb-loop 0 1 20)))
                 (with-distab (funcall initialize))
                 (remove-children-from-nth ltd 0)
                 (append-child ltd pb)
                 (let* ((pgrid (custom-svg-grid 
                                "pb-canvas-holder" 
                                "pb-canvas" 
                                23 22
                                create-prodb-cell lfn)))
                   (funcall redraw-product)
                   pgrid)))

             compute-term-coords
             (lambda (arr &optional (rshift false))
               (labels ((rec (a &optional (ret (ps:array)) (curr 0))
                          (let ((trm (aref a 0)))
                            (if (not-defined trm)
                                ret
                                (let ((nxt (+ trm curr)))
                                  (funcall rec 
                                           (subseq a 1)
                                           (append ret (list (ps:array curr nxt)))
                                           nxt))))))
                 (if rshift 
                     (funcall rec arr (ps:array) 1)
                     (funcall rec arr))))
           
             redraw-product
             (lambda ()
               (with-distab
                   (let* ((holder (dgebid "l-product-display"))
                          (x (rect-value x))
                          (f1 (if x (rect-value axis1-sum) "x"))
                          (y (rect-value y))
                          (f2 (if y (rect-value axis2-sum) "y"))
                          (p (if (and x y)
                                 (* f1 f2)
                                 (cond ((and (not x)
                                             (not y)) "xy")
                                       (x (strcat f1 "y"))
                                       (t (strcat f2 "x"))))))                       
                     (remove-children-from-nth holder 0)
                     (append-para-text holder 
                                       (strcat "$" f1 "\\cdot " f2 "=" p "$") 
                                       true)
                     (when (and x y)
                       (when (or (> f1 20) (> f2 20) (> p 400))
                         (let* ((lth (dgebid "lesson-text-holder"))
                               (zin (style-z-index lth)))
                           (setf (style-z-index lth) (1- grey-out-z-index))
                           (my-alert "The numbers entered can not fit in the grid!  Choose smaller numbers."
                                     (lambda () (funcall clear-all)
                                             (setf (style-z-index lth) zin)
                                             ))))))))
                                                  
             sides-ready
             (lambda ()
               (with-distab
                   (and (rect-value x)
                        (rect-value y))))

             set-axis-terms
             (lambda (axis terms)
               (with-distab
                   (if (= axis 1)
                         (progn 
                           (if terms
                               (setf (rect-value axis1-sum)
                                     (array-reduce terms (lambda (prev curr idx arr)
                                                           (+ prev curr)) 0)
                                     (rect-value axis1-terms) terms
                                     (rect-value x) true)
                               (setf (rect-value axis1-sum) 0
                                     (rect-value axis1-terms) (ps:array)
                                     (rect-value x) false)))
                         
                         (progn 
                           (if terms
                               (setf (rect-value axis2-sum)
                                     (array-reduce terms 
                                                   (lambda (prev curr idx arr)
                                                     (+ prev curr)) 0)
                                     (rect-value axis2-terms) terms
                                     (rect-value y) true)
                               (setf (rect-value axis2-sum) 0
                                     (rect-value axis2-terms) (ps:array)
                                     (rect-value y) false))))
                 (funcall redraw-product)
                 (if (funcall sides-ready) ;;FIXME 'if
                     (progn
                       (funcall draw-all-rectangles)
                       (funcall write-label 1)
                       (funcall write-label 2))
                     (funcall free-rectangles))))

             ;; (when 
             ;;     (str-contains foo "googlebug")
             ;;   (setf ok false))

             get-unums
             (lambda (ev el)
               (with-distab 
                   ;;(unless (not released)
                     (when  (or (= (key-code ev) 13)
                                (and (not ev) el))
                       (let ((ok true) 
                             (axis (if (string= (id-of (if ev this el))
                                                "infac1-input") 1 2)))
                         (labels ((get-number (str)
                                    (let ((res (parse-int str)))
                                      (if (and (not (is-na-n res))
                                               (>= res 0))
                                          res
                                          (progn (setf ok false)
                                                 false)))))
                           (let* ((ustring (value (if ev this el)))
                                  (len (length ustring))
                                  (lsti (1- len)))
                             (when (str-contains ustring "-")
                               (setf ok false))
                             (unless (< len 0)
                               (when (string= (char-at ustring lsti) ")")
                                 (setf ustring (subseq ustring 0 lsti )))
                               (when (string= (char-at ustring 0) "(")
                                 (setf ustring (subseq ustring 1)))
                               (let* ((ua (split ustring "+"))
                                      (len (length ua)))
                                 (if (string= (aref ua 0) "")
                                     (progn 
                                       (setf (value (if ev this el)) "")
                                       (funcall set-axis-terms axis false))
                                     (let ((nums (ps:array)))
                                       (dolist (item ua)
                                         (let ((n (funcall get-number item)))
                                           (setf nums (append nums (list n)))))
                                       (if ok (funcall set-axis-terms axis nums)
                                           (progn (funcall set-axis-terms
                                                           axis false)
                                                  (setf (value (if ev this el)) "")))))))))))))
                 
             make-product-builder-layout
             (lambda ()
               (let ((pbuilder-holder (div-id "pbuilder-holder"))
                     (top (div-id "pb-top"))
                     (bottom (div-id "pb-bottom"))
                     (canvas-holder (div-id "pb-canvas-holder"))
                     (info-holder (div-id "pb-info-holder"))
                     (left-info (div-id-class "pb-left-info" "tutor-left"))
                     (right-info (div-id-class  "pb-right-info" 
                                                "tutor-right long-right"))
                     (prod-disp-holder (div-id-class "l-prod-disp-holder" 
                                                     "padded-items"))
                     (product-display (div-id-class "l-product-display" "pb-info"))
                     (but-holder (div-id-class "l-but-holder" "padded-items"))
                     (fac1-holder (div-id-class "r-fac1-holder" "padded-items"))
                     (qh (make-header "Factors"))
                     (ph (make-header "Product"))
                     (cdot (make-text-node "$ \\cdot $"))
                     (fac2-holder (div-id-class "r-fac2-holder" "padded-items"))
                     (infac1-input (make-input-type "infac1-input" 
                                                      "infac1" "text"))
                     (infac2-input (make-input-type "infac2-input" 
                                                      "infac2" "text"))
                     (clbut (make-button "clear-all" "clear" 
                                         ("click" (with-distab clear-all))))
                     (ebut (make-button "enter-factors" "enter"
                                        ("click" (with-distab enter-factors))))
                     (but (make-button "auto-factors" "auto")))
                 (add-event-no-capture but "click" (with-distab auto))
                 (add-event-no-capture infac1-input "auto-event"
                                       (lambda (ev)
                                         (with-auto-ev-slots 
                                             (funcall evfun false element))))
                 (add-event-no-capture infac2-input "auto-event"
                                       (lambda (ev)
                                         (with-auto-ev-slots 
                                             (funcall evfun false element))))
                 (add-event-no-capture infac1-input "click"
                                       (lambda (ev) (focus this)))
                 ;;(add-event-no-capture infac1-input "keydown" get-unums)
                 (add-event-no-capture infac2-input "click"
                                       (lambda (ev) (focus this)))
                 ;;(add-event-no-capture infac2-input "keydown" get-unums)
                 (append-children pbuilder-holder top bottom)
                 (append-child top canvas-holder)
                 (append-children bottom left-info right-info)
                 (append-child right-info qh)
                 (append-child left-info ph)
                 (append-children right-info fac1-holder fac2-holder)
                 (append-child fac1-holder infac1-input)
                 (append-child fac2-holder infac2-input)
                 (append-children left-info prod-disp-holder but-holder)
                 (append-child prod-disp-holder product-display)
                 (append-children but-holder but ebut clbut)
                 pbuilder-holder))

             make-practice-layout
             (lambda ()
               (let ((pholder (div-id "practice-holder"))
                     (prow0 (div-id-class "prow1" "practice-row"))
                     (prow1 (div-id-class "prow2" "practice-row"))
                     (prow2 (div-id-class "prow3" "practice-row"))
                     (prow3 (div-id-class "prow4" "practice-row"))
                     (pmes (div-id "pmessage"))
                     
                     (buth (div-id-class "pbut-holder" "practice-left practice-item"))
                     (pdisp (div-id-class "pdisplay" "practice-middle practice-item"))
                     (emptyr (div-id-class "emptyr" "practice-right practice-item"))
                     
                     (empty1 (div-id-class "empty1" "practice-left practice-item"))
                     (entry1 (div-id-class "entry1" "practice-middle practice-item"))
                     (resp1 (div-id-class "response1" "practice-right practice-item"))

                     (empty2 (div-id-class "empty2" "practice-left practice-item"))
                     (entry2 (div-id-class "entry2" "practice-middle practice-item"))
                     (resp2 (div-id-class "response2" "practice-right practice-item"))

                     (empty3 (div-id-class "empty3" "practice-left practice-item"))
                     (entry3 (div-id-class "entry3" "practice-middle practice-item"))
                     (resp3 (div-id-class "response3" "practice-right practice-item"))

                     ;; (empty1 (div-id-class "empty1" "practice-left practice-item"))
                     ;; (entry1 (div-id-class "entry1" "practice-middle practice-item"))
                     ;; (resp1 (div-id-class "response1" "practice-right practice-item")))
                     )
                 (append-children pholder prow0 prow1 prow2 prow3 pmes)
                 (append-children prow0 buth pdisp emptyr)
                 (append-children prow1 empty1 entry1 resp1)
                 (append-children prow2 empty2 entry2 resp2)
                 (append-children prow3 empty3 entry3 resp3)
                 pholder))
             
             insert-practice
             (lambda ()
               (in-practice
                (let* ((ltd (dgebid "lesson-text-display"))
                       (pholder (funcall make-practice-layout)))
                  (remove-children-from-nth ltd 0)
                  (append-child ltd pholder)
                  (funcall generate-practice))))
                            
             );;end of setf

            ))
        ephemeral
        ))
    (init-mod1-mult-lesson-slots)
    
    ))


(define-lesson mod1-mult
    :step ((:p "You know how to multiply digits, and by 10.  The next
    step is to learn the rule which allows you to multiply numbers of
    more than 1 digit.") (:p "This is also the rule which states how
     " (:strong "multiplication" ) " and " (:strong "addition" ) " can
    be " (:strong "combined" ) ".  These are the 2 fundamental
    operations of basic mathematics.") (:p "Maybe you are
    thinking, " (:q "But what about subtraction and
    division?")) (:p "In fact, those operations can be defined in
    terms of addition and multiplication. This is what was meant by
    saying, "(:q "they are a kind of reverse") " of addition and
    multiplication.") (:p "This lesson explains the rule, and lets you
    practice using it by multiplying a single digit by a two-digit
    number."))

    :step ((:p "The next two pages will explain the rule in words, and
    after that the following pages will offer some ways of visualising
    the rule.") (:p "The visualisations do " (:strong "not"
    ) " " (:q "prove") " the rules.  Rules are " (:strong "chosen" ) " by
    mathematicians, and choices have consequences, but they cannot
    be proven.") (:p "Visualisations are offered in an attempt to help
    you to see what the rule means.  In the end, the best way to
    understand a rule is probably just to practice it."))

    :stepj ((:p "In words the rule can be stated like
    this:") (:p "Multiplication of a factor by a sum is the same
    as " (:strong "multiplying the factor by each") " of the numbers
    which add up to the sum and
    " (:strong "adding the products") ".") (:p "For example: doing the
    calculation, $9\\cdot 13$ is the same as doing $9\\cdot
    (10+3)$.") (:p "We can use the rule to say that this product is
    the same as $9\\cdot 10 + 9\\cdot 3=90+27$ which is equal to
    $117$."  ) (:p "The brackets in " (:q "$(10+3)$") " show that we
    mean that we are treating it as " (:q "one whole
    thing") ".") (:p "Multiplication is always done before addition if
    there are no brackets, so if we wrote $9\\cdot 10+3$ then it would
    mean $90+3$ which is $93$ and is " (:strong "not") " the result of
    $9\\cdot 13$."))

    :stepj ((:p "Maybe you think there's a problem with this
    rule:") (:p "Since $6+7=13$, then according to the rule, we could
    also write " (:q "$9\\cdot 13$")" as " (:q "$9\\cdot (6+7)$") ".
    Yes, the rule does allow this, but there is no problem since by
    the rule $9\\cdot (6+7)= 9\\cdot 6+ 9\\cdot 7 =54+63=117$, which
    agrees with the other calculation we did. You can try it out by
    multiplying 9 by other numbers that add to 13, if you
    like.") (:p "The reason we usually break the sum which we are
    multiplying into numbers of powers of ten (like tens, hundreds,
    thousands, etc) is because it is so easy to multiply by powers of
    ten.")  (:p "As a consequence of the decimal position system,
    multiplying by a power of ten is accomplished by simply writing an
    appropriate number of zeros on the end of the number.  This has
    been discussed in the earlier lesson titled " (:q "Basic
    Addition") "."))

    :stepj ((:p "Here is one way of visualising the rule.") (:p "If
    one of the factors is a positive, whole number, then we can say we
    have " (:strong "that" ) " many of the " (:q "thing in
    brackets") ". So, of course, we will also have " (:strong "that"
    ) " many of " (:strong "each" ) " of the things inside the
    brackets.") (:p "As an example: $3\\cdot 37=3\\cdot (30+7)$. This
    can be taken to mean we have $(30+7)$ three times: So write it
    down three times and see how many thirties and sevens you
    have.") (:p "The next page shows the result of doing this."))

    :stepj ((:table :border "1"
                    (:tr (:td "1 time") (:td "$30+7$"))
                    (:tr (:td "2 times") (:td "$30+7$"))
                    (:tr (:td "3 times") (:td "$30+7$"))
                    (:tr (:td "Total") (:td "3 thirties and 3 sevens")))
            (:p "Now what about that $3\\cdot 30$? Do we know how to
             do this?  Yes!  In fact we can say it is the same as
             $3\\cdot 3\\cdot 10$ and we have a rule which says that
             if we have more than two factors in a product, we can
             first multiply any two of them.") (:p "So $3\\cdot
             3\\cdot 10=9\\cdot 10=90$.") (:p "You do the rest of the
             calculation: Add 90 to $3\\cdot 7$: what is the result?"))

    :stepj ((:p "The calculation on the previous page was:
    $90+21=111$") (:p "Another way of visualising the rule is given on
    the next page.  The product is shown as a rectangle made up of a
    factor and a sum which you should write in the boxes
    under " (:q "Factors") ".  You should click " (:q "enter") " when
    you have written your factors.") (:p "The labels to the left and
    below of the rectangle tell how long the whole side is.  Labels to
    the right or above tell how long a part of a side is.  The label
    inside the rectangle tells the product for that part of the
    rectangle.") (:p "If you don't know what to do,
    click " (:q "auto") " and the system will choose a factor and sum
    for you.  Click " (:q "clear") " to clear the
    display") (:p "The " (:q "x") " and " (:q "y") ", are placeholders
    for the factors."))

    :display (funcall insert-product-builder)

    :step ((:p "The name of the rule is in the title of this lesson,
    above.  " (:q "Distribution") " means something
    like " (:q "sharing out") " and that is what the law does: it
    shares a multiplication out among the terms of a sum.") (:p " On
    the next page, you can practice multiplication of a one digit by a
    two-digit number, using the rule.  At each step,
    click " (:q "check") " when you are finished entering your
    answers.  If you want to practice again after completing an
    exercise, click " (:q "new") "."))

    :display (funcall insert-practice)

    :step ((:p "The distributive law allows us to extend our rules for
    the multiplication of digits to multiplication of numbers of more
    than one digit.  As an exercise, you should go back to the summary
    of the lesson titled " (:q "Basic Multiplication") "and work out
    how the rules for the multiplication of digits and zero and 1
    extend to numbers of more than one digit."))

    :step ((:h1 "Summary")
           (:ul (:li
                 (:p "Multiplication of a factor by a sum is the same
    as " (:strong "multiplying the factor by each") " of the numbers
    which add up to the sum and
    " (:strong "adding the products") "."))
                (:li "The " (:q "fancy") " name for the rule is in the
                lesson title above.  From now on, it will be referred
                to as " (:q "The distributive law")))
           (:p "Press the right arrow to complete this lesson."))
    :complete? t)
