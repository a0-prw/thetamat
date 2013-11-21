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

(defun init-mod2-mult-lesson-slots ()
  (ps:ps 
    (defun init-mod2-mult-lesson-slots ()
      (with-slots (ephemeral non-ephemeral) lesson-module   
        (with-slots (insert-product-builder
                     make-product-builder-layout create-prodb-cell
                     prodb-loop dis-styles  product-cell 
                     distab get-unums set-axis-terms
                     sides-ready redraw-product local-svg-grid-skip
                     compute-term-coords 

                     ;; TUTOR
                     initialize-ephemeral
                     clear-tio-grid
                     tutor-show-summary remove-tutor tutor-lesson-quit
                     
                     set-svg-grid-skip-for-function
                     make-tutor-display emote be-neutral neutral-mouth
                     make-hints svg-canvas make-face toggle-hints
                     summon-tutor initialize-tutor
                     summon-tutor-for-problem make-tutor-input
                     create-input-grid-for-sum-tio tutor-state
                     tutor-next-step defarcs defalt defanswer
                     do-unique-idx-c get-commutative-arc-path-specs
                     make-next-answer-name
                     make-next-answer-spec
                     make-fsm-node-name make-fsm-for-tio
                     get-tutor-base make-operand
                     get-cell-coords-string get-cell-coords 

                     insert-tutor-display
                     make-self-insert-input-for-tutor 
                     feed-self-insert-input-to-tutor
                     node-inputs node-outputs
                     insert-answer-line place-carry-digit-in-cell
                     enter-regroup-mode exit-regroup-mode

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
                     tio-rtl-arr tio-hline tio-vline
                     ;;end from scratchpad
                     ;;changed
                     make-nyi-hint
                     get-plus-answer-specs
                     tutor-new-problem make-hint-function
                     make-prodmachine-start-hint
                     make-completed-hint
                     ;;NEW
                     cdotify get-prod-answer-specs defprodmachine 
                     get-part-answer-for-altpath digit-at-position
                     get-final-prod-ans defpartproduct
                     ;;make-partial-product-hint 
                     make-hint-function-for-position
                     make-operands-hint-function
                     make-units-pprod-hint-function
                     make-tens-pprod-hint-function
                     make-answer-hint-function make-mult-regrouper-hint-function
                     hints-on make-answer-regrouper-hint-function

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
             
             hints-on
             (lambda ()
               (let ((but (dgebid "hints-on")))
                 (checked but)))
             
             insert-tutor-display
               (lambda ()
                 (let ((ltd (dgebid "lesson-text-display"))
                        (tutd (funcall make-tutor-display)))                       
                   (remove-children-from-nth ltd 0)
                   (append-child ltd tutd)))

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

               toggle-hints
               (lambda (ev)
                 (with-hint
                     (funcall display false)
                   (if (string= (id-of this)
                                "hints-on")
                       (setf hint true)
                       (setf hint false))
                   (when hint (funcall display true))))
               
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
               
               ;;svg-grid-skip 0.8
               
               set-svg-grid-skip-for-function
               (lambda (fn)
                 (cond ((string= fn :tutor)
                        (setf svg-grid-skip 1.5))
                       ((string= fn :pbuilder)
                        (setf svg-grid-skip 0.8))
                       (t (setf svg-grid-skip 1.5))))
               
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
                (let* (;;(fac1 (funcall random-int 1 10))
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
                  (setf (value fac1-holder) (strcat "10 + " t1))
                  (dispatch-event fac1-holder 
                                  (standard-event "auto-event"
                                                  (ps:create
                                                   evfun get-unums
                                                   element fac1-holder)))
                  (setf (value fac2-holder) (strcat "10 + " t2))
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
               (funcall set-svg-grid-skip-for-function :pbuilder)
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

             get-unums
             (lambda (ev el)
               (with-distab 
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
                 (add-event-no-capture infac2-input "click"
                                       (lambda (ev) (focus this)))
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

             ;; TUTOR STUFF

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
                           in-partial-product false
                           initialized false
                           prefix false
                           carried-garbage (ps:array)
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
                   carried-garbage (ps:array)
                   
                   ;; a-answer false
                   ;; b-answer false
                   
                   prefix false
                   start-nodes (ps:new (-object))
                   alternative-states (ps:new (-object))
                   current-node false
                   in-answer false
                   in-partial-product false
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

;; handle-prefix-notset
;;                    (lambda (input)
;;                      (with-tutor-state
;;                          (with-fsm
;;                              (let* ((inpa (strcat a-prefix input))
;;                                     (inpb (strcat b-prefix input))
;;                                     (currenta (ps:getprop alternative-states "A"))
;;                                     (currentb (ps:getprop alternative-states "B"))
;;                                     (arca (ps:getprop (node-outputs currenta) inpa))
;;                                     (arcb (ps:getprop (node-outputs currentb) inpb)))
;;                                (cond ((and arca arcb)
;;                                       (let ((newa (arc-to arca))
;;                                             (newb (arc-to arcb)))
;;                                         (setf (ps:getprop alternative-states "A") newa
;;                                               (ps:getprop alternative-states "B") newb)
;;                                         ;;next call is ok: they have same side (printing) effect
;;                                         (funcall emote true)
;;                                         (funcall (arc-action arca))))
;;                                      (arca (setf prefix a-prefix ;;HERE call set-current-node instead?
;;                                                  current-node (arc-to arca))
;;                                            (funcall emote true)
;;                                            (funcall (arc-action arca)))
;;                                      (arcb (setf prefix b-prefix ;;HERE call set-current-node instead?
;;                                                  current-node (arc-to arcb))
;;                                            (funcall emote true)
;;                                            (funcall (arc-action arcb)))
;;                                      (t (funcall emote false)))))))

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
                   
                   handle-partial-product
                   (lambda (input)
                     (with-tutor-fsm
                         (cond (regroup-mode
                                (setf input (strcat prefix input))
                                (funcall handle-regroup input))
                               (t 
                                (setf input (strcat prefix input))
                                (funcall handle-no-regroup input)))))
                   
                   handle-regroup 
                   ;;in-answer
                   (lambda (input)
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
                                   (let* ((input ;; (if in-partial-product
                                                 ;;     (strcat "R" input)
                                                 ;;     (strcat "R_" input))
                                           (strcat "R" input))                                            
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
                                     (if (if in-partial-product 
                                             (string= (subseq name 0 2) (strcat "R" (subseq prefix 0 1)))
                                             (string= (subseq name 0 1) "R"))
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
                                                (cond (in-partial-product
                                                       (funcall handle-partial-product input))
                                                      (in-answer
                                                       (if (string= (fsm-node-name current-node)
                                                                    "completed")
                                                           (funcall handle-completion)
                                                           (funcall handle-partial-product input))) ;;handle-prefix-set input)))
                                                           ;; (if regroup-mode 
                                                           ;;     (funcall handle-regroup input)
                                                           ;;     (funcall handle-no-regroup input))))
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

             tutor-new-problem
               (lambda (ev)
                 (let ((vn "prod1:q"))
                   (funcall remove-tutor)
                   (funcall reset-var vn (funcall use-var vn) true)))


             defarcs ;;returns an array of: digit, current name, current node
             (lambda (op-rst isa ispp)
               (labels ((defarcs-rec (ds)
                          (let* ((curr (aref ds 0))
                                 (next (aref ds 1))
                                 (currname (funcall make-fsm-node-name curr))
                                 (nextname (when next (funcall make-fsm-node-name next))))
                            (if (not-defined next) (progn (defnode currname)
                                                          (when isa (def-completed-arc curr currname))
                                                          (when ispp (def-completed-parts-arc curr currname))
                                                          (ps:array (aref curr 2) currname curr))
                                (ps:destructuring-bind (pref (x y) nextdig)
                                    next
                                  (ps:destructuring-bind (ign (cx cy) currdig)
                                      curr
                                    (defnode currname)
                                    (defnode nextname)
                                    (cond ((and ispp
                                                (= x 4)
                                                (= y 3));;its the next partial product
                                           (def-next-part-product-arc currdig currname nextname))
                                         ((or isa ispp)
                                         (def-answer-arc curr currname next nextname))
                                        ;; (ispp
                                        ;;  (def-part-answer-arc curr currname next nextname))
                                        (t
                                         (def-printer-arc curr currname next nextname)))
                                  (funcall defarcs-rec (subseq ds 1))))))))
                 (funcall defarcs-rec op-rst true)))

             ;;              defarcs ;;returns an array of: digit, current name, current node
             ;; (lambda (op-rst isa ispp)
             ;;   (labels ((defarcs-rec (ds)
             ;;              (let* ((curr (aref ds 0))
             ;;                     (next (aref ds 1))
             ;;                     (currname (funcall make-fsm-node-name curr))
             ;;                     (nextname (when next (funcall make-fsm-node-name next))))
             ;;                (if (not-defined next) (progn (defnode currname)
             ;;                                              (when isa (def-completed-arc curr currname))
             ;;                                              (when ispp (def-completed-parts-arc curr currname))
             ;;                                              (ps:array (aref curr 2) currname curr))
             ;;                    (progn
             ;;                      (defnode currname)
             ;;                      (defnode nextname)
             ;;                      (cond ((or isa ispp)
             ;;                             (def-answer-arc curr currname next nextname))
             ;;                            ;; (ispp
             ;;                            ;;  (def-part-answer-arc curr currname next nextname))
             ;;                            (t
             ;;                             (def-printer-arc curr currname next nextname)))
             ;;                      (funcall defarcs-rec (subseq ds 1)))))))
             ;;     (funcall defarcs-rec op-rst true)))
             
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
                         (let* ((opr "*")
                                (opnode (ps:array pref (ps:array 2 1) opr))
                                (opname (strcat pref "2,1_" opr))
                                (next (aref op1 0))
                                (nextname (funcall make-fsm-node-name next)))
                           (defnode nextname)
                           (defnode opname)
                           (def-printer-arc curr currname opnode opname)
                           (def-printer-arc opnode opname next nextname)
                           (ps:destructuring-bind (lastd lastname last)
                               (funcall defarcs op1)
                             (ps:array last lastname lastd))))))))

             ;; defpartanswer
             ;; (lambda ()
             ;;   false)

             ;; make-noncomm-start-arc-action
             ;; (lambda (to-name)
             ;;   (with-tutor-fsm
             ;;       (ps:destructuring-bind (coords dig)
             ;;           (split to-name "_")
             ;;         (ps:destructuring-bind (col row)
             ;;             (split coords ",")
             ;;           (let* ((cell (cell-id col row))
             ;;                  (node (funcall find-node to-name))
             ;;                  (next (node-next-name node))
             ;;                  (nextnode (funcall find-node next)))
             ;;             (lambda ()
             ;;               (funcall place-string-in-selected-cell dig)
             ;;               (funcall  set-current-node nextnode)))))))
                           
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

             defpartproduct
             (lambda (aspecs)
               (let* ((first (aref aspecs 0))
                      (firstname (funcall make-fsm-node-name first)))
                 ;;defarcs claims to return an array of: digit, current name, current node
                 (destructuring-bind (digit currname currnode)
                     (funcall defarcs aspecs false true)
                   (ps:array first firstname digit currnode currname))))
             
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
                         (funcall do-unique-idx-c n1 (ps:array 0 1) 3)
                       (ps:destructuring-bind (b0 b1)
                           (funcall do-unique-idx-c n2 (ps:array 1 0) 3)
                         (ps:array (ps:array a0 b0)
                                   (ps:array b1 a1))))
                       (let ((longer (if (> len1 len2) n1 n2))
                             (shorter (if (> len1 len2) n2 n1))
                             (gl (max len1 len2))
                             (sl (min len1 len2)))
                         (ps:destructuring-bind (a0 a1)
                             (funcall do-unique-idx-c longer (ps:array 0 1) 3)
                           (ps:destructuring-bind (b0 b1)
                               (funcall do-unique-idx-c shorter 
                                        (ps:array 1 0) 
                                        (1+ (- gl sl)))
                             (ps:array (ps:array a0 b0)
                                       (ps:array b1 a1)))))))) 

             get-plus-answer-specs
             (lambda (prefix op0cfs op1cfs anscfs)
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
                                       (push  (ps:array prefix (ps:array col row) a) acc)
                                       (funcall spec-rec nc (subseq ds0 1) (subseq ds1 1) (subseq das 1) acc)))
                                    ((= a (mod test 10))
                                     (let ((nc (1- col)))
                                       (setf acc (append acc (ps:array (ps:array prefix (ps:array col row) a)
                                                                       (ps:array (strcat "R" prefix) (ps:array nc (- row 2)) 1))))
                                       (funcall spec-rec nc (subseq ds0 1) (subseq ds1 1) (subseq das 1) acc)))
                                    (t (if (>= test 9) ;;do another carry
                                           (let ((nc (1- col)))
                                             (setf acc (append acc (ps:array (ps:array prefix (ps:array col row) a)
                                                                         (ps:array (strcat "R" prefix) (ps:array nc (- row 2)) 1))))
                                             (funcall spec-rec nc (subseq ds0 1) (subseq ds1 1) (subseq das 1) acc))
                                           (let ((nc (1- col)))
                                             (push (ps:array prefix (ps:array col row) a)
                                                   acc)
                                             (funcall spec-rec nc (subseq ds0 1) (subseq ds1 1) (subseq das 1) acc))))))))
                   (funcall spec-rec maxcol op0 op1 ans (ps:array)))))
             
             get-final-prod-ans
             (lambda (aspec1 aspec2 ans)
               (with-tutor-fsm
                   (labels ((pprods (n1str n2str)
                              (let* ((n2ds (split n2str ""))
                                     (n2-units (parse-int (aref n2ds 1)))
                                     (n2-tens (* 10 (parse-int (aref n2ds 0))))
                                     (n1 (parse-int n1str)))
                                (ps:array (strcat "" (* n2-units n1)) 
                                          (strcat "" (* n2-tens n1))))))
                     (let* ((n1s (operand-coefficients (operands-opd1 current-tutor-input)))
                            (n2s (operand-coefficients (operands-opd2 current-tutor-input))))
                       (destructuring-bind (a1n1 a1n2)
                           (funcall pprods n2s n1s)
                         (destructuring-bind (a2n1 a2n2)
                             (funcall pprods n1s n2s)
                           (ps:array (funcall get-plus-answer-specs a-prefix a2n1 a2n2 ans)
                                     (funcall get-plus-answer-specs b-prefix a1n1 a1n2 ans))))))))
                          
             digit-at-position
             (lambda (igr pos)
               (if (zerop pos)
                   (rem igr 10)
                   (funcall digit-at-position (floor (/ igr (pow 10 pos))) 0)))

             get-part-answer-for-altpath
             (lambda (path ans)
               (labels ((tens-and-units (d1 d2 &optional (addin 0))
                          (let* ((p (+ addin (* d1 d2)))
                                 (units (funcall digit-at-position p 0))
                                 (tens (funcall digit-at-position p 1)))
                            (ps:array tens units)))
                        (process-digits (pref x y comb &optional (addin 0))
                          (destructuring-bind (d1 d2) comb                           
                                (destructuring-bind (c d) (funcall tens-and-units d1 d2 addin)
                                  (cond ((zerop c)
                                         (ps:array (pd-values pref x y (strcat "" d)) false (ps:array (1- x) y)))
                                        (t (ps:array (pd-values pref x y (strcat "" d)) 
                                                         (pd-values (strcat "R" pref)  (1- x) 0 (strcat "" c))
                                                         (ps:array (1- x) y))))))))
                 (destructuring-bind (((preft1 coordt1 tens1)
                                       (prefd1 coordd1 digits1))
                                      ((preft2 coordt2 tens2)
                                       (prefd2 coordd2 digits2)))
                     path
                   (let* ((prefix (strcat (subseq preft1 0 1) "1_"))
                          (f1-t (parse-int tens1))
                          (f1-d (parse-int digits1))
                          (f2-t (parse-int tens2))
                          (f2-d (parse-int digits2))
                          (combinations (ps:array (ps:array f2-d f1-d)
                                                  (ps:array f2-d f1-t)
                                                  :tens
                                                  (ps:array f2-t f1-d)
                                                  (ps:array f2-t f1-t)
                                                  :done))
                          (current-carry 0)
                          (current-x 4)
                          (current-y 2)
                          (code (ps:array)))
                     (loop for dc in combinations
                        do  (cond ((string= dc :tens)
                                   (setf prefix (strcat (subseq prefix 0 1) "2_")
                                         code (append code (ps:array (pd-values prefix 4 3 "0")))
                                         current-x 3 current-y 3 current-carry 0))
                                  ((string= dc :done)
                                   (setf code (append code (ps:array (pd-values prefix (1- current-x) current-y "+")))))
                                  (t
                                   (destructuring-bind (digitspec carryspec (x y))
                                       (funcall process-digits prefix current-x current-y dc current-carry)
                                     (setf code (append code (ps:array digitspec)))
                                     (if carryspec 
                                         (destructuring-bind (marker (cx cy) digit)
                                             carryspec
                                           (cond ((and (= cx 2) (= cy 0)
                                                       (= current-y 2))
                                                  (setf code (append code (ps:array carryspec (pd-values prefix 2 2 (strcat "" digit))))))
                                                 ((and (= cx 1) (= cy 0)
                                                       (= current-y 3))
                                                  (setf code (append code (ps:array carryspec (pd-values prefix 1 3 (strcat "" digit))))))
                                                 (t (setf code (append code (ps:array carryspec)))))
                                           (setf current-carry (parse-int digit)))
                                         (setf current-carry 0))
                                     (setf current-x x current-y y)))))
                     
                     code
                     ))))
             
             get-prod-answer-specs
             (lambda (path1 path2)
               (let* ((aspec1 (funcall get-part-answer-for-altpath path1))
                      (aspec2 (funcall get-part-answer-for-altpath path2)))
                   (ps:array aspec1 aspec2)))
             
             make-nyi-hint
             (lambda (node)
               (let ((txt-el (create-para-text (strcat "No hint for node: "
                                                       (fsm-node-name node)))))
                 (lambda ()
                   (with-hint (setf cell0 false
                                    cell1 false
                                    text-element txt-el)))))
             make-hint-function
             (lambda (node)
               (let* ((nodename (fsm-node-name node))
                      (tarr (split nodename "_"))
                      (tag (aref tarr 0)))
                 (cond ((string= nodename "start")
                        (funcall make-prodmachine-start-hint node))
                       ((string= nodename "completed")
                        (funcall make-completed-hint node))
                       (t (funcall make-hint-function-for-position node)))))

             make-hint-function-for-position
             (lambda (node)
               (destructuring-bind (tag costr str)
                   (split (fsm-node-name node) "_")
                 (destructuring-bind (xstr ystr)
                     (split costr ",")
                   (let ((x (parse-int xstr)) (y (parse-int ystr)))
                     (cond ((and (= y 0)
                                 (= (length tag) 3))
                            ;;(string= (subseq tag 0 1) "R"))
                            (funcall make-mult-regrouper-hint-function str))
                           ((and (= y 2)
                                 (= (length tag) 2)
                                 (string= (subseq tag 0 1) "R"))
                            (funcall make-answer-regrouper-hint-function str))
                            ((or (= y 0)
                                (= y 1))
                            (funcall make-operands-hint-function str))
                           ((= y 2)
                            (funcall make-units-pprod-hint-function node x y str))
                           ((= y 3)
                            (funcall make-tens-pprod-hint-function node x y str))
                           ((= y 4)
                            (funcall make-answer-hint-function node))
                           (t (funcall make-nyi-hint node)))))))

             make-answer-regrouper-hint-function
             (lambda (str)
               (let ((txt-el (create-para-text "Write the tens digit of the sum you did.")))
                 (lambda ()
                   (with-hint (setf cell0 false
                                    cell1 false
                                    text-element txt-el)))))

             make-operands-hint-function
             (lambda (str)
               (let* ((txt (strcat "Write  " str))
                      (txt-el (create-para-text (strcat txt "."))))
                 (lambda (return-string)
                   (if return-string txt
                       (with-hint (setf cell0 false
                                        cell1 false
                                        text-element txt-el))))))

             make-mult-regrouper-hint-function
             (lambda (str)
               (lambda ()
                 (let* ((txt-el (create-para-text 
                                 (strcat "Write the tens digit of the multiplication you just did.  Write  " 
                                         str "."))))
                   (with-hint (setf cell0 false
                                    cell1 false
                                    text-element txt-el)))))

             make-units-pprod-hint-function
             (lambda (node x y str)
               (with-tutor-fsm
                   (let* ((next (node-next-name node))
                          (nxarr (split next "_"))
                          (selfregroup (string= (subseq (aref nxarr 0) 0 1) "R")))
                         (lambda ()
                           (let* ((pref (subseq prefix 0 1))
                                  (order (if (string= pref "a")
                                             (ps:array (operand-coefficients (operands-opd2 current-tutor-input))
                                                       (operand-coefficients (operands-opd1 current-tutor-input)))
                                             (ps:array (operand-coefficients (operands-opd1 current-tutor-input))
                                                       (operand-coefficients (operands-opd2 current-tutor-input)))))
                                  (miercell (dgebid (cell-id 4 1)))
                                  (digunits (text-content-of 
                                             (aref 
                                              (gebtag miercell "tspan") 0)))
                                  (mcancell (dgebid (cell-id x 0)))
                                  (tspans (gebtag mcancell "tspan"))
                                  (digtspan (aref tspans 0))
                                  (dig (when digtspan (if (> x 2) (text-content-of digtspan) false)))
                                  (carryspan (if (= x 2) (aref tspans 0) (aref tspans 1)))
                                  (carry (when carryspan (text-content-of carryspan))))
                             (destructuring-bind (multiplier multiplicand)
                                 order
                               (let* ((txt1 (strcat "You are multiplying the units digit of " 
                                                    multiplier
                                                    " with the " 
                                                    (cond ((= x 4) "units digit")
                                                          ((= x 3) "tens digit")
                                                          (t "hundreds digit (which is 0)")) " of " multiplicand))
                                      (txt2 (if carry (strcat " and must add in the regrouped digit " carry) ""))
                                      (txt3 (if selfregroup ". The result is greater than 9, so you must regroup.  Press R" ""))
                                      (txt4 (if carry (strcat ". Write the units result of multiplying " 
                                                              digunits " by " 
                                                              (if (or (= x 4)
                                                                      (= x 3))
                                                                  dig "0") 
                                                              " plus " carry ". Write " str ".")
                                                (strcat ". Write the units result of multiplying " 
                                                        digunits " by " 
                                                        (if (or (= x 4)
                                                                (= x 3))
                                                            dig "0") ". Write " str ".")))
                                      
                                      (txt-el 
                                       (create-para-text 
                                        (strcat txt1 txt2 txt3 txt4))))
                                 (with-hint (setf cell0 false
                                                  cell1 false
                                                  text-element txt-el)))))))))
             
                            
                 
               ;;(funcall make-nyi-hint node))
             
             make-tens-pprod-hint-function
             (lambda (node x y str)
               (with-tutor-fsm
                   (cond ((string= str "+")
                          (lambda ()
                            (let ((txt-el (create-para-text "You are finished multiplying and must now add the results.  Write +.")))
                              (with-hint (setf cell0 false
                                               cell1 false
                                               text-element txt-el)))))
                         ((and (= x 4) (= y 3))
                          (lambda ()
                            (let ((txt-el (create-para-text 
                                            "You are now multiplying by the tens digit, so the result will end in a zero.  Write 0.")))
                              (with-hint (setf cell0 false
                                               cell1 false
                                               text-element txt-el)))))
                         
                         (t 
                          (let* ((next (node-next-name node))
                                 (nxarr (split next "_"))
                                 (selfregroup (string= (subseq (aref nxarr 0) 0 1) "R")))
                            (lambda ()
                              (let* ((pref (subseq prefix 0 1))
                                     (order (if (string= pref "a")
                                                (ps:array (operand-coefficients (operands-opd2 current-tutor-input))
                                                          (operand-coefficients (operands-opd1 current-tutor-input)))
                                                (ps:array (operand-coefficients (operands-opd1 current-tutor-input))
                                                          (operand-coefficients (operands-opd2 current-tutor-input)))))
                                     (miercell (dgebid (cell-id 3 1)))
                                     (digunits (text-content-of 
                                                (aref 
                                                 (gebtag miercell "tspan") 0)))
                                     (mcancell (dgebid (cell-id  (1+ x) 0)))
                                     (carrycell (dgebid (cell-id x 0)))
                                     (carryspans (gebtag carrycell "tspan"))
                                     (tspans (gebtag mcancell "tspan"))
                                     (digtspan (aref tspans 0))
                                     (dig (when digtspan (if (> x 1) (text-content-of digtspan) false)))
                                     (carryspan (if (< x 3) (aref carryspans 0) (aref carryspans 1)))
                                     (carry (when carryspan (text-content-of carryspan))))
                                (destructuring-bind (multiplier multiplicand)
                                    order
                                  (let* ((txt1 (strcat "You are multiplying the tens digit of " 
                                                       multiplier
                                                       " with the " 
                                                       (cond ((= x 3) "units digit")
                                                             ((= x 2) "tens digit")
                                                             (t "hundreds digit (which is 0)")) " of " multiplicand))
                                         (txt2 (if carry (strcat " and must add in the regrouped digit " carry) ""))
                                         (txt3 (if selfregroup ". The result is greater than 9, so you must regroup.  Press R" ""))
                                         (txt4 (if carry (strcat ". Write the units result of multiplying " 
                                                                 digunits " by " 
                                                                 (if (or (= x 3)
                                                                         (= x 2))
                                                                     dig "0") 
                                                                 " plus " carry ". Write " str ".")
                                                   (strcat ". Write the units result of multiplying " 
                                                           digunits " by " 
                                                           (if (or (= x 4)
                                                                   (= x 3))
                                                               dig "0") ". Write " str ".")))
                                         
                                         (txt-el 
                                          (create-para-text 
                                           (strcat txt1 txt2 txt3 txt4))))
                                    (with-hint (setf cell0 false
                                                     cell1 false
                                                     text-element txt-el)))))))))))
             
                 
             make-answer-hint-function
             (lambda (node)
               (let* ((to-r (string= (subseq (aref (split (node-next-name node) "_")
                                                   0) 0 1)
                                     "R"))
                      (txt-el (create-para-text
                               (strcat "Add the digits in the products above your position." 
                                       (if to-r "  Press 'r' and write the units digit of the sum." 
                                           "  Write the sum.")))))
                     (lambda ()
                       (with-hint 
                           (setf cell0 false
                                 cell1 false
                                 text-element txt-el)))))


             

                        ;; (= (length tag) 2)
                        ;;  (funcall make-partial-product-hint tag nodename node))
                        
                        ;; (node (funcall make-nyi-hint node))
                        ;; (t (alert "make-hint-function needs a node.")))))

             ;; make-partial-product-hint 
             ;; (lambda (tag nodename node)
             ;;   (let* ((row (subseq tag 1 2))
             ;;          (nextname (node-next-name node))
             ;;          (nextparts (split nextname "_"))
             ;;          (regroup (if (string= (subseq (aref nextparts 0) 0 1) "R")
             ;;                       (aref nextparts 2) false))
             ;;          (f2-units (if (= 1 (parse-int row)) true false))
             ;;          (p (create-para-text (strcat "row: " row ". nextname: " nextname ". regroup: " regroup ". f2-units: " f2-units))))
             ;;     (lambda ()
             ;;       (with-hint 
             ;;           (setf text-element p
             ;;                 cell0 false
             ;;                 cell1 false)))))
                 ;; (create-para-text (strcat (if (= 1 (parse-int row))
                 ;;                                       "Multiply the units digit of the second factor by each of the digits of the first factor."
                 ;;                                       "Multiply the tenss digit of the second factor by each of the digits of the first factor.")))))

             make-completed-hint
             (lambda (node) ;;don't use node yet
                 (let* ((ans (with-tutor-state
                                (dots current-tutor-input
                                      answer coefficients)))
                       (p (create-para-text (strcat "Product correct! " ans))))
                   (lambda ()
                     (with-hint (setf text-element p
                                      cell0 false
                                      cell1 false)
                       true))))

             make-prodmachine-start-hint
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


               ;; (let* ((opr (with-tutor-state
               ;;                 (tio-operator current-tutor-input)))
               ;;        (nodename (fsm-node-name node)))
               ;;   (cond ((string=  opr "+")
               ;;          (cond ((string= nodename "start")
               ;;                 (funcall make-plusmachine-start-hint node))
               ;;                ((string= nodename "completed")
               ;;                 (funcall make-completed-hint node))
               ;;                (t (funcall make-in-plus-sum-hint node nodename))))
               ;;         ((string= opr "-")
               ;;          (cond ((string= nodename "start")
               ;;                 (funcall make-minusmachine-start-hint node))
               ;;                ((string= nodename "completed")
               ;;                 (funcall make-completed-hint node))
               ;;                (t (funcall make-in-minus-sum-hint node nodename))))
               ;;         ((string= opr "\\cdot")
               ;;          (funcall make-nyi-hint node))
               ;;         (t (alert (strcat "NYI: hints for opr: " opr))))))
                          
             defprodmachine
             (lambda (machine-spec)
               (ps:destructuring-bind (((n00 n01) pa0 ta0) ((n10 n11) pa1 ta1))
                   machine-spec
                 (defnode "start")
                 (ps:destructuring-bind (qlast0 qlastname0 qlastd0)
                     (funcall defalt n00 n01)
                   (ps:destructuring-bind (qlast1 qlastname1 qlastd1)
                       (funcall defalt n10 n11)
                     (ps:destructuring-bind (first0 firstpprod0 currdig0 curr0 currname0)
                         (funcall defpartproduct pa0)
                       (def-in-part-product-arc qlastd0 qlastname0 firstpprod0)
                       (destructuring-bind (afirst afirstname)
                           (funcall defanswer ta0)
                         (def-have-part-products-arc currdig0 currname0 afirstname)
                       (ps:destructuring-bind (first1 firstpprod1 currdig1 curr1 currname1)
                           (funcall defpartproduct pa1)
                         (def-in-part-product-arc qlastd1 qlastname1 firstpprod1)
                         (destructuring-bind (bfirst bfirstname)
                             (funcall defanswer ta1)
                         (def-have-part-products-arc currdig1 currname1 bfirstname)
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
                                    (funcall find-node "start"))
                         
                         )))))))))
                   ;;(ps:array mac1-spec mac2-spec))
             
               ;; defprodmachine
               ;; (lambda (mac1-spec mac2-spec)
               ;;   (ps:destructuring-bind (((n00 n01) pa0 ta0) ((n10 n11) pa1 ta1))
               ;;       (ps:array mac1-spec mac2-spec))
             
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
                         (cond ((string= opr "\\cdot")
                                (destructuring-bind (path1 path2)
                                    (funcall get-commutative-arc-path-specs
                                             opd1 opd2)
                                  (destructuring-bind (aspec1 aspec2)
                                      (funcall get-prod-answer-specs
                                               path1 path2)
                                    (destructuring-bind (ans1 ans2)
                                        (funcall get-final-prod-ans aspec1 aspec2 ans) ;;HERE
                                      (funcall defprodmachine (ps:array 
                                                               (ps:array path1 aspec1 ans1)
                                                               (ps:array path2 aspec2 ans2)))))))
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

             
             input-direction ;; Var slot done
             (ps:create left-to-right 0 right-to-left 1 down-to-up 2 up-to-down 3
                        selectedv 2 selectedh 0
                        reset (lambda ()
                                (with-input-direction 
                                    (setf left-to-right 0 right-to-left 1 down-to-up 2 up-to-down 3
                                          selectedv 2 selectedh 0))))

             tio-ltr-arr (input-menu-arrow "tio-ltr-arr" 0)
             tio-rtl-arr (input-menu-arrow "tio-rtl-arr" 180)

             tio-vline ;; Slot
              (lambda (id)
                (let* ((skip svg-grid-skip) (skipem (strcat skip "em"))
                       (svg (svg-create-element "svg"))
                       (bar (svg-create-element "line"))
                       (x1 (* skip 0.5)) (y1 0.0625)
                       (x2 x1) (y2 (- skip y1))
                       (x1em (strcat x1 "em")) (x2em x1em)
                       (y1em (strcat y1 "em")) (y2em (strcat y2 "em")))
                  (set-attributes svg "version" "1.2" "baseProfile" "basic"
                                  "id" id "height" skipem "width" skipem)
                  (set-attributes bar "x1" x1em "y1" y1em "x2" x2em "y2" y2em
                                  "stroke" "#000000")
                  (append-child svg bar)
                  svg))

              tio-hline ;; Slot
              (lambda (id)
                (let* ((skip svg-grid-skip) (skipem (strcat skip "em"))
                       (svg (svg-create-element "svg"))
                       (dash (svg-create-element "line"))
                       (y1 (* skip 0.5)) (x1 0.0625)
                       (y2 y1) (x2 (- skip x1))
                       (x1em (strcat x1 "em")) (x2em (strcat x2 "em"))
                       (y1em (strcat y1 "em")) (y2em (strcat y2 "em")))
                  (set-attributes svg "version" "1.2" "baseProfile" "basic"
                                  "id" id "height" skipem "width" skipem)
                  (set-attributes dash "x1" x1em "y1" y1em "x2" x2em "y2" y2em
                                  "stroke" "#000000")
                  (append-child svg dash)
                  svg))
              

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
                                                   ;;HERE or in-partial-product
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
              (lambda (row)
                (let* ((arow (if row row (with-grid-dim (1- rows))))
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
                (with-tutor-fsm
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
                  (push t1 carried-garbage)
                  (append-child t1 tspan))))
  
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
                       (dxem "0.4em") (dyem "1.25em"))
                  (set-attributes tspan "dx" dxem "dy" dyem)
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
                        (when (or in-answer in-partial-product)
                          ;; HERE or in-partial-product ?
                          (setf regroup-mode 1))
                      (when (not commutative)
                        (funcall transition (fsm-node-name current-node) true)))))

              exit-regroup-mode
              (lambda ()
                (with-tutor-state
                    (with-fsm
                        (when (or in-answer in-partial-product)
                          ;; HERE or in-partial-product ?
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
               menu-elements ;;symbol-macro in dom-utils
                         
              ;;;;;;;;;;;;;;;;;;;;;;;;;; end from scratchpad


             create-input-grid-for-sum-tio
             (lambda (tio)
               (let* ((op (tio-operator tio))
                      ;; (opdlen (max (operand-length (operands-opd1 tio))
                      ;;                    (operand-length (operands-opd2 tio))))
                      ;; (side (1+ opdlen))
                      (tio-grid (standard-svg-grid "tutor-canvas" 
                                                   "tutor-input-object" 
                                                   5 5  
                                                   create-scratchpad-cell))
                      (holder (parent-node tio-grid))
                      (pdisp (dgebid "tutor-problem-display")))
                 (setf (text-content-of pdisp) 
                       (strcat "$"
                               (operand-coefficients (operands-opd1 tio))
                               op
                               (operand-coefficients (operands-opd2 tio))
                               "$")
                       (ps:getprop tio "dimensions") (ps:create rows 5
                                                                columns 5))
                 (mj-hub-queue (array "Typeset" mj-hub pdisp))
                 tio-grid))
             

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
                                      ("\\cdot" "\\cdot")
                                      ("*" "\\cdot")
                                      (t (alert (strcat "NYI: " operation)))))))
                     (setf  current-tutor-input cti)
                     cti))))
             
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
                   (funcall make-tio-grid-menu 1) 
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

               cdotify
               (lambda (string)
                 (let ((parts (split string " ")))
                   (destructuring-bind (t1 op t2)
                       parts
                     (strcat t1 " \\cdot " t2))))
                   
               
               summon-tutor-for-problem
             (lambda (q a)
               (funcall set-svg-grid-skip-for-function :tutor)
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
                                     ((string= op "*")
                                      (funcall 
                                       cdotify (funcall use-var "prod1:q")))
                                     (t "#"))
                               "$") true))
                 (funcall summon-tutor (funcall make-tutor-input op n1 n2 a))))
                          
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
    (init-mod2-mult-lesson-slots)
    
    ))


(define-lesson mod2-mult
    :step ((:p "This lesson will first describe in words how the
    distributive law can be extended to numbers of more than 1
    digit.") (:p "This will be followed by a way to visualize the
    multiplication of positive whole numbers of several digits, using
    the " (:q "rectangle as product idea.")) (:p "After that, the
    lesson will show an efficient way to arrange your work when you
    have to multiply 2-digit numbers yourself."))

    :stepj ((:p "To multiply a 2-digit factor by another 2-digit
    factor, write them as sums, and use the distributive law
    twice.") (:p "For example, to do the multiplication $27\\cdot 39$,
    write the expression as:") (:p "$(20+7)\\cdot (30+9)$") (:p "Now
    use the distributive law to rewrite the expression
    as:") (:p "$(20+7)\\cdot 30+(20+7)\\cdot 9$") (:p "Look carefully
    at the previous line until you can see how the distributive law is
    being used."))

    :stepj ((:p "We have rewritten $27\\cdot 39$ as:") 
            (:p "$(20+7)\\cdot 30+(20+7)\\cdot 9$") 
            (:p "Now we use the distributive law again to rewrite the expression as:") 
            (:p "$20\\cdot 30+7\\cdot 30+20\\cdot 9+7\\cdot 9$") 
            (:p "We have now reduced the original problem to one we
    can easily solve, using only our knowledge of digit multiplication
    and the decimal position system. We do the multiplications and
    get:") 
            (:p "$600+210+180+63$") (:p "Why is $20\\cdot 30=600$?")
            (:p "Well, we can write it as:") 
            (:p "$2 \\cdot 10 \\cdot 3 \\cdot 10=2\\cdot 3\\cdot 10
                \\cdot 10=6\\cdot 100=600$"))

    :step ((:p "The same idea can be used with any number of factors
    made up of any number of digits: write them as sums and apply the
    distributive law repeatedly, until only digit multiplication and
    multiplication by powers of ten is left.") (:p "A traditional way
    of doing multiplication is shown later in the lesson.  However, it
    is important to realise that the method only works because it
    follows the rules of multiplication which you already
    know.") (:p "Before the method is shown, you can visualize the
    multiplication of 2-digit positive integers in the next page.  As
    before, click " (:q "auto") " if you don't know what to do, and
    look at what is written in the input boxes. You
    should " (:strong "study and investigate") " this visualisation
    until you can see how it relates to the distributive law."))
    
    :display (funcall insert-product-builder)

    :step ((:p "In the next page, you can learn one way to set out
    your working, when you have to multiply a 2-digit number by a
    2-digit number yourself.  There are other ways.") (:p "Write the
    numbers, one under the other, lining up units, tens, ... and so
    on, as you have done before.") (:p "Multiply each digit of the
    second number, with each digit of the first number, making a new
    row each time you start multiplying by a different digit in the
    second number.") (:p "As always, it is important to write the
    results in the correct positions to signify their
    meaning.") (:p "Add the two numbers which you have calculated in
    the multiplication steps, to get the final answer.")(:p "Practice
    with hints turned on, until you can do it without help.") (:p "A
    good exercise, when you have learned the method, is to dissect it
    until you can see how the distributive law is being used."))
    
    :using-q&a-vars
    ((:q&a-var "prod1" mul-2-2nat)
     (with-lesson-module-slots
         (funcall summon-tutor-for-problem 
                  (funcall use-var "prod1:q")
                  (funcall use-var "prod1:a"))))

    :step ((:p "This way of setting out your working can easily be
    extended to numbers of more than 2 digits.  If you do this, when
    you get to the hundreds digit of the second factor, you start a
    new row, and must begin by writing 2 zeros, so that the results of
    your digit multiplications will be in the correct position.
    Similarly for the thousands, ten thousands, and so on."))

    :step ((:h1 "Summary")
           (:ul (:li "Multiplication with factors of several digits is
    accomplished by repeatedly using the distributive law.")
                (:li "A practical and efficient way of actually
                 writing the working when doing the calculation, is to
                 write one factor under another, with units, tens,
                 hundreds, ... (and so on), lining up vertically.  The
                 calculation is then achieved by multiplying each
                 digit in the second factor by each digit in the first
                 factor, remembering to regroup, if necessary, and
                 remembering to write each answer or partial-answer
                 digit in the correct place to signify the appropriate
                 number of powers of ten.")
           (:p "Press the right arrow to complete this lesson.")))
    :complete? t)
