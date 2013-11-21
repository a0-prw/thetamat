(in-package :autotutor)

(defun common-script ()
  (ps:who-ps-html 
   (:script 
    :type "text/javascript"
    (ps:ps 
      
      ;;(ps:lisp (format nil "use strict"))
      ;; These are used later in macroexpansions, so do not move them
      ;; (FIXME) ??? wat ??
      (defvar svg-grid-skip 1.5)
      (defvar input-menu-icon-pixels 16)
      ;;;;;;;
      
      (defvar off-screen-answer-handlers (ps:new (-Object)))

      (defvar scratchpad
        (ps:create
         initialized false))

      (defvar left-to-right-arrow (input-menu-arrow "ltr-arr" 0))
      (defvar up-to-down-arrow (input-menu-arrow "utd-arr" 90))
      (defvar right-to-left-arrow (input-menu-arrow "rtl-arr" 180))
      (defvar down-to-up-arrow (input-menu-arrow "dtu-arr" -90))

      (defvar selected-cell null)

      (defvar sp-code) ;;FIXME remove!!

      ;;Get rid of the eval stuff, maybe but why??  'It's dangerous if
      ;;you eval user supplied data', but we don't, so I'm keeping it
      ;;OTOH DO check user supplied strings!!!!
        
      ;;(defun dynload (filename) (let ((sc (create-element
      ;;"script"))) (set-attributes sc "type" "text/javascript" "src"
      ;;"filename") (when (defined (typeof sc)) (append-child (aref
      ;;(gebtag document "head") 0) sc))))

      (defun save-and-replace-off-screen-answer-handlers ()
        (save-off-screen-answer-handlers)
        (replace-off-screen-answer-handlers))
      
      (defun restore-off-screen-answer-handlers ()
        (with-slots (destroy-svg-grid) scratchpad
          (let ((svg (doc-get-el-by-id "scratch")))
            (when svg (funcall destroy-svg-grid))))
        (dotimes (n all-tables)
          (loop for input in 
               (get-elements-by-tag-name
                (doc-get-el-by-id (table-id n)) "input")
             for id = (id-of input)
             do (progn (set-attribute input "onkeydown" 
                                      (ps:getprop off-screen-answer-handlers
                                                  id))
                       (set-attribute input "onclick" nil))))
        (setf off-screen-answer-handlers (ps:new (-object))))

      ;; (defun restore-off-screen-answer-handlers ()
      ;;   (with-slots (destroy-svg-grid) scratchpad
      ;;   (let ((svg (doc-get-el-by-id "scratch")))
      ;;     (when svg (funcall destroy-svg-grid)))
      ;;   (loop for input in 
      ;;        (get-elements-by-tag-name
      ;;         (doc-get-el-by-id (table-id current-table)) "input")
      ;;      for id = (id-of input)
      ;;      do (progn (set-attribute input "onkeydown" 
      ;;                               (ps:getprop off-screen-answer-handlers
      ;;                                           id))
      ;;                (set-attribute input "onclick" nil))))
      ;;   (setf off-screen-answer-handlers (ps:new (-object))))

      (defun load-script (stext sid)
        (with-slots (id type text defer) sc
          (let ((sc (create-element "script")))
            (setf id sid
                  type "text/javascript"
                  text stext
                  defer true)
            ;; (set-attributes sc "type" "text/javascript"
            ;;                 "id" id
            ;;               "text" text
            ;;               "defer" true)
          (when (defined (typeof sc))
            (append-child (aref (gebtag document "head") 0) sc)))))        
      
      (defun summon-scratch-pad (event id)
        (with-slots (make-sp-menu ;;FIXME tailor it to module
                     ;;name see get-all-tables kitchen sink.  make
                     ;;menu for scratchpad dependent on module name
                     summon-scratch-pad initialized) scratchpad
          ;;          (let ((ml (get-attribute (dgebid (table-id current-table))"data-spmenu-level")))
          (labels ((callback (response)
                     (setf sp-code (parse-json response))
                     ;;(eval (parse-json response))
                     (eval sp-code) ;;HERE
                     ;;(load-script sp-code "spload")
                     ;;(funcall make-sp-menu ml) ;;HERE
                     (funcall summon-scratch-pad event id)))
            (if initialized 
                (funcall summon-scratch-pad event id)
                (ajax_load_scratchpad callback)))))

      ;; (defun summon-scratch-pad (event id)
      ;;   (with-slots (make-basic-sp-menu ;;FIXME tailor it to module
      ;;                ;;name see get-all-tables kitchen sink.  make
      ;;                ;;menu for scratchpad dependent on module name
      ;;                summon-scratch-pad initialized) scratchpad
      ;;     (labels ((callback (response)
      ;;                (setf sp-code (parse-json response))
      ;;                (eval (parse-json response))
      ;;                (eval sp-code) ;;HERE
      ;;                ;;(load-script sp-code "spload")
      ;;                (funcall make-basic-sp-menu)
      ;;                (funcall summon-scratch-pad event id)))
      ;;       (if initialized 
      ;;           (funcall summon-scratch-pad event id)
      ;;           (ajax_load_scratchpad callback)))))
      
      ;; (defun prepare-input-menu-holder ()
      ;;   (with-slots (init-element) drag-drop
      ;;     (let ((parent (dots document body))
      ;;           (imh (div-id "input-menu-holder"))
      ;;           (menuh (create-element "h1"))
      ;;           (sinpm (div-id-class "select-input-menu" "scratchpad-menu")))
      ;;       (append-text menuh "ScratchPad Menu")
      ;;       (append-children imh menuh sinpm)
      ;;       (append-child parent imh)
      ;;       (funcall init-element imh)
      ;;       imh)))

      (defun prepare-input-menu-holder (holder-id menu-id menu-class menu-text) 
        (with-slots (init-element) drag-drop
          (let ((parent (dots document body))
                (imh (div-id holder-id))
                (menuh (create-element "h1"))
                (sinpm (div-id-class menu-id menu-class)))
            (append-text menuh menu-text)
            (append-children imh menuh sinpm)
            (append-child parent imh)
            (funcall init-element imh)
            imh)))


      ;; (defun prepare-svg-canvas ()
      ;;   (with-slots (init-element) drag-drop
      ;;     (let ((parent (dots document body))
      ;;           (svgc (div-id-class "svg-canvas" "svg-container"))
      ;;           (msgdisp (div-id-class "message-display" "scratch-pad-info"))
      ;;           (probdisp (div-id-class "problem-display" "scratch-pad-info")))
      ;;       (append-text probdisp "&nbsp;")
      ;;       (append-child msgdisp probdisp)
      ;;       (append-children svgc msgdisp probdisp)
      ;;       (append-child parent svgc)
      ;;       (funcall init-element svgc)
      ;;       svgc)))
      
      (defun prepare-calced-canvas (canvas-id canvas-class 
                                    msg-disp-id msg-disp-class
                                    prob-disp-id prob-disp-class)
        (with-slots (init-element) drag-drop
          (let ((parent (dots document body))
                (svgc (div-id-class canvas-id canvas-class))
                (msgdisp (div-id-class msg-disp-id msg-disp-class))
                (probdisp (div-id-class prob-disp-id prob-disp-class)))
            (append-text probdisp "&nbsp;")
            (append-child msgdisp probdisp)
            (append-children svgc msgdisp probdisp)
            (append-child parent svgc)
            (funcall init-element svgc)
            svgc)))
      

      (defvar file-object ;;(ps:new (-object)))
        (ps:create
         files (lambda (fo) (files fo))
         size (lambda (fo) (size fo))))

      (defvar lesson-module 
        (ps:create 
         initialized false
         ephemeral (ps:new (-object))
         non-ephemeral (ps:new (-object))))

      (defvar special-lesson-module)
      (defvar can-quit-lesson false)

      (defun svg-grid-loop-function (startx starty)
        (lambda (height width columns skip rows skipem svg content-fn id)
          (loop for y from starty below height by skip
             for yv from starty below rows by 1
             do (loop for x from startx below width by skip
                   for xv from startx below columns by 1
                   do (funcall content-fn x y xv yv skipem svg id)))))
      
      ;; ;; DO NOT move to scratchpad
      (defun custom-svg-grid (parent id rows columns content-fn loop-fn
                                &optional (prefix-id false))
        (let* ((skip svg-grid-skip)
               (canvas (if (stringp parent)
                           (doc-get-el-by-id parent)
                           parent))
               (svg (svg-create-element "svg"))
               (skipem (strcat skip "em"))
               (height (* rows skip))
               (hem (strcat height "em"))
               (width (* columns skip))
               (wem (strcat width "em")))
          (set-attributes svg "version" "1.2" "baseProfile" "basic"
                          "id" id "height" hem "width" wem)
          (funcall loop-fn height width columns 
                   skip rows skipem svg content-fn (if prefix-id id false))
          (append-child canvas svg)
          (setf (dots canvas style display) "inline")
          svg))
      
      (defun standard-svg-grid (parent id rows columns content-fn prefix-id)
        (let* ((skip svg-grid-skip)
               (canvas (if (stringp parent)
                           (doc-get-el-by-id parent)
                           parent))
               (svg (svg-create-element "svg"))
               (skipem (strcat skip "em"))
               (height (* rows skip))
               (hem (strcat height "em"))
               (width (* columns skip))
               (wem (strcat width "em")))
          (set-attributes svg "version" "1.2" "baseProfile" "basic"
                          "id" id "height" hem "width" wem)
          (loop for y from 0 below height by skip
             for yv from 0 below rows by 1
             do (loop for x from 0 below width by skip
                   for xv from 0 below columns by 1
                   do (funcall content-fn x y xv yv skipem svg 
                               (if prefix-id id false))))
          (append-child canvas svg)
          (setf (dots canvas style display) "inline")
          svg))
      
      (define-with-cell-variables create-scratchpad-cell
          (set-attributes g "id" (cell-id xv yv nil)
                          "class" "scratch-pad-cell"
                          "partial-answer" "false")
        (set-attributes rect "x" xem "y" yem "width" skipem
                        "height" skipem "fill" "none"
                        "pointer-events" "all"
                        "stroke" "#cdcdcd" "stroke-opacity" "0.3")
        (add-event-no-capture rect "click" select-or-deselect-cell)
        (append-child g rect)
        (append-child svg g))

      (defun select-or-deselect-cell (ev)
        (let ((curr selected-cell)
              (me (id-of (parent-node this))))
          (if curr 
              (let ((old (first-child (doc-get-el-by-id curr))))
                (set-attribute old "fill" "none")
                (if (string= me curr) ;;deselect
                    (setf selected-cell null)
                    (progn 
                      (setf selected-cell me)
                      (set-attribute this "fill" "rgba(139, 203, 138, 0.4)"))))
              (progn
                (setf selected-cell me)
                (set-attribute this "fill" "rgba(139, 203, 138, 0.4)")))))
      
      ;; (defun drag (ev)
      ;;   (funcall (dots ev data-transfer set-data)
      ;;             "Text" (dots ev target id)))

      ;; (defun drop (ev)
      ;;   (prevent-default ev)
      ;;   (let ((data (funcall (dots ev data-transfer get-data) "Text")))
      ;;     (append-child (dots ev target) (doc-get-el-by-id data))))

      ;; (defun allow-drop (ev)
      ;;   (prevent-default ev))
      (defun grey-out (z)
        (let ((mask (div-id "mask"))
              (svg (svg-create-element "svg"))
              (z-in (if z z 5)))
          (set-attributes svg "height" "1000em" 
                          "width" "1000em"
                          "z-index" z-in)
          (setf grey-out-z-index z-in)
          (append-child mask svg)
          (append-child (dots document body) mask)
          z-in))

      (defun add-to-class-list (id-or-thing name)
        (let* ((obj (if (stringp id-or-thing) (dgebid id-or-thing)
                        id-or-thing))
               (oclass (get-attribute obj "class")))
          (if oclass (set-attribute obj "class" (strcat name " " oclass))
              (set-attribute obj "class" name))))

      (defun backwards (direc)
        (case direc
          ("rtl" true)
          ("ltr" false)
          (t (alert (strcat "unknown direction: " direc)))))

      (defun forwards (direc)
        (not (backwards direc)))

      (defun my-alert (str cont)
        (let ((msgh (div-id "my-alert-message-holder"))
              (msg (div-id-class "my-alert-message" "alerts"))
              (bh (div-id "my-alert-button-holder"))
              (butt (make-button "my-alert-button" "OK")))
          (add-event-no-capture butt "click" (make-remove-my-alert cont))
          (append-child bh butt)
          (append-para-text msg str)
          (append-children msgh msg bh)
          (grey-out)
          ;;(set-position msgh "fixed")
          (set-display msgh "block")
          (set-z-index msgh (1+ grey-out-z-index))
          (append-child (dots document body) msgh)
          msgh))

      (defun make-remove-my-alert (cont)
        (lambda (ev)
          (let ((msgh (doc-get-el-by-id "my-alert-message-holder")))
            (remove-child (dots document body) msgh)
            (remove-grey-out))
          (if cont
              (funcall cont)
              false)))

      ;; (defun grey-out ()
      ;;   (let ((mask (div-id "mask"))
      ;;         (svg (svg-create-element "svg")))
      ;;     (set-attributes svg "height" "1000em" "width" "1000em")
      ;;     (append-child mask svg)
      ;;     (append-child (dots document body) mask)))

      (defun make-lesson-review-item (description genex)
        (ps:destructuring-bind (modname short long)
            description
          (if (not (or (string= modname "false")  
                       (string= modname "undefined")))
              (let* ((item (div-class "review-menu-item"))
                     (menuspan (create-element "span"))
                     (tn (make-text-node short))
                     (popupspan (create-element "span"))
                     (ptn (make-text-node long)))
                (set-attribute menuspan "class" "hover-class")
                (append-child menuspan tn)
                (set-width popupspan "20em")
                (append-child popupspan ptn)
                (append-child menuspan popupspan)
                (append-child item menuspan)
                (add-event-no-capture item "click"
                                      (if genex
                                          (make-exercise-review-item-handler 
                                           modname)
                                          (make-lesson-review-item-handler 
                                           modname)))
                item)
              false)))

      (defun load-non-ephemeral-first (then)
        (with-slots (initialized) lesson-module
          (labels ((callback (response)
                     (let ((code (parse-json response)))
                       ;;(eval code) HERE
                       (load-script code "lmload")
                       (setf initialized true)
                       (load-ephemeral then))))
            (ajax_load_non_ephemeral callback))))
      
      (defun load-ephemeral (imod)
        (with-slots (destroy-svg-grid) scratchpad
          (with-slots (non-ephemeral initialized) lesson-module
            (with-slots (current-lesson
                         current-lesson-title
                         current-lesson-text init-var 
                         current-module-name) non-ephemeral
              (labels ((callback (response)
                         (let* ((varfns-stack (parse-json response))
                                (globalvars (aref varfns-stack 0))
                                (globalfns (aref varfns-stack 1))
                                (lesson-stack (aref varfns-stack 2))
                                (title (aref varfns-stack 4))
                                (initfns (aref varfns-stack 3))) ;;initfns is the main script text
                           (setf current-lesson-title title)
                           (when globalvars (when (aref globalvars 0)
                                              (dolist (pair globalvars)
                                                (let* ((varstring (aref pair 0))
                                                       (val (aref pair 1))(compiled (eval val))
                                                       (isfun (string= (typeof compiled);;HERE 
                                                                       "function"))
                                      (fn (if isfun compiled val)))
                                 (funcall init-var varstring fn)))))
                         (when globalfns (when (aref globalfns 0)
                                           (dolist (fn globalfns)
                                             (eval fn))));;HERE
                         (when initfns (eval initfns));;HERE
                         (if lesson-stack  ;;instead of array , could
                                           ;;build js object with keys
                                           ;;= numbers and values =
                                           ;;functions, so can still
                                           ;;call them by index.  Then
                                           ;;object can be loaded,
                                           ;;without evalling ?
                             (if (aref lesson-stack 0)
                                 (let ((spad (doc-get-el-by-id "scratch"))
                                       (compiled (ps:array))
                                       (ln (length lesson-stack)))
                                   (grey-out)
                                   (when spad (funcall destroy-svg-grid))
                                   (loop for code in lesson-stack for n from 0
                                      do (setf (aref compiled n) (eval code))) ;;HERE
                                   (setf current-lesson compiled)
                                   (setf current-lesson-text 0)
                                   (funcall 
                                    (aref current-lesson current-lesson-text))
                                   (when (dots lesson-module ephemeral initialize-ephemeral)
                                     (funcall (dots lesson-module ephemeral initialize-ephemeral)))
                                   )
                                 (progn
                                   (setf current-module-name nil)
                                   (setf can-quit-lesson false)
                                   (alert (ps:lisp (format nil "No lesson for this module.~%Please continue with exercises.")))))
                             (progn
                               (setf current-module-name nil)
                               (setf can-quit-lesson false)
                               (alert (ps:lisp (format nil
                                                       "No lesson for this module.~%Please continue with exercises."))))))))
              (setf current-module-name imod)
              (ajax_get_current_lesson imod callback))))))
      
      (defun logout ()
        (labels ((callback (response)
                   (reload-page)))
          (ajax_user_clicked_logout owner callback)))
      
      (defun remove-grey-out ()
        (let ((mask (doc-get-el-by-id "mask")))
          (when mask (remove-child (dots document body) mask))))

      (defun mj-bracket (str)
        (strcat "$" str "$"))

      (defun is-open (id)
        (menu-open-p id))

      (defun tag-name (thing)
        (dots thing tag-name))
      
      (defun shuffle (arr)
        (labels ((decorate (arr)
                   (let ((tmp (array)))
                     (dolist (a arr)
                       (push (list a (random)) tmp))
                     tmp))
                 (undecorate (arr)
                   (let ((tmp (array)))
                     (dolist (a arr)
                       (push (aref a 0) tmp))
                     tmp)))
          (let* ((a1 (decorate arr))
                 (a2 (sort a1 (lambda (a b)
                                (- (aref a 1) (aref b 1)))))
                 (a3 (undecorate a2)))
            a3)))
      
      (defun append-para-text (parent text typeset)
        (let* ((p (create-element "p"))
               (tn (make-text-node text)))
          (append-child p tn) 
          (append-child parent p)
          (when typeset (mj-hub-queue (array "Typeset" mj-hub p)) tn)))

      ;; (defun append-text (parent txt)
      ;;   (let ((tn (make-text-node txt)))
      ;;     (append-child parent tn)))

      ;;(when typeset (mj-hub-queue (array "Typeset" mj-hub parent)) tn)))
      ;;NB added typeset 05/03/2013  (and removed)
                  
      (defun is-vowel (letter)
        (case letter
          ("a" true)
          ("e" true)
          ("i" true)
          ("o" true)
          ("u" true)
          (t false)))
      
      (defun quantify-noun (n word indefinite)
        (let* ((fl (aref word 0))
               (lidx (1- (length word)))
               (ll (aref word lidx))
               (v (is-vowel fl))
               (s (eql ll "s")))
          (if (not s)
              (throw (strcat "Not in plural form: " word))
              (cond ((and (= (parse-int n) 1) (not v))
                     (strcat "a " (subseq word 0 lidx)))
                    ((and (= (parse-int n) 1) v)
                     (strcat "an " (subseq word 0 lidx)))
                    (t (if indefinite word
                           (strcat n " " word)))))))

      ;; (defun make-button (id text)
      ;;   (let ((but (create-element "button")))
      ;;     (setf (dots but text-content) text)
      ;;     (set-attribute but "id" id)
      ;;     but))

       ;;;; FIXME should probably be macros
	
      (defun create-checkbox (id)
	  (let ((el (create-element "input")))
	    (set-attribute el "type" "checkbox")
	    (set-attribute el "id" id)
	    el));;setting text-content first clobbers children!!! wtf?
      
      ;; (defun create-radiobutton (id) ;;commented thursday 14 march
      ;;   (let ((el (create-element "input")))
      ;;       (set-attribute el "type" "radio")
      ;;       (set-attribute el "id" id)
      ;;       el))

      (defun create-text-input (id)
        (let ((el (create-element "input")))
          (set-attribute el "type" "text")
          (set-attribute el "id" id)
          el))

      (defun every (fn list)
        (let ((retval true))
          (dolist (item list)
            (unless (funcall fn item)
              (progn (setf retval false)
                     (return retval))))
          retval))

      ;; (defun every (fn list)
      ;;   (let ((stop false))
      ;;     (loop for item in list unless stop
      ;;        do (unless (funcall fn item)
      ;;             (setf stop true)))
      ;;     (return (not stop))))

      (defun mapfor (fn list)
        (let ((retval (ps:array)))
          (dolist (item list)
            (let ((res (funcall fn item)))
              (push res retval)))
          retval))
      
      ;;(defun is-true (thing)
      ;;  (if thing true false))
           
      ;;;;;

      (defvar banner-height 40)

      (defvar current-table 0)
      
      (defvar all-tables)
      
      (defvar xrecords (ps:array))
      
      (defvar correct (ps:lisp (correct)))
      
      (defvar incorrect (ps:lisp (incorrect)))

      (defvar default-text-color "#327232")

      (defvar grey-out-z-index)

      (defun display-xrecord ()
        (when (defined (aref xrecords current-table))
          (let* ((panel (doc-get-el-by-id "right"))
                 (current (doc-get-el-by-id "xrecord"))
                 (xrecel (div-id-class "xrecord" "pupil-info"))
                 (tot (make-xrecord-display "total" "xrec-info" "Total: " total))
                 (correct (make-xrecord-display 
                           "correct" "xrec-info" "Correct: " right))
                 (incorrect (make-xrecord-display
                             "incorrect" "xrec-info" "Incorrect: " wrong)))
            (append-child xrecel tot)
            (append-child xrecel correct)
            (append-child xrecel incorrect)
            (if current (replace-child panel xrecel current)
                (append-child panel xrecel)))))

      ;;;; ADDED
      (defvar content-object 
        (ps:create init-script (lambda () false)))

      (defun make-special-content-item (desc)
        (let ((game-name (aref desc 0))
              (modname (aref desc 1))
              (data (aref desc 2))
              (item (div-class "sc-menu-item")))
          (append-text item game-name)
          (add-event-no-capture item "click" (make-special-content-item-handler
                                              modname game-name data))
          item))
      
      (defun make-special-content-item-handler (modname name data)
        (lambda (ev)
          (cancel-bubble ev)
          (close-special-content-menu)
          (load-special-content-script modname name data)))

      (defun close-special-content-menu ()
        (let ((menu (doc-get-el-by-id "replay-menu")))
          (prune-tree-from-nth menu 1)
          (add-event-no-capture menu "click" open-special-content-menu)))

      
      ;;END ADDED SPECIAL CONTENT CONTENT-OBJECT SHARED
      
      (defun cancel-bubble (ev)
	(let ((event (if ev ev (dots window event))))
	  (if (dots event stop-propagation)
	      (funcall (dots event stop-propagation))
	      (if (not (null (dots event cancel-bubble)))
		  (setf (dots event cancel-bubble) t)))))

      (defun page-x-y (ev)
	(let ((e (if ev ev (dots window event))))
	  (if (or (dots e page-x) (dots e page-y))
	      (ps:array (dots e page-x) (dots e page-y))
	      (ps:array (dots e client-x) (dots e client-y)))))
      
      (defun screen-x-y (ev)
	(let ((e (if ev ev (dots window event))))
	  (ps:array (dots e screen-x) (dots e screen-y))))
;;;; x-coord and y-coord are macros in dom-utils

      ;; (defun absolute-position (obj currleft currtop)
      ;;   (when (not-defined currleft)
      ;;     (setf currleft 0 currtop 0))
      ;;   (if (not-defined obj) (list currleft currtop)
      ;;       (absolute-position (offset-parent obj)
      ;;                          (+ currleft (offset-left obj))
      ;;                          (+ currtop (offset-top obj)))))
        
          ;; (do* ((obj obj (offset-parent obj))
          ;;      (currleft (offset-left obj) (+ currleft (offset-left obj)))
          ;;      (currtop (offset-top obj)  (+ currtop (offset-top obj))))
          ;;     ((not-defined obj) (list currleft currtop))))
               
      (defun place-thing (thing coords)
	(set-attribute thing "style.position" "absolute")
	(set-attribute thing "style.left" (strcat (x-coord coords) "px"))
	(set-attribute thing "style.top" (strcat (y-coord coords) "px")))

      (defun place-thing-atclick (ev thing)
	(let ((coords (screen-x-y ev)))
	  (place-thing thing coords)))

      ;;FIXME macro?
      ;; (defun get-work-on-screen-bool ()
      ;;   (if (string= work-on-screen "true")
      ;;       true
      ;;       false))

      ;; (defun set-work-on-screen-bool (bool)
      ;;   (if bool (setf work-on-screen "true") (setf work-on-screen "false")))

      (defun handle-work-on-screen ()
        (if (and work-on-screen ;;do nothing
                 (checked this))
            nil
            (progn (set-work-on-screen true)
                   (setf (dots (doc-get-el-by-id "work-on-screen") checked) true)
                   (save-and-replace-off-screen-answer-handlers))))
      
      (defun handle-work-off-screen ()
        (if (and ;;(equal (get-work-on-screen-bool work-on-screen) false)
             (not work-on-screen)
             (checked this))
            nil
            (progn (set-work-on-screen false)
                   (setf (dots (doc-get-el-by-id "work-off-screen") checked) true)
                   (restore-off-screen-answer-handlers))))
      
      (defun alert-input-error (content)
        (alert (strcat "Input error: " 
                       content
                       (ps:lisp 
                        (format nil "~%is undefined in this context.~%"))
                       (ps:lisp
                        (format nil "~%Please change your answer.")))))
            
      (defun set-caption (wonsc)
	;;(hide-caption)
	(let* ((parent (hide-thing-by-id "caption"))
               ;;(doc-get-el-by-id "caption"))
	       (old (nth-child-node parent 0))
	       (gnu (make-text-node 		     
                     (get-attribute (doc-get-el-by-id 
                                     (table-id current-table)) "data-caption"))))
	  (if old
	      (replace-child parent gnu old)
	      (append-child parent gnu)))
        (if wonsc
            (let ((wonsc-bool work-on-screen)
                  (selwa (doc-get-el-by-id "select-work-area-form"))
                  (wonsc (doc-get-el-by-id "work-on-screen"))
                  (woffsc (doc-get-el-by-id "work-off-screen")))
              (append-child parent selwa)
              (setf (dots wonsc checked) wonsc-bool)
              (setf (dots woffsc checked) (not wonsc-bool))
              (add-event-no-capture wonsc "click" handle-work-on-screen)
              (add-event-no-capture woffsc "click" handle-work-off-screen)
              (setf (dots selwa style display) "block")
              (when wonsc-bool 
                (save-and-replace-off-screen-answer-handlers))))
        (show-caption))
      
      (defun hide-table ()
	(let ((the-table (doc-get-el-by-id (table-id current-table))))
	  (setf (dots the-table style display) "none")))
      
      (defun reveal-table (index)
	(loop for tidx from 0 below all-tables 
	   for this-table = (doc-get-el-by-id (table-id tidx))
	   do
	     (if (not (= tidx current-table))
		 (setf (dots this-table style display) "none")
		 (setf (dots this-table style display) "table"))))
      
      (defun reveal-next-table ()
	(setf current-table (next-table-index all-tables))
	(reveal-table current-table)
	(set-caption)
	(display-xrecord))
      
      (defun page-next-table ()
	(hide-table)
	(reveal-next-table))
      
      (defun make-text-for-display (id glass string)
	(let* ((el (div-id-class id glass))
	       (tn (make-text-node string)))
	  (append-child el tn)
	  el))

      (defun remove-children-from-nth (parent nth)
	(let ((times (- (how-many-children parent) nth))
	      (children (children-of parent)))
	  (dotimes (time times parent)
	    (remove-child parent (aref children nth)))))
      
      (defun remove-child-nodes-from-nth (parent nth)
        (let ((times (- (how-many-child-nodes parent) nth))
              (children (child-nodes-of parent)))
          (dotimes (time times parent)
            (remove-child parent (aref children nth)))))

      ;; (defun remove-children-from-nth (parent nth)
      ;;   (let ((times (- (how-many-children parent) nth))
      ;;         (children (children-of parent)))
      ;;     (dotimes (time times parent)
      ;;       (remove-child parent (aref children nth)))))
      
      (defun remove-children (parent)
	(remove-children-from-nth parent 0))

      (defun clear-display ()
	(clear-content (doc-get-el-by-id "content"))
	(setf (dots (doc-get-el-by-id "pager") style display) "none")
	(setf xrecords (ps:array))
	(setf current-table 0)
	(remove-children-from-nth (doc-get-el-by-id "right") 2))
      
      (defun clear-content (content)
	;;we keep the first three and the last element of content, removing
	;;other elements since they are old tables of exercises from the last
	;;pupil who was displayed.
	(hide-caption)
	(let* ((nth 3)
	       (end (1- (how-many-children content)))
	       (times (- end nth)))
	  (dotimes (time times content)
	    (let ((child (aref (children-of content) nth)))
	      (remove-child content child)))))

      (defun name-class (glass name)
	(concatenate 'string glass name))
      
      (defun to-menu (sek sekfn div)
	(let ((menu div))
	  (dolist (s sek menu)
	    (let* ((chdiv (div-class (name-class 
				      (get-attribute div "class") "-item"))))
	      (funcall sekfn s chdiv)
	      (append-child menu chdiv)))))

      (defvar drag-drop
	  (with-slots (start-drag-mouse  
		       key-h-t-m-l start-drag-keys key-speed 
		       initial-mouse-x initial-mouse-y start-x
		       start-y d-x-keys d-y-keys dragged-object
		       init-element start-drag drag-mouse 
		       drag-keys set-position switch-key-events
		       release-element) drag-drop
	    (with-variables-create 
	 	((key-h-t-m-l "<a href='#' class='keyLink'>#</a>") (key-speed 10)
	 	 initial-mouse-x initial-mouse-y start-x start-y d-x-keys d-y-keys
	 	 dragged-object)
	      init-element 
	      (lambda (element)
	 	(with-slots (onmousedown inner-h-t-m-l) element
	 	  (with-slots (onclick related-element) last-link
	 	    (when (stringp element)
	 	      (setf element (doc-get-el-by-id element)))
	 	    (setf onmousedown start-drag-mouse
	 		  inner-h-t-m-l (strcat inner-h-t-m-l key-h-t-m-l))
	 	    (let* ((links (get-elements-by-tag-name element "a"))
	 		   (last-link (aref links (1- (length links)))))
	 	      (setf related-element element
	 		    onclick start-drag-keys)
	 	      element))))
	      start-drag-mouse
	      (lambda (e)
		(with-slots (client-x client-y) evt
		  (funcall start-drag this)
		  (let* ((evt (if e e (dots window event))))
		    (setf initial-mouse-x client-x
			  initial-mouse-y client-y)
		    (add-event-no-capture document "mousemove" drag-mouse)
		    (add-event-no-capture document "mouseup" release-element))
		  false))
	      start-drag-keys
	      (lambda ()
		(with-slots (related-element) this
		  (funcall start-drag related-element)
		  (setf d-x-keys 0 d-y-keys 0)
		  (add-event-no-capture document "keydown" drag-keys)
		  (add-event-no-capture document "keypress" switch-key-events)
		  false))
	      start-drag
	      (lambda (obj)
		(with-slots (offset-left offset-top class-name) obj
		  (when dragged-object (funcall release-element))
                  (let ((ignore (get-attribute obj "data-ignore-banner")))
                    (setf start-x offset-left
                          start-y 
                          (if ignore offset-top
                              (- offset-top banner-height))
                          dragged-object obj))))
              ;; (lambda (obj)
	      ;;   (with-slots (offset-left offset-top class-name) obj
	      ;;     (when dragged-object (funcall release-element))
	      ;;     (setf start-x offset-left
	      ;;   	start-y (- offset-top banner-height)
	      ;;   	dragged-object obj)))
              ;;class-name (strcat class-name " dragged"))))
	      drag-mouse
	      (lambda (e)
		(with-slots (client-x client-y) evt
		  (let* ((evt (if e e (dots window event)))
			 (d-x (- client-x initial-mouse-x))
			 (d-y (- client-y initial-mouse-y)))
		    (funcall set-position d-x d-y)
		    false)))
	      drag-keys
	      (lambda (e)
		(with-slots (key-code prevent-default) evt
		  (let* ((evt (if e e (dots window event)))
			 (key key-code))
		    (ps:switch key
		      (37)
		      (63234 (setf d-x-keys (- d-x-keys key-speed))
			     break)
		      (38)
		      (63232 (setf d-y-keys (- d-y-keys key-speed))
			     break)
		      (39)
		      (63235 (setf d-x-keys (+ d-x-keys key-speed))
			     break)
		      (40)
		      (63233 (setf d-y-keys (+ d-y-keys key-speed))
			     break)
		      (13)
		      (27 (funcall release-element)
			  (funcall return false))
		      (ps:default (funcall return true)))
		    (funcall set-position d-x-keys d-y-keys)
		    (when prevent-default
		      (funcall prevent-default))))
		false)
	      set-position
	      (lambda (dx dy)
		(setf (style-left dragged-object) (strcat start-x dx "px"))
		(setf (style-top dragged-object) (strcat start-y dy "px"))
		false)
	      switch-key-events
	      (lambda ()
		(remove-event-no-capture document "keydown" drag-keys)
		(remove-event-no-capture document "keypress" switch-key-events)
		(add-event-no-capture document "keypress" drag-keys)
		false)
	      release-element
	      (lambda ()
		(remove-event-no-capture document "mousemove" drag-mouse)
		(remove-event-no-capture document "mouseup" release-element)
		(remove-event-no-capture document "keypress" drag-keys)
		(remove-event-no-capture document "keypress" switch-key-events)
		(remove-event-no-capture document "keydown" drag-keys)
		;; (setf (dots dragged-object class-name)
		;;       (replace (dots dragged-object class-name)
		;; 	       "/dragged/" ""))
		(setf dragged-object null)))))
            
      ))))

(defun setup-mathjax ()
  (who:with-html-output-to-string (*standard-output* nil :indent t)
    ;; (:script :type "text/javascript"
    ;;          (who:fmt (ps:ps 
    ;;     		(ps:chain 
    ;;     		 ((ps:@ -math-jax -hub -config) 
    ;;     		  (ps:create te-x
    ;;                                  (ps:create extensions
    ;;                                             (ps:array "AMSmath.js"
    ;;                                                       "AMSsymbols.js"))))))))
    (:script :type "text/javascript"
	     :src "/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
      (:script :type "text/javascript"
	     (who:fmt (ps:ps 
			(ps:chain 
			 ((ps:@ -math-jax -hub -config) 
			  (ps:create tex2jax 
				     (ps:create  
				      inline-math 
				      (ps:array 
				       (ps:array "$" "$") 
				       (ps:array "\\(" "\\("))
				      preview
				      (ps:array
				       "loading"))
                                     te-x
                                     (ps:create extensions
                                                (ps:array "AMSmath.js"
                                                          "AMSsymbols.js"))
                                     ))))))))

(defun banner ()
  (ps:who-ps-html  (:script  :type  "text/javascript"
    (ps:ps
      (defvar popup 
        (ps:create 
         open (lambda ()
                (when (null (ps:@ this element ))
                  (progn 
                    (setf (ps:@ this element) ((ps:@ document create-element) "div"))
                    (setf (ps:@ this element id) "mtapopup")
                    (setf (ps:@ this element class-name) "bannertext")
                    (setf  (ps:@ this element inner-h-t-m-l)
                           "&Theta;Mat is The Teaching Assistant for Mathematics.")))
                (ps:chain
                 ((ps:@ ((ps:@ document get-element-by-id) "stdh") style set-property) 
                  "display" "none" ""))
                (ps:chain
                 ((ps:@ document body append-child)
                  (ps:@ this element)))
                (setf click-off-popup (ps:chain ((ps:@ document get-element-by-id) "mtapopup")))
                (setf (ps:@ click-off-popup onclick) (ps:@ popup close)))
         close (lambda () 
                 (ps:chain
                  ((ps:@ ((ps:@ document get-element-by-id) "stdh") style set-property) 
                   "display" "" ""))
                 (ps:chain ((ps:@ document body remove-child) this)))))
      (setf click-on-stdh 
	    (ps:chain ((ps:@ document get-element-by-id) "stdh")))
      (setf (ps:@ click-on-stdh onclick) (ps:@ popup open))
      (defvar click-off-popup)))))

(defun-shared-ajax load-non-ephemeral ()
  ((*teacher-script-processor* *pupil-script-processor*) json-response)
  (lesson-module-ne))

(defun-shared-ajax get-current-lesson (module)
    ((*pupil-script-processor* *teacher-script-processor*) json-response)
  (let* ((key (intern  module))
         (ret (gethash key *lesson-db*)))
    (if ret (cdr ret) ())))

(defun-shared-ajax get-new-qna-values (varname module currval)
    ((*pupil-script-processor* *teacher-script-processor*) json-response)
  (let* ((mkey (intern module))
         (vkey (intern varname))
         (lesson (gethash mkey *lesson-db*))
         (fn (when lesson (gethash vkey (car lesson)))))
    (when fn (different-var-value varname fn currval))))

(defun-shared-ajax load-scratchpad ()
    ((*pupil-script-processor* *teacher-script-processor*) json-response)
  *scratchpad-script*)

(defun-shared-ajax get-voluntary-special-content (modname)
    ((*pupil-script-processor* *teacher-script-processor*) json-response)
  (let* ((mod (intern modname))
         (cont (get mod 'special-content)))
    (unless cont (error "No special content"))
    cont))

(defun-shared-ajax user-clicked-logout (user)
    ((*pupil-script-processor* 
      *teacher-script-processor*
      ) json-response)
  (with-session-name user
    ;;(error 'user-logged-out :name user)))
    ;;(tbnl:redirect "/login" :host *host* :port *port*)))
    (user-logout user)))
