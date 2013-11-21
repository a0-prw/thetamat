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

(defun pupil-script () ;;name
  (ps:who-ps-html 
   (:script 
    :type "text/javascript"
    (ps:ps 

      ;;(ps:lisp (format nil "use strict"))
      
      (defvar owner "") ;;(ps:lisp name))
      (defvar work-on-screen false) ;;(ps:lisp (pupil-work-on-screen name)))
      
      ;;(define-display-xrecord)
      
      ;;FIXME remove
      (defun cheat (x)
        (with-slots (ephemeral non-ephemeral) lesson-module
          (with-slots (last-lesson-text current-lesson-text next-lesson-text)
              non-ephemeral
            (setf last-lesson-text x
                  current-lesson-text (1+ x))
            (funcall next-lesson-text))))
     
      (defun mktxt (text)
        (let* ((p (create-element "p"))
               (tn (make-text-node text)))
          (append-child p tn) 
          p))

      (defun insert-answers ()
        (labels ((callback (response)
                   (let* ((arr (parse-json response))
                          (exs (length arr)))
                     (dotimes (x exs)
                       (let* ((currtab (table-id x))
                              (exa (aref arr x))
                              (ins (gebtag (dgebid currtab) "input"))
                              (inn (length ins)))
                         (dotimes (n inn)
                           (setf (value (aref ins n))
                                 (aref exa n))))))))
          (ajax_cheat_current owner callback)))                           
      
      (defun show-key3 (e)
        (let ((which (dots e which))
              (key-code (dots e key-code)))
          (alert (strcat (ps:lisp (format nil "e.which:~%")) which
                         (ps:lisp (format nil "~%e.keyCode:~%")) key-code))))
      
      
      (defun add-test-key-event (type)
        (add-event-no-capture document type show-key3))
      
      (defun remove-test-key-event (type)
        (remove-event-no-capture document type show-key3))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (defun set-work-on-screen (bool)
        (setf work-on-screen bool)
        (labels ((callback (response)
                   ;;FIXME we do nothing with the response
                   ))
          (ajax_pupil_set_work_on_screen owner bool callback)))

      (defun save-off-screen-answer-handlers ()
        (loop for tid from 0 below all-tables
           do (loop for input in 
                   (get-elements-by-tag-name
                    (doc-get-el-by-id (table-id tid)) "input")
                 for id = (id-of input)
                 do (setf (ps:getprop off-screen-answer-handlers id)
                          (get-attribute input "onkeydown")))))
      
      (defun replace-off-screen-answer-handlers (just-block)
        (loop for tid from 0 below all-tables
           do (loop for input in 
                   (get-elements-by-tag-name
                    (doc-get-el-by-id (table-id tid)) "input")
                 for id = (id-of input)
                 do (progn (set-attribute input "onkeydown" nil)
                           (set-attribute 
                            input "onclick" (if just-block nil
                                                (strcat "Javascript: "
                                                        'summon-scratch-pad
                                                        "(event, '" id "')")))))))
      
      (defun save-and-block-default-answer-handlers ()
        (cond ((not work-on-screen)
               (save-off-screen-answer-handlers)
               (replace-off-screen-answer-handlers true))
              (t (replace-off-screen-answer-handlers true))))
      
      (defun check-sum (aid value)
        (with-slots (communicate problem-stack) scratchpad
          (with-slots (empty ps-next) problem-stack
        (labels ((callback (response)
                   (let* ((taxfree (parse-json response))
                          (newmath (aref taxfree 0)))
                     (cond ((and (numberp newmath)
                                 (= newmath 1))
                            (alert-input-error (aref taxfree 1)))
                           (t
                            (let* ((qid (id-to aid "Q"))
                                   (oldmath (aref (mj-get-all-jax qid) 0))
                                   (tempdiv (plain-div))
                                   (mark (progn (set-inner-html tempdiv (aref taxfree 1))
                                                (first-child tempdiv)))
                                   (answer-cell (doc-get-el-by-id aid))
                                   (spmsg (doc-get-el-by-id "messages"))
                                   (input-field (nth-of-children answer-cell 0)))
                              (mj-hub-queue (array "Text" oldmath newmath))
                              (replace-child answer-cell mark input-field)
                              (cond ((equal (get-attribute mark "src") correct)
                                     (incf (dots (aref xrecords current-table) right))
                                     (when spmsg
                                       (funcall communicate "Correct!")))
                                    (t 
                                     (incf (dots (aref xrecords current-table) wrong))
                                     (when spmsg
                                       (funcall communicate "No, that was wrong."))))
                              (display-xrecord)
                              (maybe-update-module))))
                     )))
          (if (string= value "") nil
              (ajax_check_sum owner aid value callback))))))

      (defvar completed-tables (ps:array))

      (defun maybe-update-module ()
        (let ((ex-done (exercise-complete current-table)))
          (if ex-done
              (when (module-complete)
                (update-module)))))
      
      (defun completed (thing)
        (if thing true false))

      (defun module-complete ()
        (every completed completed-tables))

      (defun exercise-complete (idx)
        (let ((obj (aref xrecords idx)))
          (with-slots (right wrong total) obj
            (let ((completed (+ wrong right)))
              (if (= completed total)
                  (let ((ret (ps:array idx right total)))
                    (setf (aref completed-tables idx) ret)
                    ret)
                  false)))))

      (defun update-module ()
        (close-lesson-review-menu)
        (labels ((callback (response)
                   (let ((msg (parse-json response)))
                     (alert msg))
                   (get-all-tables-and-start)))
          (ajax_update_module owner (stringify-json completed-tables) callback)))

      (defun build-xrecords (tables)
        (loop for tidx from 0 below (how-many-children tables)
           for table = (aref (child-nodes-of tables) tidx)
           for status = (ps:create index tidx right 0 total 0)
           for xrec = (ps:create total 0 wrong 0 right 0)
           do (loop for rowidx from 0 below (how-many-children table)
                 do (let ((row  (aref (child-nodes-of table) rowidx)))
                      (loop for cellidx from 0 below (how-many-children row)
                         do (let* ((cell (aref (child-nodes-of row) cellidx))
                                   (react (answer-cell-p (id-of cell))))
                              (when react 
                                (progn (incf (dots xrec total))
                                       (incf (dots status total)))
                                (let ((mark (get-attribute (nth-of-children cell 0) "src")))
                                  (when mark
                                    (if (marked-right mark)
                                        (progn (incf (dots xrec right))
                                               (incf (dots status right)))
                                        (incf (dots xrec wrong)))))
                                )))))
             (with-slots (right wrong total) xrec
               (setf (aref completed-tables tidx) 
                     (if (= (+ right wrong) total) 
                         (ps:array tidx right total)
                         false)))
             (setf (aref xrecords tidx) xrec)))

      (defun set-lesson (imod)
        (with-slots (initialized) lesson-module
          (if imod
              (if (not initialized)
                  (load-non-ephemeral-first imod)
                  (load-ephemeral imod))
              (let* ((table0 (doc-get-el-by-id "table_0"))
                     (lesson-mode (get-attribute table0 "data-lessons"))
                     (module (get-attribute table0 "data-module"))
                     (placement (get-attribute table0 "data-placement"))
                     (cycle (get-attribute table0 "data-cycle")))
                (when (not (equal placement "true"))
                  (when (equal lesson-mode "must")
                    (when (equal cycle 1)
                      (if (not initialized)
                          (load-non-ephemeral-first module)
                          (load-ephemeral module)))))))))

      (defvar special-lesson-module)

      (defun process-tables (tab-content content skip-update)
        (let ((tables  (plain-div))
              (pg (doc-get-el-by-id "pager")))
          (set-inner-html tables tab-content)
          (clear-content content)
          (build-xrecords tables)
          (setf all-tables (how-many-children tables))
          (if (> all-tables 1)
              (set-display pg "block")
              (set-display pg "none"))
          (loop for tidx from 0 below all-tables 
             do (let ((tid (table-id tidx)))
                  (insert-before content
                                 (aref (children-of tables) tid)
                                 pg))
               (when (not (= tidx current-table))
                 (setf (dots (doc-get-el-by-id tid) style display) "none")))
          (mj-hub-queue (array "Typeset" mj-hub "content"))
          (mj-hub-queue display-xrecord)
          (mj-hub-queue (array set-caption true))
          (when (not skip-update)
            (mj-hub-queue maybe-update-module))
          (mj-hub-queue set-lesson)))

      (defun process-special-lesson (vararr tab-content)
        (clear-display)
        (with-slots (init-script) content-object
          (with-slots (initialized) lesson-module
            (let ((lesson-mode (aref vararr 0)) 
                  (module (aref vararr 1)) 
                  (placement (aref vararr 2)) 
                  (cycle (aref vararr 3)))
              (if (and  (not (equal placement "true")) 
                        (equal lesson-mode "must") 
                        (equal cycle 1))
                  (progn (setf special-lesson-module module)
                         (if (not initialized)
                             (load-non-ephemeral-first module)
                             (load-ephemeral module)))
                  (let* ((pager (doc-get-el-by-id "pager"))
                         (repmenu (doc-get-el-by-id "replay-menu"))
                         (right (doc-get-el-by-id "right"))
                         (type (aref tab-content 0))
                         (content (aref tab-content 1)))
                    (if (and type content)
                        (cond ((string= type "SCRIPT")
                               (eval content)
                               (funcall init-script))
                              (t (alert (strcat "Unknown type: " 
                                                type ": " content))))
                        (alert (strcat "Not implemented: " tab-content)))
                    
                    (dolist (el (ps:array pager repmenu right))
                      (set-display el "none"))
                    ))))))

      (defvar all-modules
        (ps:create modules (ps:array)))

      (defun insert-exercise-menu ()
        (unless (dgebid "modall")
          (let ((men (div-id-class "modall" "pmenu"))
                (h1 (create-element "h1"))
                (right (dgebid "right")))
            (set-display right "none")
            (append-text h1 "Exercise Menu")
            (append-child men h1)
            (append-child (dots document body) men)
            (add-event-no-capture men "click" open-exercise-menu))))
      
      (defun process-completed-all (mods)
        (with-all-modules
            (setf modules mods)
          (insert-exercise-menu)))

      (defun open-exercise-menu (ev)
        (let ((menu this))
          (labels ((callback (response)
                     (let* ((arr (parse-json response))
                            (curr (aref arr 0))
                            (mods (aref arr 1))
                            (html (aref arr 2)))
                       (dolist (desc mods)
                         (let ((item (make-lesson-review-item desc true)))
                           (when item
                             (append-child menu item))))
                       (remove-event-no-capture menu "click" 
                                                open-exercise-menu)
                       ;;experiment
                       (process-tables html (dgebid "content") true)
                       (setf (get-style (dgebid "right") "display") "block")
                       false)))
            (ajax_get_exercise_review_items owner callback))))

      (defun get-all-tables-and-start ()
          (let ((found (dgebid "intro-text-holder")))
            (when found (remove-child (parent-node found) found)))
          (setf current-table 0 completed-tables (ps:array))
          (labels ((callback (response)
                     (setf xrecords (ps:array))
                     (let* ((pr (parse-json response))
                            (mods (aref pr 0))
                            (is-table (aref pr 1)) 
                            (tab-content (aref pr 2)) 
                            (vararr (aref pr 3)) 
                            (content (doc-get-el-by-id "content")))
                       (cond ((string= is-table "true")
                              (process-tables tab-content content))
                             ((string= is-table "completed")
                              (process-completed-all mods))
                             (t (process-special-lesson vararr tab-content))))))
            (ajax_get_all_div_tables owner callback)))
      
      (defun open-lesson-review-menu (ev)
        (let ((menu this))
          (labels ((callback (response)
                     (let ((arr (parse-json response)))
                       (dolist (desc arr)
                         (let ((item (make-lesson-review-item desc)))
                           (when item
                             (append-child menu item))))
                       (remove-event-no-capture menu "click" 
                                                open-lesson-review-menu)
                       false)))
            (ajax_get_lesson_review_items owner callback))))
          
      ;;(defvar can-quit-lesson false)
      
      (defun make-exercise-review-item-handler (modname)
        (lambda (ev)
          (labels ((callback (response)
                     (let* ((arr (parse-json response))
                            (status (aref arr 0)))
                       (when (string= status "ok")
                         (clear-display)
                         (process-tables (aref arr 1) 
                                         (dgebid "content")
                                         true)))))
            (ajax_get_new_tables_for_module owner modname callback))))
      
      (defun make-lesson-review-item-handler (modname)
        (lambda (ev)
          (cancel-bubble ev)
          (let ((rev (doc-get-el-by-id "review-menu")))
            (setf can-quit-lesson true)
            (set-lesson modname))))

      (defun close-lesson-review-menu ()
        (let ((rev (doc-get-el-by-id "review-menu")))
          (prune-tree-from-nth rev 1)
          (add-event-no-capture rev "click" open-lesson-review-menu)))

      (defun insert-lesson-review-menu ()
        (let ((men (div-id-class "review-menu" "pmenu"))
              (h1 (create-element "h1"))
              (tn (make-text-node "Lesson Review")))
          (append-child h1 tn)
          (append-child men h1)
          (append-child (dots document body) men)
          (add-event-no-capture men "click" open-lesson-review-menu)))

      (defun load-special-content-script (modname cname data)
        (with-slots (init-script 
                     start voluntary 
                     voluntary-game game-name) content-object
          (labels ((callback (response)
                     (let* ((rep (parse-json response))
                            (type (aref rep 0))
                            (script (aref rep 1)))
                       (if (and type script (string= type "SCRIPT"))
                           (eval script)
                           (alert (strcat "Unknown special-content: "
                                          (ps:lisp #\Newline)
                                          type)))
                       (with-hidden-elements
                           ((pager "pager") (revmenu "review-menu")
                            (repmenu "replay-menu") (right "right"))
                         (setf game-name cname
                               voluntary true
                               start (lambda ()
                                       (funcall voluntary-game 
                                                (aref data 0)
                                                (aref data 1))))
                         (funcall start)))))
            (ajax_get_voluntary_special_content modname callback))))

      (defun open-special-content-menu (ev)
        (remove-event-no-capture this "click" open-special-content-menu)
        (let ((menu this))
          (labels ((callback (response)
                     (let* ((garr (parse-json response))
                            (exists (aref (aref garr 0) 0))) ;;false when :false
                       (if exists
                           (dolist (desc garr)
                             (append-child menu
                                           (make-special-content-item desc)))
                           (append-text menu "Nothing yet")))))
            (ajax_get_special_content_podata owner callback))))

      (defun insert-special-content-menu ()
        (let ((men (div-id-class "replay-menu" "pmenu"))
              (h1 (create-element "h1")))
          (append-text h1 "Memory Games")
          (append-child men h1)
          (append-child (dots document body) men)
          (add-event-no-capture men "click" open-special-content-menu)))
      
      (defun initialize-page ()
        (let ((ban (doc-get-el-by-id "banner"))
              (lout (dgebid "logout"))
              (callback (lambda (resp)
                          (let* ((dat (parse-json resp))
                                 (un (dots dat owner))
                                 (wos (dots dat pwos))
                                 (pc (dgebid "page-container")))
                            (set-attribute pc "data-owner" un)
                            (set-attribute pc "data-work-on-screen" wos)
                            (setf (dots window document title) un
                                  owner un
                                  work-on-screen (if (string= wos "true")
                                                     true
                                                     false))
                          (get-all-tables-and-start)))))
          (add-event-no-capture lout "click" logout)
          (prepare-input-menu-holder "input-menu-holder" "select-input-menu"
                                     "scratchpad-menu" "Scratchpad Menu")
          (prepare-calced-canvas  "svg-canvas" "svg-container"
                               "message-display" "scratch-pad-info"
                               "problem-display" "scratch-pad-info")
          (setf (style-height ban) (strcat banner-height "px"))
          
          (insert-lesson-review-menu)
          (insert-special-content-menu)
          ;;(get-all-tables-and-start)
          (ajax_get_page_values callback)))
     
      (setf (ps:@ window onload) (initialize-page))))))

;; Special content is for when the exercise is not appropriate for the
;; table form.  See content/define-lesson.lisp.  get-special-content
;; must return a list of TYPE CONTENT.  If TYPE is string= to
;; "SCRIPT", the CONTENT is evaled in javascript and the global
;; variable content-object is set to the result of the eval.  Then
;; contentObject.initScript() is called.

;; This must return a list of 4 items: 0) the modules the pupil has
;; viewed, 1) a string declaring the status of is-table (currently can
;; be "false" "true" or "completed" - mean what they say), 2) html
;; table of exercises if is-table is "true" || special-content:TYPE
;; CONTENT (see above comment), and finally 3) nil (if is-table is
;; "true") or a list of 4 items again o) "must" or "can" if the pupil
;; must or can view the lessons (currently always "can") i) the
;; current-module ii) t or nil (pupil-placement) and finally finally
;; iii) pupil-cycle which you will have to figure out for yourself the
;; meaning of and good luck with that.
(defun-ajax get-all-div-tables (name) 
    (*pupil-script-processor* json-response)
  (with-session-name name
    (let* ((current-module (car (pupil-modules name)))
           (special (special-modulep current-module)))
      (if (null current-module)
          (list (pupil-viewed name) 
                "completed"
                "fubar!"
                nil)
          (list (pupil-viewed name)
                (if special "false" "true")
                (if (not special) 
                    (who::string-list-to-string
                     (who::tree-to-template
                      (make-q&a-divs name)))
                    (get-special-content current-module))
                (if (not special)
                    nil
                    (list (if (pupil-view-lessons name) "must" "can")
                          current-module
                          (pupil-placement name)
                          (pupil-cycle name))))))))

(defun-ajax get-lesson-review-items (pname)
    (*pupil-script-processor* json-response)
  (with-session-name pname 
    (let* ((curr (car (pupil-modules pname)))
           (seen (pupil-viewed pname))
           (mlist (if curr (push curr seen) seen)))
      (mapcar #'list-module-description mlist))))

(defun-ajax get-exercise-review-items (pname)
    (*pupil-script-processor* json-response)
  (with-session-name pname
    ;;pupil is finished the exercises -
    ;;open-exercise-menu-content
    (let ((currmod (or (pupil-last-access pname)
                       (setf (pupil-last-access pname) (last-module))))
          (html (tree-to-html
                 (make-q&a-divs pname))))
      (list currmod
            (mapcar #'list-module-description 
                    +modular-progression+)
            html))))

(defun-ajax get-new-tables-for-module (pname modname)
    (*pupil-script-processor* json-response)
  (with-session-name pname
    (setf (pupil-current-problems pname) nil
          (pupil-last-access pname) (intern modname))
    ;;FIXME check that modname is not evil
    (list "ok" (tree-to-html (make-q&a-divs pname)))))

;; (defun-ajax get-current-problems-for-module (pname modname)
;;     (*pupil-script-processor* json-response)
  

;; (defun-ajax get-exercise-review-items (pname)
;;     (*pupil-script-processor* json-response)
;;   (with-session-name pname
;;     ;;pupil is finished the exercises -
;;     ;;open-exercise-menu-content
;;     (list (or (pupil-last-access pname)
;;               (setf (pupil-last-access pname) (last-module)))
;;           (mapcar #'list-module-description 
;;                   +modular-progression+))))

(defun-ajax get-addsub-scores (pname)
    (*pupil-script-processor* json-response)
  (with-session-name pname
    (list (pupil-basic-add pname)
          (shuffle +all-digit-pairs+)
          (shuffle +sub-digit-pairs+))))

(defun-ajax get-muldiv-scores  (pname)
    (*pupil-script-processor* json-response)
  (with-session-name pname
    (list (pupil-basic-mul pname)
          (shuffle +all-mul-pairs+)
          (shuffle +div-mul-pairs+))))
        ;;(shuffle (sub-digit-pairs))))

;;FIXME regex answer for junk: test according to expected answer type.?
;; before format-answer-for-display call?
(defun-ajax check-sum (name tdid answer) 
    (*pupil-script-processor* json-response) ;;changed
  (with-session-name name
    (let* ((pandr (pupil-current-problems name))
           (lid (parse-table-cell-id tdid))
           (exidx (table-cell-exidx lid))
           (ex (exercise-n exidx pandr))
           (de (get-didactic-element (exercise-de ex)))
           (atype (atype de))
           (trima (string-trim '(#\Space) answer))
           (dispanswer (format-answer-for-display atype trima))
           (maxinput (reply de))  ;;(get-didactic-element de)))
           (probidx (table-cell-probidx lid))
           (prob (problem-n probidx (problems ex)))
           (good (second prob)))
      (handler-case 
          (let ((ga (good-answer atype (remove-whitespace answer) good)))
            (if ga
                (setf prob (append (subseq prob 0 1) `(,dispanswer t t)))
                (setf prob (append (subseq prob 0 2) `(nil ,dispanswer))))
            (setf (nth probidx (caddr (nth exidx (cadr pandr)))) prob)
            (setf (pupil-current-problems name) pandr)
            (display-problem prob exidx probidx maxinput t))
        (input-not-a-number (c)
          (let ((input (input c)))
            (display-problem nil nil nil nil nil input)))))))

(defun-ajax pupil-set-work-on-screen (name bool)
    (*pupil-script-processor* json-response)
  (with-session-name name
    (let ((val (st-json:read-json bool)))
      (setf (pupil-work-on-screen name) (if (eql val :true) 
                                            "true"
                                            "false")))))

(defun-ajax update-module (pupil results)
    (*pupil-script-processor* json-response)
  (with-session-name pupil
    (let ((rlists (st-json:read-json results)))
      (process-update-module pupil rlists))))

(defun-ajax complete-lesson (pupil module)
    (*pupil-script-processor* json-response)
  (with-session-name pupil
    (let* ((label (intern module))
           (already (pupil-viewed pupil)))
      (setf (pupil-cycle pupil) 2)
      (unless (or (null (pupil-modules pupil))
                  (member label already :test #'eql))
        (setf (pupil-viewed pupil) (push label (pupil-viewed pupil)))))))

(defun-ajax refresh-mcq (n)
    (*pupil-script-processor* json-response)
  ;;FIXME with-session-name ??
  (random-digit-mcq (parse-integer n)))

(defun-ajax update-pupil-record (pname game sarr)
    (*pupil-script-processor* json-response)
  (with-session-name pname
    (let ((new (st-json:read-json sarr))
          ;;FIXME xss danger here: this stuff gets displayed
          ;; suggest ...
          ;;(let ((new (when (listp new (mapcar
          ;;#'(lambda (thing) (if (stringp thing)
          ;;(tbnl:escape-for-html thing) thing)) new))))) ...          
          (gamesym (intern game)))
      (pupil-update-memory-game pname new gamesym))))

(defun-ajax get-special-content-podata (pname)
    (*pupil-script-processor* json-response)
  (with-session-name pname
    (let ((seen (pupil-viewed pname))
          (gather ()))
      (dolist (m seen (reverse gather))
        (let ((poacc (get m 'pupil-object-special-content-data)))
          (when poacc
            (destructuring-bind (names access-fn)
                poacc
              (let ((data (funcall access-fn pname)))
                (setf gather
                      (append (mapcar #'(lambda (a)
                                          (list a m data))
                                      names) gather)))))))
      gather)))

(defun-ajax get-page-values ()
    (*pupil-script-processor* json-response)
  (with-dangerous-session-name
    (st-json:jso "owner" dangerous-name
                 "pwos" (pupil-work-on-screen dangerous-name))))

;; FIXME REMOVE TESTING 

(defun-ajax cheat-current (pupil)
    (*pupil-script-processor* json-response)
  (with-session-name pupil
    (get-answers (pupil-current-problems pupil))))
