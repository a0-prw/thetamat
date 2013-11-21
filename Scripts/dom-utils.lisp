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

;; DOM utilities

(ps:defpsmacro matches (regex string)
  `((ps:@ ,regex test) ,string))

(ps:defpsmacro set-timeout (fun delay &rest args)
  `((ps:@ window set-timeout) ,fun ,delay ,args))

(ps:defpsmacro clear-timeout (id)
  `((ps:@ window clear-timeout) , id))

(ps:defpsmacro reload-page ()
  `((ps:@ window location reload) true))

(ps:defpsmacro standard-event (name detail)
  `(ps:new (-custom-event ,name (ps:create "detail" ,detail))))

(ps:defpsmacro dots (&rest args)
  `(ps:chain ,@args))

(ps:defpsmacro filter (array callback &optional thi)
  (if thi
      `((ps:@ ,array filter) ,callback ,thi)
      `((ps:@ ,array filter) ,callback))) ;;untested

(ps:defpsmacro map (array callback &optional thi)
  (if thi
      `((ps:@ ,array map) ,callback ,thi)
      `((ps:@ ,array map) ,callback))) ;;untested

(ps:defpsmacro join (array sep)
  `((ps:@ ,array join) ,sep))

(ps:defpsmacro to-string (number base)
  `((ps:@ ,number to-string) ,base))

(ps:defpsmacro files (finp)
  `(ps:@ ,finp files))

(ps:defpsmacro size (fo)
  `(ps:@ ,fo size))

(ps:defpsmacro class-name (el)
  `(ps:@ ,el class-name))

(ps:defpsmacro remove-class (el str)
  `((ps:@ ,el class-list remove) ,str))

(ps:defpsmacro add-class (el str)
  `((ps:@ ,el class-list add) ,str))

(ps:defpsmacro toggle-class (el str)
  `((ps:@ ,el class-list toggle) ,str))

(defmacro define-with-object-access (name &rest definitions)
  (let* ((defs (group definitions 2))
         (declarations (mapcar #'car defs))
         (slot-access-name (symb 'with- name '-object-access))
         (definer-name (symb 'define- name '-object-access))
         (js-object-name (symb name '-object)))
    `(progn 
       (ps:defpsmacro ,slot-access-name (&body body)
         `(with-slots ,',declarations ,',js-object-name
            ,@body))
       (ps:defpsmacro ,definer-name ()
         `(defvar ,',js-object-name
            (ps:create
             ,@',definitions))))))

(define-with-object-access file
    files (lambda (fo) (ps:@ fo files))
    size (lambda (fo) (ps:@ fo size)))

;;Useage: (define-file-object-access)
;;        (with-file-object-access ...)

(ps:defpsmacro offset-parent (el)
  `(ps:@ ,el offset-parent))

(ps:defpsmacro offset-left (el)
  `(ps:@ ,el offset-left))

(ps:defpsmacro offset-top (el)
  `(ps:@ ,el offset-top))

(ps:defpsmacro clone-node-deep (node)
  `((ps:@ ,node clone-node) true))

(ps:defpsmacro parent-node (thing)
  `(dots ,thing parent-node))

(ps:defpsmacro first-child (el)
  `(dots ,el first-child))

(ps:defpsmacro last-child (el)
  `(dots ,el last-child))

(ps:defpsmacro next-sibling (el)
  `(dots ,el next-sibling))

(ps:defpsmacro doc-create-event (type)
  `((ps:@ document create-event) ,type))

(ps:defpsmacro init-event (evt type bubble cancelable)
  `((ps:@ ,evt init-event) ,type ,bubble ,cancelable))

;; (ps:defpsmacro init-mouse-event (evt type)
;;   `((ps:@ ,evt init-mouse-event) ,type ))

(ps:defpsmacro dispatch-event (el event)
  `((ps:@ ,el dispatch-event) ,event))

(ps:defpsmacro prevent-default (ev)
  `((ps:@ ,ev prevent-default)))

(ps:defpsmacro append-child (parent child)
  `((ps:@ ,parent append-child) ,child))

(ps:defpsmacro append-children (parent &rest children)
  `(progn ,@(mapcar #'(lambda (c) `(append-child ,parent ,c)) children)))

(ps:defpsmacro previous-sibling (el)
  `(dots ,el previous-sibling))

(ps:defpsmacro plain-div ()
    `(let ((el ((ps:@ document create-element) "div")))
      el))

(ps:defpsmacro create-element (type)
  `((ps:@ document create-element) ,type))

(ps:defpsmacro div-id (id)
  `(let ((el ((ps:@ document create-element) "div")))
    ((ps:@ el set-attribute) "id" ,id)
    el))

(ps:defpsmacro div-class (glass)
  `(let ((el (plain-div)))
     ((ps:@ el set-attribute) "class" ,glass)
     el))

(ps:defpsmacro div-id-class (id glass)
  `(let ((el ((ps:@ document create-element) "div")))
     ((ps:@ el set-attribute) "id" ,id)
     ((ps:@ el set-attribute) "class" ,glass)
     el))

(ps:defpsmacro make-text-node (string)
  `(let ((tn ((ps:@ document create-text-node) ,string)))
     tn))

(ps:defpsmacro create-span-text (txt)
  `(let ((sp (create-element "span")))
     (setf (inner-html sp) ,txt)
     sp))

(ps:defpsmacro append-text (parent txt)
  `(let ((tn (make-text-node ,txt)))
     (append-child ,parent tn)))

(ps:defpsmacro create-para-text (txt)
  `(let ((para (create-element "p")))
     (append-text para ,txt)
         ;;(tn (make-text-node ,txt)))
     para))

(ps:defpsmacro doc-get-el-by-id (id)
  `((ps:@ document get-element-by-id) ,id))

(ps:defpsmacro dgebid (id)
  `((ps:@ document get-element-by-id) ,id))

(ps:defpsmacro get-elements-by-tag-name (parent tag)
  `((ps:@ ,parent get-elements-by-tag-name) ,tag))

(ps:defpsmacro gebtag (parent tag)
  `((ps:@ ,parent get-elements-by-tag-name) ,tag))

(ps:defpsmacro nth-child-node (parent n)
  `(ps:@ ,parent child-nodes ,n))

(ps:defpsmacro nth-of-children (parent n)
  `(aref (ps:@ ,parent children) ,n))

(ps:defpsmacro replace-child (parent new old)
  `((ps:@ ,parent replace-child) ,new ,old))

(ps:defpsmacro remove-attribute (el att)
  `((ps:@ ,el remove-attribute) ,att))

(ps:defpsmacro set-attribute (el att val)
  `((ps:@ ,el set-attribute) ,att ,val))

(ps:defpsmacro set-attributes (el &rest pairs)
  `(progn
     ,@(mapcar  (lambda (p) `(set-attribute ,el ,@p))
                (group pairs 2))))

(ps:defpsmacro make-text-input-element (max sz &key password)
  `(let ((inp (create-element "input")))
     (set-attributes inp 
                     "maxlength" ,max
                     "size" ,sz
                     "type" ,(if password "password" "text"))
     inp))

(ps:defpsmacro make-number-roller (id min max step value chars &optional handler)
  `(let ((inp (create-element "input")))
     (set-attributes inp 
                     "id" ,id
                     "min" ,min
                     "max" ,max
                     "step" ,step
                     "value" ,value
                     "size" ,chars
                     "type" "number")
     ,@(when handler (list `(add-event-no-capture inp ,(car handler)
                                                  ,(cadr handler))))
     inp))

(ps:defpsmacro make-integer-roller (id min max step value chars &optional handler)
  `(let ((inp (create-element "input"))
         (validator (lambda (na)
                      (let ((isint
                             (= (length (strcat "" na))
                                (length (strcat "" 
                                                (parse-int 
                                                 (/ na ,step))))))
                            (divisor (parse-int ,step))
                            (min (parse-int ,min))
                            (max (parse-int ,max))
                            (intval (parse-int ,value)))
                        (setf na (if isint (parse-int na) intval))
                        (cond ((< na min) intval)
                              ((> na max) intval)
                              (t na))))))
     (set-attributes inp 
                     "id" ,id
                     "min" ,min
                     "max" ,max
                     "step" ,step
                     "value" ,value
                     "size" ,chars
                     "type" "number")
     ,@(when handler (list `(add-event-no-capture inp ,(car handler)
                                                  ,(cadr handler))))
     (ps:array inp validator)))

(ps:defpsmacro make-input-type (id name type &optional handler)
  `(let ((inp (create-element "input")))
     (set-attributes inp 
                     "id" ,id
                     "name" ,name
                     "type" ,type)
     ,@(when handler (list `(add-event-no-capture inp ,(car handler)
                                                  ,(cadr handler))))
     inp))

(ps:defpsmacro make-radio-input-element (control-name)
  `(let ((inp (create-element "input")))
     (set-attributes inp 
                     "name" ,control-name
                     "type" "radio")
     inp))

;;; Move
(ps:defpsmacro add-to-pupils-selected-to (what id)
  (ecase what
    (include `(push ,id selected-pupils-to-add))
    (delete `(push ,id selected-pupils-to-delete))))

(ps:defpsmacro remove-from-pupils-selected-to (what id)
  (ecase what
    (include `(array-remove ,id selected-pupils-to-add))
    (delete `(array-remove ,id selected-pupils-to-delete))))
;;; End move            

(ps:defpsmacro make-button (id text &optional handler)
  (let ((but (gensym "BUT")))
    (if handler
  ;;handler is a list ("event" fn)
        `(let ((,but (create-element "button")))
           (setf (text-content-of ,but) ,text)
           (set-attribute ,but "id" ,id)
           (add-event-no-capture ,but ,(car handler) ,(cadr handler))
            ,but)
        `(let ((,but (create-element "button")))
           (setf (text-content-of ,but) ,text)
           (set-attribute ,but "id" ,id)
           ,but))))

(ps:defpsmacro create-input-element (id &key (type "text") 
                                        handler control-name)
  (let ((inp (gensym "INP")))
    (cond ((and handler control-name)
           `(let ((,inp (create-element "input")))
              (set-attribute ,inp "type" ,type)
              (set-attribute ,inp "id" ,id)
              (set-attribute ,inp "name" ,control-name)
              (add-event-no-capture ,inp ,(car handler) ,(cadr handler))
              ,inp))
          (handler
           `(let ((,inp (create-element "input")))
              (set-attribute ,inp "type" ,type)
              (set-attribute ,inp "id" ,id)
              (add-event-no-capture ,inp ,(car handler) ,(cadr handler))
              ,inp))
          (control-name
           `(let ((,inp (create-element "input")))
              (set-attribute ,inp "type" ,type)
              (set-attribute ,inp "id" ,id)
              (set-attribute ,inp "name" ,control-name)
              ,inp))
          (t           
           `(let ((,inp (create-element "input")))
              (set-attribute ,inp "type" ,type)
              (set-attribute ,inp "id" ,id)
              ,inp)))))

(ps:defpsmacro create-radiobutton (id &key handler control-name)
  `(create-input-element ,id :handler ,handler :control-name ,control-name
                         :type "radio"))

(ps:defpsmacro selection-start (element)
  `(ps:@ ,element selection-start))

(ps:defpsmacro selection-end (element)
  `(ps:@ ,element selection-end))

(ps:defpsmacro focus (element)
  `((ps:@ ,element focus)))

(ps:defpsmacro get-attribute (el att)
  `((ps:@ ,el get-attribute) ,att))

(ps:defpsmacro node-type (node)
  `(dots ,node node-type))

(ps:defpsmacro isa-text-node (node)
  `(if (= (node-type ,node) 3) true false))

(ps:defpsmacro remove-child (parent child)
  `((ps:@ ,parent remove-child) ,child))

(ps:defpsmacro children-of (parent)
  `(ps:@ ,parent children))

(ps:defpsmacro child-nodes-of (parent)
  `(ps:@ ,parent child-nodes))

(ps:defpsmacro text-content-of (node)
  `(ps:@ ,node text-content))

(ps:defpsmacro how-many-children (parent)
  `(ps:@ ,parent children length))

(ps:defpsmacro how-many-child-nodes (parent)
  `(ps:@ ,parent child-nodes length))

(ps:defpsmacro import-doc-el (response)
  `((ps:@ document import-node) (ps:@ ,response document-element) t))

(ps:defpsmacro id-of (thing)
  `(ps:@ ,thing id))

(ps:defpsmacro insert-before (place thisbefore this)
  `((ps:@ ,place insert-before) ,thisbefore ,this))

(ps:defpsmacro value (el)
  `(ps:@ ,el value))

(ps:defpsmacro checked (el)
  `(ps:@ ,el checked))

(ps:defpsmacro is-checked (el)
  `(equal (checked ,el) true))

(ps:defpsmacro get-elements-by-tag-name (element name)
  `((ps:@ ,element get-elements-by-tag-name) ,name))

(ps:defpsmacro get-elements-by-class-name (element name)
  `((ps:@ ,element get-elements-by-class-name) ,name))

(ps:defpsmacro gebclan (element name)
  `((ps:@ ,element get-elements-by-class-name) ,name))

(ps:defpsmacro style-display (thing)
  `(ps:@ ,thing style display))

(ps:defpsmacro style-z-index (thing)
  `(ps:@ ,thing style z-index))

(ps:defpsmacro style-left (thing)
  `(ps:@ ,thing style left))

(ps:defpsmacro style-height (thing)
  `(ps:@ ,thing style height))

(ps:defpsmacro style-right (thing)
  `(ps:@ ,thing style right))

(ps:defpsmacro style-top (thing)
  `(ps:@ ,thing style top))

(ps:defpsmacro style-color (thing)
  `(ps:@ ,thing style color))

(ps:defpsmacro replace (fromparent this withthis)
  `((ps:@ ,fromparent replace) ,this ,withthis))

(ps:defpsmacro inner-html (ofthing)
  `(ps:@ ,ofthing inner-h-t-m-l))

(ps:defpsmacro set-inner-html (ofthing to)
  `(setf (ps:@ ,ofthing inner-h-t-m-l) ,to))

(ps:defpsmacro z-index (thing)
  `(ps:@ ,thing style z-index))

;; INLINE utilities:
(ps:defpsmacro prune-tree-from-nth (parent nth)
  `(ps:try 
    (remove-children-from-nth ,parent ,nth)
    (:catch (error)
      (remove-child-nodes-from-nth ,parent ,nth))))

(ps:defpsmacro insert-el (el &optional (dd nil))
  (if dd
      `(with-slots (init-element) drag-drop
         (progn (funcall init-element ,el)
                (append-child (dots document body) ,el)))
      `(append-child (dots document body) ,el)))

(ps:defpsmacro delete-el (el)
  `(remove-child (dots document body) ,el))

(ps:defpsmacro not-defined (thing)
  `(equal ,thing undefined))

(ps:defpsmacro defined (thing)
  `(not (not-defined ,thing)))

;; Move to js-lib
(ps:defpsmacro zerop (n)
  `(= ,n 0))
;; End move

;; MathJax stuff

(ps:defpsmacro mj-get-all-jax (id)
  `((ps:@ -math-jax -hub get-all-jax) ,id))

(ps:defpsmacro mj-hub-queue (&rest qitems)
  `((ps:@ -math-jax -hub -queue) ,@qitems))

(ps:defpsmacro mj-callback-queue (&rest stuff)
  `((ps:@ -math-jax -callback -queue) ,@stuff))

(ps:defpsmacro mj-callback (thing)
  `((ps:@ -math-jax -callback) ,thing))

(ps:defpsmacro mj-source-element (thing)
  `((ps:@ ,thing source-element)))

(ps:defpsmacro mj-original-text (thing)
  `(dots ,thing original-text))

;; (ps:defpsmacro queue-callback (callb q)
;;   `((ps:@ ,q -push) ,callb))

(ps:define-ps-symbol-macro mj-hub (ps:@ -math-jax -hub))

;; Move to App lib
(ps:defpsmacro make-xrecord-display (id glass description place)
  `(let ((el (div-id-class ,id ,glass)))
     (append-child el (make-text-node 
		       (strcat ,description 
			       (dots (aref xrecords current-table) ,place))))
     el))
;; end move

;; (ps:defpsmacro contains (str substr)
;;   `((ps:@ ,str contains) ,substr))

(ps:defpsmacro str-contains (str substr &optional (start-index 0))
  `(not (= ((ps:@ ,str  index-of) ,substr ,start-index) -1)))

(ps:defpsmacro char-at (str index)
  `((ps:@ ,str char-at) ,index))
					      
(ps:defpsmacro strcat (&rest args)
  `(concatenate 'string ,@args))

(ps:defpsmacro code-char (code)
  `((ps:@ -string from-char-code) ,code))

(ps:defpsmacro char-code-at (str idx)
  `((ps:@ ,str char-code-at) ,idx))

(ps:defpsmacro key-code (ev)
  `(ps:@ ,ev key-code))

(ps:defpsmacro numeric-input (ev)
  `(let ((kc (key-code ,ev)))
     (if (and (<= 48 kc)
              (>= 57 kc))
         true
         false)))

(ps:defpsmacro is-backspace (ev)
  `(let ((kc (key-code ,ev)))
     (if (= kc 8)
         true
         false)))

(ps:defpsmacro key-char (ev)
  `(code-char (key-code ,ev)))

(ps:defpsmacro ems (n)
  `(strcat ,n "em"))

(ps:defpsmacro split (parent regx)
  `((ps:@ ,parent split) ,regx))

;;NO
;; (ps:defpsmacro switchcc (n id)
;;   `(,n (click-on-id ,id)
;;        break))

;; (ps:defpsmacro switchcc (name &rest pairs)
;;   `(ps:switch ,name
;;      ,@(mapcar (lambda (pair)
;;                  `(,(car pair)
;;                     (progn (funcall prevent-default)
;;                            (funcall click-on-id ,(cadr pair))
;;                     break))) (group pairs 2))
;;      (ps:default false)))

(ps:defpsmacro switchcc (name &rest pairs)
  `(if (dots window chrome)
       (ps:switch ,name
     ,@(mapcar (lambda (pair)
                 `(,(car pair)
                    (progn (funcall prevent-default)
                           (funcall click-on-id ,(cadr pair))
                    break))) (group pairs 2))
     (ps:default false))
       (ps:switch ,name
         ,@(mapcar (lambda (pair)
                     `(,(car pair)
                        (if ctrl-key false
                            (progn (funcall prevent-default)
                                   (funcall click-on-id ,(cadr pair))
                                   break)))) (group pairs 2))
         (ps:default 
             (if ctrl-key false
                 (when (or (and (>= ,name 48)
                                (<= ,name 111))
                           (>= ,name 123))
               (funcall prevent-default))
                 false)))))

(ps:defpsmacro tx (thing)
  `(strcat "" ,thing))

(ps:defpsmacro relocate (address)
  `(setf (ps:@ window location) ,address))

(ps:defpsmacro parse-json (response)
  `(let* ((ret ((ps:@ -j-s-o-n parse) ,response))
          (redirect (dots ret redirect)))
     (if redirect 
         (relocate redirect)
         ret)))

;; (ps:defpsmacro parse-json (response)
;;   `(let ((ret ((ps:@ -j-s-o-n parse) ,response)))
;;      ret))

(ps:defpsmacro stringify-json (thing)
  `((ps:@ -j-s-o-n stringify) ,thing))

(ps:defpsmacro make-action-for (action name &optional fullname)
  (if fullname
      `(concatenate 'string "javascript:" ,action "('" ,name "', '" ,fullname "')")
      `(concatenate 'string "javascript:" ,action "('" ,name "')")))

;; (ps:defpsmacro make-action-for (action name)
;;   `(concatenate 'string "javascript:" ,action "('" ,name "')"))


(ps:defpsmacro table-id (index)
  `(concatenate 'string "table_" ,index))

(ps:defpsmacro id-to (id designator)
  `(concatenate 'string ,designator ((ps:@ ,id slice) 1)))

(ps:defpsmacro next-table-index (nexs)
  `(let ((next (ps:rem (+ current-table 1) ,nexs)))
     next))

(ps:defpsmacro answer-cell-p (id)
  `(if  (equal ((ps:@ ,id slice) 0 1) "A") t nil))

(ps:defpsmacro marked-right (string)
  `(if (equal ,string correct) t nil))

(ps:defpsmacro system-name (pair)
  `(aref ,pair 0))

(ps:defpsmacro full-name (pair)
  `(aref ,pair 1))

(ps:defpsmacro is-empty (arr)
  `(if (equal (dots ,arr length) 0)
       t
       false))
;; Move to dom-lib
(ps:defpsmacro get-style (obj type)
  `(ps:@ ,obj style ,type))

;; (ps:defpsmacro set-style (obj type style)
;;   `(setf (get-style ,obj ,type) ,style))
;; use setf instead

(ps:defpsmacro set-left (el val)
  `(setf (dots ,el style left) ,val))

(ps:defpsmacro set-right (el val)
  `(setf (dots ,el style right) ,val ))

(ps:defpsmacro set-display (el val)
  `(setf (dots ,el style display) ,val))

(ps:defpsmacro set-width (el val)
  `(setf (dots ,el style width) ,val ))

(ps:defpsmacro set-top (el val)
  `(setf (dots ,el style top) ,val ))

(ps:defpsmacro set-color (el val)
  `(setf (dots ,el style color) ,val ))

(ps:defpsmacro set-z-index (el val)
  `(setf (dots ,el style z-index) ,val ))

(ps:defpsmacro set-position (el val)
  `(setf (dots ,el style position) ,val ))
;; end move

(ps:defpsmacro x-coord (carr)
  `(aref ,carr 0))

(ps:defpsmacro y-coord (carr)
  `(aref ,carr 1))

(ps:defpsmacro hide-thing (el)
  `(set-display ,el "none"))

(ps:defpsmacro show-thing (el)
  `(set-display ,el "block"))

(ps:defpsmacro hide-thing-by-id (id)
  `(let ((the-thing (doc-get-el-by-id ,id)))
     (when the-thing
       (hide-thing the-thing)
       the-thing)))

(ps:defpsmacro show-thing-by-id (id)
  `(let ((the-thing (doc-get-el-by-id ,id)))
     (when the-thing 
       (show-thing the-thing)
       the-thing)))
;; app-lib
(ps:defpsmacro hide-caption ()
  `(hide-thing-by-id "caption"))

(ps:defpsmacro show-caption ()
  `(show-thing-by-id "caption"))
;;end app-lib

(ps:defpsmacro subseq (string start &optional end)
  (if end
      `((ps:@ ,string slice) ,start ,end)
      `((ps:@ ,string slice) ,start)))

(ps:defpsmacro html-collection-to-array (hc)
  `((ps:@ -array prototype slice call) ,hc))

(ps:defpsmacro array-reduce (array fn &optional initial)
  (if initial
      `((ps:@ ,array reduce) ,fn ,initial)
      `((ps:@ ,array reduce) ,fn)))

(ps:defpsmacro push (thing array)
  `((ps:@ ,array push) ,thing))

(ps:defpsmacro nreverse (array)
  `((ps:@ ,array reverse)))

(ps:defpsmacro pop (array)
  `((ps:@ ,array pop)))

(ps:defpsmacro array-remove (thing array)
  `((ps:@ ,array splice) ((ps:@ ,array index-of) ,thing) 1))

(ps:defpsmacro array-remove-idx (idx array)
  `((ps:@ ,array splice) ,idx 1))

(ps:defpsmacro index-of (thing array)
  `((ps:@ ,array index-of) ,thing))

(ps:defpsmacro is-included-in (thing arr)
  `(not (= (index-of ,thing ,arr) -1)))

(ps:defpsmacro named-item (htmlcoll id)
  `((,htmlcoll named-item) ,id))

(ps:defpsmacro jsn (sym)
  `(ps:lisp (ps:symbol-to-js-string ',sym)))

(ps:defpsmacro with-sub-data (&body body)
  `(with-slots (nauthorized
                eu-currency-p dansk-moms
                price-data ccy ccyfmt form make-updater 
                server interface-id get-current-selection 
                enrolled unused cancelled
                per-teacher per-pupil current update-free-used) sub-data
     (with-slots (currency "per-teacher" "per-pupil") price-data
       ,@body)))

(ps:defpsmacro with-all-modules (&body body)
  `(with-slots (modules) all-modules
     ,@body))

;; (ps:defpsmacro with-sub-data-funs (&body body)
;;   `(with-slots (ccy per-teacher per-pupil current) sub-data
;;      ,@body))

;; random-int is defined in lesson-modules-ne inside non-ephemeral object
;;app-lib
(ps:defpsmacro random-remove (array)
  `(with-slots (non-ephemeral) lesson-module
     (with-slots (random-int) non-ephemeral
       (let* ((ln (length ,array))
              (rid (funcall random-int 0 (1- ln)))
              (thing (aref ,array rid)))
         ((ps:@ ,array splice) rid 1)
         thing))))

(ps:defpsmacro menu-open-p (id)
  `(let ((tmp (doc-get-el-by-id ,id)))
     (if (not (equal (nth-of-children tmp 0) undefined))
         tmp
         false)))

(ps:defpsmacro ul-menu-open-p (id)
  `(let* ((tmp (doc-get-el-by-id ,id))
          (els (if tmp (get-elements-by-tag-name tmp "ul") (ps:array))))
     (if (not (zerop (length els)))
         tmp
         false)))
;;end app-lib

;;svg-lib
(ps:defpsmacro polyline-string (&rest coords)
  `(concatenate 'string ,@coords))

;; (ps:defpsmacro menu-open-p (id)
;;   `(not (equal (nth-of-children (doc-get-el-by-id ,id) 0) undefined)))

;; svg utils

(ps:defpsmacro svg-create-element (type)
  `((ps:@ document create-element-n-s) "http://www.w3.org/2000/svg" ,type))

(ps:defpsmacro svg-set-attribute (parent att val)
  `((ps:@ ,parent set-attribute-n-s) "http://www.w3.org/1999/xlink" ,att ,val))
    
(ps:defpsmacro svg-set-attributes (el &rest pairs)
  `(progn
     ,@(mapcar  (lambda (p) `(svg-set-attribute ,el ,@p))
                (group pairs 2))))


;; (ps:defpsmacro svg-create-element (type)
;;   `((ps:@ document create-element-n-s) "http://www.w3.org/2000/svg" ,type))

(ps:defpsmacro svg-set-x-y (element x y)
  `(progn
     (set-attribute ,element "x" (strcat "" ,x))
     (set-attribute ,element "y" (strcat "" ,y))))

(ps:defpsmacro svg-draw-line (element x1 y1 x2 y2 color width id klass)
  `(progn
     (let ((line (svg-create-element "line"))
           (group (svg-create-element "g")))
       (when ,id (set-attribute line "id" ,id))
       (when ,klass (set-attribute line "class" ,klass))
       (set-attribute line "x1" (strcat ,x1 "em"))
       (set-attribute line "y1" (strcat ,y1 "em"))
       (set-attribute line "x2" (strcat ,x2 "em"))
       (set-attribute line "y2" (strcat ,y2 "em"))
       (set-attribute line "stroke" ,color)
       (set-attribute line "stroke-width" ,width)
       (append-child group line)
       (append-child ,element group)
       group)))

(ps:defpsmacro svg-defs (&rest defs)
  `(let ((defs (svg-create-element "defs")))
     (append-children defs ,@defs)
     defs))

(ps:defpsmacro svg-group (id)
  `(let ((group (svg-create-element "g")))
     (svg-set-attribute group "id" ,id)
     group))

(ps:defpsmacro svg-translate (x y)
  `(strcat "translate(" ,x "," ,y ")"))

(ps:defpsmacro svg-use-group (varname parent use-id &rest specs)
  `(let ((,varname (svg-create-element "use")))
    (svg-set-attributes ,varname "xlink:href" (strcat "#" ,use-id)
                    ,@specs)
    (append-child ,parent ,varname)))

;; (ps:defpsmacro svg-use-group (parent use-id transform)
;;   `(let ((fuse (svg-create-element "use")))
;;     (set-attributes fuse "xlink:href" (strcat "#" ,use-id)
;;                     "transform" ,transform)
;;     (append-child ,parent fuse)))

;;app-lib
;;Careful! Messy macro variables should use gensym, you sloppy bugger!
(ps:defpsmacro draw-left-arrowhead (svg skip x y color width)
  `(let* ((offset (* ,skip (/ 1 4)))
          (lax1 (+ ,x offset))
          (lay1 (- ,y offset))
          (lax2 lax1)
          (lay2 (+ ,y offset)))
     (svg-draw-line ,svg ,x ,y lax1 lay1 ,color ,width nil nil)
     (svg-draw-line ,svg ,x ,y lax2 lay2 ,color ,width nil nil)))

(ps:defpsmacro draw-right-arrowhead (svg skip x y color width)
  `(let* ((offset (* ,skip (/ 1 4)))
          (rax1 (- ,x offset))
          (ray1 (- ,y offset))
          (rax2 rax1)
          (ray2 (+ ,y offset)))
     (svg-draw-line ,svg ,x ,y rax1 ray1 ,color ,width nil nil)
     (svg-draw-line ,svg ,x ,y rax2 ray2 ,color ,width nil nil)))

(ps:defpsmacro draw-unit-markers (svg skip x y color width 
                                      firstn length units label)
  `(with-slots (non-ephemeral) lesson-module
     (with-slots (label-mark) non-ephemeral
  (if (not (dots ,units split))
       (let* ((nunits (/ 1 ,units))
              (times (* ,length nunits))
              (offset (* ,skip (/ 1 5)))
              ;;(xstart (+ ,x ,skip))
              (unity1 (- ,y offset))
              (unity2 (+ ,y offset)))
         (loop for mark from 1 below times
            for currx = (+ ,x (* ,skip ,units mark))
            for idn = (+ ,firstn (* ,units mark))
            for unitid = (strcat "nl-mark(" idn ")")
            do  (let ((g (svg-draw-line ,svg currx 
                                        unity1 currx 
                                        unity2 ,color ,width unitid "nl-marker")))
                  (when ,label (funcall label-mark g idn)))))
       (let* ((rat (mapfor parse-int (split ,units "/")))
              (num (aref rat 0))
              (den (aref rat 1))
              (number (/ num den))
              (nunits (/ 1 number))
              (times (* ,length nunits))
              (fnum (* ,firstn nunits))
              (offset (* ,skip (/ 1 5)))
              ;;(xstart (+ ,x ,skip))
              (unity1 (- ,y offset))
              (unity2 (+ ,y offset)))
         (loop for mark from 1 below times
            for currx = (+ ,x (* ,skip number mark))
            for lnum = (+ fnum mark)
            for idn = (strcat lnum "/" den)
            for unitid = (strcat "nl-mark(" idn ")")
            do  (let ((g (svg-draw-line ,svg currx 
                                        unity1 currx unity2 
                                        ,color ,width unitid "nl-marker")))
                  (when ,label (funcall label-mark g idn)))))))))

(ps:defpsmacro input-menu-arrow (id rotation)
  `(let* ((skip svg-grid-skip) (skipem (strcat skip "em"))
          (svg (svg-create-element "svg"))
          (arr (svg-create-element "polyline"))
          (max-xy (* skip input-menu-icon-pixels))
          (viewbox (strcat 0 " " 0 " " max-xy " " max-xy))
          (x1 (* max-xy 0.0625)) (y1 (* max-xy 0.5))
          (x2 (- max-xy x1)) (y2 y1)
          (x3 (- max-xy (* max-xy 0.2))) (y3 (- y1 (* y1 0.4)))
          (x4 x3) (y4 (+ y1 (* y1 0.4)))
          (x5 x2) (y5 y2))
     (set-attributes svg "version" "1.2" "baseProfile" "basic"
                     "id" ,id "height" skipem "width" skipem
                     "viewbox" viewbox)
     (set-attributes arr "points" 
                     (strcat x1 "," y1 " " x2 "," y2 " " x3 "," y3 " "
                             x4 "," y4 " " x5 "," y5 "")
                     "fill" "#000000" "stroke" "#000000")
     (set-attribute arr "transform" (strcat "rotate(" ,rotation "," 
                                            (* 0.5 (+ x1 x2)) "," y1 ")"))
     (append-child svg arr)
     svg))

(ps:defpsmacro cell-id (x y &optional name)
  (if name
      `(strcat ,name "(" ,x "," ,y ")")
      `(strcat "cell(" ,x "," ,y ")")))

(ps:defpsmacro define-with-cell-variables (name &body body)
  `(defun ,name (x y xv yv skipem svg prefix-id)
     (let* ((g (svg-create-element "g"))
               (rect (svg-create-element "rect"))
               (xem (strcat x "em"))
               (yem (strcat y "em")))
       (when prefix-id 
         (set-attribute g "id" (cell-id xv yv prefix-id)))
       ,@body)))

(ps:defpsmacro define-anonymous-with-cell-variables (&body body)
  `(lambda (x y xv yv skipem svg prefix-id)
     (let* ((g (svg-create-element "g"))
               (rect (svg-create-element "rect"))
               (xem (strcat x "em"))
               (yem (strcat y "em")))
       (when prefix-id 
         (set-attribute g "id" (cell-id xv yv prefix-id)))
       ,@body)))

(defmacro make-input-menu-identifiers (name)
  (let* ((summon-name (symb 'summon- name '-input))
         (create-cell-id (symb name '-cell-id))
         (dismiss-name (symb 'dismiss- name '-input))
         (create-cell-name (symb 'create- name '-cell)))
    `'(,summon-name ,create-cell-id ,dismiss-name ,create-cell-name)))

(make-input-menu-identifiers alpha)

(ps:defpsmacro with-slots-declare-input-menu-identifiers (slots obj &body body)
  (let ((withimids 
         (append (make-input-menu-identifiers numer)
                 (append (make-input-menu-identifiers alpha) slots))))
  `(with-slots ,withimids ,obj ,@body)))

(ps:defpsmacro define-basic-input-menu-lambdas (name rows cols)
  (let* ((summon-name (symb 'summon- name '-input))
         (symb-name (string-downcase (symbol-name name)))
         (holder-id  (concatenate 'string  symb-name "-input"))
         (grid-id (concatenate 'string  symb-name "-grid"))
         (content-var (symb name '-content))
         (create-cell-id (symb name '-cell-id))
         (dismiss-name (symb 'dismiss- name '-input))
         (create-cell-name (symb 'create- name '-cell)))
    `(setf
       ,summon-name 
       (lambda (ev)
         (let ((,name (div-id ,holder-id)))
           (funcall (dots drag-drop init-element) ,name))
           (let ((svg (standard-svg-grid 
                       ,name ,grid-id ,rows ,cols ,create-cell-name)))
             (append-child (dots document body) ,name)))
       ,create-cell-id 
       (lambda (string) (strcat ,(concatenate 'string symb-name "-") string))
       ,dismiss-name 
       (lambda (ev)(remove-child (dots document body)
                                 (doc-get-el-by-id ,holder-id)))
       ,create-cell-name
       (define-anonymous-with-cell-variables
           (let* ((content (svg-create-element "text"))
                 (tspan (svg-create-element "tspan"))
                 (string (aref ,content-var yv xv))
                 (txt (make-text-node string)))
            (append-child tspan txt)
            (append-child content tspan)
            (set-attributes tspan "font-size" "0.70em" "dx" "0.2em" "dy" "1em")
            (set-attributes content "x" xem "y" yem)
            (set-attributes rect "x" xem "y" yem "width" skipem
                            "id" (funcall ,create-cell-id string)
                            "height" skipem "fill" "none"
                            "pointer-events" "all"
                            "stroke" "#327232")
            (if (= (length string) 1)
                (add-event-no-capture rect "click" insert-parent-string)
                (add-event-no-capture rect "click" ,dismiss-name))
            (append-child g rect)
            (append-child g content)
            (append-child svg g))))))
;; end app-lib

;; dom-lib
(ps:defpsmacro add-event-no-capture (obj evt fn)
  `((ps:@ ,obj add-event-listener) ,evt ,fn false))

(ps:defpsmacro remove-event-no-capture (obj evt fn)
  `((ps:@ ,obj remove-event-listener) ,evt ,fn false))

(ps:defpsmacro add-event-with-capture (obj evt fn)
  `((ps:@ ,obj add-event-listener) ,evt ,fn true))

(ps:defpsmacro remove-event-with-capture (obj evt fn)
  `((ps:@ ,obj remove-event-listener) ,evt ,fn true))
;; end dom-lib

;; LIBRARY macros (app or js specific)

(ps:defpsmacro last (arr)
  `(aref ,arr (1- (length ,arr))))

;;mod2-div-int
(ps:defpsmacro with-divops (&body body)
  `(ps:with-slots (basic-digit
                   second-digit
                   ) operand-generator
     ,@body))

(ps:defpsmacro with-tcp (&body body)
  `(ps:with-slots (initialize
                   current-problem) tutor-current-problem
     ,@body))

(ps:defpsmacro in-cp (&body body)
  `(with-tcp 
       (ps:with-slots (divisor 
                       result remainder result-string result-array 
                       remainder-string
                       dividend-array divisor-string
                       tmp-y-offset tmp-x-offset
                       tmp-remainder tmp-remainder-string
                       tmp-partial-product
                       dividend-string problem-string)
           current-problem
         ,@body)))

(ps:defpsmacro in-tutor (&body body)
  `(with-slots (initialize 
                get-power-name get-subtrahend
                instruct-pupil show-pupil explain-pupil 
                live-input-elements live-result-elements) tutor
     ,@body))

(ps:defpsmacro in-tutor-cp (&body body)
  `(in-tutor (in-cp ,@body)))
                              
(ps:defpsmacro string-downcase (str)
  `((ps:@ ,str to-lower-case)))

(ps:defpsmacro string-trim (str)
  `((ps:@ ,str trim)))

(ps:defpsmacro ceil (value)
  `((ps:@ -math ceil) ,value))

(ps:defpsmacro with-current-variable (obj &body body)
  `(with-slots (genfn value) ,obj
       ,@body))

(ps:defpsmacro pow (base x)
  `((ps:@ -math pow) ,base ,x))

(ps:defpsmacro to-fixed (numvar decs)
  `((ps:@ ,numvar to-fixed) ,decs))

;; (ps:defpsmacro units (n)
;;   `((digit-at-position ,n 0)))

(ps:defpsmacro mod (x y)
  `(ps:rem ,x ,y))
;;Tutor stuff

;; Macros for tutor input
(ps:defpsmacro mkcn (coords)
  `(strcat "cell(" ,coords ")"))

(ps:defpsmacro operand-coefficients (opd)
  `(with-slots (coefficients) ,opd
    coefficients))  

(ps:defpsmacro tio-operands (tio)
  `(with-slots (operands) ,tio
    operands))

(ps:defpsmacro tio-operator (tio)
  `(with-slots (operator) ,tio
    operator))

(ps:defpsmacro operands-opd1 (tio)
  `(let ((operands (tio-operands ,tio)))
     (with-slots (opd1) operands
       opd1)))

(ps:defpsmacro operands-opd2 (tio)
  `(let ((operands (tio-operands ,tio)))
     (with-slots (opd2) operands
       opd2)))

(ps:defpsmacro operand-length (opd)
  `(with-slots (length) ,opd
     length))

(ps:defpsmacro with-tutor-state (&body body)
  `(with-slots (fsm 
                initialized grid-clear current-tutor-input
                selected-cell-color plain-color 
                hint reset) tutor-state
     ,@body))

(ps:defpsmacro identical-operands ()
  `(with-tutor-state
       (dots current-tutor-input identical-operands)))

(ps:defpsmacro with-fsm (&body body)
  `(with-slots (initialized 
                set-current-node current-variable
                arcs nodes add-node find-node add-arc
                find-arc start-nodes alternative-states
                a-prefix b-prefix current-hint
                prefix in-answer regroup-mode
                in-partial-product
                start-machine current-node transition
                handle-non-commutative handle-borrowing
                borrower-target carried-garbage
                handle-multichar-input multichar-count
                handle-prefix-set handle-prefix-notset
                handle-alternative-states handle-regroup
                handle-partial-product in-partial-product
                handle-completion commutative
                handle-no-regroup) fsm
     ,@body))

(ps:defpsmacro with-input-direction (&body body)
  `(with-slots (left-to-right  
                right-to-left down-to-up up-to-down 
                reset selectedv selectedh) input-direction
     ,@body))

(ps:defpsmacro with-tutor-fsm (&body body)
  `(with-tutor-state
       (with-fsm
           ,@body)))

(ps:defpsmacro with-hint (&body body)
  `(with-tutor-fsm
       (with-slots (text-element 
                    cell0 cell1 remove clear-color
                    words display paint colors)
           current-hint
         ,@body)))

(ps:defpsmacro with-colors (&body body)
  `(with-slots (color0-color 
                color0-name color1-color
                color1-name) 
       colors
     ,@body))

(ps:defpsmacro with-words (&body body)
  `(with-hint 
       (with-slots (inthe ora anda square writea period)
           words
         ,@body)))

(ps:defpsmacro with-grid-dim (&body body)
  `(with-tutor-state
       (with-slots (dimensions) current-tutor-input
         (with-slots (rows columns) dimensions
           ,@body))))

(ps:defpsmacro machine-type ()
  `(with-tutor-state
       (dots current-tutor-input operator)))

(ps:defpsmacro node-inputs (node)
  `(ps:@ ,node inputs))

(ps:defpsmacro node-outputs (node)
  `(ps:@ ,node outputs))

(ps:defpsmacro node-next-name (node)
  `(ps:@ ,node next))

(ps:defpsmacro fsm-node-name (node)
  `(ps:@ ,node fsm-node-name))

(ps:defpsmacro node-hint-function (node)
  `(ps:@ ,node hint-function))

(ps:defpsmacro find-node-for-prefix (pref input)
  `(ps:getprop (ps:getprop alternative-states ,pref) ,input))

(ps:defpsmacro arc-to (arc)
  `(ps:@ ,arc to))

(ps:defpsmacro arc-from (arc)
  `(ps:@ ,arc from))

(ps:defpsmacro arc-action (arc)
  `(ps:@ ,arc action))

(ps:defpsmacro arc-label (arc)
  `(ps:@ ,arc label))

(ps:defpsmacro defnode (name)
  `(with-tutor-state
       (with-fsm
           (unless (funcall find-node ,name)
             (funcall add-node ,name)))))

(ps:defpsmacro defarc (from label to &optional action)
  `(with-tutor-state
       (with-fsm
           (funcall add-arc ,from ,label ,to ,action))))

;; (ps:defpsmacro def-writer-arc (from label to)
;;   `(with-tutor-state
;;        (with-fsm
;;            (funcall add-arc ,from ,label ,to (funcall make-writer-action ,to)))))

(ps:defpsmacro def-printer-arc (from fromname to toname)
  `(with-tutor-state
       (with-fsm
           (ps:destructuring-bind (pref (fromcol fromrow) ds)
               ,from
             (ps:destructuring-bind (pref (tocol torow) ign)
                 ,to
               (let ((label (strcat "have-" pref  fromcol "," fromrow "_" ds))
                     (tocell (cell-id tocol torow)))
                 ;;(strcat "cell(" tocol "," torow ")")))
                 (funcall add-arc ,fromname label ,toname
                          (lambda ()
                            (funcall place-string-in-selected-cell ds)
                            (funcall focus tocell)))))))))

(ps:defpsmacro def-start-arc (pref to-name)
  `(with-tutor-state
       (with-fsm
           (let* ((fn (strcat ,pref "start"))
                  (al (strcat fn "-arc"))
                  (startn (funcall find-node "start"))
                  (action (if (not commutative)
                              (funcall make-noncomm-start-arc-action ,to-name)
                              null)))
             (defnode fn)
             (let* ((from startn)
                    (to (funcall find-node ,to-name))
                    (arc (ps:create from from
                                    to to
                                    label al
                                    action action)))
               (push arc arcs)
               (setf (ps:getprop from "next") ,to-name)
               (setf (ps:getprop (node-outputs from) 
                                 ,to-name)
                     arc)
               (setf (ps:getprop (node-inputs to) 
                                 fn)
                     arc)
               arc)))))
             ;;(funcall add-arc fn al ,to)))))

(ps:defpsmacro def-have-operands-arc (ds fromname toname)
  `(with-tutor-state
       (with-fsm
           (funcall add-arc ,fromname "have-operands" ,toname
                    (lambda ()
                      (setf in-answer true)
                      ;;(setf prefix "")??
                      (funcall place-string-in-selected-cell ,ds)
                      (funcall insert-answer-line)
                      (with-grid-dim
                          (let ((cell (cell-id (1- columns) (1- rows))))
                            ;;(strcat "cell(" (1- columns) "," (1- rows) ")")))
                            (funcall focus cell))))))))
                      ;;(funcall swaph-input-direction)                      

(ps:defpsmacro def-have-part-products-arc (ds fromname toname)
  `(with-tutor-state
       (with-fsm
           (funcall add-arc ,fromname "have-pprods" ,toname
                    (lambda ()
                      (setf in-answer true
                            in-partial-product false)
                      (setf prefix (subseq ,toname 0 2))
                      (funcall place-string-in-selected-cell ,ds)
                      (funcall insert-answer-line)
                      (with-grid-dim
                          (let ((cell (cell-id (1- columns) (1- rows))))
                            ;;(strcat "cell(" (1- columns) "," (1- rows) ")")))
                            (funcall focus cell))))))))

(ps:defpsmacro def-in-part-product-arc (ds fromname toname)
  `(with-tutor-state
       (with-fsm
           (funcall add-arc ,fromname "in-pprods" ,toname
                    (lambda ()
                      (let* ((cc (cell-coords selected-cell))
                             (xi (parse-int (cell-x cc)))
                             (yi (parse-int (cell-y cc))))
                        (funcall insert-answer-line 2)
                        (setf in-partial-product true)
                        (setf prefix (subseq ,toname 0 3))
                        (funcall place-string-in-selected-cell ,ds)
                        ;;(funcall insert-answer-line)
                        (with-grid-dim
                            (let ((cell (cell-id xi (1+ yi))))
                              ;;(strcat "cell(" (1- columns) "," (1- rows) ")")))
                              (funcall focus cell)))))))))

(ps:defpsmacro def-next-part-product-arc (ds fromname toname)
  `(with-tutor-state
       (with-fsm
           (funcall add-arc ,fromname "in-pprods" ,toname
                    (lambda ()
                      (setf prefix (subseq ,toname 0 3))
                      (dolist (c carried-garbage)
                        (let ((pn (parent-node c)))
                          (remove-child pn c)))
                      (funcall place-string-in-selected-cell ,ds)
                      (with-grid-dim
                          (let ((cell (cell-id 4 3)))
                            (funcall focus cell))))))))

;; (ps:defpsmacro def-have-operand-arc (ds fromname to toname)
;;   `(with-tutor-state
;;        (with-fsm
;;            (ps:destructuring-bind ((col row) ign)
;;                ,to
;;              (let ((dcell (cell-id col row)))
;;                ;;(strcat "cell(" col "," row ")")))
;;                (funcall add-arc ,fromname "have-first-operand" ,toname 
;;                         (lambda ()
;;                           (funcall place-string-in-selected-cell ,ds)
;;                           (funcall focus dcell))))))))

(ps:defpsmacro def-completed-parts-arc (curr currname)
  `(ps:array ,curr,currname))


(ps:defpsmacro def-completed-arc (from fromname)
  `(with-tutor-state 
       (with-fsm
           (ps:destructuring-bind (pre (col row) dig)
               ,from
             (defnode ,fromname)
             (defnode "completed")
             (funcall add-arc ,fromname "sum-completed" "completed"
                      (lambda ()
                        (let ((gnew (dgebid "tutor-ic-new"))
                              (butt (make-button "tutor-new-sum" "New" 
                                                 ("click" tutor-new-problem))))
                          (append-child gnew butt)
                          (funcall place-string-in-selected-cell dig))))))))


                        ;; (let ((scr (dgebid "tutor-msg-screen"))
                        ;;       (ans (with-tutor-state
                        ;;                (dots current-tutor-input
                        ;;                      answer coefficients))))
                          
                        ;;   (remove-children-from-nth scr 0)
                        ;;   (append-para-text scr (strcat "Sum correct! " ans)))))))))
;;(alert "Sum completed!"))))))))

(ps:defpsmacro def-answer-arc (from fromname to toname)
  `(with-tutor-state 
       (with-fsm
           (ps:destructuring-bind (fpre (fcol frow) fdig)
               ,from
             (ps:destructuring-bind (tpre (tcol trow) tdig)
                   ,to
               (let* ((nls (split ,fromname "_"))
                      (pr? (aref nls 0))
                      (fcell (dgebid (cell-id fcol frow)))
                      ;;(strcat "cell(" fcol "," frow ")")))
                      (tcell (cell-id tcol trow))
                      ;;(strcat "cell(" tcol "," trow ")"))
                      (label (strcat "have-" ,fromname))
                      (fn (if (or (string= pr? "R")
                                  (string= (subseq pr? 0 1) "R"))
                              (lambda ()
                                (funcall place-carry-digit-in-cell
                                         fdig fcell)
                                (funcall focus tcell))
                              (lambda ()
                                (funcall place-string-in-selected-cell
                                         fdig)
                                (funcall focus tcell)))))
                 (funcall add-arc ,fromname label ,toname fn)))))))     

;; (ps:defpsmacro def-part-answer-arc (from fromname to toname)
;;   `(with-tutor-state 
;;        (with-fsm
;;            (ps:destructuring-bind (fpre (fcol frow) fdig)
;;                ,from
;;              (ps:destructuring-bind (tpre (tcol trow) tdig)
;;                    ,to
;;                (let* ((nls (split ,fromname "_"))
;;                       (pr? (aref nls 0))
;;                       (fcell (dgebid (cell-id fcol frow)))
;;                       ;;(strcat "cell(" fcol "," frow ")")))
;;                       (tcell (cell-id tcol trow))
;;                       ;;(strcat "cell(" tcol "," trow ")"))
;;                       (label (strcat "have-" ,fromname))
;;                       (fn (if (string= pr? "R")
;;                               (lambda ()
;;                                 (funcall place-carry-digit-in-cell
;;                                          fdig fcell)
;;                                 (funcall focus tcell))
;;                               (lambda ()
;;                                 (funcall place-string-in-selected-cell
;;                                          fdig)
;;                                 (funcall focus tcell)))))
;;                  (funcall add-arc ,fromname label ,toname fn)))))))     
              
(ps:defpsmacro cell-coords (name)
  `(split (subseq ,name 5 (1- (length ,name))) ","))

(ps:defpsmacro selected-cell-coords ()
  `(subseq selected-cell 5 (1- (length selected-cell))))

(ps:defpsmacro cell-x (coords)
  `(aref ,coords 0))

(ps:defpsmacro cell-y (coords)
  `(aref ,coords 1))

  
(ps:define-ps-symbol-macro 
    menu-elements ;;done
    (ps:create 
      config 
      (ps:create
       0 '((("" "nop" "even config")
            ("" "nop" "odd text config")
            ("R" "str" "x text config")
            )))
      ;;       ("clear" "clr" "odd text config")
      ;;       ("?" "?" "x config")))      
      ;;  )
      self-insert
      (ps:create
       0 '((("0" "0" "even self-insert")
            ("1" "1" "odd self-insert")
            ("2" "2" "x self-insert"))
           (("3" "3" "even self-insert")
            ("4" "4" "odd self-insert")
            ("5" "5" "x self-insert"))
           (("6" "6" "even self-insert")
            ("7" "7" "odd self-insert")
            ("8" "8" "x self-insert"))
           (("9" "9" "even self-insert")
            ("+" "+" "odd self-insert")
            ("-" "-" "x self-insert")))
       1 '((("*" "*" "even self-insert")
            ("hline" "hline" "odd self-insert")
            ("vline" "vline" "x self-insert"))))
      movement
      (ps:create
       0 '((("-->" "swph" "even movement")
            ("^" "swpv" "odd movement")
            ("" "nop" "x movement"))
           (("MV H" "movh" "even movement")
            ("Mv V" "movv" "odd movement")
            ("" "nop" "x movement"))))
      delquit
      (ps:create
       0 '((("Del Mv" "de" "even text delete")
            ("Del" "dels" "odd text delete")
            ("" "nop" "x text delete"))))
      ;; entry
      ;; (ps:create
      ;;  0 '((("Underline" "eq" "even text entry")
      ;;       ("Enter" "enter" "odd text entry")
      ;;       ("Confirm" "confirm" "x text entry "))) )
      ))

(ps:defpsmacro pd-values (r x y v)
  `(ps:array ,r (ps:array ,x ,y) ,v))

;;; End tutor stuff

;;; mod-table
(ps:defpsmacro with-mtab (&body body)
  `(with-slots (product-click-handler
                rectangle draw-rectangle free-rectangle
                released handler-grab
                handler-release swap-factors
                clear-pqs initialize
                ) mtab
     ,@body))

(ps:defpsmacro rect-value (ref)
  `(dots rectangle ,ref))

(ps:defpsmacro with-pb-styles (&body body)
  `(with-slots (zero multiplicand multiplier product-off product-on)
       pb-styles
     ,@body))    

(ps:defpsmacro get-muldiv-pairs (which)
  (let ((code ()))
    (dolist (p (if (eql which 'division)
                   +div-mul-pairs+
                   +all-mul-pairs+))
      (destructuring-bind (op a b)
          p
        (push `(ps:array ,op ,a ,b) code)))
    `(progn (ps:array ,@code))))

(ps:defpsmacro collect-by-divisor (dvor)
  (let ((code ()))
    (loop for p in +div-mul-pairs+
       do (destructuring-bind (op t1 t2)
              p
            (when (= dvor t2)
              (push `(ps:array ,op ,t1 ,t2) code))))
    `(ps:array ,@code)))

;;mod1-mult

(ps:defpsmacro in-practice (&body body)
  `(with-slots (digit-factor 
                sum-factor  user-term1 user-term2
                user-partial-product1 user-partial-product2
                user-total-sum initialize generate-practice
                display-practice-start handle-response1
                generate-practice-stage2 error-response1
                handle-response2 clear-message error-response2
                generate-practice-stage3 handle-response3 
                error-response3 continue-message)
       practice-problem
     ,@body))

(ps:defpsmacro with-auto-ev-slots (&body body)
  `(with-slots (detail) ev
     (with-slots (evfun element) detail
       ,@body)))
    

(ps:defpsmacro with-distab (&body body)
  `(with-slots (rectangle draw-rectangle free-rectangles
                initialize released draw-all-rectangles
                write-label write-minor-label write-area-label
                auto clear-all enter-factors
                ) distab
     ,@body))

(ps:defpsmacro with-dis-styles (&body body)
  `(with-slots (zero multiplicand multiplier product-off product-on)
       dis-styles
     ,@body))    

;;(ps:defpsmacro write-major-label (label axis)


;;(ps:defpsmacro write-minor-label (label interval axis)


;; (ps:defpsmacro score-code (&body body)
;;   `(labels ((callback (response)
;;               (let* ((parr (parse-json response))
;;                      (bt (aref parr 0))
;;                      (imp (aref parr 1))
;;                      (opt (aref parr 2)))
;;                 (when (and bt imp opt)
;;                   (setf best-time bt
;;                         improvements imp
;;                         optotal opt))
;;                 parr)))
;;      ,@body))

(ps:defpsmacro shift (arr)
  `((ps:@ ,arr shift)))

(ps:defpsmacro is-array (thing)
  `((ps:@ -array is-array) ,thing))

(ps:defpsmacro is-capitalized (char)
  `(and (stringp ,char)
        (let ((code (char-code-at ,char 0)))
          (and (<= 65 code) (>= 90 code)))))

(ps:defpsmacro grey-el (el)
  `(set-color ,el "#c3c3c3"))

(ps:defpsmacro sort (arr fn)
  `((ps:@ ,arr sort) ,fn))

(ps:defpsmacro make-header (text &optional (level "h1"))
  `(let ((h (create-element ,level)))
     (append-text h ,text)
     h))

(ps:defpsmacro make-ulist (&rest items)
  `(let ((ul (create-element "ul")))
     ,@(mapcar (lambda (item)
                 `(let ((li (create-element "li")))
                    (append-text li ,item)
                    (append-child ul li))) items)
     ul))

(ps:defpsmacro make-formrow (&rest items)
  `(let ((fr (div-class "formrow"))
         (formcells (ps:array ,@items)))
     (dolist (fc formcells)
       (append-child fr fc))
     fr))

(ps:defpsmacro make-formcell-with-text (txt)
  `(let ((fc (div-class "formcell")))
     (append-text fc ,txt)
    fc))

(ps:defpsmacro make-formcell (&key empty element text append)
  (cond (empty `(let ((fc (div-id-class ,empty "formcell")))
                  fc))
        ((and text
              (not append)) `(make-formcell-with-text ,text))
        (element `(let ((fc (div-class "formcell")))
                    (append-child fc ,element)
                    fc))
        ((and append 
              (not text)) `(let ((fc (div-class "formcell")))
                             (append-children fc ,@append)
                             fc))
        ((and text 
              append) `(let ((fc (div-class "formcell")))
                         (append-text fc ,text)
                         (append-children fc ,@append)
                         fc))
        (t (error "Unknown make-formcell combination:
:empty - ~S~%:element - ~S~%:text - ~S~%:append - ~S~%" empty element text append))))
         

(ps:defpsmacro make-ctrl-item (id type txt &optional handler)
  (let ((glass (cond ((eql type 'pupil) 
                      "ctrl-item do-pupil")
                     ((eql type 'group)
                      "ctrl-item do-group")
                     ((eql type 'teaching)
                      "ctrl-item do-teaching")
                     ((eql type 'account)
                      "ctrl-item do-account")
                     (t (error "Unknown type: ~A" type)))))
    (if handler
        `(let ((tmp (div-id-class ,id ,glass)))
           (append-text tmp ,txt)
           (add-event-no-capture tmp "click" ,handler)
              tmp)
        `(let ((tmp (div-id-class ,id ,glass)))
           (append-text tmp ,txt)
           tmp))))

(ps:defpsmacro process-response-group-structure (&optional continue)
  `(let* ((script (parse-json response))
          ;;(top (aref script 0))
          (ngm (doc-get-el-by-id "new-gmenu"))
          (placeholder (doc-get-el-by-id "new-gmenu-content"))
          (ngmc (plain-div)))
     (setf (inner-html ngmc) script)
     (replace-child ngm ngmc placeholder)
     (setf (id-of ngmc) "new-gmenu-content")
     (install-gmenu-handlers)
     (progn ,continue)))

(ps:defpsmacro with-restored-group-state (&body body)
  (let ((res (gensym "WRGS")))
    `(with-slots (save-state restore-state clear-state) group-menu-state
       (funcall save-state)
       (let ((,res (funcall  (lambda () ,@body))))
         (funcall restore-state)
         (funcall clear-state)
         ,res))))

(ps:defpsmacro with-updated-groups (&body body)
  (let ((body-fun (gensym "BODYFUN"))
        (callback (gensym "CALLBACK"))
        (top (gensym "TOP"))
        (grs (gensym "GRS"))
        (script (gensym "SCRIPT"))
        (ngm (gensym "NGM"))
        (placeholder (gensym "PLACEHOLDER"))
        (ngmc (gensym "NGMC"))
        (res (gensym "RES")))
    `(labels ((,body-fun ()
                ,@body)
              (,callback (response)
                ;;(with-restored-group-state
                    (let* ((,script (parse-json response))
                           ;;(top (aref script 0))
                           (,ngm (doc-get-el-by-id "new-gmenu"))
                           (,placeholder (doc-get-el-by-id "new-gmenu-content"))
                           (,ngmc (plain-div)))
                      (setf (inner-html ,ngmc) ,script)
                      (replace-child ,ngm ,ngmc ,placeholder)
                      (setf (id-of ,ngmc) "new-gmenu-content")
                      (install-gmenu-handlers)
                      (let ((,res (funcall ,body-fun)))
                        ,res))));;)
     (let ((,top (doc-get-el-by-id "new-gmenu")))
       (if (not ,top)
           (alert "Please open your Groups Menu first!")
           (let ((,grs (get-elements-by-tag-name 
                           (doc-get-el-by-id "new-gmenu-content") "ul")))
             (if (not (zerop (length ,grs)))
                 ;;(with-restored-group-state
                     (funcall ,body-fun);;)
                 (ajax_get_groups_html owner ,callback))))))))

(ps:defpsmacro with-updated-pupils (&body body)
  (let ((body-fun (gensym "WUPBODYFUN"))
        (callback (gensym "WUPCALLBACK"))
        (arr (gensym "WUPARR"))
        (res (gensym "WUPRES"))
        (P1 (GENSYM "WUPP1")))
  `(labels ((,body-fun ()
              ,@body)
            (,callback (response)
              (let* ((,arr (parse-json response)))
                (if (not-defined (aref ,arr 0))
                    (display-enroll-form)
                    (progn (setf pupil-list ,arr)
                           (let ((,res (funcall ,body-fun)))
                             ,res))))))
     (let ((,p1 (aref pupil-list 0)))
       (if (not-defined ,p1)
           (ajax_get_teachers_pupils_list owner ,callback)
           (funcall ,body-fun))))))

(ps:defpsmacro set-score-code (varname)
  `(setf best-time (aref ,varname 0)
         improvements (aref ,varname 1)
         optotal (aref ,varname 2)))

(ps:defpsmacro with-hidden-elements (ellist &body body)
  (let ((letl (mapcar #'(lambda (p)
                             `(,(car p) (doc-get-el-by-id ,(cadr p)))) ellist))
        (varl (mapcar #'car ellist)))
  `(progn (let (,@letl)
            ,@body
            (dolist (el (ps:array ,@varl))
              (when el ;;ADDED
                (set-display el "none")))))))
        
;; (ps:defpsmacro define-display-xrecord ()
;;   `(defun display-xrecord ()
;;      (let* ((panel (doc-get-el-by-id "right"))
;;             (current (doc-get-el-by-id "xrecord"))
;;             (xrecel (div-id-class "xrecord" "pupil-info"))
;;             (tot (make-xrecord-display "total" "xrec-info" "Total: " total))
;;             (correct (make-xrecord-display 
;;                       "correct" "xrec-info" "Correct: " right))
;;             (incorrect (make-xrecord-display
;;                         "incorrect" "xrec-info" "Incorrect: " wrong)))
;;        (append-child xrecel tot)
;;        (append-child xrecel correct)
;;        (append-child xrecel incorrect)
;;        (if current (replace-child panel xrecel current)
;;            (append-child panel xrecel)))))

(ps:defpsmacro let-by-id (bindings &rest body)
  (let ((trbinds ()))
    (dolist (b bindings)
      (push `(,(car b) (doc-get-el-by-id ,(cadr b))) trbinds))
    `(let ,trbinds ,@body)))

(ps:defpsmacro with-variables-create (varlist &rest objslots)
  (let ((code ()))
    (dolist (v varlist)
      (if (atom v)
	  (setf code (append `(,v undefined) code))
	  (let ((var (car v))
		(val (cadr v)))
	    (if val
		(setf code (append `(,var ,val) code))
		(setf code (append `(,var undefined) code))))))
    `(ps:create ,@code ,@objslots)))

(ps:defpsmacro define-anonymous-cell-type-test (type)
  (let (top bottom)
    (cond ((eql type 'fraction)
           (setf top "num")
           (setf bottom "den"))
          ((eql type 'power)
           (setf top "exp")
           (setf bottom "bas"))
          (t (error "Unknown type: ~S" type)))
    `(lambda (cell)
        (let ((gs (get-elements-by-tag-name cell "g")))
          (if gs
              (if (= (length gs) 2)
                  (let ((firstg (aref gs 0))
                        (secondg (aref gs 1)))
                    (if (or (or (string= (subseq (id-of firstg) 0 3) ,top)
                                (string= (subseq (id-of firstg) 0 3) ,bottom))
                            (or (string= (subseq (id-of secondg) 0 3) ,bottom)
                                (string= (subseq (id-of secondg) 0 3) ,top)))
                        true
                        false))
                  false)
              false)))))

(ps:defpsmacro define-anonymous-click-cell-type-function (type)
   (let (top bottom)
     (cond ((eql type 'fraction)
            (setf top "num")
            (setf bottom "den"))
           ((eql type 'power)
            (setf top "exp")
            (setf bottom "bas"))
           (t (error "Unknown type: ~S" type)))
     `(lambda (cell)
        (with-slots (selectedh left-to-right right-to-left) input-direction
          (let* ((gs (get-elements-by-tag-name cell "g"))
                 (firstg (aref gs 0))
                 (secondg (aref gs 1))
                 (event (doc-create-event "MouseEvents"))
                 (lower)
                 (upper))
            (init-event event "click" false false)
            (if (string= (subseq (id-of secondg) 0 3) ,bottom)
                (progn
                  (setf lower (aref (get-elements-by-tag-name secondg "rect") 0)
                        upper (aref (get-elements-by-tag-name firstg "rect") 0)))
                (progn
                  (setf lower (aref (get-elements-by-tag-name firstg "rect") 0)
                        upper (aref (get-elements-by-tag-name secondg "rect") 0))))
            (if (= selectedh left-to-right)
                (dispatch-event lower event)
                (dispatch-event upper event)))))))

(ps:defpsmacro define-click-cell-type-function (name type)
   (let (top bottom)
     (cond ((eql type 'fraction)
            (setf top "num")
            (setf bottom "den"))
           ((eql type 'power)
            (setf top "exp")
            (setf bottom "bas"))
           (t (error "Unknown type: ~S" type)))
     `(defun ,name (cell)
        (with-slots (selectedh left-to-right right-to-left) input-direction
          (let* ((gs (get-elements-by-tag-name cell "g"))
                 (firstg (aref gs 0))
                 (secondg (aref gs 1))
                 (event (doc-create-event "MouseEvents"))
                 (lower)
                 (upper))
            (init-event event "click" false false)
            (if (string= (subseq (id-of secondg) 0 3) ,bottom)
                (progn
                  (setf lower (aref (get-elements-by-tag-name secondg "rect") 0)
                        upper (aref (get-elements-by-tag-name firstg "rect") 0)))
                (progn
                  (setf lower (aref (get-elements-by-tag-name firstg "rect") 0)
                        upper (aref (get-elements-by-tag-name secondg "rect") 0))))
            (if (= selectedh left-to-right)
                (dispatch-event lower event)
                (dispatch-event upper event)))))))
    
;; helper functions

(defun make-info-display-line (arg)
    (destructuring-bind (line-id title-id title-text field-id)
        arg
      `(let ((line (div-id-class ,line-id "display-line"))
             (tit (div-id-class ,title-id "display-title"))
             (fid (div-id-class ,field-id "display-field")))
         (append-children line tit fid)
         (append-para-text tit ,title-text)
         line)))

(defun make-info-display-section-title (title)
  (if title
      `(let* ((tith (div-class "display-section-title"))
              (h1 (create-element "h1")))
         (append-child tith h1)
         (append-para-text h1 ,title)
         tith)
      'false))

(defun make-info-display-section (arg)
  (destructuring-bind (section-id section-title &rest lines)
      arg
    (let* ((vars ())
           (code (mapcar #'(lambda (l)
                             (let ((g (gensym "LINE")))
                               (push g vars)
                               `(,g ,(make-info-display-line l)))) lines)))
      `(let ((sec (div-id-class ,section-id "display-section"))
              (tit ,(make-info-display-section-title section-title))
              ,@code)
         (when tit (append-child sec tit))
         (append-children sec ,@(nreverse vars))
         sec))))

(ps:defpsmacro make-info-display (place &rest sections)
  (let* ((vars ())
         (code (mapcar #'(lambda (s)
                           (let ((g (gensym "SEC")))
                             (push g vars)
                             `(,g ,(make-info-display-section s)))) sections)))
    `(let (,@code)
       (append-children ,place ,@(nreverse vars))
       ,place)))

(ps:defpsmacro get-enroll-pupil-info ()
  `(ps:lisp 
    (tree-to-html 
     '((:p "(If you prefer, you can instead click " 
        (:a :href "" :id "upload-file-list" "here,") 
        " to upload a file containing a list of pupils.)")
       (:p "You can reset the pupil's password later, by clicking on " 
        (:q "set password") " in the control panel on the left.")
       (:p "When the pupil has been enrolled, the system will inform you of the pupil's username and password, "
        (:emph "which they will need to login."))
       (:p
        (:strong "It is your responsibility to keep a record of this information."))))))

(ps:defpsmacro get-upload-pupil-file-info ()
  `(ps:lisp
    (tree-to-html
     `((:p "For security and reliability reasons, the system is quite
     strict about the layout and contents of the file which you
     upload.")
       (:p "The following characters should not appear in a
       pupil-file, and may cause the upload to
       fail: " (:q "#") " (number
       sign), " (:q ";") " (semicolon)," (:q ":") " (colon)
       or " (:q "|") " (vertical bar)." )
       
       (:ul 
        (:li "The file must be plain text.  If you use some office
        program to write the file, you must save it as plain text.")
        (:li "Each pupil entry must look like this: "
             (:ul (:li "(" (:q "Firstname")  " " (:q "Second Names") " X)")))
        (:li "X can be either the letter T or the word NIL.")
        (:li "Each entry must contain 3 items between the parentheses.")
        (:li "The first item determines the pupil's first name.")
        (:li "The second item determines the pupils other
        names (optional middle names or letters) and surname.")
        (:li "The third item determines wether or not the pupil is
        starting from grade 1.  It is either the unquoted
        word " (:q "NIL") "if the pupil is not starting from grade 1,
        or the unquoted letter " (:q "T") "."))

       (:p "If any of the pupil names contain any of the characters: &amp;
       &lt; &gt; &#039; &quot; then the string will be altered by the
       system to prevent certain types of security problems.  It is
       best to avoid these characters.")
       (:p "It is also best (for your own sake) to give pupils a
       unique name, so that you can distinguish between them on your
       page. The system will not get confused by identical names, but
       you might.")

       (:p (:a :href ,',(format nil "~A~A"
       *default-uri* "examples/good.txt") "Here") "is an example file
       showing acceptable pupil entries.  The first pupil is starting
       from grade 1.")))))

;;A double-quote character is one character.  It is not acceptable to try to use 2 single quote characters in place of 1 double-quote character.         
         
     
