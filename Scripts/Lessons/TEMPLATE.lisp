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

(eval-when (:load-toplevel)
  (format t "~%; Loaded mod1-div-int.lisp")
  (print-time-now))

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
    :step ((:p "Fill me in."))

    
    :step ((:h1 "Summary")
           (:ul (:li "This")
                (:li "And this"))
           (:p "Press the right arrow to complete this lesson."))
    :complete? t)
