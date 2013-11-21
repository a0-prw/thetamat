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

(load "Scripts/dom-utils.lisp")
(load "Scripts/scratchpad.lisp")
(load "Scripts/common.lisp")
(load "Scripts/pupil.lisp")
(load "Scripts/teacher.lisp")
(load "Scripts/login.lisp")
(load "Scripts/Lessons/lesson-modules-ne.lisp")
(load "Scripts/Lessons/generator-links.lisp")
(load "Scripts/Lessons/mod-digs-nat.lisp")
(load "Scripts/Lessons/mod-basic-add-nat.lisp")
(load "Scripts/Lessons/mod-basic-sub-nat.lisp")
(load "Scripts/Lessons/mod-position-sys1-nat.lisp")
(load "Scripts/Lessons/mod-sub-position-sys-nat.lisp")
(load "Scripts/Lessons/mod-position-sys2-nat.lisp")
(load "Scripts/Lessons/mod-basic-int.lisp")
(load "Scripts/Lessons/mod-table.lisp")
(load "Scripts/Lessons/mod1-mult.lisp")
(load "Scripts/Lessons/mod2-int.lisp")
(load "Scripts/Lessons/mod2-mult.lisp")
(load "Scripts/Lessons/mod1-div-int.lisp")
(load "Scripts/Lessons/mod2-div-int.lisp")

(setf *scratchpad-script* (scratchpad-script))
