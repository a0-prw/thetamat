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

(setf (get 'mod-digs-nat 'script-generator) 
      'init-mod-digs-nat-lesson-slots

      (get 'mod-basic-add-nat 'script-generator) 
      'init-mod-basic-add-nat-lesson-slots 

      (get 'mod-basic-sub-nat 'script-generator) 
      'init-mod-basic-sub-nat-lesson-slots

      (get 'mod-position-sys1-nat 'script-generator)
      'init-mod-position-sys1-nat-lesson-slots

      (get 'mod-sub-position-sys-nat 'script-generator)
      'init-mod-sub-position-sys-nat-lesson-slots

      (get 'mod-position-sys2-nat 'script-generator)
      'init-mod-position-sys2-nat-lesson-slots

      (get 'mod-basic-int 'script-generator)
      'init-mod-basic-int-lesson-slots

      (get 'mod-table 'script-generator)
      'init-mod-table-lesson-slots

      (get 'mod1-mult 'script-generator)
      'init-mod1-mult-lesson-slots

      (get 'mod2-int 'script-generator)
      'init-mod2-int-lesson-slots

      (get 'mod2-mult 'script-generator)
      'init-mod2-mult-lesson-slots

      (get 'mod1-div-int 'script-generator)
      'init-mod1-div-int-lesson-slots

      (get 'mod2-div-int 'script-generator)
      'init-mod2-div-int-lesson-slots)
