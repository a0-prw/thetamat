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

;;FIXME re-enable the timer ignore-errors when thoroughly tested
(progn 
  (format t "System loaded.~%")
  (format t "Initializing random number generator.~%")
  (setf *random-state* (make-random-state t))
  (format t "Establishing user register hash.~%")
  (place-register)
  (format t "Restoring user register.~%")
  (initialize-user-registry)

  (format t "Establishing unique-name-count hash.~%")
  (place-name-count-store)
  (format t "Restoring unique-name-count.~%")
  (initialize-name-count)

  ;; (format t "Establishing client-user register hash.~%")
  ;; (place-client-user-register)
  ;; (format t "Restoring client-user register.~%")
  ;; (initialize-client-user-registry)

  ;; (format t "Initializing expiry list.~%")
  ;; (initialize-expiry-list)
  (format t "Establishing server instance.~%")
  (make-server :ssl t)
  ;; (format t "Establishing vpn server instance.~%")
  ;; (make-vpn-server)
  ;; (format t "Starting vpn server.~%")
  ;; (start-vpn-server)
  ;; (format t "Starting expiry-list comber.~%")
  ;; (trivial-timers:schedule-timer 
  ;;  (trivial-timers:make-timer #'(lambda ()
  ;;                                 ;;(ignore-errors
  ;;                                 (comb-expiry-list-new))
  ;;                             :name "comber" :thread t)
  ;;  ;;)
  ;;  10 :repeat-interval *comb-interval*)
  (setf *read-eval* nil)
  (setf tbnl:*session-secret* (suggest-password))
  (format t "Starting server.~%")
  (start))
