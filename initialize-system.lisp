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
