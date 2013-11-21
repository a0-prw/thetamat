(format t "~%Loading system.~%")
(ql:quickload :thetamat :verbose t)

(in-package :autotutor)

(setf +modular-progression+ (mapcar #'module-label (topsort)))


