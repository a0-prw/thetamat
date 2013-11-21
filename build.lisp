(format t "~%Loading system.~%")
(ql:quickload :autotutor :verbose t)

(in-package :autotutor)

(setf +modular-progression+ (mapcar #'module-label (topsort)))


