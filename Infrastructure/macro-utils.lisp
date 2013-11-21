(in-package :autotutor)

(defmacro define-redirectfn (name arglist productioncode developmentcode)
  (if ctmt:*in-production*
      `(progn
         (defun ,name ,arglist ,productioncode))
      `(progn
         (defun ,name ,arglist ,developmentcode))))

;; these next 3 from Paul Graham's book "On Lisp"

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))


