
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

(defpackage :autotutor 
  (:use :cl)
  (:nicknames :at))

(defpackage :thetamat-common
  (:use :cl)
  (:nicknames :ctmt)
  (:export :*autotutor-name* :*private-key* :*certificate*
           ;;on ALL HOSTS (home servers) in an installation
           :*in-production*
           :*autotutor-server* :*autotutor-server-port*))

(in-package :ctmt)

(defvar *in-production* nil)

(defvar *private-key*)
(defvar *certificate*)

(defvar *autotutor-server*)
(defvar *autotutor-server-port* 4040)

(progn (if *in-production*
           
           (setf *autotutor-server* "a.b.c.d"
                 ;; configure the above with an ip-address
                 *private-key* "ssl/blahblah.key"
                 ;; your private key above
                 *certificate* "ssl/blahblah.pem")
                 ;; your certificate above
           
           (setf *autotutor-server* "localhost"
                 *private-key* "ssl/server-20120805.key" 
                 *certificate* "ssl/server-20120805.crt" )))

(defvar *autotutor-name* "blah.foo.org")

(in-package :autotutor)

(defparameter *latex-backslash* "\\")
(defvar *default-latex-syntax*
  '((plus "+") (minus "-") (times "\\cdot") (divided-by "\\div") (dec ".")))

(defvar *language* 'danish)
;;can be 'danish 'british-english or 'american-english

(setf (who:html-mode) :html5)
;;(setf ps:*use-strict* t)

(defvar *host* ctmt:*autotutor-server*)
(defvar *port* ctmt:*autotutor-server-port*)

(defvar *default-uri* (format nil "https://~A:~D/" *host* *port*))



