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

(defvar *translations-dictionary*
  (make-hash-table))

(defun dictionary-entry (index tr-alist)
  "Store TR-ALIST in the *TRANSLATIONS-DICTIONARY* at key INDEX"
  (setf (gethash index *translations-dictionary*) tr-alist))

(defun enter-translations (translations)
  "TRANSLATIONS looks like this: 
'((am-eng-entry ((BRITISH-ENGLISH translation)
                (DANISH translation))
                (new ...)*)
  (am-eng-entry ...))  Where 'new' is any other languages.  This
 function writes a file containing thelist of #idx\"am-eng-entry\"#
 items which have the correct index for use in the code."
  (with-open-file (stream "translations/translations.txt" 
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
    
    (loop for tr in translations
       for idx from 1
       do (dictionary-entry idx (cadr tr))
       do (format stream "#~D\"~A\"#~%" idx (car tr)))))

(defun dictionary-lookup (index)
  (multiple-value-bind (translation found)
      (gethash index *translations-dictionary*)
    (unless found (error "The requested translation does not exist."))
    (let ((tr (cadr (assoc *language* translation))))
      (unless tr (error "The requested translation was not found for language: ~S." *language*))
      tr)))

(defun translator (stream sub-char numarg)
  "Read all the characters between #\" and \"# returning them as a
  string if *language* is 'american-english, otherwise replacing them
  with the string found in the translation dictionary at index NUMARG.
  If no translation is found, signal an error."
  (declare (ignore sub-char))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (if (eql *language* 'american-english)
        (coerce (nreverse chars) 'string)
        (if (null numarg)
            (error "A translation index is required.")
            (dictionary-lookup numarg)))))

(set-dispatch-macro-character #\# #\" #'translator)
