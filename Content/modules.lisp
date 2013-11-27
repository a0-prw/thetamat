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

;;Replace with games
(define-module 'mod-digs-nat
    '((american-english "The Numberline - 1")
      (danish "Tallinien - 1"))
  '((american-english "Introduction to the integer numberline, with addition and subtraction of positive digits with results a natural number.")
    (danish "Introduktion til tallinien med hele tal, med addition og subtraktion af positive cifre med resutat et naturligt tal."))
  nil
  :special-module t)

(define-module 'mod-basic-add-nat
    '((american-english "Basic Addition")
      (danish "Grundlæggende Addition"))
  '((american-english "Introduction to the decimal position system with addition of numbers less than 20 with results a natural number.")
    (danish "Introduktion til titalssytemet med addition af tal under 20 med resultat et naturligt tal."))
'(mod-digs-nat))

(define-module 'mod-basic-sub-nat
    '((american-english "Basic Subtraction")
      (danish "Grundlæggende Subtraktion"))
  '((american-english "Continued introduction to the decimal position system with
subtraction  of numbers less than 20 with results a
natural number (no regrouping of higher powers of ten, aka borrowing).")
    (danish "Fortsat introduktion til titalssystemet, med subtraktion af tal under 20 uden behov for tier-omgruppering."))
'(mod-basic-add-nat))

(define-module 'mod-position-sys1-nat
    '((american-english "Addition and Subtraction - 1")
      (danish "Addition og Subtraktion - 1"))
  
  '((american-english "Addition and subtraction of numbers less than 100 with results a
natural number (no regrouping of higher power of 10, aka borrowing).")
    (danish "Addition og subtraktion af tal under 100 med resultat et naturligt tal uden behov for tier-omgruppering."))
  
'(mod-basic-sub-nat))

(define-module 'mod-sub-position-sys-nat
    '((american-english "Addition and Subtraction - 2")
      (danish "Addition og Subtraktion - 2"))

  '((american-english "Subtraction (with regrouping of higher power of 10) of
numbers less than 100 and addition of natural numbers less than 1000,
with results a natural number.")
    (danish "Subtraktion med behov for tier-omgruppering af naturlige tal under 100 og addition af naturlige tal under 1000"))

    '(mod-position-sys1-nat))

(define-module 'mod-position-sys2-nat
    '((american-english "Addition and subtraction - 3")
      (danish "Addition og Subtraktion - 3"))

  '((american-english "Addition of natural numbers less than 10000
of 3 terms. Subtraction of natural numbers 
less than 1000. Result a natural number.")
    (danish "Addition af tre naturlige tal under 10000."))

  '(mod-sub-position-sys-nat))

(define-module 'mod-basic-int
    '((american-english "Negative Numbers - 1")
      (danish "Negative Tal - 1"))

  '((american-english "Addition of small integers with result possibly negative.")
    (danish "Addition af små hele tal med resultat muligvis negativt."))

  '(mod-position-sys2-nat))

(define-module 'mod-table
    '((american-english "Basic Multiplication")
      (danish "Grundlæggende Multiplikation"))

  '((american-english "Multiplication of natural numbers less than 11.")
    (danish "Multiplikation af naturlige tal under 11."))

  '(mod-basic-int)
  :special-module t)

(define-module 'mod1-mult
    '((american-english "Multiplication - 1")
      (danish "Multiplikation - 1"))

  '((american-english "The Distributive law of multiplication over addition: 
Multiplication of 1-digit by 2-digit natural numbers")
    (danish "Den distributive lov af multiplikation over addition: Multiplikation af 1-cifer med 2-cifre naturlige tal."))

  '(mod-table))

(define-module 'mod2-int
    '((american-english "Negative Numbers - 2")
      (danish "Negative tal - 2"))

  '((american-english "Negative numbers: Multiplication.  The rules for
multiplying mixed-sign and negative digits.")
    (danish "Negative Tal: Multiplikation.  Lovene for multiplikation af positive og negative tal."))

  '(mod1-mult))
;;" and multiplication of signed digits."

;;FIXME 2d-1d division

(define-module 'mod2-mult
    '((american-english "Multiplication - 2")
      (danish "Multiplikation - 2"))

  '((american-english "Multiplication of 2-digit by 2-digit natural numbers and integers")
    (danish "Multiplikation af 2-cifrede naturlige tal med 2-cifrede naturlige tal og hele tal."))

  '(mod2-int))

(define-module 'mod1-div-int
    "Division - 1"
  '((american-english "Division of integers by integers with result an integer.")
    (danish "Division af hele tal med et helt tal med resultat et helt tal"))

  '(mod2-mult))

(define-module 'mod2-div-int
    "Division - 2"
  '((american-english "Long division method: Division of integers by integers with result an integer.")
    (danish "En metode til lang division:  Division af hele tal med resultat et helt tal."))

  '(mod1-div-int))

;;FIXME rational-numbers module for decimal representations and fractions

(define-module 'mod-basic-dec
    '((american-english "Signed Decimals")
      (danish "Positive og negative Decimaltal"))

  '((american-english "Addition and subtraction of signed decimals
less than 10.")
    (danish "Addition og subtraktion af positive og negative decimale"))

  '(mod2-div-int))

(define-module 'mod-add/sub-pfrac
    '((american-english "Proper Fractions")
      (danish "Ægte Brøker"))

  '((american-english "Addition and subtraction of proper fractions with 
denominator a digit")
    (danish "Addition og subtraktion af ægte brøker med nævneren et ciffer"))

  '(mod-basic-dec))

;;Fixme multiplication of fractions

(define-module 'mod3-mult
    '((american-english "Multiplication - 3")
      (danish "Multiplikation - 3"))

  '((american-english "Multiplication of small (2-digit) signed decimals")
    (danish "Multiplikation af positive og negative decimale (med tiendedele."))

  '(mod-add/sub-pfrac))


  
