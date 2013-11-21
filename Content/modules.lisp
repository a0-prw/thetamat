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
    "The Numberline - 1"
  "Introduction to the integer numberline, with addition and
subtraction of positive digits with results a natural number."  
  nil
  :special-module t)

(define-module 'mod-basic-add-nat
    "Basic Addition"
    "Introduction to the decimal position system with addition of
numbers less than 20 with results a natural number."  
'(mod-digs-nat))

(define-module 'mod-basic-sub-nat
    "Basic Subtraction"
    "Continued introduction to the decimal position system with
subtraction  of numbers less than 20 with results a
natural number (no regrouping of higher powers of ten, aka borrowing)."
'(mod-basic-add-nat))

(define-module 'mod-position-sys1-nat
    "Addition and Subtraction - 1"
    "Addition and subtraction of numbers less than 100 with results a
natural number (no regrouping of higher power of 10, aka borrowing)."
'(mod-basic-sub-nat))

(define-module 'mod-sub-position-sys-nat
    "Addition and Subtraction - 2"
    "Subtraction (with regrouping of higher power of 10) of
numbers less than 100 and addition of natural numbers less than 1000,
with results a natural number." 
    '(mod-position-sys1-nat))

(define-module 'mod-position-sys2-nat
    "Addition and subtraction - 3"
    "Addition of natural numbers less than 10000
of 3 terms. Subtraction of natural numbers 
less than 1000. Result a natural number."
  '(mod-sub-position-sys-nat))

(define-module 'mod-basic-int
    "Negative Numbers - 1"
    "Addition of small integers with result possibly negative."
  '(mod-position-sys2-nat))

(define-module 'mod-table
    "Basic Multiplication"
    "Multiplication of natural numbers less than 11."
  '(mod-basic-int)
  :special-module t)

(define-module 'mod1-mult
    "Multiplication - 1"
    "The Distributive law of multiplication over addition: 
Multiplication of 1-digit by 2-digit natural numbers"
  '(mod-table))

(define-module 'mod2-int
    "Negative Numbers - 2"
  "Negative numbers: Multiplication.  The rules for
multiplying mixed-sign and negative digits."
  '(mod1-mult))
;;" and multiplication of signed digits."

;;FIXME 2d-1d division

(define-module 'mod2-mult
    "Multiplication - 2"
    "Multiplication of 2-digit by 2-digit natural numbers and integers"
  '(mod2-int))

(define-module 'mod1-div-int
    "Division - 1"
  "Division of integers by integers with result an integer."
  '(mod2-mult))

(define-module 'mod2-div-int
    "Division - 2"
  "Long division method: Division of integers by integers with result an integer."
  '(mod1-div-int))

;;FIXME rational-numbers module for decimal representations and fractions

(define-module 'mod-basic-dec
    "Signed Decimals"
    "Addition and subtraction of signed decimals
less than 10."
  '(mod2-div-int))

(define-module 'mod-add/sub-pfrac
    "Proper Fractions"
    "Addition and subtraction of proper fractions with 
denominator a digit"
  '(mod-basic-dec))

;;Fixme multiplication of fractions

(define-module 'mod3-mult
    "Multiplication - 3"
    "Multiplication of small (2-digit) signed decimals"
  '(mod-add/sub-pfrac))


  
