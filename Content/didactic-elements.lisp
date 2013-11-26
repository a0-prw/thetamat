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

;;indirect didactic elements are for cases where there is a risk of
;;repeating the same question, if the action just selects 'blindly'
;;from the question space.  Indirect action is much more expensive,
;;since it creates a local set (has to be local because hunchentoot is
;;threaded) of all the possible operands and removes each random
;;choice from the set, ==> so no repeats, but don't use it if there is
;;a big question set.  Also, NB: calling indirect acions from a lesson
;;is disgustingly inefficient, since they only get called once per
;;call from user.  The 'mod-digs-nat lesson calls digadd's action. mea
;;culpa, prw.  Solution: have non-display didactic elements for quick calls

(define-indirect-didactic-element 
    'digadd ;;tag
    "Addition" ;;caption
    "$x+y, \\; 0\\leq x,y < 10, \\; x,y \\in \\mathbb{N}$" ;;short description
    '((american-english "Addition with nonnegative digits.")
      (danish "Addition med ikke-negative cifre"));;long description
  0 ;;menu-level
  1 ;;required pass (1 == 100%)
  (define-indirect-action
      (loop for i from 0 to 9
         append (loop for j from i to 9
                     collect (list i j)))
      (make-sum (digit-nat) (op-plus) nil nil)) ;;action
  'natural ;;return type
  2 ;;max input length
  '(mod-digs-nat)) ;;home module


;; (define-didactic-element 
;;     'digadd ;;tag
;;     "Addition" ;;caption
;;     "$x+y, \\; 0\\leq x,y < 10, \\; x,y \\in \\mathbb{N}$" ;;short description
;;   "Addition with nonnegative digits." ;;long description
;;   0 ;;menu-level
;;   1 ;;required pass (1 == 100%)
;;   #'(lambda () (make-sum (digit-nat) (op-plus))) ;;action
;;   'natural ;;return type
;;   2 ;;max input length
;;   '(mod-digs-nat)) ;;home module

;; (define-didactic-element 
;;     'digadd-nz
;;     "Addition"
;;   "$x+y, \\; 0<x,y< 10, \\; x,y \\in \\mathbb{N}$"
;;   "Addition with positive digits."
;;   0
;;   1
;;   #'(lambda () (make-sum (digit-nz-nat) (op-plus)))
;;   'natural
;;   2
;;   nil)

(define-didactic-element
    'digadd-nz
    "Addition"
  "$x+y, \\; 0<x,y< 10, \\; x,y \\in \\mathbb{N}$"
  '((american-english "Addition with positive digits.")
    (danish "Addition med ikke-negative cifre."))
  0
  1
  #'(lambda () (make-sum (digit-nz-nat) (op-plus)))
  'natural
  2
  nil)

(define-indirect-didactic-element
    'tiny-nc-plus
    "Addition"
  "$x+y,\\; 0\\leq x,y<20, \\; x,y \\in \\mathbb{N}$"
  '((american-english "Addition of natural numbers less than 20 (no power of ten regrouping, ie, no so-called carrying).")
    (danish "Addition af naturlige tal under 20 uden tier-overgang."))
  0
  9/10
  (define-indirect-action 
      (loop for i from 0 to 9
         append (loop for j from i 
                   until (>= (+ i j) 10)
                   append `((,i ,j)
                            (,(+ 10 i) ,j)
                            (,(+ 10 i) ,(+ 10 j)))))
      (make-sum (tiny-nc-nat) (op-plus) nil nil))
  ;;#'(lambda () (make-sum (tiny-nc-nat) (op-plus)))
  'natural
  2
  nil)

;; (define-didactic-element
;;     'tiny-nc-plus
;;     "Addition"
;;   "$x+y,\\; 0\\leq x,y<20, \\; x,y \\in \\mathbb{N}$"
;;   "Addition of natural numbers less than 20 (no power of ten overflow, ie, no 'borrowing')."
;;   9/10
;;   #'(lambda () (make-sum (tiny-nc-nat) (op-plus)))
;;   'natural
;;   2
;;   nil)

(define-indirect-didactic-element
    'tiny-c-plus
    "Addition"
  "$x+y,\\; 0\\leq x,y<20, \\; x,y \\in \\mathbb{N}$"
  '((american-english "Addition of natural numbers less than 20 (possibly with power of ten regrouping in the units, ie, so-called carrying).")
    (danish "Addition af naturlige tal under 20 muligvis med tier-overgang."))
  0
  9/10
  (define-indirect-action 
      (loop for i from 10 to 19
         append (loop for j from i to 19
                   collect (list i j)))
      (make-sum (tiny-c-nat) (op-plus) nil nil))
  ;;#'(lambda () (make-sum (tiny-c-nat) (op-plus)))
  'natural
  2
  nil)

(define-internal-didactic-element
    '%tiny-c-plus
    "Addition"
  "$x+y,\\; 0\\leq x,y<20, \\; x,y \\in \\mathbb{N}$"
  '((american-english "This is for use on server. Not recommended.")
    (danish "Til brug på serveren, intern funktion."))
  0
  9/10
  #'(lambda () (make-sum (tiny-c-nat) (op-plus)))
  'natural
  2
  nil)

(define-didactic-element
    'small-c-plus
    "Addition"
  "$x+y,\\; 0\\leq x,y<50, \\; x,y \\in \\mathbb{N}$"
  '((american-english "Addition of natural numbers less than 50 (possibly with power of ten regrouping, ie, so-called carrying).")
    (danish "Addition af naturlige tal under 50 muligvis med tier-overgang."))
  0 
  9/10
  #'(lambda () (make-sum (small-c-nat) (op-plus)))
  ;;#'(lambda () (make-sum (tiny-c-nat) (op-plus)))
  'natural
  2
  nil)

(define-didactic-element 
    'addnat-lt20
    "Addition"
  "$x+y,\\; 0\\leq x,y<20, \\; x,y \\in \\mathbb{N}$"
  '((american-english "Addition with natural numbers less than 20.")
    (danish "Addition af naturlige tal under 20."))
  0
  9/10
  #'(lambda () (make-sum (tiny-nat) (op-plus)))
  'natural
  2
  '(mod-basic-add-nat))

(define-didactic-element 
    'addnat-lt100
    "Addition"
  "$x+y,\\; 0\\leq x,y<100, \\; x,y \\in \\mathbb{N}$"
  '((american-english "Addition with natural numbers less than 100.")
    (danish "Addition af naturlige tal under 100."))
  0
  9/10
  #'(lambda () (make-sum (medium-nat) (op-plus)))
  'natural
  3
  '(mod-position-sys1-nat))

(define-didactic-element
    'addnat-lt1000
    "Addition"
  "$x+y,\\; 0\\leq x,y<1000, \\; x,y \\in \\mathbb{N}$"
  '((american-english "Addition with natural numbers less than 1000.")
    (danish "Addition af naturlige tal under 1000."))
  0
  9/10
  #'(lambda () (make-sum (big-nat) (op-plus)))
  'natural
  4
  '(mod-sub-position-sys-nat))

(define-didactic-element
    'addnat-3lt10000
    "Addition"
  "$x+y+z,\\; 0\\leq x,y,z<10000, \\; x,y,z \\in \\mathbb{N}$"
  '((american-english "Addition with three natural numbers less than 10000.")
    (danish "Addition af tre naturlige tal under 10000."))
  0
  8/10
  #'(lambda () (make-sum (verybig-nat) (op-plus) t))
  'natural
  5
  '(mod-position-sys2-nat))

(setf (n-ex (get-didactic-element 'addnat-3lt10000)) 5)
;; Subtraction FIXME depends on addition

(define-indirect-didactic-element
    'digsubn
    '((american-english "Subtraction")
      (danish "Subtraktion"))
    "$x-y=z\\mid  x,y<10, \\; x,y,z \\in \\mathbb{N}$"
    '((american-english "Subtraction of digits with result a natural number.")
      (danish "Subtraktion af cifre med resultat et naturligt tal."))
  0
  1
  (define-indirect-action  
      (loop for i from 9 downto 1
         append (loop for j from i downto 1
                   unless (zerop (+ i (- j))) 
                   collect (list i (- j))))
      ;; (loop for i from 9 downto 1
      ;;    append (loop for j from i downto 1
      ;;              collect (list i (- j))))
      (make-sum (tiny-eint-nat) (op-minus) nil nil))
  'integer
  1
  '(mod-digs-nat))


(define-indirect-didactic-element
    'teen-sub-nb
    '((american-english "Subtraction")
      (danish "Subtraktion"))
    "$x-y=z\\mid 10\\leq x,y < 20, \\; x,y,z\\in \\mathbb{N}$"
    '((american-english "Subtraction (not requiring the regrouping of a higher power of ten, ie, so-called borrowing) of numbers between 10 and 20 with result a natural number.")
      (danish "Subtraktion af tal mellem 10 og 20 med resultat et naturligt tal, uden behov for tier-omgruppering."))
  0
  1
  ;;#'(lambda () (make-sum (digit-tsub-nat) (op-minus)))
  (define-indirect-action 
      (loop for i from 1 to 9
         append (loop for j from i downto 1
                   append (list  (list (+ 10 i) (- j))
                                 (list (+ 10 i) (- (+ 10 j))))))
      (make-sum (digit-tsub-nat) (op-minus) nil nil))
  'natural
  1
  nil)

;; (define-didactic-element
;;     'digsubn-nz
;;     "Subtraction"
;;     "Subtraction with nonzero, nonnegative digits with result &gt;
;; -1."
;;   "Subtraction with nonzero, nonnegative digits with result a
;;   nonnegative integer."  1
;;   #'(lambda () (make-sum (digit-int-nat) (op-minus)))
;;   'integer
;;   1
;;   nil)

;;fixme add nlinedig depends on digsubn

;;Fixme subnat-lt20
;; (define-didactic-element  ;;fucked up numbers chosen
;;     'subnat-lt20
;;     "Subtraction"
;;   "$x-y=z\\mid x,y < 20, \\; x,y,z\\in \\mathbb{N}$"
;;   "Subtraction of natural numbers less than 20 with the result a natural number (possibly with exchange of higher power of ten, ie, so-called borrowing)."
;;   0
;;   1
;;   #'(lambda () (make-sum (tiny-eint-nat) (op-minus)))
;;   'integer
;;   4
;;   '(mod-basic-sub-nat))

(define-indirect-didactic-element  
    'subnat-lt20 ;;(make-sum (tiny-eint-nat) (op-minus)))
    '((american-english "Subtraction")
      (danish "Subtraktion"))
  "$x-y=z\\mid x,y < 20, \\; x,y,z\\in \\mathbb{N}$"
  '((american-english "Subtraction of natural numbers less than 20 with the result a natural number (possibly with regrouping of higher power of ten, ie, so-called borrowing).")
    (danish "Subtraktion af naturlige tal under 20 med resultat et naturligt tal, muligvis med behov for tier-omgruppering."))
  0
  1
  #'(lambda () 
      (let ((els 190)
            (arr (make-array 190 :initial-contents 
                          (loop for i from 19 downto 1
                             append (loop for j from i downto 1
                                   collect (list i (- j)))))))
        (lambda ()
          (let ((it (aref arr (random els)))
                (obj (make-sum (tiny-eint-nat) (op-minus) nil nil)))
            (decf els)
            (setf arr (delete it arr))
            (setf (slot-value (slot-value obj 'operands) 'list) it)
            obj))))
  'integer
  4
  '(mod-basic-sub-nat))


(define-didactic-element
    'subnat-easy
    '((american-english "Subtraction")
      (danish "Subtraktion"))
  "$x-y=z\\mid x,y < 100, \\; x,y,z\\in \\mathbb{N}$"
  '((american-english "Subtraction of natural numbers less than 100 with all the digits of the subtrahend less than or equal to the corresponding digits of the minuend (ie, no regrouping of powers of ten or so-called borrowing).")
    (danish "Subtraktion af naturlige tal under 100 med resultat et naturligt tal, uden behov for tier-omgruppering."))
  0
  9/10
  #'(lambda () (make-sum (medium-eint-nat) (op-minus)))
  'integer
  3
  '(mod-position-sys1-nat))

(define-didactic-element
    'small-b-minus
    '((american-english "Subtraction")
      (danish "Subtraktion"))
  "$x-y,\\; 50<x<100,y<50, \\; x,y \\in \\mathbb{N}$"
  '((american-english "Subtraction of natural numbers with result less than 50 (with regrouping of power of ten, ie, so-called borrowing).")
    (danish "Subtraktion af naturlige tal med resultat mindre end 50, med behov for tier-omgruppering."))
  0
  9/10
  #'(lambda () (make-sum (small-b-nat) (op-minus)))
  ;;#'(lambda () (make-sum (tiny-c-nat) (op-plus)))
  'natural
  2
  nil)

(define-didactic-element
    'subnat-z-100
    '((american-english "Subtraction")
      (danish "Subtraktion"))
  "$x-y,\\; x=100,y<100, \\; x,y \\in \\mathbb{N}$"
  '((american-english "Subtraction of natural numbers with result less than 100 (with repeated regrouping of power of ten, ie, so-called borrowing).")
    (danish "Subtraktion af naturlige tal med resultat under 100, med behov for tier-omgruppering."))
  0
  9/10
  #'(lambda () (make-sum (medium-z-nat) (op-minus)))
  'natural
  2
  nil)

(define-didactic-element
    'subnat-lt100
    '((american-english "Subtraction")
      (danish "Subtraktion"))
  "$x-y=z\\mid x,y < 100, \\; x,y,z\\in \\mathbb{N}$"
  '((american-english "Subtraction of natural numbers less than 100 with result a natural number (possibly with regrouping of higher power of ten, ie, so-called borrowing).")
    (danish "Subtraktion af naturlige tal mindre end 100 med resultat et naturligt tal, muligvis med behov for tier-omgruppering."))
  0
  9/10
  #'(lambda () (make-sum (medium-int-nat) (op-minus)))
  'integer
  3
  '(mod-sub-position-sys-nat))

(define-didactic-element
    'subnat-lt1000
    '((american-english "Subtraction")
      (danish "Subtraktion"))
    "$x-y=z\\mid x,y < 1000, \\; x,y,z\\in \\mathbb{N}$"
    '((american-english "Subtraction of natural numbers less than 1000 with result a natural number (possibly with regrouping of higher power of ten, ie, so-called borrowing).")
      (danish "Subtraktion af naturlige tal mindre end 1000 med resultat et naturligt tal, muligvis med behov for tier-omgruppering."))
  0
  9/10
  #'(lambda () (make-sum (big-int-nat) (op-minus)))
  'integer
  4
  '(mod-position-sys2-nat))
  

(define-didactic-element
    'nline20
    '((american-english "Placement")
      (danish "Placering"))
  "$x+y=z \\mid x,y<10, x,y,z\\in \\mathbb{Z}$"
  '((american-english "Placement on the numberline between -20 and 20 at a given distance from a given number")
    (danish "Placering på tallinien mellem -20 og 20 af et givet tal på et givet afstand."))
  0
  1
  #'(lambda () (make-nline (digit-int-nline) (before-or-after)))
  'integer
  2
  '(mod-basic-int))

(define-didactic-element
    'addint20
    "Addition"
    "$x+y,  \\; \\lvert x \\rvert ,\\lvert y \\rvert \\leq 20, \\; x,y\\in \\mathbb{Z}$"
    '((american-english "Addition and subtraction of integers between -20 and 20.")
      (danish "Addition og subtraktion af hele tal mellem -20 og 20."))
  0
  9/10
  #'(lambda () (make-sum (tiny-int) (op-plus)))
  'integer
  2
  '(mod-basic-int))

;;decimal addition and subtraction

(define-didactic-element
    'adddec-lt10
    "Addition"
  "$x+y, \\;  x,y\\leq 10, \\; x,y\\in \\mathbb{Q}^{+}$"
  '((american-english "Addition using the decimal representation of positive rational numbers less than 10.  Only tenths (ie, 1 decimal place).")
    (danish "Addition af decimale repræsenationer af positive rationale tal under 10.  Kun tiendedele (dvs, et tal efter kommaet.)"))
  2
  9/10
  #'(lambda () (make-sum (digit-podec) (op-plus)))
  'decimal
  4
  '(mod-basic-dec))

(define-didactic-element
    'subdec-lt10
    '((american-english "Subtraction")
      (danish "Subtraktion"))
    "$x-y, \\; x,y\\leq10, \\;  x,y\\in \\mathbb{Q}$"
    '((american-english "Subtraction using the decimal representation of rational numbers less than 10.  Only tenths (ie, 1 decimal place).")
      (danish "Subtraktion af decimale ræpresentationer af rationale tal under 10. Kun tiendedele (dvs. et tal efter kommaet.)"))
    2
    9/10
    #'(lambda () (make-sum (digit-negdec) (op-minus)))
    'decimal
    4
    '(mod-basic-dec))

;; multiplication FIXME mult of 10s and mul with powers of ten

;;FIXME explicit position system exercises.

(define-didactic-element
    'table
    '((american-english "Multiplication")
      (danish "Multiplikation"))
    "$x\\cdot y, \\;  xy\\leq 10, \\; x,y\\in \\mathbb{N}^{+}$"
    '((american-english "Multiplication of digits and 10.")
      (danish "Multiplikation af cifre og 10."))
  1
  1
  #'(lambda () (make-product (table) (op-times)))
  'natural
  3
  ;;pow10 multpow10
  '(mod-table)) ;;mod-rat

(define-didactic-element
    'mul-1-2nat
    '((american-english "Multiplication")
      (danish "Multiplikation"))
    "$x\\cdot y, \\; x<10, y<100, x,y\\in \\mathbb{N}$"
    '((american-english "Multiplication of digit by a natural number less than 100.")
      (danish "Multiplikation af et ciffer og et naturligt tal under 100.")) 
    1
    9/10
    #'(lambda () (make-product (digit-by-2nat) (op-times)))
    'natural
    3
    '(mod1-mult))

(define-didactic-element
    'mul-digints
    '((american-english "Multiplication")
      (danish "Multiplikation"))
  "$x\\cdot y, \\; \\lvert x \\rvert ,\\lvert y \\rvert<10, \\; x,y\\in \\mathbb{Z}, $"
  '((american-english "Multiplication of 2 integer digits.")
    (danish "Multiplikation af 2 cifre som er hele tal."))
  1
  1
  #'(lambda () (make-product (digit-int) (op-times)))
  'integer
  4
  '(mod2-int))

(define-didactic-element
    'mul-2-2nat
    '((american-english "Multiplication")
      (danish "Multiplikation"))
  "$x\\cdot y, \\; x,y<100, x,y\\in \\mathbb{N}$" 
  '((american-english "Multiplication of 2 natural numbers less than 100.")
    (danish "Multiplikation af to naturlige tal under 100."))
  1
  9/10
  #'(lambda () (make-product (medium-nat) (op-times)))
  'natural
  5
  '(mod2-mult))

(define-didactic-element
    'mul-2-2int
    '((american-english "Multiplication")
      (danish "Multiplikation"))
     "$x\\cdot y, \\;  \\lvert x \\rvert ,\\lvert y \\rvert<100, \\; x,y\\in \\mathbb{Z}$"
     '((american-english "Multiplication of 2 integers with absolute values less than 100.")
       (danish "Multiplikation af to hele tal med absolutte værdier under 100."))
  1
  9/10
  #'(lambda () (make-product (medium-int) (op-times)))
  'integer
  5
  '(mod2-mult))

(define-didactic-element
    'div-int-tinyint
    "Division"
     "$xy\\div y, \\;  \\lvert x \\rvert ,\\lvert y \\rvert<20, \\; x,y\\in \\mathbb{Z}$"
     '((american-english "Division of the product of 2 integers between -20 and 20, by one of the integers.")
       (danish "Division af produktet af to hele tal mellem -20 og 20 med et af tallene."))
  1
  9/10
  #'(lambda () (make-product (tiny-div-int) (op-divided-by)))
  'integer
  5
  '(mod1-div-int))

(define-didactic-element
    'div-int-mediumint
    "Division"
     "$xy\\div y, \\;  \\lvert x \\rvert ,\\lvert y \\rvert<100, \\; x,y\\in \\mathbb{Z}$"
     '((american-english "Division of the product of 2 integers between -100 and 100, by one of the integers.")
       (danish "Division af to hele tal mellem -100 og 10 med et af tallene."))
  1
  9/10
  #'(lambda () (make-product (medium-div-int) (op-divided-by)))
  'integer
  5
  '(mod2-div-int))


(define-didactic-element
    'mul-digit-podec
    '((american-english "Multiplication")
      (danish "Multiplikation"))
    "$x\\cdot y, \\; x,y\\in \\mathbb{Q}^{+}$"
    '((american-english "Multiplication of the decimal representation of 2 positive rationals (only tenths, ie, max. 1 decimal place).")
      (danish "Multiplikation af de decimale ræpresentationer af to positive rationale tale (kun tiendedele, dvs. maks. et tal efter kommaet)."))
    2
    9/10
    #'(lambda () (make-product (digit-podec) (op-times)))
    'decimal
    5
    '(mod3-mult))    

(define-didactic-element
    'mul-digit-negdec
    '((american-english "Multiplication")
      (danish "Multiplikation"))
    "$x\\cdot y, \\; x,y\\in \\mathbb{Q}$"
    '((american-english "Multiplication of the  decimal representation of 2 rationals (only tenths, ie, max. 1 decimal place).")
      (danish "Multiplikation af de decimale ræpresentationer af to rationale tal (kun tiendeele, dvs. maks. et tal efter kommaet."))
    2
    9/10
    #'(lambda () (make-product (digit-negdec) (op-times)))
    'decimal
    6
    '(mod3-mult))

;; TODO a didactic progression for rational arithmetic

(define-didactic-element
    'add-sub-digit-pfrac
    "Addition"
  "$\\frac{a}{b}+\\frac{c}{d} \\; a,b,c,d < 10 \\in \\mathbb{N}, \\frac{a}{b}, \\frac{c}{d}\\in \\mathbb{Q}$"
  '((american-english "Addition of 2 proper fractions with numerator and denominator digits")
    (danish "Addition af to ægte brøker med nævner og tæller cifre."))
  2
  9/10
  #'(lambda () (make-sum (digit-prop-frac-signed) (op-plus)))
  'rational
  7
  '(mod-add/sub-pfrac))

;;FIXME cleanup the mess after learning-space build.

