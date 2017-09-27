;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Sets) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))
;; #############################################
;; SETS PROBLEM SET
;; #############################################

;; From the prompt:
;; A Set is a collection of items.
;; Your sets should be able to hold any Racket value.
;; There elements are not sorted and there is no order.
;; However every element in a set is unique, i.e., there are no duplicates.


;; A Set is one of:
;; - '()
;; - (cons RValue Set)
;; CONSTRAINTS:
;; A Set cannot contain multiples of the same element
;; A Set is not sorted, there is no order


;; An RValue is any native value in Racket, some examples are:
;; - Number
;; - String
;; - Symbol
;; - Posn

;; Template : Set -> ???
#;(define (temp-set aset)
    (cond [(empty? aset) ...]
          [(cons? aset) ... (first aset)...
                        ... (temp-set (rest aset))]))


(define set0 '())
(define set1 (list 1 3 'a "bloo" 5))
(define set2 (list 4 3 'a 'b 'c 'd))

;; set-add : Set RValue -> Set
(check-expect (set-add set0 1) (list 1))
(check-expect (set-add set1 1) set1)
(define (set-add aset aval)
  (if (set-contains? aset aval)
      aset
      (cons aval aset)))

;; set-remove : Set RValue -> Set
(check-expect (set-remove set0 1) set0)
(check-expect (set-remove set1 3) (list 1 'a "bloo" 5))
(define (set-remove aset aval)
  (cond [(empty? aset) '()]
        [(cons? aset) (if (equal? (first aset) aval)
                          (set-remove (rest aset) aval)
                          (cons (first aset) (set-remove (rest aset) aval)))]))


;; set-contains? : Set RValue -> Boolean
;; checks to see if a set contains a specific element
(check-expect (set-contains? set0 'a) #false)
(check-expect (set-contains? set1 'a) #true)
(define (set-contains? aset aval)
  (cond [(empty? aset) #false]
        [(cons? aset) (or (equal? aval (first aset))
                          (set-contains? (rest aset) aval))]))


;; set-equals? : Set Set -> Boolean
;; checks to see if two sets are equal
(check-expect (set-equals? set1 set1) #true)
(check-expect (set-equals? set1 set2) #false)
(define (set-equals? aset bset)
  (if (= (set-size aset) (set-size bset))
      (set-equal-check aset bset)
      #false))

;; set-equal-check : Set Set -> Boolean
;; performs the helper operation for the set-equals?
(check-expect (set-equals? set1 set1) #true)
(check-expect (set-equals? set1 set2) #false)
(define (set-equal-check aset bset)
  (cond [(empty? aset) #true]
        [(cons? aset) (and (set-contains? bset (first aset))
                           (set-equal-check (rest aset) bset))]))

;; set-size : Set -> Number
;; returns the number of elements in a set
(check-expect (set-size set0) 0)
(check-expect (set-size set2) 6)
(define (set-size aset)
  (cond [(empty? aset) 0]
        [(cons? aset) (+ 1 (set-size (rest aset)))]))


;; set-union : Set Set -> Set
;; returns a set containing all the elements contained in both sets
(check-expect (set-union set0 set1) set1)
(check-expect (set-union set1 set2) (list 1 "bloo" 5 4 3 'a 'b 'c 'd))
(define (set-union aset bset)
  (cond [(empty? aset) bset]
        [(cons? aset) (if (set-contains? bset (first aset))
                          (set-union (rest aset) bset)
                          (cons (first aset) (set-union (rest aset) bset)))]))


;; set-intersection : Set Set -> Set
;; returns a set containing only the elements contained in both sets
(check-expect (set-intersection set0 set1) set0)
(check-expect (set-intersection set1 set2) (list 3 'a))
(define (set-intersection aset bset)
  (cond [(empty? aset) '()]
        [(cons? aset) (if (set-contains? bset (first aset))
                          (cons (first aset)
                                (set-intersection (rest aset) bset))
                          (set-intersection (rest aset) bset))]))


;; set-symmetric-diff : Set Set -> Set
;; returns a set containing only elements unique to either original set
(check-expect (set-symmetric-diff set1 set2) (list 1 "bloo" 5 4 'b 'c 'd))
(check-expect (set-symmetric-diff set0 set1) set1)
(define (set-symmetric-diff aset bset)
  (set-remove-set (set-union aset bset) (set-intersection aset bset)))

;; set-remove-set : Set Set -> Set
;; removes a set of elements from another set
;; INTERP: aset is the set you're keeping, bset is the set you're removing
(check-expect (set-remove-set set1 set0) set1)
(check-expect (set-remove-set set1 (list 1 "bloo")) (list 3 'a 5))
(define (set-remove-set aset bset)
  (cond [(empty? bset) aset]
        [(cons? bset) (set-remove
                       (set-remove-set aset (rest bset))
                       (first bset))]))

;; is-set? : Anything -> Boolean
;; determines if the input is a Set
(check-expect (is-set? set0) #true)
(check-expect (is-set? set1) #true)
(check-expect (is-set? set2) #true)
(check-expect (is-set? 1) #false)
(define (is-set? aset)
  (if (list? aset)
      (cond [(empty? aset) #true]
            [(cons? aset) (and (not (set-contains? (rest aset) (first aset)))
                               (is-set? (rest aset)))]) #false))
