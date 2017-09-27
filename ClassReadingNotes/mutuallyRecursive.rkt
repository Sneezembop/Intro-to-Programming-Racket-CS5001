;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mutuallyRecursive) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))

(define-struct person (name yob children))
;; A Person is a (make-person String Number LoC)


;; A LoC is one of
;; - '()
;; - (cons Person LoC)

;; MUTUALLY RECURSIVE TEMPLATES

#;(define (person-temp aper)
    (... (person-name aper) ...
         (person-yob aper) ...
         (loc-temp (person-children aper)) ...))

#; (define (loc-temp aloc)
     (cond [(empty? aloc) ...]
           [(cons? aloc) ...(person-temp (first aloc))
                         ...(loc-temp (rest aloc))]))


(define person1
  (make-person "Mike" 1958
               (list (make-person "Joe" 1988 '())
                     (make-person "Aaron" 1993
                                  (list (make-person "Blue" 2018 '()))))))

;; Person -> Number
;; count the decendants of the person
(check-expect (count-des person1) 3)
(check-expect (count-des (make-person "Blue" 2018 '())) 0)
(define (count-des aper)
  (count-children (person-children aper)))


(define (count-children aloc)
  (cond [(empty? aloc) 0]
        [(cons? aloc) (+ 1 (count-des (first aloc))
                         (count-children (rest aloc)))]))


;; An Atom is one of:
;; - Symbol
;; - String
;; - Number

;; An S-exp is one of:
;; - Atom
;; - LoS-exp

;; A LoS-exp is one of:
;; - '()
;; - (cons S-exp LoS-exp)


#;(define (atom-temp a)
    (cond [(symbol? a) ...]
          [(string? a) ...]
          [(number? a) ...]))

;; Any -> Boolean
;; is it an atom?
(check-expect (atom? 5) #true)
(check-expect (atom? person1) #false)
(define (atom? a)
  (or (symbol? a)
      (string? a)
      (number? a)))

#;(define (s-exp-temp s)
    (cond [(atom? s) ...(atom-temp s)]
          [(list? s) ...(los-temp s)]))

#;(define (los-temp alos)
    (cond [(empty? alos) ....]
          [(cons? alos) ... (s-exp-temp(first alos))
                        ... (los-temp (rest alos))]))

(define sexp1 '(+ 5 ( "a" "b" "c") (* 4 6)))

;; S-exp Atom -> Boolean
;; check to see if a given stom occurs in the s-exp
(check-expect (s-exp-find sexp1 5) #true)
(check-expect (s-exp-find sexp1 "d") #false)
(check-expect (s-exp-find sexp1 "a") #true)
(check-expect (s-exp-find sexp1 '*) #true)

(define (s-exp-find s a)
  (cond [(atom? s) (atom=? s a)]
        [(list? s) (los-find s a)]))

;; Atom Atom -> Boolean
;; are the atoms equal?
(check-expect (atom=? 'a 'b) #false)
(check-expect (atom=? 'a 'a) #true)
(check-expect (atom=? 'a 2) #false)
(define (atom=? s a)
  (cond [(and (symbol? s)(symbol? a)) (symbol=? s a)]
        [(and (string? s)(string? a)) (string=? s a)]
        [(and (number? s)(number? a)) (= s a)]
        [else #false]))

;; LoS-exp Atom -> Boolean
;; checks to see if a given atom is in the list of S-Exps
(check-expect (los-find '() 5) #false)
(check-expect (los-find '(5) 5) #true)
(define (los-find alos a)
  (cond [(empty? alos) #false]
        [(cons? alos) (or (s-exp-find (first alos) a)
                          (los-find (rest alos) a))]))
