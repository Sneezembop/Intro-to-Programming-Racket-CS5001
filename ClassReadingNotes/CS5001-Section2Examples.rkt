;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname CS5001-Section2Examples) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp")) #f)))
(define (area-of-disk r)
  (* 3.14 (sqr r)))

(check-expect (area-of-disk 5) 78.5)

(define (area-of-ring outer inner)
  (- (area-of-disk outer) (area-of-disk inner)))
(check-expect (area-of-ring 3 2) 15.7)

;; Number -> Number
;; converts farhenheit to celsius
(define (fahrenheit->celsius input)
  (* (- input 32) (/ 5 9)))
(check-expect (fahrenheit->celsius -40) -40)


;; Number -> Number
;; converts dollars to euros
(define (dollar->euro dollars)
  (* dollars 0.89))

(check-expect (dollar->euro 1) 0.89)

;; Number, Number -> Number
;; calculates the area of a triangle
(define (area-triangle base height)
  (* 1/2 (* base height)))
(check-expect (area-triangle 1 2) 1)

;; Number, Number, Number -> Number
;; Converts three digits to a number in reverse order
(define (convert3 singles tens hundreds)
  (+ singles (+ (* tens 10) (* hundreds 100))))
(check-expect (convert3 1 2 3) 321)

;; Number -> Number
;; A mathmatical formula
(define (f1 n)
  (+ 10 (sqr n)))
(check-expect (f1 1) 11)

(define (wage h)
  (* 12 h))

(define (tax h)
  (* 0.15 (wage h)))

(define (netpay h)
  (- (wage h) (tax h)))

(define (sum-coins pennies nickles dimes quarters)
  (+ (* pennies 0.01)
     (+ (* nickles 0.05)
        (+ (* dimes 0.1)
           (* quarters 0.25)))))

(define (total-profit attendees)
  (- (* (- 5 .5) attendees) 20))
