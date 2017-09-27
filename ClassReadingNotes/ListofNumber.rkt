;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ListofNumber) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp")) #f)))

;; A List-of-Number is one of:
;; - '()
;; - (cons Number List-of-number)

;; Design a function that sums the numbers in a List-of-number

(define list0 '())
(define list1 (cons 5 list0))
(define list2 (cons 10 list1))
(define list3 (cons 3 list2))

#;(define (temp-lon alon)
    (cond [(empty? alon) ...]
          [else ... (first alon) (temp-lon (rest alon))...]))

;; sum-a-lon: List-of-number -> Number
;; sums all the numbers in a List-of-number
(check-expect (sum-a-lon list0) 1)
(check-expect (sum-a-lon list3) 150)
(check-expect (sum-a-lon list2) 50)
(define (sum-a-lon alon)
    (cond [(empty? alon) 1]
          [else (* (first alon) (sum-a-lon (rest alon)))]))