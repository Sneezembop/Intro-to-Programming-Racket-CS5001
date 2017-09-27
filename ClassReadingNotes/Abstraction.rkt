;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Abstraction) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))
;; [X -> Y] [List-of-X] -> [List-of-Y]
;; perform a function to every member of a list
(check-expect (map-op add1 (list 1 2 3)) (list 2 3 4))
(check-expect (map-op append-to (list "to" "from" "bloo"))
              (list "toto" "tofrom" "tobloo"))
(check-expect (map-op posn-x (list (make-posn 30 30)
                                   (make-posn 50 70)))
              (list 30 50))
(check-expect (map-op build-cir (list 5 10)) (list (circle 5 "solid" "red")
                                                   (circle 10 "solid" "red")))
(define (map-op op alot)
  (cond [(empty? alot) '()]
        [(cons? alot) (cons (op (first alot))
                            (map-op op (rest alot)))]))


;; String -> String
;; append "to" to a string
(check-expect (append-to "blah") "toblah")
(define (append-to str)
  (string-append "to" str))

;; Number -> Image
;; creates a circle with a certain radius
(check-expect (build-cir 5) (circle 5 "solid" "red"))
(define (build-cir num)
  (circle num "solid" "red"))



;; [X -> Boolean] [List-of X] -> [List-of X]
;; conditionally keeps some items in a list
(check-expect (keep-items even? '()) '())
(check-expect (keep-items even? '(1 2 3 4)) '(2 4))
(define (keep-items op alot)
  (cond [(empty? alot) '()]
        [(cons? alot) (if (op (first alot))
                          (cons (first alot) (keep-items op (rest alot)))
                          (keep-items op (rest alot)))]))


;; [Number Number -> Number] [List-of Numbers] -> Number
;; performs the operation on the entire list
(check-expect (process + 0 (list 1 2 3)) 6)
(check-expect (process * 1 (list 1 2 3)) 6)
(define (process op ec alot)
  (cond [(empty? alot) ec]
        [(cons? alot) (op (first alot) (process op ec (rest alot)))]))
