;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname general-recursion) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))
(define SMALL 20)

;; Number -> Image
;; create Sierpinski triangles until size SMALL

;; Generative: create three triangles of size side/2
;; and place two on the bottom and one on top

;; Trivial Case: side length is <= SMALL

;; Termination:


(check-expect (sierpinski SMALL) (triangle SMALL 'outline 'red))
(check-expect (sierpinski (* 2 SMALL))
              (above (triangle SMALL 'outline 'red)
                     (beside (triangle SMALL 'outline 'red)
                             (triangle SMALL 'outline 'red))))
(define (sierpinski side)
  (cond [(<= side SMALL) (triangle side 'outline 'red)]
        [else (local [(define half-sized (sierpinski (/ side 2)))]
                (above half-sized (beside half-sized half-sized)))]))


;; Natural Natural -> Natural
;; computes the greatest common divisior of two numbers
(check-expect (great-com-div 10 5) 5)
(check-expect (great-com-div 12 8) 4)
(check-expect (great-com-div 7 3) 1)
(define (great-com-div num1 num2)
  (local [(define (gcd? acc)
            (cond [(= acc 1) 1]
                  [(and (= 0 (modulo num1 acc)) (= 0 (modulo num2 acc))) acc]
                  [else (gcd? (- acc 1))]))]
    (gcd? (min num1 num2))))


;; Natural Natural -> Natural
;; computes the greatest common divisior of two numbers better
(check-expect (great-com-div2 10 5) 5)
(check-expect (great-com-div2 12 8) 4)
(check-expect (great-com-div2 7 3) 1)
(define (great-com-div2 num1 num2)
  (local [(define (gcd? anum bnum)
            (cond [(zero? (remainder anum bnum)) (min anum bnum)]
                  [else (gcd? (min anum bnum) (remainder anum bnum))]))]
    (gcd? num1 num2)))
