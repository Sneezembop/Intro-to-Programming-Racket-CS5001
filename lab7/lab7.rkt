;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))
; A Ball is a (make-ball Nat Mode Color [Nat -> Posn])
(define-struct ball (r mode color placement))
; - where r is the ball's radius
; - mode is the ball's mode
; - color is the ball's color
; - and placement is a function that, given the current time,
;   outputs a new coordinate for the ball to be drawn at
 
; A Mode is one of:
; - 'solid
; - 'outline

(define HEIGHT 500)
(define WIDTH 500)

(define BALL-1 (make-ball 5 'solid 'red
                          (λ (t) (make-posn 20 (modulo t HEIGHT)))))
(define BALL-2 (make-ball 7 'outline 'blue
                          (λ (t) (make-posn (modulo t WIDTH) 100))))

(define BG (empty-scene HEIGHT WIDTH))
; ball-temp : Ball -> ???
#;(define (ball-temp b)
  (... (ball-r b) ... (mode-temp (ball-mode b)) ...
       (ball-color b) ... (ball-placement b) ...))
 
; mode-temp : Mode -> ???
#;(define (mode-temp m)
  (... (cond [(symbol=? m 'solid) ...]
             [(symbol=? m 'outline) ...]) ...))

; A World is a (make-world Nat [List-of Ball])
(define-struct world (t balls))
; - where t is the amount of time that has passed
; - and balls is the balls of the world

(define WORLD-1 (make-world 0 '()))
(define WORLD-2 (make-world 10 (list BALL-1 BALL-2)))

; world-temp : World -> ???
#;(define (world-temp w)
  (... (world-t w) ... (ball-list-temp (world-balls w)) ...))
 
; ball-list-temp : [List-of Ball] -> ???
#;(define (ball-list-temp alob)
  (... (cond [(empty? alob) ...]
             [(cons? alob)
              ... (ball-temp (first alob)) ...
              ... (ball-list-temp (rest alob)) ...]) ...))

; main : [List-of Ball] -> World
; Run this game with this list of initial balls
(define (main init-list)
  (big-bang (make-world 0 init-list)
            [on-tick tick]
            [to-draw draw]
            [on-mouse place-ball]))

;; tick : World -> World
;; increments the world's time by 1
(check-expect (tick WORLD-1) (make-world 1 '()))
(define (tick aworld)
  (make-world (add1 (world-t aworld))
              (world-balls aworld)))

;; draw-ball : Ball Posn Image -> Image
;; draws a ball at the given posn
(check-expect (draw-ball BALL-1 (make-posn 50 50) BG)
              (place-image (circle 5 'solid 'red) 50 50 BG))
(define (draw-ball aball apos animg)
  (place-image (circle (ball-r aball) (ball-mode aball) (ball-color aball))
               (posn-x apos) (posn-y apos) animg))

;; make-drawer : Nat -> [Ball Image -> Image]
;; creates a function that takes a ball and image and draws it
(check-expect ((make-drawer 1) BALL-1 BG)
              (draw-ball BALL-1 ((ball-placement BALL-1) 1) BG))
(define (make-drawer t)
  (λ (aball img) (draw-ball aball ((ball-placement aball) t) img)))

;; draw : World -> Image
;; draws the world
(define (draw aworld)
  (local [;; Ball -> Image
          (define (drawball aball y)
            ((make-drawer (world-t aworld)) aball y))]
    (foldr drawball BG (world-balls aworld))))

; A BallGenerator is a [Nat Nat Nat -> [Nat -> Posn]]
; Given the time, x-coordinate, and y-coordinate of when and where a
; ball is created, create a function that, given the current time of
; the world, will output a Posn
 
; Example:
; move-horizontally : BallGenerator
(define (move-horizontally t0 x0 y0)
  (λ (t) (make-posn (modulo (+ x0 (- t t0)) WIDTH) y0)))
(check-expect ((move-horizontally 3 5 8) 10) ; 7 seconds have passed
              (make-posn 12 8))

; move-horizontally-back : BallGenerator
(define (move-horizontally-back t0 x0 y0)
  (λ (t) (make-posn (modulo (- x0 (- t t0)) WIDTH) y0)))


; move-vertically : BallGenerator
(define (move-vertically t0 x0 y0)
  (λ (t) (make-posn x0 (modulo (+ y0 (- t t0)) HEIGHT))))
(check-expect ((move-vertically 3 5 8) 10)
              (make-posn 5 15))

; move-vertically-back : BallGenerator
(define (move-vertically-back t0 x0 y0)
  (λ (t) (make-posn x0 (modulo (- y0 (- t t0)) HEIGHT))))



;; place-ball : World Nat Nat MouseEvent -> World
;; Adds a ball to the world
#;(check-expect (place-ball WORLD-1 0 0 "button-down")
              (make-world 0 (list BALL-1)))
#;(check-expect (place-ball WORLD-1 0 0 "button-up")
              WORLD-1)
(define (place-ball aworld x y mousev)
  (cond [(string=? mousev "button-down")
         (make-world
          (world-t aworld)
          (cons (make-ball
                 (+ 1 (random 30)) 'solid 'red
                 ((select-random GENERATORS)(world-t aworld) x y))
                (world-balls aworld)))]
        [else aworld]))

;; select-random : List<X> -> X
;; returns a random element of a list
#;(check-random (select-random GENERATORS)
              (list-ref GENERATORS (random (length GENERATORS))))
(define (select-random alist)
  (list-ref alist (random (length alist))))

;; move-diag : BallGenerator
;; makes the ball diagonally
(define (move-diag t0 x0 y0)
  (λ (t) (make-posn (modulo (+ x0 (- t t0)) WIDTH)
                    (modulo (+ y0 (- t t0)) HEIGHT))))

;; move-back-diag : BallGenerator
;; moves the ball in the other diagonally
(define (move-back-diag t0 x0 y0)
  (λ (t) (make-posn (modulo (+ x0 (- t t0)) WIDTH)
                    (modulo (- y0 (- t t0)) HEIGHT))))



(define GENERATORS (list move-horizontally
                         move-horizontally-back
                         move-vertically
                         move-vertically-back
                         move-diag
                         move-back-diag))


;; #########################################################
;; PART 2
;; #########################################################


;; two : [X -> X] -> [X -> X]
;; creates a function that is a double of the input function
(check-expect ((two add1) 2) 4)
(define (two fx)
  (λ (x) (fx (fx x))))

;; three :[X -> X] -> [X -> X]
;; applies a function to an input thrice
(check-expect ((three add1) 2) 5)
(define (three fx)
  (λ (x) (fx (fx (fx x)))))

;; one: [X -> X] -> [X -> X]
;; applies a funciton to an input once
(check-expect ((one add1) 1) 2)
(define (one fx)
  (λ (x) (fx x)))

;; zero [X -> X] -> [X -> X]
;; applies a function to an input zeroce
(check-expect ((zero add1) 1) 1)
(define (zero fx)
  (λ (x) x))

; A Repeater is a [X -> X] -> [X -> X]
; That, given a unary function f, outputs a
; function that will repeatedly apply f

(check-expect (rep->nat zero) 0)
(check-expect (rep->nat one) 1)
(check-expect (rep->nat two) 2)
(check-expect (rep->nat three) 3)
(check-expect (rep->nat (λ (f) (λ (x) ((three f) ((two f) x))))) 5)
(define (rep->nat func)
  ((func add1) 0))


;; rep-add1 : ([X -> X] -> [X -> X]) -> ([X -> X] -> [X -> X])
;; Takes a repeater and applies it one additional time
(check-expect (((rep-add1 one) add1) 1) 3)
(check-expect (rep->nat (rep-add1 three))4)
(define (rep-add1 r)
  (λ (fx) (λ (x) (fx ((r fx) x)))))


; A Nat (natural number) is one of:
; - 0
; - (add1 Nat)

;; nat->rep : Nat -> ([X -> X] -> [X -> X])
(check-expect (((nat->rep 3) add1)1) 4)
(check-expect (((nat->rep 0) add1) 0) 0)
(define (nat->rep n)
  (cond [(= n 0) zero]
        [else (rep-add1 (nat->rep (- n 1)))]))

