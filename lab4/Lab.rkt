;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp")) #f)))
;; #############################################
;; PROBLEM 1
;; #############################################

;; A List of Strings (LoS) is one
;; - empty
;; - (cons String LoS)

;; Template
;; los-fn: LoS -> ???
#;(define (los-fn los)
    (cond
      [(empty? los) ...]
      [(cons? los) ... (first los) ...
                   ... (los-fn (rest los)) ...]))

(define list1 '())
(define list2 (cons "asbs" list1))
(define list3 (cons "a" list2))
(define list4 (cons "3jei" list3))

;; sizes: LoS -> Number
;; Counts all the characters in a LoS
(check-expect (sizes list1) 0)
(check-expect (sizes list2) 4)
(define (sizes los)
  (cond
    [(empty? los) 0]
    [(cons? los) (+ (string-length (first los))(sizes (rest los)))]))

;; contains: LoS String -> Boolean
;; returns true if the input string is in the LoS
(check-expect (contains list1 "asbs") #false)
(check-expect (contains list2 "asbs") #true)
(check-expect (contains list2 "bloo") #false)
(define (contains los astr)
  (cond
    [(empty? los) #false]
    [(cons? los) (if (string=? (first los) astr)
                     #true
                     (contains (rest los) astr))]))


;; snoc LoS String -> LoS
;; adds the string to the end of the list
(check-expect (snoc list1 "bloo") (cons "bloo" '()))
(check-expect (snoc list2 "bloo") (cons "asbs" (cons "bloo" '())))
(define (snoc los astr)
  (cond
    [(empty? los) (cons astr '())]
    [(cons? los) (cons (first los)(snoc (rest los) astr))]))

;; ################################################
;; PROBLEM 2
;; ################################################


;; A Posn is a (make-posn Number Number)

;;; Template
;; posn-fn: Posn -> ???
#;(define (temp-posn aposn)
    (... (posn-x aposn)...
         (posn-y aposn)...))


;; A List of Posns (LoP) is one of
;; - empty
;; - (cons Posn LoP)

;;; Template
;; lop-fn: LoP -> ???
#;(define (temp-lop alop)
    (cond [(empty? alop) ...]
          [(cons? alop) ... (first alop)...
                        (temp-lop (rest alop))]))

(define lop0 '())
(define lop1 (cons (make-posn 2 3) lop0))
(define lop2 (cons (make-posn 4 5) lop1))
(define lop3 (cons (make-posn 5 6) lop2))
(define lop4 (cons (make-posn 501 7) lop3))
(define lop5 (cons (make-posn 5 600) lop4))

;; count-posn : LoP -> Number
;; Counts the number of posns in a LoP
(check-expect (count-posn lop0) 0)
(check-expect (count-posn lop1) 1)
(check-expect (count-posn lop2) 2)

(define (count-posn alop)
  (cond [(empty? alop) 0]
        [(cons? alop) (+ 1 (count-posn (rest alop)))]))

;; posn-equal? : LoP LoP -> Boolean
;; checks to see if both LoPs have the same elements
(check-expect (posn-equal? lop0 lop0) #true)
(check-expect (posn-equal? lop1 lop1) #true)
(check-expect (posn-equal? lop1 lop0) #false)
(check-expect (posn-equal? lop0 lop2) #false)
(check-expect (posn-equal? lop1 lop2) #false)

(define (posn-equal? alop blop)
  (cond [(and (empty? alop)(empty? blop)) #true]
        [(and (cons? alop) (cons? blop))
         (if (posn-check (first alop) (first blop))
             (posn-equal? (rest alop) (rest blop)) #false)]
        [else #false]))

;; posn-check : posn posn -> boolean
;; checks to see if both posns are the same
(check-expect (posn-check (make-posn 1 2) (make-posn 1 2)) #true)
(check-expect (posn-check (make-posn 1 2) (make-posn 0 2)) #false)
(check-expect (posn-check (make-posn 1 2) (make-posn 1 3)) #false)
(define (posn-check posn1 posn2)
  (if (and (= (posn-x posn1) (posn-x posn2))
           (= (posn-y posn1) (posn-y posn2))) #true #false))


(define WIDTH 500)
(define HEIGHT 500)

;; remove-out-of-scene : LoP -> LoP
;; removes all posns from the list that are outside the boundaries
(check-expect (remove-out-of-scene lop0) lop0)
(check-expect (remove-out-of-scene lop2) lop2)
(check-expect (remove-out-of-scene lop4) lop3)
(check-expect (remove-out-of-scene lop5) lop3)
(define (remove-out-of-scene alop)
  (cond [(empty? alop) alop]
        [(cons? alop) (if (out-of-bounds? (first alop))
                          (remove-out-of-scene (rest alop))
                          (cons (first alop)
                                (remove-out-of-scene (rest alop))))]))


;; out-of-bounds? : posn -> boolean
;; checks to see if a posn is outside the canvas defined by HEIGHT and WIDTH
(check-expect (out-of-bounds? (make-posn 5001 3)) #true)
(check-expect (out-of-bounds? (make-posn 4 5001)) #true)
(check-expect (out-of-bounds? (make-posn -2 4)) #true)
(check-expect (out-of-bounds? (make-posn 5 -10)) #true)
(check-expect (out-of-bounds? (make-posn 5 3)) #false)
(define (out-of-bounds? aposn)
  (cond [( > (posn-x aposn) WIDTH) #true]
        [( < (posn-x aposn) 0) #true]
        [( > (posn-y aposn) HEIGHT) #true]
        [( < (posn-y aposn) 0) #true]
        [else #false]))


;; ################################################
;; PROBLEM 3
;; ################################################


;; A Corners is a Posn Posn Posn Posn
;; Defines the four corner positions of a quadrilateral shape
(define-struct corners (first second third fourth))

#;(define (temp-corn acorn)
    (... (posn-x (corners-first)) ...
         (posn-y (corners-first)) ...
         (posn-x (corners-second)) ...
         (posn-y (corners-second)) ...
         (posn-x (corners-third)) ...
         (posn-y (corners-third)) ...
         (posn-x (corners-fourth)) ...
         (posn-y (corners-fourth)) ...))

(define posn1 (make-posn 0 0))
(define posn2 (make-posn 0 2))
(define posn3 (make-posn 2 2))
(define posn4 (make-posn 2 0))
(define posn4b (make-posn 3 0))

(define corn1 (make-corners posn1 posn2 posn3 posn4))
(define corn2 (make-corners posn1 posn2 posn3 posn4b))

;; is-square? : Corners -> Boolean
;; determines if the shape defined is square
(check-expect (is-square? corn1) #true)
(check-expect (is-square? corn2) #false)
(define (is-square? acorn)
  (if (= (posn-distance (corners-first acorn) (corners-second acorn))
         (posn-distance (corners-second acorn) (corners-third acorn))
         (posn-distance (corners-third acorn) (corners-fourth acorn))
         (posn-distance (corners-fourth acorn) (corners-first acorn)))
      #true #false))



;; posn-distance : Posn -> Number
;; returns the distance between two posns
(check-expect (posn-distance posn1 posn2) 2)
(check-expect (posn-distance posn3 posn4) 2)
(check-expect (posn-distance posn4 posn4b) 1)
(define (posn-distance aposn bposn)
  (sqrt (+ (sqr (- (posn-x bposn) (posn-x aposn)))
           (sqr (- (posn-y bposn) (posn-y aposn))))))


;; A List of Corners (LoC) is one of:
;; -- empty
;; -- (cons Corners LoC)

#;(define (temp-loc aloc)
    (cond [(empty? aloc) ...]
          [(cons? aloc) ... (first aloc) (temp-loc (rest aloc))]))

(define loc0 '())
(define loc1 (cons corn1 loc0))
(define loc2 (cons corn2 loc1))
(define loc2b (cons corn2 loc0))
(define loc3 (cons corn1 loc1))

;; squ-exist? : LoC -> Boolean
;; given a List of Corners returns
;; true if there is at least one square in the list
(check-expect (squ-exist? loc0) #false)
(check-expect (squ-exist? loc1) #true)
(check-expect (squ-exist? loc2b) #false)
(check-expect (squ-exist? loc2) #true)
(define (squ-exist? aloc)
  (cond [(empty? aloc) #false]
        [(cons? aloc) (if (is-square?(first aloc))
                          #true
                          (squ-exist? (rest aloc)))]))

;; all-square? : Loc -> Boolean
;; given a LoC returns true if all elements of the LoC are squares
(check-expect (all-square? loc0) #false)
(check-expect (all-square? loc1) #true)
(check-expect (all-square? loc2) #false)
(check-expect (all-square? loc3) #true)
(define (all-square? aloc)
  (cond [(empty? aloc) #false]
        [(cons? aloc) (cond [(and (is-square? (first aloc))
                                  (empty? (rest aloc))) #true]
                            [(and (is-square? (first aloc))
                                  (cons? (rest aloc)))
                             (all-square? (rest aloc))]
                            [else #false])]))

;; move-shape : LoC Number Number -> LoC
;; moves every Posn in a Loc down by two numbers
(check-expect (move-shape loc0 5 5) loc0)
(check-expect (move-shape loc1 5 5) (cons (make-corners (make-posn 5 5)
                                                        (make-posn 5 7)
                                                        (make-posn 7 7)
                                                        (make-posn 7 5)) '()))
(define (move-shape aloc x y)
  (cond [(empty? aloc) aloc]
        [(cons? aloc) (cons (move-corners (first aloc) x y)
                            (move-shape (rest aloc) x y))]))


;; move-corners: Corners Number Number -> Corners
;; moves all the Posns in a Corners by two numbers
(check-expect (move-corners corn1 5 5) (make-corners (make-posn 5 5)
                                                     (make-posn 5 7)
                                                     (make-posn 7 7)
                                                     (make-posn 7 5)))
(define (move-corners acorn x y)
  (make-corners (move-posn (corners-first acorn) x y)
                (move-posn (corners-second acorn) x y)
                (move-posn (corners-third acorn) x y)
                (move-posn (corners-fourth acorn) x y)))


;; move-posn: Posn Number Number -> Posn
;; moves a Posn by two numbers
(check-expect (move-posn posn1 5 5) (make-posn 5 5))
(define (move-posn apos x y)
  (make-posn (+ x (posn-x apos)) (+ y (posn-y apos))))



;; ################################################
;; PROBLEM 4
;; ################################################
(require 2htdp/image)
(require 2htdp/universe)

(define-struct world (pin square))
;; A World is (make-world Posn Corners)
;; INTERP: represents the pin (location) of the center of the square and
;;         the square to draw

(define BACKGROUND (empty-scene 500 500))
(define INIT-WORLD (make-world (make-posn 250 250)
                               (make-corners (make-posn 0 0)
                                             (make-posn 20 0)
                                             (make-posn 0 20)
                                             (make-posn 20 20))))
;; draw-world : World -> Image
;; draws the square on the canvas
(check-expect (draw-world INIT-WORLD)
              (place-image (square 20 "solid" "blue") 250 250 BACKGROUND))
(define (draw-world aworld)
  (place-image (square
                (posn-distance (corners-first (world-square aworld))
                               (corners-second (world-square aworld)))
                "solid" "blue") (posn-x (world-pin aworld))
                                (posn-y (world-pin aworld)) BACKGROUND))
(define SPEED 10)

;; move-left : World -> World
;; moves the pin to the left by SPEED
(check-expect (move-left INIT-WORLD)
              (make-world (make-posn 240 250)
                          (make-corners (make-posn 0 0)
                                        (make-posn 20 0)
                                        (make-posn 0 20)
                                        (make-posn 20 20))))
(define (move-left aworld)
  (make-world (move-posn (world-pin aworld) (* -1 SPEED) 0)
              (world-square aworld)))

;; move-right : World -> World
;; moves the pin to the right by SPEED
(check-expect (move-right INIT-WORLD)
              (make-world (make-posn 260 250)
                          (make-corners (make-posn 0 0)
                                        (make-posn 20 0)
                                        (make-posn 0 20)
                                        (make-posn 20 20))))
(define (move-right aworld)
  (make-world (move-posn (world-pin aworld) (* 1 SPEED) 0)
              (world-square aworld)))
;; move-up : World -> World
;; moves the pin upwards by SPEED
(check-expect (move-up INIT-WORLD)
              (make-world (make-posn 250 240)
                          (make-corners (make-posn 0 0)
                                        (make-posn 20 0)
                                        (make-posn 0 20)
                                        (make-posn 20 20))))
(define (move-up aworld)
  (make-world (move-posn (world-pin aworld) 0 (* -1 SPEED))
              (world-square aworld)))
;; move-down : World -> World
;; moves the pin downwards by SPEED
(check-expect (move-down INIT-WORLD)
              (make-world (make-posn 250 260)
                          (make-corners (make-posn 0 0)
                                        (make-posn 20 0)
                                        (make-posn 0 20)
                                        (make-posn 20 20))))
(define (move-down aworld)
  (make-world (move-posn (world-pin aworld) 0 (* 1 SPEED))
              (world-square aworld)))

(big-bang INIT-WORLD
          (on-tick move-down 0.1)
          (to-draw draw-world))

