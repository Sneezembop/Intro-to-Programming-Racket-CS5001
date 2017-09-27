;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ProblemSet) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp")) #f)))
;;> Total : 51/57

;;> Missing template signatures.

;;> Problem 1:
;;> Nested templates should be like:
#;(define (temp-presentation apres)
    (... (presentation-title apres) ...
         (temp-person (presentation-author apres)) ...
         (temp-affil (presentation-affiliation apres)) ...
         (temp-date (presentation-date apres)) ...))

;;> Problem 4:
;;> you need to write a helper function for los-replace-first, los-replace-all
;;> is complicated and you must write a helper functions.

;;> Problem 5:
;;> your function for sparkle-move is complicated and you
;;> could write helper functions for  cond clause
;; #############################################
;; PROBLEM 1
;; #############################################


(define-struct person (first last))
;; A Person is (make-person String String)
;; INTERP: holds the first and second name for a person

#;(define (temp-person aper)
    (... (person-first aper)... (person-last aper)))

(define-struct affiliation
  (institution street-no street-name zip state country))
;; An Affiliation is
;;        (make-affiliation String Number String Number Symbol String)
;; INTERP: holds the name of the instituion, institution's streen number and 
;; street name, zip code state and country. 
;; State is given as a symbol of size 2, e.g. 'WA

#;(define (temp-affil affil)
    (... (affiliation-institution affil) ...
         (affiliation-street-no affil) ...
         (affiliation-street-name affil) ...
         (affiliation-zip affil) ...
         (affiliation-state affil) ...
         (affiliation-country affil) ...))

(define-struct date (year month day))
;; A Date is (make-date Number Number Number) 
;; INTERP: represents a date as YYYY MM DD

#;(define (temp-date adate)
    (... (date-year adate) ...
         (date-month adate) ...
         (date-day adate) ...))

(define-struct presentation (title author affiliation date))
;; A Presentation is (make-presentation Sring Person Affiliation Date) 
;; INTERP: represents a presentation (a talk) with a title, 
;; the name of the author, the author's affiliation and 
;; the date of the presentation

#;(define (temp-presentation apres)
    (... (presentation-title apres) ...
         (presentation-author apres) ...
         (presentation-affiliation apres) ...
         (presentation-date apres) ...))


;; #############################################
;; PROBLEM 2
;; #############################################

(require 2htdp/universe)
(require 2htdp/image)


(define LEFT 'left)  
(define RIGHT 'right)
(define UP 'up)
(define DOWN 'down)

(define BG (empty-scene 500 500))


(define BALL (circle 20 "solid" "red"))

;; A Direction is one of
;; - LEFT
;; - RIGHT
;; - UP
;; - DOWN
;; INTERP: represents the direction of the moving ball

;; Template 
;; direction-fn: Direction -> ??? 
;;(define (direction-fn a-direction)
;;  (cond 
;;    [(symbol=? a-direction UP) ...]
;;    [(symbol=? a-direction DOWN) ...]
;;    [(symbol=? a-direction LEFT) ...]
;;    [(symbol=? a-direction RIGHT) ...]))



(define-struct world (location direction))
;; A World is (make-world Posn Direction)
;; INTERP: represents a ball on the canvas with a cartesian coordinate
;;         for location and a direction

;; Template 
;; world-fn: World -> ???
;;(define (world-fn w)
;;  ... (world-location w) ... 
;;  ... (direction-fn (world-direction w)) ...)

(define INIT-WORLD (make-world (make-posn 250 250) 'up))
(define LEFT-WORLD (make-world (make-posn 250 250) 'left))
(define DOWN-WORLD (make-world (make-posn 250 250) 'down))
(define RIGHT-WORLD (make-world (make-posn 250 250) 'right))

;;; Signature
;; change-direction: World KeyEvent -> World
;;; Purpose
;; Given the current world and a key-event return
;; a new update world if the user pressed up, down, left or right
;; arrow keys. If any other keys are pressed the world is unchanged.
(check-expect (change-direction INIT-WORLD "up") INIT-WORLD)
(check-expect (change-direction INIT-WORLD "down")
              (make-world (make-posn 250 250) 'down))
(check-expect (change-direction INIT-WORLD "left")
              (make-world (make-posn 250 250) 'left))
(check-expect (change-direction INIT-WORLD "right")
              (make-world (make-posn 250 250) 'right))
(check-expect (change-direction INIT-WORLD "b") INIT-WORLD)
(define (change-direction w key-event)
  (cond
    [(key=? key-event "up") (make-world (world-location w)
                                        'up)]
    [(key=? key-event "down") (make-world (world-location w)
                                          'down)]
    [(key=? key-event "left") (make-world (world-location w)
                                          'left)]
    [(key=? key-event "right") (make-world (world-location w)
                                           'right)]
    [else w]))



;;; Signature 
;; ball-draw: World -> Image 
;;; Purpose 
;; Given a world create the corresponding image with a red solid circle 
;; with radius 20 at the appropriate location found inside world.
(check-expect (ball-draw INIT-WORLD) (place-image BALL 250 250 BG))
(define (ball-draw w)
  (place-image BALL (posn-x (world-location w))
               (posn-y (world-location w)) BG))

;;; Signature 
;; ball-move: World -> World 
;;; Purpose
;; Given a world return a new world that moves the circle 
;; 10 points in the direction specified by the input world.
(check-expect (ball-move INIT-WORLD) (make-world (make-posn 250 240) 'up))
(check-expect (ball-move LEFT-WORLD) (make-world (make-posn 240 250) 'left))
(check-expect (ball-move DOWN-WORLD) (make-world (make-posn 250 260) 'down))
(check-expect (ball-move RIGHT-WORLD) (make-world (make-posn 260 250) 'right))
(define (ball-move w)
  (cond [(symbol=? (world-direction w) 'up)
         (make-world
          (make-posn (posn-x (world-location w))
                     (- (posn-y (world-location w)) 10)) (world-direction w))]
        [(symbol=? (world-direction w) 'down)
         (make-world
          (make-posn (posn-x (world-location w))
                     (+ (posn-y (world-location w)) 10)) (world-direction w))]
        [(symbol=? (world-direction w) 'left)
         (make-world
          (make-posn (- (posn-x (world-location w)) 10)
                     (posn-y (world-location w))) (world-direction w))]
        [(symbol=? (world-direction w) 'right)
         (make-world
          (make-posn (+ (posn-x (world-location w)) 10)
                     (posn-y (world-location w))) (world-direction w))]))



#;(big-bang INIT-WORLD
            (on-tick ball-move 0.1)
            (on-draw ball-draw)
            (on-key change-direction))

;; #############################################
;; PROBLEM 3
;; #############################################

;; A List-of-Booleans (LoB) is one of:
;; - Empty
;; - (cons Boolean LoB)

(define lob0 '())
(define lob1 (cons #true lob0))
(define lob2 (cons #true lob1))
(define lob3 (cons #true (cons #false (cons #true '()))))
(define lob4 (cons #false (cons #false (cons #false '()))))

;; Template
#;(define (temp-lob alob)
    (cond [(empty? alob) ...]
          [(cons? alob) ... (first alob)... (temp-lob (rest alob))]))

;; lob-or : LoB -> Boolean
;; operates a logical OR on all the elements of a LoB
(check-expect (lob-or lob2) #true)
(check-expect (lob-or lob3) #true)
(check-expect (lob-or lob4) #false)
(check-expect (lob-or lob0) #false)
(define (lob-or alob)
  (cond [(empty? alob) #false]
        [(cons? alob) (or (first alob) (lob-or (rest alob)))]))

;; lob-and : LoB -> Boolean
;; operates a logical AND on all the elements of a LoB
(check-expect (lob-and lob0) #false)
(check-expect (lob-and lob2) #true)
(check-expect (lob-and lob4) #false)
(define (lob-and alob)
  (cond [(empty? alob) #false]
        [(cons? alob) (if (empty? (rest alob))
                          #true
                          (and (first alob) (lob-and (rest alob))))]))

;; #############################################
;; PROBLEM 4
;; #############################################

;; A List-of-Strings (LoS) is one of:
;; - Empty
;; - (cons String LoS)

;; Template
#; (define (temp-los alos)
     (cond [(empty? alos) ...]
           [(cons? alos) ... (first alos) (temp-los (rest alos))]))

(define los0 '())
(define los1 (cons "asdf" los0))
(define los2 (cons "brus" los1))
(define los3 (cons "asdf" los2))
(define los4 (cons "g" los3))

;; los-total-length : LoS -> Number
;; sums the total number of characters in an LoS
(check-expect (los-total-length los0) 0)
(check-expect (los-total-length los4) 13)
(define (los-total-length alos)
  (cond [(empty? alos) 0]
        [(cons? alos) (+ (string-length (first alos))
                         (los-total-length (rest alos)))]))

;; los-contains : LoS String -> Boolean
;; Checks wether the LoS contains the String
(check-expect (los-contains los4 "asdf") #true)
(check-expect (los-contains los4 "ssss") #false)
(define (los-contains string-list s)
  (cond [(empty? string-list) #false]
        [(cons? string-list) (if (string=? (first string-list) s)
                                 #true
                                 (los-contains (rest string-list) s))]))

;; los-replace-first : LoS String String -> LoS
;; Replaces the first instance of a string with a second string
(check-expect (los-replace-first los4 "asdf" "qwer")
              (cons "g" (cons "qwer" (cons "brus" (cons "asdf" '())))))
(check-expect (los-replace-first los4 "qwer" "b") los4)
(define (los-replace-first string-list old new)
  (cond [(empty? string-list) '()]
        [(cons? string-list) (if (string=? (first string-list) old)
                                 (cons new (rest string-list))
                                 (cons (first string-list)
                                       (los-replace-first
                                        (rest string-list) old new)))]))

;; los-replace-all : LoS String String -> LoS
;; Replace any instance of the first string with the second string
(check-expect (los-replace-all los4 "asdf" "qwer")
              (cons "g" (cons "qwer" (cons "brus" (cons "qwer" '())))))
(check-expect (los-replace-all los4 "qwer" "b") los4)
(define (los-replace-all string-list old new)
  (cond [(empty? string-list) '()]
        [(cons? string-list) (if (string=? (first string-list) old)
                                 (cons new (los-replace-all
                                            (rest string-list) old new))
                                 (cons (first string-list)
                                       (los-replace-all
                                        (rest string-list) old new)))]))


;; #############################################
;; PROBLEM 5
;; #############################################


;; a direction is one of 
;;  - 'up
;;  - 'down
;;  - 'left
;;  - 'right

(define SPARKLEBG (empty-scene 800 500))

(define-struct sparkle [location size direction speed])
;; A Sparkle is (make-sparkle Posn PosInt Direction PosInt)
;; INTERP: represents a star with its current location, 
;;      the star's size (measured by the side length of the enclosing pentagon, 
;;      see the documentation in the Image Library), the star's direction and 
;;      the star's speed

(define sparkle0 (make-sparkle (make-posn 200 300) 10 'up 10))
(define sparkle1 (make-sparkle (make-posn 400 200) 15 'down 15))
(define sparkle2 (make-sparkle (make-posn 600 400) 20 'right 10))
(define sparkle3 (make-sparkle (make-posn 100 500) 25 'left 10))

;; A List-of-Sparkle (LoSp) is one of:
;; - Empty
;; - (cons Sparkle LoSp)

;; TEMPLATE
#;(define (temp-losp alosp)
    (cond [(empty? alosp) ...]
          [(cons? alosp) ... (first alosp) (temp-losp (rest alosp))]))

(define LoSp0 '())
(define LoSp1 (cons sparkle0 LoSp0))
(define LoSp2 (cons sparkle1 LoSp1))
(define LoSp3 (cons sparkle2 LoSp2))
(define LoSp4 (cons sparkle3 LoSp3))

;; get-color : Number -> String
;; returns a color name based on a numerical value
(check-expect (get-color 1) "red")
(check-expect (get-color 2) "blue")
(check-expect (get-color 3) "green")
(check-expect (get-color 4) "purple")
(check-expect (get-color 5) "yellow")
(check-expect (get-color 6) "orange")
(check-expect (get-color 7) "black")
(check-expect (get-color 8) "cyan")
(define (get-color num)
  (cond [(<= num 1) "red"]
        [(and (> num 1) (<= num 2)) "blue"]
        [(and (> num 2) (<= num 3)) "green"]
        [(and (> num 3) (<= num 4)) "purple"]
        [(and (> num 4) (<= num 5)) "yellow"]
        [(and (> num 5) (<= num 6)) "orange"]
        [(and (> num 6) (<= num 7)) "black"]
        [(> num 7) "cyan"]))

;; sparkles-draw : LoSp -> Image
;; draws all of the sparkles onto the canvas
(check-random (sparkles-draw LoSp1)
              (place-image (star 10 "solid"
                                 (get-color (random 8))) 200 300 SPARKLEBG))
(check-expect (sparkles-draw LoSp0) SPARKLEBG)
(define (sparkles-draw alosp)
  (cond [(empty? alosp) SPARKLEBG]
        [(cons? alosp) (place-image
                        (star (sparkle-size (first alosp))
                              "solid" (get-color (random 8)))
                        (posn-x (sparkle-location (first alosp)))
                        (posn-y (sparkle-location (first alosp)))
                        (sparkles-draw (rest alosp)))]))

;; sparkles-move : LosP -> LosP
;; moves sparkles in their specified direction
(check-expect (sparkles-move LoSp0) LoSp0)
(check-expect (sparkles-move LoSp1)
              (cons (make-sparkle
                     (make-posn 200 290) 10 'up 10) LoSp0))
(define (sparkles-move alosp)
  (cond [(empty? alosp) '()]
        [(cons? alosp) (cons (sparkle-move (first alosp))
                             (sparkles-move (rest alosp)))]))


;; sparkle-move : Sparkle -> Sparkle
;; moves a sparkle in the specified direction
(check-expect (sparkle-move sparkle0)
              (make-sparkle (make-posn 200 290) 10 'up 10))
(check-expect (sparkle-move sparkle1)
              (make-sparkle (make-posn 400 215) 15 'down 15))
(check-expect (sparkle-move sparkle2)
              (make-sparkle (make-posn 610 400) 20 'right 10))
(check-expect (sparkle-move sparkle3)
              (make-sparkle (make-posn 90 500) 25 'left 10))
(define (sparkle-move asp)
  (cond [(symbol=? (sparkle-direction asp) 'up)
         (make-sparkle (make-posn (posn-x (sparkle-location asp))
                                  ( - (posn-y (sparkle-location asp))
                                      (sparkle-speed asp)))
                       (sparkle-size asp)
                       (sparkle-direction asp)
                       (sparkle-speed asp))]
        [(symbol=? (sparkle-direction asp) 'down)
         (make-sparkle (make-posn (posn-x (sparkle-location asp))
                                  (+ (posn-y (sparkle-location asp))
                                     (sparkle-speed asp)))
                       (sparkle-size asp)
                       (sparkle-direction asp)
                       (sparkle-speed asp))]
        [(symbol=? (sparkle-direction asp) 'left)
         (make-sparkle (make-posn (- (posn-x (sparkle-location asp))
                                     (sparkle-speed asp))
                                  (posn-y (sparkle-location asp)))
                       (sparkle-size asp)
                       (sparkle-direction asp)
                       (sparkle-speed asp))]
        [(symbol=? (sparkle-direction asp) 'right)
         (make-sparkle (make-posn (+ (posn-x (sparkle-location asp))
                                     (sparkle-speed asp))
                                  (posn-y (sparkle-location asp)))
                       (sparkle-size asp)
                       (sparkle-direction asp)
                       (sparkle-speed asp))]))

(big-bang LoSp4
          (on-tick sparkles-move 1)
          (on-draw sparkles-draw))
