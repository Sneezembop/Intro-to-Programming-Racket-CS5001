;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname firstlectureCS5001) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
#|(string-append "I said " "hey")
(string-length (substring "hey, what's going on?" 5))

(overlay (circle 60 "solid" "cyan") (circle 100 "solid" "magenta"))

(place-image (circle 100 "solid" "red")
             350 200
             (empty-scene 700 400))

(overlay (circle 60 "solid" "cyan") (circle 100 "solid" "magenta"))
(overlay (circle 20 "solid" "cyan") (circle 50 "solid" "magenta")) 

;; Number Number -> Image
;; draw two concentric circles
(define (makedonut inner outer)
  (overlay (circle inner "solid" "cyan")
           (circle outer "solid" "magenta")))

(check-expect (makedonut 20 50)
              (overlay (circle 20 "solid" "cyan")
                       (circle 50 "solid" "magenta")))

|#

;; Number -> Number
;; compute the area of a circle
(define (getCircleArea radius)
  (* pi (* radius radius)))

(check-within (getCircleArea 1) pi 0.01)


;; A Book is a (make-book String String Number)
(define-struct book (author title price))

(define warBook(make-book "Winston Churchill" "How I Won the War" 9.95))
(define otherBook(make-book "Someone Else" "How I Won the War" 10.95)) 


;; Book -> Boolean
;; is the book by Winston Churchill?
(define (by-winston? a-book)
  (string=? "Winston Churchill" (book-author a-book)))

(check-expect (by-winston? warBook) true)
(check-expect (by-winston? otherBook) false)


;; A Novel is a (make-novel Author String Number)
(define-struct novel (author title price))

;; An Author is a (make-author String String Number)
(define-struct author (first last yob))
;; interp. yob is the year the author was born

(define poems(make-novel (make-author "Robert" "Frost" 1933) "A Collection of Poems" 9.95))