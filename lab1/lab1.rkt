;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab1) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp")) #f)))
;; This is my first DrRacket comment

;; Tip exercise
(define (tip amount)
  (* amount 0.20))

;; Tests for tip function
(check-expect (tip 35) 7)
(check-expect (tip 10) 2)

;; Tip Percentage: Number, Number -> Number
;; returns the total bill given the charges and desired tip percentage.
;; tip is supposed to be a whole number less than 100
(define (tip-percentage bill tip)
  (+ bill (* bill (/ tip 100))))

;; Test for tip percentage
(check-expect (tip-percentage 35 20) 42)

;; Tip Percentage: Number, Number -> Number
;; returns the total bill given the charges and desired tip percentage.  The tip is rounded before totalling up.
;; tip is supposed to be a whole number less than 100
(define (tip-percentage-round bill tip)
  (+ bill (round (* bill (/ tip 100)))))

;; Test for tip percentage round, testing for rounding up and rounding down
(check-expect (tip-percentage-round 30 16) 35)
(check-expect (tip-percentage-round 30 14) 34)

(define TT #true)

(define-struct test (name id))

;; Tip-Conditional: Number -> Number
;; returns the total bill given based on the charges and some conditional tip percentage
(define (tip-conditional bill)
 (cond
  [(<= bill 100) (tip-percentage bill 15)]
  [(and (> bill 100)(<= bill 400)) (tip-percentage bill 20)]
  [(> bill 400) (tip-percentage bill 22)]))

(check-expect (tip-conditional 100) 115)
(check-expect (tip-conditional 200) 240)
(check-expect (tip-conditional 500) 610)


;; exercise 4: Creating Images
(require 2htdp/image)

;; Image-Creator: Symbol -> Image
;; creates one of three images based on request
(define (image-creator symbol)
  (cond
    [(symbol=? symbol 'circle) (circle 100 "solid" "red")]
    [(symbol=? symbol 'square) (square 50 "solid" "blue")]
    [(symbol=? symbol 'star) (star 50 "solid" "green")]))

(check-expect (image-creator 'circle) (circle 100 "solid" "red"))
(check-expect (image-creator 'square) (square 50 "solid" "blue"))
(check-expect (image-creator 'star) (star 50 "solid" "green"))