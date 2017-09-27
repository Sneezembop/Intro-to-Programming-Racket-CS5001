;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trafficlightexample) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp")) #f)))
(define RADIUS 100)


;; A TL (Traffic Light) is one of:
;; - "red"
;; - "yellow"
;; - "green"

#;(define (tl-temp a-tl)
  (cond[(string=? a-tl "red") ...]
       [(string=? a-tl "yellow") ...]
       [(string=? a-tl "green") ...]))

;; TL -> TL
;; launches the traffic light animation
(define (main a-tl)
  (big-bang a-tl
            [to-draw draw-light]
            [on-tick change-color .3]))


;; TL -> Image
;; Draws the traffic light
(check-expect (draw-light "red") (circle RADIUS "solid" "red"))
(check-expect (draw-light "yellow") (circle RADIUS "solid" "yellow"))
(check-expect (draw-light "green") (circle RADIUS "solid" "green"))
(define (draw-light a-tl)
  (circle RADIUS "solid" a-tl))

;; TL -> TL
;; Decides when the traffic light should change
(check-expect (change-color "red") "green")
(check-expect (change-color "green") "yellow")
(check-expect (change-color "yellow") "red")
(define (change-color a-tl)
  (cond[(string=? a-tl "red") "green"]
       [(string=? a-tl "yellow") "red"]
       [(string=? a-tl "green") "yellow"]))