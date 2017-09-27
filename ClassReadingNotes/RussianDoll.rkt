;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname RussianDoll) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp")) #f)))
#;(define-struct doll (contents))

;;(make-doll (make-doll "solid"))


;; A RD is one of:
;; - "solid"
;; - (make-doll "solid")
;; - (make-doll (make-doll "solid))
;; - (make-doll (make-doll (make-doll "solid)))
;; ...


#;(define (rd-temp ard)
  (cond [(string? ard) ...]
        [(string? (doll-contents ard)) ...]
        [(string? (doll-contents (doll-contents ard))) ...]))


;; #########################################################################################

(define-struct doll (contents))

;; A RD is one of:
;; - "solid"
;; - (make-doll RD)

(define r1 "solid")
(define r2 (make-doll "solid"))
(define r3 (make-doll (make-doll "solid")))
(define r4 (make-doll (make-doll (make-doll "solid"))))

#;(define (rd-temp ard)
    (cond [(string? ard) ...]
          [(doll? ard) ... (rd-temp(doll-contents ard))]))

;; RD -> Number
;; count the shells of the doll
(check-expect (count-shell r1) 0)
(check-expect (count-shell r2) 1)
(check-expect (count-shell r3) 2)
(define (count-shell ard)
  (cond [(string? ard) 0]
        [(doll? ard) (+ 1 (count-shell (doll-contents ard)))]))


;; ##########################################################

(define-struct ice-cream (flavor remainder))

;; An Ice-Cream is either:
;; - A String ("cone")
;; - A (make-ice-cream String Ice-Cream)

(define c1 "cone")
(define c2 (make-ice-cream "chocolate" "cone"))
(define c3 (make-ice-cream "vanilla" (make-ice-cream "strawberry" "cone")))
(define c4 (make-ice-cream "mint chocolate chip" (make-ice-cream "superman" (make-ice-cream "moosetracks" "cone"))))

#;(define (temp-ice-cream icc)
    (cond [(string? icc) ...]
          [(ice-cream? icc) ...(ice-cream-flavor icc)
                            ...(temp-ice-cream (ice-cream-remainder icc))]))

;; Scoops: Ice-Cream -> String
;; prints what scoops are on the ice cream cone
(check-expect (scoops c2) "chocolate, ")
(check-expect (scoops c1) "")
(check-expect (scoops c4) "mint chocolate chip, superman, moosetracks, ")
(define (scoops icc)
    (cond [(string? icc) ""]
          [(ice-cream? icc) (string-append (ice-cream-flavor icc) ", " (scoops (ice-cream-remainder icc)))])) 
