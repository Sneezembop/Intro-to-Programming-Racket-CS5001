;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lab-problems) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))
;; #########################################
;; PROBLEM 1
;; #########################################

;; A Cent is a NonNegInt
;; WHERE: 0 <= cent <= 99
;; INTERP: represents cents in a price
;;
;; A Dollar is a NonNegInt
;; INTERP: represents dollars in a price

(define-struct price(dollars cents))
;; A Price is a (make-price dollars cents)
;; INTERP: represents the price of an item in dollars and cents
;;
;; A List of Prices(LoP) is one of:
;; - empty
;; - (cons Price LoP)

(define price0 (make-price 3 50))
(define price1 (make-price 4 55))
(define price2 (make-price 0 15))

(define lop0 '())
(define lop1 (cons price0 lop0))
(define lop2 (cons price1 lop1))
(define lop3 (cons price2 lop2))

;; lop-total-price : LoP -> Price
(check-expect (lop-total-price lop0) (make-price 0 0))
(check-expect (lop-total-price lop1) (make-price 3 50))
(check-expect (lop-total-price lop2) (make-price 8 5))
(check-expect (lop-total-price lop3) (make-price 8 20))
(define (lop-total-price alop)
  (cond [(empty? alop) (make-price 0 0)]
        [(cons? alop) (add-price (first alop) (lop-total-price (rest alop)))]))

;; add-price : Price Price -> Price
;; adds two prices together
(check-expect (add-price price0 price2) (make-price 3 65))
(check-expect (add-price price0 price1) (make-price 8 5))
(define (add-price p1 p2)
  (if (<= 100 (+ (price-cents p1) (price-cents p2)))
      (make-price (+ 1 (price-dollars p1) (price-dollars p2))
                  (- (+ (price-cents p1) (price-cents p2)) 100))
      (make-price (+ (price-dollars p1) (price-dollars p2))
                  (+ (price-cents p1) (price-cents p2)))))


;; #########################################
;; PROBLEM 2
;; #########################################

;; A Name is a String
;; INTERP: represents the names of items in the grocery store.

;; A Section is one of:
;; - 'produce
;; - 'dairy
;; - 'meat
;; - 'bakery
;; - 'frozen
;; - 'general
;; INTERP: represents sections of the grocery store.

;; TEMPLATE : Section -> ???
#;(define (temp-section asec)
    (cond [(symbol=? 'produce) ...]
          [(symbol=? 'dairy) ...]
          [(symbol=? 'meat) ...]
          [(symbol=? 'bakery) ...]
          [(symbol=? 'frozen) ...]
          [(symbol=? 'general) ...]))



;; A Quantity is a NonNegInt
;; INTERP: represents the quantity of an item purchased


;; A Price is a (make-price dollars cents)
;; INTERP: represents the price of an item in dollars and cents
;; struct defined above

;; An Item is a (make-item Name Section Quantity Price)
;; INTERP: represents an item in the grocery story with a name, section,
;; quantity and price
(define-struct item (name section quantity price))

(define milk (make-item "Milk" 'dairy 2 (make-price 2 50)))
(define chocolate (make-item "Chocolate" 'general 3 (make-price 1 50)))
(define apple (make-item "Apple" 'produce 5 (make-price 0 25)))
(define steak (make-item "Steak" 'meat 1 (make-price 10 00)))
(define baguette (make-item "Baguette" 'bakery 2 (make-price 5 50)))
(define fzwaffles (make-item "Frozen Waffles" 'frozen 5 (make-price 3 00)))


;; Template
;; Item -> ???
#; (define (item-fn an-item)
     (cond [(item-name an-item)...]
           [(item-section an-item)...]
           [(item-quantity an-item)...]
           [(item-price an-item)...]))

;; A List of Items (LoI) is one of:
;; - '()
;; - (cons Item LoI)

(define loi0 '())
(define loi1 (list milk chocolate))
(define loi2 (list apple steak))
(define loi3 (list baguette fzwaffles))
(define loi4 (list milk chocolate apple steak baguette fzwaffles))

;; total-grocery-bill : LoI -> Price
;; adds up the price for all the grocery items
(check-expect (total-grocery-bill loi0) (make-price 0 0))
(check-expect (total-grocery-bill loi1) (make-price 9 50))
(check-expect (total-grocery-bill loi4) (make-price 46 75))
(define (total-grocery-bill aloi)
  (cond [(empty? aloi) (make-price 0 0)]
        [(cons? aloi) (add-price (multiply-price
                                  (item-quantity (first aloi))
                                  (item-price (first aloi)))
                                 (total-grocery-bill (rest aloi)))]))

;; multiply-price : NaturalNumber Price -> Price
;; multiplies a price by an integer number
(check-expect (multiply-price 1 (make-price 2 2)) (make-price 2 2))
(check-expect (multiply-price 2 (make-price 1 1)) (make-price 2 2))
(define (multiply-price n apr)
  (cond [(> n 0) (add-price apr (multiply-price (- n 1) apr))]
        [else (make-price 0 0)]))


;; loi-bakery-discount : LoI -> LoI
;; applies a 15% discount to 'bakery items
(check-expect (loi-bakery-discount loi0) loi0)
(check-expect (loi-bakery-discount loi3)
              (list (apply-item-discount baguette) fzwaffles))
(define (loi-bakery-discount aloi)
  (cond [(empty? aloi) '()]
        [(cons? aloi)
         (if (symbol=? 'bakery (item-section (first aloi)))
             (cons (apply-item-discount (first aloi))
                   (loi-bakery-discount (rest aloi)))
             (cons (first aloi)
                   (loi-bakery-discount (rest aloi))))]))

;; apply-item-discount : Item -> Item
;; applies a 15% discount to an item's price
(check-expect (apply-item-discount steak)
              (make-item "Steak" 'meat 1 (make-price 8 50)))
(define (apply-item-discount anitem)
  (make-item (item-name anitem)
             (item-section anitem)
             (item-quantity anitem)
             (discount-price (item-price anitem))))


;; discount-price : Price -> Price
;; applies a 15% discount to a price
(check-expect (discount-price (make-price 10 00)) (make-price 8 50))
(define (discount-price apr)
  (add-price (make-price (floor (* 0.85 (price-dollars apr)))
              (floor (* 0.85 (price-cents apr))))
             (make-price 0 (floor (* 100 (- (* 0.85 (price-dollars apr))
                                        (floor
                                         (* 0.85 (price-dollars apr)))))))))



;; #########################################
;; PROBLEM 3
;; #########################################

(require 2htdp/image)
(require 2htdp/universe)



;; Data Definitions

;; A Missile is a Posn
;; INTERP: Represents a missile as a position on a scene

;; A Target is a Posn
;; INTERP: Represents a target as a position on a scene

;; A KeyEvent is one of:
;; - "right"
;; - "left"
;; - "up"
;; - "down"
;; INTERP: Represents the input from when a user presses a key

;; Template: KeyEvent -> ???
#;(define (temp-key akey)
  (cond [(key=? "right" akey) ...]
        [(key=? "left" akey) ...]
        [(key=? "up" akey) ...]
        [(key=? "down" akey) ...]))

;; Constants

(define WIDTH 500)
(define HEIGHT 500)
(define MISSILE-SIZE 20)
(define TARGET-SIZE 50)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define TARGET-IMAGE (square TARGET-SIZE "solid" "blue"))
(define HIT-TARGET-IMAGE (square TARGET-SIZE "solid" "red"))
(define TARGET-POSITION (make-posn 250 250))

(define MISSILE-IMAGE (circle MISSILE-SIZE "solid" "black"))
(define SPEED 20)



;; move-missile : KeyEvent Posn -> Posn
;; moves a missle by Speed in the given direction
(check-expect (move-missile "up" (make-posn 0 0)) (make-posn 0 (* -1 SPEED)))
(check-expect (move-missile "down" (make-posn 0 0)) (make-posn 0 SPEED))
(check-expect (move-missile "left" (make-posn 0 0)) (make-posn (* -1 SPEED) 0))
(check-expect (move-missile "right" (make-posn 0 0)) (make-posn SPEED 0))
(define (move-missile akey apos)
  (cond [(key=? "right" akey) (make-posn (+ (posn-x apos) SPEED) (posn-y apos))]
        [(key=? "left" akey) (make-posn (- (posn-x apos) SPEED) (posn-y apos))]
        [(key=? "up" akey) (make-posn (posn-x apos) (- (posn-y apos) SPEED))]
        [(key=? "down" akey) (make-posn (posn-x apos)(+ (posn-y apos) SPEED))]))

;; hit? : Posn Posn -> Boolean
;; checks to see if the missile hit the target
(check-expect (hit? (make-posn 50 50) (make-posn 50 50)) #true)
(check-expect (hit? (make-posn 0 0) (make-posn 500 500)) #false)
(define (hit? missile target)
    (and (<= (- (posn-x target) (/ TARGET-SIZE 2))
           (posn-x missile)
           (+ (posn-x target) (/ TARGET-SIZE 2)))
       (<= (- (posn-y target) (/ TARGET-SIZE 2))
           (posn-y missile)
           (+ (posn-y target) (/ TARGET-SIZE 2)))))
  