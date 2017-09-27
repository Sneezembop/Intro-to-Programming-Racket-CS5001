;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Loop) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))
;;> Total: 43/45
;;> append-from-fold: can be written as : (foldr cons lox2 lox1).

;; #####################################
;; Loop Functions
;; #####################################


(define usd-list (list 1.50 1 5.50))
;; convert-euro : List-of-Number -> List-of-Number
;; converts a list of USD values to EUR
(check-expect (convert-euro usd-list) (list 1.83 1.22 6.71))
(define (convert-euro alon )
  (local [;; Number-> Number
          ;; converts a USD value to EUR
          (define (make-euro num)
            (* 1.22 num))]
    (map make-euro alon)))


(define far-list (list -40 32 212))
;; convertFC : List-of-Number -> List-of-Number
;; converts a list of Fahrenheit values to Celsius
(check-within (convertFC far-list) (list -40 0 100) .1)
(define (convertFC alon)
  (local [;; Number->Number
          ;; converts Far to Cel
          (define (conFC num)
            (* (/ 5 9) (- num 32)))]
    (map conFC alon)))


(define posn-list (list (make-posn 50 25)
                        (make-posn 10 10)
                        (make-posn 30 60)))
;; translate : List-of-Posn -> List-of-List-of-Number
;; converts a list of Posn to a list of pairs of numbers
(check-expect (translate posn-list)
              (list (list 50 25) (list 10 10) (list 30 60)))
(define (translate alon)
  (local [;;Posn -> List-of Numbers
          ;; converts a posn to a list of num
          (define (conPosn apos)
            (list (posn-x apos) (posn-y apos)))]
    (map conPosn alon)))


;; An IRecord (IR) is a
;; (make-ir String String Number Number)
(define-struct ir (name desc acq$ sal$))

(define ir0 (make-ir "coffee" "hot black liquid" 0.5 4.50))
(define ir1 (make-ir "oil" "hot black liquid" 100 5000))
(define ir2 (make-ir "tar" "hot black liquid" 25 250))
(define ir3 (make-ir "ichor" "hot black liquid" 5 10))

;; A List-of-IRecord (LoIR) is one of:
;; - '()
;; - (cons IR LoIR)

(define loir0 (list ir0 ir1 ir2 ir3))
(define loir1 (list ir0 ir3))

;; eliminate-expensive : Number LoIR -> LoIR
;; removes all elements from a list above a specified pricepoint
(check-expect (eliminate-expensive 200 loir0) loir1)
(define (eliminate-expensive ua aloir)
  (local [;; IR -> Boolean
          ;; checks if an IR is below a certain price
          (define (expensive? anir)
            (> ua (ir-sal$ anir)))]
    (filter expensive? aloir)))


;; build-evens : Number -> List-of-Number
;; makes a list of the first N even numbers
(check-expect (build-evens 4) (list 2 4 6 8))
(define (build-evens n)
  (local [;; Number -> Number
          ;; Adds 1 and multiplies by 2
          (define (mult2 x)
            (* 2 (add1 x)))]
  (build-list n mult2)))


;; append-from-fold : List<X> List<Y> -> List<X & Y>
;; combines multiple lists into a single list
(check-expect (append-from-fold (list 1 2 3) (list 'a 'b 'c))
              (list 1 2 3 'a 'b 'c))
(check-expect (append-from-fold (list 1 2 3) '()) (list 1 2 3))
(define (append-from-fold list1 list2)
  (local [;; List -> List
          ;; combines both lists
          (define (combine-list x y)
            (if (empty? y)
                (cons x list2)
                (cons x y)))]
    (foldr combine-list '() list1)))
             

;; map-from-foldr : [X -> Z] [List-of-X] -> [List-of-Z]
;; maps an operation onto every element in a list
(check-expect (map-from-foldr '(1 2 3) add1) '(2 3 4))
(define (map-from-foldr alon op)
  (local [;; X -> [List-of Z]
          ;; Maps the operation
          (define (applyOp x y)
            (cons (op x) y))]
  (foldr applyOp '() alon)))
