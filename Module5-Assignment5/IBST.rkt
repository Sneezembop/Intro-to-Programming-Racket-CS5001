;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname IBST) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))
;;> Total: 78/78

;; #############################################
;; IBST PROBLEM SET
;; #############################################

;; An Integer Binary Search Tree (IBST) is one of:
;; - (make-leaf)
;; - (make-branch Number IBST IBST)
;; constraint:
;;  1. everything to the left of the root is less than the root
;;  2. everything to the right of the root is greater than the root
;;  3. 1 & 2 are true for every subtree
(define-struct leaf())
(define-struct branch(num left right))


;; Template IBST -> ?????
#;(define (ibst-temp abst)
    (cond [(leaf? abst) ...]
          [(branch? abst) ... (branch-num abst) ...
                          ... (ibst-temp (branch-left abst)) ...
                          ... (ibst-temp (branch-right abst))]))

(define ibst0 (make-leaf))
(define ibst1 (make-branch 2 (make-branch 1 ibst0 ibst0) ibst0))
(define ibst2
  (make-branch 5
               (make-branch 2
                            (make-branch 1 ibst0 ibst0)
                            (make-branch 4
                                         (make-branch 3 ibst0 ibst0) ibst0))
               (make-branch 7 ibst0 ibst0)))
(define lf (make-leaf))

;; ibst-contains? : IBST Number -> Boolean
;; checks to see if an IBST contains the given number
(check-expect (ibst-contains? ibst0 1) #false)
(check-expect (ibst-contains? ibst1 2) #true)
(check-expect (ibst-contains? ibst2 3) #true)
(define (ibst-contains? abst num)
  (cond [(leaf? abst) #false]
        [(branch? abst) (or (= num (branch-num abst))
                            (ibst-contains? (branch-left abst) num)
                            (ibst-contains? (branch-right abst) num))]))

;; ibst-add : IBST Number -> IBST
;; adds a new number to the IBST
(check-expect (ibst-add ibst0 1) (make-branch 1 lf lf))
(check-expect (ibst-add ibst1 3) (make-branch 2 (make-branch 1 lf lf)
                                              (make-branch 3 lf lf)))
(check-expect (ibst-add ibst2 2) ibst2)

(define (ibst-add abst num)
  (cond [(leaf? abst) (make-branch num lf lf)]
        [(branch? abst) (cond [(< num (branch-num abst))
                               (make-branch
                                (branch-num abst)
                                (ibst-add (branch-left abst) num)
                                (branch-right abst))]
                              [(> num (branch-num abst))
                               (make-branch
                                (branch-num abst)
                                (branch-left abst)
                                (ibst-add (branch-right abst) num))]
                              [else abst])]))

;; A List of Numbers (LoN) is one of:
;; - '()
;; - (cons Number LoN)

;; ibst-inorder : IBST -> LoN
;; traverses the ibst in order and returns a sorted List of Numbers
(check-expect (ibst-inorder ibst0) '())
(check-expect (ibst-inorder ibst1) (list 1 2))
(check-expect (ibst-inorder ibst2) (list 1 2 3 4 5 7))
(define (ibst-inorder abst)
  (cond [(leaf? abst) '()]
        [(branch? abst) (append
                         (ibst-inorder (branch-left abst))
                         (list (branch-num abst))
                         (ibst-inorder (branch-right abst)))]))

;; ibst-postorder : IBST -> LoN
;; traverses the ibst in post-order and returns a sorted List of Numbers
(check-expect (ibst-postorder ibst0) '())
(check-expect (ibst-postorder ibst1) (list 1 2))
(check-expect (ibst-postorder ibst2) (list  1 3 4 2 7 5))
(define (ibst-postorder abst)
  (cond [(leaf? abst) '()]
        [(branch? abst) (append 
                         (ibst-postorder (branch-left abst))
                         (ibst-postorder (branch-right abst))
                         (list (branch-num abst)))]))

;; ibst-preorder : IBST -> LoN
;; traverses the ibst in pre-order and returns a sorted List of Numbers
(check-expect (ibst-preorder ibst0) '())
(check-expect (ibst-preorder ibst1) (list 2 1))
(check-expect (ibst-preorder ibst2) (list  5 2 1 4 3 7))
(define (ibst-preorder abst)
  (cond [(leaf? abst) '()]
        [(branch? abst) (append
                         (list (branch-num abst))
                         (ibst-preorder (branch-left abst))
                         (ibst-preorder (branch-right abst)))]))

;; ibst-depth : IBST -> Number
;; returns the depth of the IBST
(check-expect (ibst-depth ibst0) 0)
(check-expect (ibst-depth ibst1) 2)
(check-expect (ibst-depth ibst2) 4)
(define (ibst-depth abst)
  (cond [(leaf? abst) 0]
        [(branch? abst)
         (+ 1 (max
               (ibst-depth (branch-left abst))
               (ibst-depth (branch-right abst))))]))

;; is-ibst? : Anything -> Boolean
;; checks to see that the IBST is valid
(check-expect (is-ibst? ibst0) #true)
(check-expect (is-ibst? ibst2) #true)
(check-expect (is-ibst? (make-branch 1 (make-branch 3 lf lf) lf)) #false)
(check-expect (is-ibst? 1) #false)
(define (is-ibst? n)
  (cond [(leaf? n) #true]
        [(branch? n)
         (and (if (branch? (branch-left n))
                  (< (branch-num (branch-left n)) (branch-num n))
                  #true)
              (if (branch? (branch-right n))
                  (> (branch-num (branch-right n)) (branch-num n))
                  #true)
              (is-ibst? (branch-left n))
              (is-ibst? (branch-right n)))]
        [else #false]))

;; ibst-remove : IBST Number -> IBST
;; checks for and removes the number from the IBST
(check-expect (ibst-remove ibst0 1) ibst0)
(check-expect (ibst-remove ibst1 1) (make-branch 2 lf lf))
(check-expect (ibst-remove ibst2 4) (make-branch 5
               (make-branch 2
                            (make-branch 1 lf lf)
                            (make-branch 3 lf lf))
               (make-branch 7 lf lf)))

(define (ibst-remove abst num)
  (if (ibst-contains? abst num)
      (ibst-build (lon-remove (ibst-postorder abst) num))
      abst))


;; lon-remove : LoN Number -> LoN
;; removes the Number from a LoN
(check-expect (lon-remove (list 1 2 3 4) 3) (list 1 2 4))
(check-expect (lon-remove (list 1 2 3 4) 5) (list 1 2 3 4))
(define (lon-remove alon num)
  (cond [(empty? alon) '()]
        [(cons? alon) (if (= num (first alon))
                          (lon-remove (rest alon) num)
                          (cons (first alon) (lon-remove (rest alon) num)))]))

;; ibst-build : LoN -> IBST
;; builds an IBST from a List of Number
(check-expect (ibst-build '()) lf)
(check-expect (ibst-build (list 2 4 1)) (make-branch 1 lf (make-branch 4  
                                                     (make-branch 2 lf lf)lf)))
(define (ibst-build alon)
  (cond [(empty? alon) lf]
        [(cons? alon) (ibst-add (ibst-build (rest alon)) (first alon))]))