;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname binarytreeExample) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))
;; A BT is one of:
;; - Number
;; (make-node BT BT)

(define-struct node (left right))

(define bt1 (make-node 10 20))
(define bt2 (make-node 15 bt1))
(define bt3 (make-node bt2 bt1))

;;BT Number -> Boolean
;; checks if the number is contained int he tree
(check-expect (check-tree 5 10) #false)
(check-expect (check-tree bt2 10) #true)
(check-expect (check-tree bt3 4) #false)
(define (check-tree atree n)
  (cond [(number? atree) (= n atree)]
        [(node? atree) (or (check-tree (node-left atree) n)
                           (check-tree (node-right atree) n))]))

;; BT -> Number
;; sums the numbers in the tree
(check-expect (sum-tree bt1) 30)
(check-expect (sum-tree 5) 5)
(define (sum-tree atree)
  (cond [(number? atree) atree]
        [(node? atree) (+ (sum-tree (node-left atree))
                          (sum-tree (node-right atree)))]))


(define-struct leaf ())
(define-struct branch (key info left right))
;; A BST is one of:
;; - (make-leaf)
;; - (make-branch NaturalNumber String BST BST)
;; constraint:
;;  1. everything to the left of the root is less than the root
;;  2. everything to the right of the root is greater than the root
;;  3. 1 & 2 are true for every subtree

(define lf (make-leaf))
(define atree (make-branch 10 "a"
                           (make-branch 8 "b"
                                        (make-branch 7 "c" lf lf)
                                        lf)
                           (make-branch 12 "d"
                                        lf
                                        (make-branch 14 "e"
                                                     (make-branch 13 "f" lf lf)
                                                     lf))))


;; BST NaturalNumber -> String
;; retrieve the info that comes with a given key in the tree
(check-expect (retrieve atree 12) "d")
(check-expect (retrieve atree 13) "f")
(check-error (retrieve atree 19) "not found")
(check-error (retrieve lf 14) "not found")

(define (retrieve abst key)
  (cond [(leaf? abst) (error "not found")]
        [(branch? abst) (cond [(= (branch-key abst) key) (branch-info abst)]
                              [(> (branch-key abst) key)
                               (retrieve (branch-left abst) key)]
                              [else (retrieve (branch-right abst) key)])]))


(define test-tree0 (make-branch 10 "a" lf lf))


;; grow : BST NaturalNumber String -> BST
;; add a key and info to a BST
(check-expect (grow lf 14 "a") (make-branch 14 "a" lf lf))
(check-expect (grow test-tree0 11 "b")
              (make-branch 10 "a" lf
                           (make-branch 11 "b" lf lf)))
(check-expect (grow test-tree0 9 "b")
              (make-branch 10 "a"
                           (make-branch 9 "b" lf lf) lf))
(check-error (grow test-tree0 10 "b") "no new info")
(define (grow abst key info)
  (cond [(leaf? abst) (make-branch key info lf lf)]
        [(branch? abst) (cond [(= (branch-key abst) key) (error "no new info")]
                              [(> (branch-key abst) key)
                               (make-branch
                                (branch-key abst)
                                (branch-info abst)
                                (grow (branch-left abst) key info)
                                (branch-right abst))]
                              [else (make-branch
                                     (branch-key abst)
                                     (branch-info abst)
                                     (branch-right abst)
                                     (grow (branch-right abst) key info))])]))

(define-struct kip( key info))

(define BRANCH-LIST
  (list (make-kip 10 "a") (make-kip 11 "b")
        (make-kip 12 "c") (make-kip 13 "d")
        (make-kip 45 "x") (make-kip 155 "t")
        (make-kip 22 "ww") (make-kip 1 "ca")
        (make-kip 144 "f") (make-kip 25 "x")
        (make-kip 66 "rt") (make-kip 200 "cg")
        (make-kip 63 "xx") (make-kip 69 "xp")
        (make-kip 2 "er") (make-kip 5 "fff")))

(define test-list0
  (list (make-kip 10 "a")))
(define test-list1
  (list (make-kip 10 "a") (make-kip 11 "b")))

;; grow-tree : list-of-kip BST -> BST
;; grows a tree from a list of key-info pairs
(check-expect (grow-tree test-list0 lf) (make-branch 10 "a" lf lf))

(define (grow-tree lok abst)
  (if (cons? lok)
      (grow-tree (rest lok)
                 (grow abst (kip-key (first lok))
                       (kip-info (first lok)))) abst))



;; A LON is one of:
;; - '()
;; - (cons Number LON)


;; BST -> LON
;; makes a list of all of the keys in the tree
(check-expect (list-nums lf) '())
(check-expect (list-nums test-tree0)(cons 10 '()))
(check-expect (list-nums atree) (list 7 8 10 12 13 14))
(define (list-nums bst)
  (cond [(leaf? bst) '()]
        [(branch? bst) (append (list-nums (branch-left bst))
                               (list (branch-key bst))       
                               (list-nums (branch-right bst)))]))


(define-struct unary (exp))
(define-struct add (left right))
;; An Exp is one of:
;; - 'x
;; - Number
;; (make-unary Exp)
;; (make-add Exp Exp)

(define exp1 (make-add (make-unary 5) (make-add 6 'x)))
(define exp2 (make-add (make-unary (make-add 4 5))
                       (make-add (make-unary 'x) 'x)))


;; subst  : Exp Number -> Exp
;; replace all occurances of 'x with the given number
(check-expect (subst exp1 10) (make-add (make-unary 5) (make-add 6 10)))
(check-expect (subst 5 10) 5)
(check-expect (subst 'x 10) 10)

(define (subst anexp num)
  (cond [(symbol? anexp) num]
        [(number? anexp) anexp]
        [(unary? anexp) (make-unary (subst (unary-exp anexp) num))]
        [(add? anexp) (make-add (subst (add-left anexp) num)
                           (subst (add-right anexp) num))]))

