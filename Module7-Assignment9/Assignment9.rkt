;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;> Total: 35/36
;;> Overall good code.
;;> You need to handle the cases where the paths are empty.

;; A Node is a Symbol 
;; INTERP: represents the name of a node in a graph

;; A Distance is a PosInt
;; INTERP: represents distance in miles

;; An Edge is (list Node Distance Node)
;; e.g. (list 'A 10 'B)
;; INTERP: represents an edge from 'A to 'B with the
;;         distance from 'A to 'B being 10 miles

;; A Path is a List<Edge>
;; A Graph is a Set<Edge>
;; NOTE: you can use the definition of Set from your previous assignment.


(define te0 (list 'A 10 'B))
(define te1 (list 'B 15 'C))
(define te2 (list 'C 10 'D))
(define te3 (list 'D 40 'E))
(define te4 (list 'R 100 'Q))

(define test-path0 (list te0 te1 te2 te3))
(define test-path1 (list te0 te2 te3))

(define test-graph0 (list te0 te1 te2 te3 te4))

;; valid-path? : Graph Path -> Boolean
;; returns true if the path can be followed in order on a given graph
(check-expect (valid-path? test-graph0 test-path0) #true)
(check-expect (valid-path? test-graph0 test-path1) #false)
(define (valid-path? agra apath)
  (local [;; Edge Symbol Path -> Boolean
          ;; itterates through the path checking that each edge is OK
          (define (path-check curedge curloc thepath)
            (cond [(empty? thepath) (and (symbol=? curloc (first curedge))
                                         (member? curedge agra))]
                  [else (and (symbol=? curloc (first curedge))
                             (member? curedge agra)
                             (path-check (first thepath)
                                         (third curedge)
                                         (rest thepath)))]))]
    (path-check (first apath) (first (first apath)) (rest apath))))

;; valid-st-path? : Graph Symbol Symbol Path -> Boolean
;; returns true if the path can be followed
;;   from the start symbol to the end symbol
(check-expect (valid-st-path? test-graph0 'B 'D test-path0) #true)
(check-expect (valid-st-path? test-graph0 'A 'E test-path1) #false)
(check-expect (valid-st-path? test-graph0 'D 'B test-path0) #false)
(check-expect (valid-st-path? test-graph0 'F 'G test-path0) #false)
(define (valid-st-path? g s t p)
  (local [;; Path Symbol -> Number
          ;; returns the index of the edge that starts at the start symbol
          (define (start-edge apath astart accu)
            (cond [(empty? apath) -1]
                  [(symbol=? astart (first (first apath))) accu]
                  [else (start-edge (rest apath) astart (+ 1 accu))]))
          ;; List Number -> List
          ;; removes the first N items in the list
          (define (remove-list-items alist accu)
            (cond [(or (empty? alist)
                       (= accu -1))'()]
                  [(= accu 1) alist]
                  [else (remove-list-items (rest alist) (sub1 accu))]))
          ;; Path Symbol -> Number
          ;; Same as Start-Edge but for the end node
          (define (end-edge apath anend accu)
            (cond [(empty? apath) -1]
                  [(symbol=? anend (third (first apath))) accu]
                  [else (end-edge (rest apath) anend (+ 1 accu))]))
          ;; Path -> Path
          ;; Shortenes the path so it starts at the Start and ends at the End
          (define (shorten-path apath)
            (remove-list-items
             (reverse
              (remove-list-items
               (reverse p)
               (end-edge p t 0))) (start-edge p s 0)))
          ;; Define the shortened path so we can use it more easily later
          (define sh-p (shorten-path p))]
    (if (empty? sh-p)
        #false
        (valid-path? g sh-p))))

;; neighbors : Graph Symbol -> List<Edge>
;; Returns all the edges that start with the given symbol
(check-expect (neighbors test-graph0 'B) (list te1))
(check-expect (neighbors test-graph0 'Q) '())
(define (neighbors g s)
  (filter (λ (x) (symbol=? s (first x))) g))


;; defining more test graphs
(define edge0 (list 'A 10 'B))
(define edge1 (list 'A 15 'C))
(define edge2 (list 'B 25 'D))
(define edge3 (list 'C 50 'E))
(define edge4 (list 'D 45 'A))
(define edge5 (list 'D 15 'E))

(define graph0 (list edge0 edge1 edge2 edge3 edge4 edge5))

;; find-st-path : Graph Symbol Symbol -> Path or Boolean
;; returns a path from the start to the finish or #false if no path exists
(check-expect (find-st-path test-graph0 'B 'D) (list te1 te2))
(check-expect (find-st-path test-graph0 'B 'B) '())
(check-expect (find-st-path test-graph0 'A 'R) #false)
(check-expect (find-st-path test-graph0 'F 'G) #false)
(check-expect (find-st-path graph0 'A 'E) (list edge0 edge2 edge4 edge1 edge3))
(define (find-st-path g s t)
  (cond[(symbol=? s t) '()]
       [else (local[;;
                    (define (build-path a)
                      (find-st-path/edges g (neighbors g s) t))]
               (cond[(boolean? (build-path 1)) #false]
                    [else (build-path 1)]))]))




;; find-st-path/edges : Graph List<Edge> Symbol -> Path or Boolean
;; returns all possible paths from each edge to the destination.
(check-expect (find-st-path/edges test-graph0 (list te1) 'D) (list te1 te2))
(check-expect (find-st-path/edges test-graph0 (list te4) 'E) #false)
(define (find-st-path/edges g loe t)
  (cond[(empty? loe) #false]
       [else (local[;;
                    (define (build-path a)
                      (find-st-path (remove (first loe) g)
                                    (third (first loe)) t))]
               (cond
                 [(boolean? (build-path 1))
                  (find-st-path/edges g (rest loe) t)]
                 [else (append (list (first loe)) (build-path 1))]))]))




;; find-shortest-distance-st-path : Graph Symbol Symbol -> Path or Boolean
;; returns th shortest valid path from s to t in a graph
(check-expect (find-shortest-distance-st-path test-graph0 'B 'D)
              (list te1 te2))
(check-expect (find-shortest-distance-st-path graph0 'A 'E)
              (list edge0 edge2 edge5))
(check-expect (find-shortest-distance-st-path test-graph0 'A 'R) #false)
(define (find-shortest-distance-st-path g s t)
  (foldr (λ (x y) (if (<= (path-length x) (path-length y))x y)) #false
         (map (λ (x) (find-st-path x s t)) (shuffle-graphs g))))


;; path-length : Path or Boolean -> Number
;; returns the length of a path
(check-expect (path-length (list te1 te2)) 25)
(check-expect (path-length #false) 100000)
(define (path-length p)
  (cond[(boolean? p) 100000]
       [(empty? p) 0]
       [else (+ (second (first p)) (path-length (rest p)))]))


;; shuffle-graphs : Graph -> List<Graph>
;; creates all possible orderings of graph elements
;; NOTE: I had to look up some advice on how to make this function as
;;    I couldn't use the "permutation" function in the student language.
;;    This basically works by creating a list with the new element inserted
;;    into every possible location. This will guarentee that I'll
;;    get every possible permutation of the graph. This function is pretty
;;    inefficient but will work to make sure that I find the shortest path.
(check-expect (shuffle-graphs '()) (list '()))
(check-expect (shuffle-graphs (list te0 te1)) (list (list te0 te1)
                                                    (list te1 te0)))
(check-expect (shuffle-graphs (list te0 te1 te2)) (list (list te0 te1 te2)
                                                        (list te1 te0 te2)
                                                        (list te1 te2 te0)
                                                        (list te0 te2 te1)
                                                        (list te2 te0 te1)
                                                        (list te2 te1 te0)))
                                                      
(define (shuffle-graphs g)
  (cond[(empty? g)(list empty)]
       [else
        (apply append
               (map (λ (x)
                      (map (λ (n) (insert x n (first g)))
                           (build-list (add1 (length x)) identity)))
                    (shuffle-graphs (rest g))))]))

;; insert : List<X> Number X -> List<X>
;; inserts the element into the list at the given location
(check-expect (insert '(A B C D) 2 'E) '(A B E C D))
(check-expect (insert '() 5 'E) '(E))
(check-expect (insert '(A B C D) 0 'E) '(E A B C D))
(define (insert alist loc element)
  (if (or (= 0 loc) (empty? alist))
      (cons element alist)
      (cons (first alist) 
            (insert (rest alist) (sub1 loc) element))))


