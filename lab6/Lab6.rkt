;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ########################################
;; Higher-order functions
;; ########################################

;; map : [X -> Y] List<X> -> List<Y>
;; andmap: [X -> Boolean] List<X> -> Boolean
;; ormap: [X -> Boolean] List<X> -> Boolean
;; foldr: [X Y -> Y] Y List<X> -> Y
;; foldl: [X Y -> Y] Y List<X> -> Y



;; [Number -> String] List of Number -> List of String
;;> (map number->string (list 1 2 3 4))
;;(list "1" "2" "3" "4")


;; [Number Number -> Number] Number List-of-Number -> Number
;;> (foldr + 0 (list 1 2 3 4))
;;10


;;[Number Number -> Number] Number List-of-Number -> Number
;;> (foldl + 0 (list 1 2 3 4))
;;10

;; [String -> Number] ([Number -> String] List of Number -> List of String])
;;                       -> List of Number
;;> (map string-length
;;       (map number->string (list 12 234 12 3456 89765)))
;;(list 2 3 2 4 5)

;; [Number -> Boolean]
;;      ([String -> Number]
;;            ([Number -> String] List<Number> -> List<String>)
;;      -> List<Number>)
;; -> Boolean
;;> (andmap even?
;;          (map string-length
;;               (map number->string (list 12 234 12 3456 89765))))
;;#false

;; add1-append: Number List<Number> -> List<Number>
;; Given a number, n, and a list of numbers add 1 to n and
;; add the result to the list
#;(define (add1-append n lst)
    (cons (add1 n) lst))


;; [Number List<Number> -> List<Number>] List<Number> List<Number>
;;       -> List<Number>
;;> (foldl add1-append empty (list 1 2 3 4))
;;(list 5 4 3 2)

;; [Number List<Number> -> List<Number>] List<Number> List<Number>
;;       -> List<Number>
;;>  (foldr add1-append empty (list 1 2 3 4))
;;(list 2 3 4 5)

;; ########################################
;; Managing a class of students
;; ########################################


;; A Student is a:
;; (make-student String PosInt List<PosInt>)
(define-struct student(name id grades))

(define nick(make-student "Nick" 1 (list 90 95 85)))
(define oz(make-student "Oz" 2 (list 50 75 25)))
(define aj(make-student "Aj" 3 (list 100 100 100)))

(define los0 '())
(define los1 (list nick oz aj))

;; avg-grades : List<Student> [Student -> Number] -> List<Number>
;; returns a list of the student's average grades
(check-expect (avg-grades los0) '())
(check-expect (avg-grades los1) (list 90 50 100))
(define (avg-grades alos)
  (local [;; Student -> Number
          ;; returns the average of a list of numbers
          (define (list-avg astud)
            (/ (foldr + 0 (student-grades astud))
               (length (student-grades astud))))]
    (map list-avg alos)))


;; who-passed : List<Student> Number -> List<Student>
;; returns a list of students who have a passing avg grade
(check-expect (who-passed los0 95) '())
(check-expect (who-passed los1 95) (list aj))
(define (who-passed alos number)
  (local [;; Student -> Boolean
          ;; checks to see if the student passed
          (define (passed? astud)
            (< number
               (/ (foldr + 0 (student-grades astud))
                  (length (student-grades astud)))))]
    (filter passed? alos)))

;; who-failed: List<Student> Number -> List<Student>
;; returns a list of students who failed
(check-expect (who-failed los0 95) '())
(check-expect (who-failed los1 95) (list nick oz))
(define (who-failed alos number)
  (local [;; Student -> Boolean
          ;; checks to see if the student failed
          (define (passed? astud)
            (> number
               (/ (foldr + 0 (student-grades astud))
                  (length (student-grades astud)))))]
    (filter passed? alos)))

;; add-bonus: List<Student> Number -> List<Student>
;; adds bonus points to each student's grades
(check-expect (add-bonus los0 1) '())
(check-expect (add-bonus los1 10)
              (list (make-student "Nick" 1 (list 100 105 95))
                    (make-student "Oz" 2 (list 60 85 35))
                    (make-student "Aj" 3 (list 110 110 110))))
(define (add-bonus alos bonus)
  (local [;; Number -> Number
          ;; Adds the bonus to the grade
          (define (add-num anum)
            (+ anum bonus))
          ;; Student -> Student
          ;; Adds the bonus to the grades of the student
          (define (apply-bonus astud)
            (make-student
             (student-name astud)
             (student-id astud)
             (map add-num (student-grades astud))))]
    (map apply-bonus alos)))

;; subtract-bonus : List<Student> Number -> List<Student>
;; subtracts bonus points from each student's grades
(check-expect (subtract-bonus los0 1) '())
(check-expect (subtract-bonus los1 10)
              (list (make-student "Nick" 1 (list 80 85 75))
                    (make-student "Oz" 2 (list 40 65 15))
                    (make-student "Aj" 3 (list 90 90 90))))
(define (subtract-bonus alos bonus)
  (add-bonus alos (* -1 bonus)))


;; ########################################
;; Generic Binary Trees
;; ########################################

;; An SBT is one of
;; - empty
;; - (make-snode String SBT SBT)

(define-struct snode (val left right))

;; An IBT is one of
;; - empty
;; - (make-inode Integer IBT IBT)

(define-struct inode (val left right))

;; A BT<X> is one of
;; - empty
;; - (make-node X BT<X> BT<X>)
(define-struct node (val left right))


(define test-ibt0 empty)
(define test-ibt1 (make-inode 3 (make-inode 4 empty empty) empty))
;; ibt-sum: IBT -> Number
;; sums all the values in the IBT
(check-expect (ibt-sum test-ibt0) 0)
(check-expect (ibt-sum test-ibt1) 7)
(define (ibt-sum anibt)
  (cond [(empty? anibt) 0]
        [(inode? anibt) (+ (inode-val anibt)
                           (ibt-sum (inode-left anibt))
                           (ibt-sum (inode-right anibt)))]))

(define test-sbt0 empty)
(define test-sbt1 (make-snode "bloo" (make-snode "oop" empty empty) empty))
;; sbt-append: SBT -> String
;; appends all the strengs together
(check-expect (sbt-append test-sbt0) "")
(check-expect (sbt-append test-sbt1) "bloooop")
(define (sbt-append asbt)
  (cond [(empty? asbt) ""]
        [(snode? asbt)
         (string-append (snode-val asbt)
                        (sbt-append (snode-left asbt))
                        (sbt-append (snode-right asbt)))]))


(define test-bt0 empty)
(define test-bt1 (make-node 3 (make-node 4 empty empty) empty))
(define test-bt2 (make-node "bloo" (make-node "oop" empty empty) empty))
;; bt-op : BT<X> [X Y -> Y] Y -> Y
;; applies an operation to compound all the elements of a BT
(check-expect (bt-op test-bt0 + 0) 0)
(check-expect (bt-op test-bt1 + 0) 7)
(check-expect (bt-op test-bt2 string-append "") "bloooop")
(define (bt-op abt op base)
  (cond [(empty? abt) base]
        [(node? abt) (op (node-val abt)
                         (bt-op (node-left abt) op base)
                         (bt-op (node-right abt) op base))]))


;; ibt-sum2: BT<Number> -> Number
;; sums all the values in the BT
(check-expect (ibt-sum2 test-bt0) 0)
(check-expect (ibt-sum2 test-bt1) 7)
(define (ibt-sum2 abt)
  (bt-op abt + 0))


;; sbt-append2: BT<String> -> String
;; appends all the strengs together
(check-expect (sbt-append2 test-bt0) "")
(check-expect (sbt-append2 test-bt2) "bloooop")
(define (sbt-append2 abt)
  (bt-op abt string-append ""))
