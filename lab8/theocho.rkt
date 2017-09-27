;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname theocho) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "convert.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp")) #f)))
;; ###############################################
;; Public Transit
;; ###############################################

; A Station is a (make-station String [List-of String] [List-of String])
(define-struct station [name lines neighbors])
; - where name is the name of the station
; - lines is the list of lines that this station is on
; - and neighbors is the list of stations you can get to
; - immediately from this one

(define DOWNTOWN
  (list (make-station "North Station" '("orange" "green") '("Haymarket"))
        (make-station "Bowdoin" '("blue") '("Govt Center"))
        (make-station "Haymarket" '("orange" "green")
                      '("North Station" "Govt Center" "State"))
        (make-station "Govt Center" '("green" "blue")
                      '("Bowdoin" "Haymarket" "Park St" "State"))
        (make-station "Aquarium" '("blue") '("State"))
        (make-station "Park St" '("red" "green")
                      '("Govt Center" "Boylston" "Downtown Crossing"))
        (make-station "State" '("orange" "blue")
                      '("Haymarket" "Govt Center" "Aquarium"
                                    "Downtown Crossing"))
        (make-station "Boylston" '("green") '("Park St"))
        (make-station "Downtown Crossing" '("red" "orange")
                      '("Park St" "State" "Chinatown" "South Station"))
        (make-station "Chinatown" '("orange") '("Downtown Crossing"))
        (make-station "South Station" '("red") '("Downtown Crossing"))))
(define GREENLINE-SPLIT
  (list (make-station "Copley" '("green")
                      '("Arlington" "Prudential" "Hynes Convention Ctr"))
        (make-station "Hynes Convention Ctr" '("green") '("Copley" "Kenmore"))
        (make-station "Kenmore" '("green")
                      '("Hynes Convention Ctr"
                        "Blandford St" "St Marys St" "Fenway"))
        (make-station "Prudential" '("green") '("Copley"))
        (make-station "Blandford St" '("green") '("Kenmore"))
        (make-station "St Marys St" '("green") '("Kenmore"))
        (make-station "Fenway" '("green") '("Kenmore"))))


; can-ride-in-time? : String String Number [List-of Station] -> Boolean
; Can I get from a to b in time t?
(check-expect (can-ride-in-time? "Chinatown" "Aquarium" 10 DOWNTOWN) true)
(check-expect (can-ride-in-time? "Fenway" "Blandford St" 5 GREENLINE-SPLIT)
              false)
(define (can-ride-in-time? a b t the-map)
  (local [(define the-station (get-station a the-map))]
    (cond [(string=? a b) #true]
          [else
           (and (>= t 3)
                (ormap
                 (λ (neighbor) (can-ride-in-time? neighbor b (- t 3) the-map))
                 (station-neighbors the-station)))])))

; get-station : String [List-of Station] -> Station
; Produces the first station with this name
(define (get-station the-name the-map)
  (cond [(empty? the-map) (error (string-append "Could not find station "
                                                station-name))]
        [(cons? the-map)
         (if (string=? (station-name (first the-map)) the-name)
             (first the-map) (get-station the-name (rest the-map)))]))

;; all-possible-subsets : [List-of Station] -> [List-of [List-of String]]
;; produces the powerset of all station names
(define AJ-SPLIT (list (make-station "Park" '("aj")
                                     (list "Bwalk"))
                       (make-station "Bwalk" '("aj")
                                     (list "Park" "Go!"))
                       (make-station "Go!" '("aj")
                                     (list "Park"))))
(check-expect (all-possible-subsets AJ-SPLIT)
              (list
               (list "Park")
               (list "Park" "Bwalk")
               (list "Park" "Bwalk" "Go!")
               (list "Park" "Go!")
               (list "Bwalk")
               (list "Bwalk" "Go!")
               (list "Go!")))
(define (all-possible-subsets the-map)
  (cond [(empty? the-map) '()]
        [else (sets-union (set-add (station-name (first the-map))
                                   (all-possible-subsets (rest the-map)))
                          (all-possible-subsets (rest the-map)))]))

;; set-add : Element List -> List-of [List-of Elements]
;; unions a set with all of the sets in a list
(check-expect (set-add 'a '( b c )) (list (list 'a) '(a b) '(a c)))
(check-expect (set-add 'b (list (list 'a 'c) (list 'a) 'd))
              (list (list 'b) (list 'b 'a 'c)
                    (list 'b 'a)
                    (list 'b 'd)))
(define (set-add anel los)
  (cons (list anel)
        (map (λ (set) (if (list? set)
                          (cons anel set)
                          (cons anel (list set)))) los)))

;; sets-union : Set Set -> Set
;; Unions two lists of elements
(check-expect (sets-union '(a b c) '(d c a)) '(a b c d))
(define (sets-union aset bset)
  (append aset (filter (λ (x) (not (member? x aset))) bset)))



;; all-connected? : List<String> Map -> Boolean
;; returns true if all stations are connected on the map
(check-expect (all-connected? (list "Park" "Bwalk") AJ-SPLIT) #true)
(check-expect (all-connected? (list "Park" "Bwalk" "Go!") AJ-SPLIT) #false)
(define (all-connected? lost map)
  (andmap (λ (stat)
            (is-connected? stat
                           (filter
                            (λ (x) (not (equal? x stat))) lost) map)) lost))


;; is-connected? : String List<String> Map -> Boolean
;; returns true if the station is connected to all other stations in the list
(check-expect (is-connected? "Park" (list "Bwalk") AJ-SPLIT) #true)
(check-expect (is-connected? "Park"
                             (list "Bwalk" "Go!") AJ-SPLIT) #false)
(define (is-connected? stat lost map)
  (andmap (λ (statcomp) (member? statcomp (get-conn-list stat map))) lost))

;; get-conn-list : String Map -> List<String>
;; returns a list of connected stations
(check-expect (get-conn-list "Park" AJ-SPLIT) (list "Bwalk"))
(check-expect (get-conn-list "Bwalk" AJ-SPLIT) (list "Park" "Go!"))
(define (get-conn-list stat map)
  (cond [(empty? map) '()]
        [(string=? stat (station-name (first map)))
         (station-neighbors (first map))]
        [else (get-conn-list stat (rest map))]))


;; biggest-cycle : Map -> List<Station>
;; returns the largest list of stations that are all connected
(check-expect (biggest-cycle AJ-SPLIT) (list "Park" "Bwalk"))
#;(define (biggest-cycle map)
  (foldr (λ (x y) (if (>= (length x) y)
                      x
                      (length x))) 0
         (filter (λ (sta) (all-connected? sta map))
                 (all-possible-subsets map))))
