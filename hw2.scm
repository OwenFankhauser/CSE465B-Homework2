#lang scheme
; ---------------------------------------------
; DO NOT REMOVE OR CHANGE ANYTHING UNTIL LINE 26
; ---------------------------------------------

; zipcodes.scm contains all the US zipcodes.
; This file must be in the same folder as hw2.scm file,
; and you should not modify it. Your code
; should work for other instances of this file.
(require "zipcodes.scm")

; Helper function
(define (mydisplay value)
	(display value)
	(newline)
)

; Helper function
(define (line func)
        (display "--------- ")
        (display func)
        (display " ------------")
        (newline)
)

; ================ Solve the following functions ===================
; Return a list with only the negatives items
(define (negatives lst)
  (cond
    [(null? lst) '()] ;Empty...basecase
    [(negative? (car lst))
     (cons (car lst) (negatives (cdr lst)))] ;Keep negative
    [else (negatives (cdr lst))] ;Discard positive
    )
)

(line "negatives")
(mydisplay (negatives '()))  ; -> ()
(mydisplay (negatives '(-1)))  ; -> (-1)
(mydisplay (negatives '(-1 1 2 3 4 -4 5)))  ; -> (-1 -4)
(mydisplay (negatives '(1 1 2 3 4 4 5)))  ; -> ()
(line "negatives")
; ---------------------------------------------

; Returns true if the two lists have identical structure
; in terms of how many elements and nested lists they have in the same order
(define (struct lst1 lst2)
  (cond
    [(and (null? lst1) (null? lst2)) #t] ;Both list empty at the same time...basecase
    [(or (null? lst1) (null? lst2)) #f] ;One list empties before the other
    [(and (list? (car lst1)) (list? (car lst2))) ;If elements are lists, check their structures
     (and (struct (car lst1) (car lst2)) 
          (struct (cdr lst1) (cdr lst2)))]
    [(or (list? (car lst1)) (list? (car lst2))) #f] ;one is a list, one is not
    [else (struct (cdr lst1) (cdr lst2))] ;recursion
    )
)

(line "struct")
(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c))))  ; -> #t
(mydisplay (struct '(a b c d (c a b)) '(1 2 3 (a b c))))  ; -> #f
(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c) 0)))  ; -> #f
(line "struct")
; ---------------------------------------------

; Returns a list of two numeric values. The first is the smallest
; in the list and the second is the largest in the list. 
; lst -- contains numeric values, and length is >= 1.
(define (minAndMax lst)
  (define (holder lst curr_min curr_max)
  (cond
    [(null? lst) (list curr_min curr_max)] ;Empty... basecase
    [else
     (holder (cdr lst)
             (if (< (car lst) curr_min) (car lst) curr_min) ;replace min
             (if (> (car lst) curr_max) (car lst) curr_max))])) ;replace max
  (holder (cdr lst) (car lst) (car lst))) ;initial call

(line "minAndMax")
(mydisplay (minAndMax '(1 2 -3 4 2)))  ; -> (-3 4)
(mydisplay (minAndMax '(1)))  ; -> (1 1)
(line "minAndMax")
; ---------------------------------------------

; Returns a list identical to the first list, while having all elements
; that are inside nested loops taken out. So we want to flatten all elements and have
; them all in a single list. For example '(a (a a) a))) should become (a a a a)
(define (flatten lst)
  (cond
    [(null? lst) '()] ;Empty... basecase
    [(list? (car lst)) ;if an element is a list
     (append (flatten (car lst)) (flatten (cdr lst)))] ;remove first element of nested list and appends in place
    [else (cons (car lst) (flatten (cdr lst)))])) ;if an element is an atom, do nothing

(line "flatten")
(mydisplay (flatten '(a b c)))  ; -> (a b c)
(mydisplay (flatten '(a (a a) a)))  ; -> (a a a a)
(mydisplay (flatten '((a b) (c (d) e) f)))  ; -> (a b c d e f)
(line "flatten")
; ---------------------------------------------

; The paramters are two lists. The result should contain the cross product
; between the two lists: 
; The inputs '(1 2) and '(a b c) should return a single list:
; ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
; lst1 & lst2 -- two flat lists.
(define (crossproduct lst1 lst2)
  (define (pair_first first lst)
    (cond
      [(null? lst) '()] ;Empty...basecase
      [else (cons (list first (car lst)) ;make a pair list
                  (pair_first first (cdr lst)))]))
  (cond
    [(null? lst1) '()] ;Empty...basecase
    [else (append (pair_first (car lst1) lst2) ; Pair the first element of lst1 with every element of lst2
                  (crossproduct (cdr lst1) lst2))])) ;recurison

(line "crossproduct")
(mydisplay (crossproduct '(1 2) '(a b c)))
(line "crossproduct")
; ---------------------------------------------

; Returns the first latitude and longitude of a particular zip code.
; if there are multiple latitude and longitude pairs for the same zip code,
; the function should only return the first pair. e.g. (53.3628 -167.5107)
; zipcode -- 5 digit integer
; zips -- the zipcode DB- You MUST pass the 'zipcodes' function
; from the 'zipcodes.scm' file for this. You can just call 'zipcodes' directly
; as shown in the sample example
(define (getLatLon zipcode zips)
    (define (find entries)
    (cond
      ((null? entries) '()) ; Empty...basecase
      ((= (car (car entries)) zipcode) ; Check first entry
       (let ((entry (car entries)))
         (if (= (length entry) 6) ;Validate line
             (list (list-ref entry 4) ; Find latitude (element 5) and longitude (element 6)
                   (list-ref entry 5))
             '()))) ;Return empty if not found
      (else (find (cdr entries))))) ;recursion
  (find zips)) ;initial call



(line "getLatLon")
(mydisplay (getLatLon 45056 zipcodes))
(line "getLatLon")
; ---------------------------------------------

; Returns a list of all the place names common to two states.
; state1 -- the first state to look for
; state2 -- the second state to look for
; zips -- the zipcode DB
(define (getCommonPlaces state1 state2 zips)
  '())

(line "getCommonPlaces")
(mydisplay (getCommonPlaces "OH" "MI" zipcodes))
(line "getCommonPlaces")
; ---------------------------------------------

; Returns the number of zipcode entries for a particular state.
; state -- state
; zips -- zipcode DB
(define (zipCount state zips)
  (define (count entries)
    (cond
      ((null? entries) 0) ;Empty... basecase
      ((equal? (caddr (car entries)) state) ; Check
       (+ 1 (count (cdr entries)))) ;If  match, add 1
      (else (count (cdr entries))))) ;If no match, do nothing

  (count zips)) ;intial call

(line "zipCount")
(mydisplay (zipCount "OH" zipcodes))
(line "zipCount")
; ---------------------------------------------

; Some sample predicates
(define (POS? x) (> x 0))
(define (NEG? x) (< x 0))
(define (LARGE? x) (>= (abs x) 10))
(define (SMALL? x) (not (LARGE? x)))

; Returns a list of items that satisfy a set of predicates.
; For example (filterList '(1 2 3 4 100) '(EVEN?)) should return the even numbers (2 4 100)
; (filterList '(1 2 3 4 100) '(EVEN? SMALL?)) should return (2 4)
; lst -- flat list of items
; filters -- list of predicates to apply to the individual elements

(define (filterList lst filters)
  (define (satisfies_all element filters)
    (cond
      ((null? filters) #t) ; Empty... basecase
      ((not ((car filters) element)) #f) ;Doesn't pass predicate check
      (else (satisfies_all element (cdr filters)))))
  (define (filter_elements lst filters)
    (cond
      ((null? lst) '()) ;Empty... basecase
      ((satisfies_all (car lst) filters); check?
       (cons (car lst) (filter_elements (cdr lst) filters)))
      (else (filter_elements (cdr lst) filters))))
  (filter_elements lst filters)) ;Initial call


(line "filterList")
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS? even?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS? even? LARGE?)))
(line "filterList")
; ---------------------------------------------

; #### Only for Graduate Students ####
; Returns a list of all the place names common to a set of states.
; states -- is list of state names
; zips -- the zipcode DB
(define (getCommonPlaces2 states zips)
	'("Oxford" "Franklin")
)

(line "getCommonPlaces2")
(mydisplay (getCommonPlaces2 '("OH" "MI" "PA") zipcodes))
(line "getCommonPlaces2")

; ---------------------------------------------

; #### Only for Graduate Students ####
; Returns the distance between two zip codes in "meters".
; Use lat/lon. Do some research to compute this.
; You can find some info here: https://www.movable-type.co.uk/scripts/latlong.html
; zip1 & zip2 -- the two zip codes in question.
; zips -- zipcode DB
(define (getDistanceBetweenZipCodes zip1 zip2 zips)
	0
)

(line "getDistanceBetweenZipCodes")
(mydisplay (getDistanceBetweenZipCodes 45056 48122 zipcodes))
(line "getDistanceBetweenZipCodes")
; ---------------------------------------------



