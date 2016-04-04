#lang racket

(define qlst1 '( ("The Big Short" 1) ("Spotlight" 2)  ("Room" 3) ("Zootopia" 4)) )
(define qlst2 '( ("The Lady in the Van" 4) ("The Witch" 2)  ("Hail, Caesar" 3) ("Deadpool" 1) ("Brooklyn" 5)) )
(define qlst3 '( ("Creed" 1) ("Revenant" 2)))
(define t '( "Moonlight" 3))
(define mqueue '())

;a. (name  m) // returns the name of the movie m
(define name
  (lambda (m)
    (car m)
 ))
;b. (priority  m) // returns the priority of the movie m
(define priority
  (lambda (m)
    (car (cdr m))
 ))
;c. (compare m1 m2)  returns #t if movie m1  has a lower priority than movie m2. Otherwise return #f.
(define compare
  (lambda (m1 m2)
    (< (priority m1)(priority m2))
 ))
;d. (sort qlst  compare ) works
;e. (printMovie m )    //prints the movie m
; not the right output ----------------------------------------------------------------------------------!
(define printMovie
  (lambda(m)
    (println(cons(name m) (priority m)))
 ))
;f. (printQueue  qlst )   // prints the movie queue qlst using printMovie.
; works but has an error --------------------------------------------------------------------------------!
(define printQueue
  (lambda (qlst)
    (if (null? qlst)
        '()
        ((printMovie(car(sort qlst compare))) (printQueue(cdr qlst)))
 )))
;g. ( memberMQ?   s  qlst)  // Checks if s is the name of a movie   in the movie queue qlst.
(define memberMQ?
  (lambda (s qlst)
    (cond
      ((null? qlst) #f)
      ( (equal? s (name(car qlst))) #t)
      (else (memberMQ? s (cdr qlst)))
 )))
;h. (getPriority  s  qlst)  returns the priority of the movie named s  in  qlst.
; priority(car(car qlst)) returning name ----------------------------------------------------------------!
(define getPriority
  (lambda (s qlst)
    (cond
      ((null? qlst) #f)
      ( (equal? s (name(car qlst))) priority(car(car qlst)))
      (else (getPriority s (cdr qlst)))
 )))
;i. (getName  k  qlst)  returns the name of the  movie in qlst with priority k.
(define getName
  (lambda (k qlst)
    (if (length qlst


;(define test1
;  (lambda ()
;    (begin
;      (displayln (priority t)) 
;      (printQueue qlst2)
;      (displayln (memberMQ? "The Witch" qlst2) )
;      (displayln (getPriority "Room" qlst1))
;      (displayln (getName 1 qlst2))
;      (displayln (sort (removeByPriority 2 qlst2) compare) )
;      (displayln (sort(removeByName "Spotlight" qlst1)compare ) )
;      (displayln qlst1)
;      (displayln (sort (addMQ "Only Yesterday" qlst2) compare ))
;      (displayln qlst2)
;      (displayln (sort (addMQ "Star Wars" qlst1)compare))
;      (displayln ( sort (insertMQ "Triple 9" 2 qlst2) compare))
;      (displayln  (sort ( updatePriority "The Lady in the Van" 5 qlst2) compare))
;      (displayln  (removeByPriority 1 ( addMQ "Big Short" (addMQ "The Martian" qlst3))))
;      (displayln  (insertMQ "Knight of Cups" 1 ( addMQ "Eddie the Eagle" mqueue)))
;      (displayln (validMQ? qlst1))
;      (displayln (validMQ? (list t)))
;      (displayln (validMQ? t))
;      (displayln (validMQ? '()))
;      
; )))
