#lang racket

(define qlst1 '( ("The Big Short" 1) ("Spotlight" 2)  ("Room" 3) ("Zootopia" 4)) )
(define qlst2 '( ("The Lady in the Van" 4) ("The Witch" 2)  ("Hail, Caesar" 3) ("Deadpool" 1) ("Brooklyn" 5)) )
(define qlst3 '( ("Creed" 1) ("Revenant" 2)))
(define t '( "Moonlight" 3))
(define mqueue '())

;(name  m) // returns the name of the movie m
(define name
  (lambda (m)
    (car m)
 ))
;(priority  m) // returns the priority of the movie m
(define priority
  (lambda (m)
    (car (cdr m))
 ))
; (compare m1 m2)  returns #t if movie m1  has a lower priority than movie m2. Otherwise return #f.
(define compare
  (lambda (m1 m2)
    (< (priority m1)(priority m2))
 ))


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
