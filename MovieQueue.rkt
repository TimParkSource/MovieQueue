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

;d. (sort qlst  compare ) works Created a Helper function to call it
(define sortedList
  (lambda (qlst)
    (sort qlst compare)
    ))

;e. (printMovie m )    //prints the movie m 
(define printMovie
  (lambda(m)
    (display (name m)) (display #\:) (display (priority m))
 ))

;f. (printQueue  qlst )   // prints the movie queue qlst using printMovie.
; works but has an error --------------------------------------------------------------------------------!
(define printQueue
  (lambda (qlst)
    (if (> 0 (length qlst))
        '()
        ( (newline) (printMovie (car (sort qlst compare) ) ) (printQueue (cdr (sort qlst compare))) )
    ;(cond
     ; ((null? qlst) #f)
     ; (= 1 (length qlst)  (newline) (printMovie (car (sort qlst compare) ) ))
    ;  (else (newline) (printMovie (car (sort qlst compare) ) ) (printQueue (cdr (sort qlst compare))) )
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
(define getPriority
  (lambda (s qlst)
    (cond
      ((null? qlst) #f)
      ( (equal? s (name(car qlst)) ) car(car(cdr(car qlst))))
      (else (getPriority s (cdr qlst)))
 )))

;i. (getName  k  qlst)  returns the name of the  movie in qlst with priority k.
(define getName
  (lambda (k qlst)
    (cond
      ((null? qlst) #f)
      ( (equal? k (priority(car qlst)) ) car(car(car qlst)))
      (else (getName k (cdr qlst)))
 )))

;Helper function to update the priority. Function recieves lst and x is 1 to start. List is updated and reassembled
(define updateLst
  (lambda (qlst x)
    (cond
      ((empty? qlst) '())
      ((= 1 (length qlst)) (list(reverse(cons x (cdr (reverse(car (sortedList qlst))))))))     ;
      (else (cons (reverse(cons x (cdr (reverse(car (sortedList qlst)))))) (updateLst (cdr(sortedList qlst)) (+ 1 x) )))
 )))
;helper function to order j and k
(define delete
  (lambda (k lst)
    (map (lambda (x)
      (cons (name x) (list
        (if (>= (priority x) k)
          (- (priority x) 1)
          (priority x)
           )))) lst)
    )
  )

;j. (removeByPriority  k qlst  )  
; needs to be ordered --------------------------------------------------------------------------------!
(define removeByPriority
  (lambda (k qlst)
    (cond
      ((empty? qlst) '())
      ((equal? k (priority( car (sortedList qlst)))) (cdr (sortedList qlst)))
      (else (cons(car(sortedList qlst)) (removeByPriority k (cdr(sortedList qlst))))))
      (updateLst qlst 1)
      
 ))

;k. (removeByName  s  qlst  )
; needs to be ordered --------------------------------------------------------------------------------!
(define removeByName
  (lambda (s qlst)
    (cond
      ((empty? qlst) '())
      ((equal? s (name( car (sortedList qlst)))) (cdr (sortedList qlst)))
      (else (cons (car (sortedList qlst)) (removeByName s (cdr (sortedList qlst)))))
 )))

;l. (addMQ  s qlst)
(define addMQ
  (lambda (s qlst)
    (reverse(cons(cons s( cons (+ 1 (length qlst)) '())) (reverse(sortedList qlst))))
    ))


;helper function used by insertMQ
(define insert
  (lambda (k lst)
    (map (lambda (x)
         (cons (name x) (list
           (if (>= (priority x) k)
               (+ (priority x) 1)
               (priority x)))))
    lst)
 ))
;m. (insertMQ  s k qlst)
(define insertMQ
  (lambda (s k qlst)
    (sort (cons(cons s (list k))(insert k qlst))compare)
 ))

;n. (updatePriority s  k  qlst)
(define updatePriority
  (lambda (s k qlst)
    (if (memberMQ? s qlst)
        (if (<= k (length qlst))
            (insertMQ s k (removeByName s qlst))
            (ssortedList qlst)
            )
        (sortedList qlst)
 )))

;o. (validMQ?  z)
(define validMQ?
  (lambda (z)
    (if(null? z) #t
       (andmap (lambda (x)
         (if (pair? x)
           (if (string? (car x))
             (if (number? (car (cdr x)))
               #t 
               #f
              )
           #f
           )
         #f
         )
         )
         z)
 )))

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
