;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define x
  (cons "Venus"
  (cons "Mercury"
    '())))

;8.3 programming with lists
;alon -> a list of names -> Boolean
;determines whether "Flatt" is contained in alon

(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))])) 

(define (contains-flatt-2? alon)
  (cond
    [(empty? alon) #false]
    [(string=? (first alon) "Flatt") #true]
    [else (contains-flatt-2? (rest alon))]))

(define (average ne-l)
  (/ (sum ne-l)
     (how-many ne-l)))
 
(define (sum ne-l) 
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (sum (rest ne-l)))])) 
 
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [else (+ (how-many (rest alot)) 1)]))

(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))
 

;tests
(check-expect (contains-flatt-2? (cons "Find" '()))
              #false)
(check-expect (contains-flatt-2? (cons "Flatt" '()))
              #true)
(check-expect
  (contains-flatt-2?
    (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)

(check-expect
 (contains-flatt-2?
  (cons "Fagan"
   (cons "Findler"
    (cons "Fisler"
      (cons "Flanagan"
        (cons "Flatt"
          (cons "Felleisen"
            (cons "Friedman" '()))))))))
 #true)

(check-expect
 (member? "Flatt" (cons "brian" (cons "Flatt" '())))
 #true)

(check-expect 
 (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)

(check-expect
 (how-many 
  (cons "Fagan"
   (cons "Findler"
    (cons "Fisler"
      (cons "Flanagan"
        (cons "Flatt"
          (cons "Felleisen"
            (cons "Friedman" '()))))))))
 7)

(check-expect
 (average (cons 2 (cons 1 (cons 3 '())))) 2)

(check-expect
 (average (cons 1 '())) 1)

(check-expect
 (cat
  (cons "Fagan"
   (cons "Findler"
    (cons "Fisler"
      (cons "Flanagan"
        (cons "Flatt"
          (cons "Felleisen" '())))))))
 "FagenFindlerFislerFlanaganFlattFelleisen")






  