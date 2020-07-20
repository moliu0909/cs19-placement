;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname p2-code) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;imports
(require 2htdp/image)

;---------------------------------------------------------------------------

;score-by-length :: List-of-strings -> Number
;determines the sum of the lengths of words are in List-of-strings
(define (score-by-length alos)
  (cond
    [(empty? alos) 0]
    [else (+ (string-length (first alos)) (score-by-length (rest alos)))]))

;score-by-length tests
;empty list
(check-expect
 (score-by-length '())
 0)

;empty string
(check-expect
 (score-by-length (list "" "" "" ""))
 0)

;white space 
(check-expect
 (score-by-length (list " " " " " " ))
 3)


;capital & lower case
(check-expect
 (score-by-length (list "Me" "You" "Us"))
 7)

;numbers & non-alphanumeric characters
(check-expect
 (score-by-length (list "ß3" "y0ü"))
 5)

;---------------------------------------------------------------------------

;overlay-all :: List-of-images -> Image
;overlay a list of images with earlier ones over later ones
(define (overlay-all aloi)
  (cond
    [(empty? aloi) (square 10 "solid" "white")]
    [else (place-image (first aloi) 5 5 (overlay-all (rest aloi)))]))

;empty list
(check-expect
 (overlay-all '())
 (square 10 "solid" "white"))

;empty image
(check-expect
 (overlay-all (list empty-image empty-image)) ;STOP! this doesn't work 
 (square 10 "solid" "white")) 

;single image list 
(check-expect
 (overlay-all (list (circle 5 "solid" "blue")))
 (place-image
  (circle 5 "solid" "blue")
  5 5 
  (square 10 "solid" "white")))

;multiple images list
(check-expect
 (overlay-all (list (circle 5 "solid" "blue") (square 3 "solid" "yellow")))
 (place-images
  (list (circle 5 "solid" "blue") (square 3 "solid" "yellow"))
  (list (make-posn 5 5) (make-posn 5 5))
  (square 10 "solid" "white")))

;---------------------------------------------------------------------------

;bar-graph :: List-of-numbers -> Image
;generate a bar graph in which the numbers in the list represent bar heights

; A List-of-numbers is one of: 
; – '()
; – (cons NonNegativeNumber List-of-numbers)

(define (bar-graph alon)
  (cond
    [(empty? alon) (square 1 "solid" "white")]
    [else (beside/align "bottom" (rectangle 10 (first alon) "solid" "black")
          (bar-graph (rest alon)))]))
 
;empty list
(check-expect
 (bar-graph '())
 (square 1 "solid" "white"))

;list with 0
(check-expect
 (bar-graph (list 0))
 (beside/align "bottom"
               (rectangle 10 0 "solid" "black") ;STOP! what's this shape? 
               (square 1 "solid" "white")))

;single number list
(check-expect
 (bar-graph (list 1))
 (beside/align "bottom"
               (rectangle 10 1 "solid" "black")
               (square 1 "solid" "white")))

;multiple number list
(check-expect
 (bar-graph (list 3 7 12 6))
 (beside/align "bottom"
               (rectangle 10 3 "solid" "black")
               (rectangle 10 7 "solid" "black")
               (rectangle 10 12 "solid" "black")
               (rectangle 10 6 "solid" "black")
               (square 1 "solid" "white")))

;---------------------------------------------------------------------------


