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

;single image in list 
(check-expect
 (overlay-all (list (circle 5 "solid" "blue")))
 (place-image
  (circle 5 "solid" "blue")
  5 5 
  (square 10 "solid" "white")))

;multiple images in list
(check-expect
 (overlay-all (list (circle 5 "solid" "blue") (square 3 "solid" "yellow")))
 (place-images
  (list (circle 5 "solid" "blue") (square 3 "solid" "yellow"))
  (list (make-posn 5 5) (make-posn 5 5))
  (square 10 "solid" "white")))

;---------------------------------------------------------------------------


 
