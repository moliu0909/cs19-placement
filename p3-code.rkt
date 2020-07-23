;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname p3-code) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;elim-contains-char :: Char List-of-strings -> List-of-strings
;consumes a list of strings and produces the same list excluding wirds wuth a given character

;contains? :: char string -> #boolean
;checks whether the string contains the given character 
(define (contains? char str)
  (cond
    [(equal? "" str) #false]
    [else (define sl (sting->list str))
          (cond
            [(equal? char (first sl) #true)]
            [else (contains? char (rest sl))])]))

;tests for contains?
(check-expect
 (contains? #\b "dabana") #true)

;functions: elim-contains-char
(define (elim-contains-char char alos)
  (cond
    [(empty? alos) '()]
    [(contains? char (first alos)) (elim-contains-char char (rest alos))] ; skip this string
    [else (append '() (first alos))])) ;append onto the list

;-----------------------------------------------------------------------------------------------

;valid-words :: List-of-strings List-of-chars -> List-of-strings
;consumes a list of words (represented as strings) and a list of letters..
;..produces the list of same words, in the same order, that contain only those letters

(define (valid-words alos aloc)
  (cond
    [(empty? alos) ...]
    [else ...]))


;-----------------------------------------------------------------------------------------------

;unique :: List-of-any -> List-of-any
;Given a list of values, produces a list of the same values in the same order excluding duplicates

(define (unique aloa)
  (cond
    [(empty? aloa) '()] ;returns empty list
    [(equal? (first aloa) (first (rest aloa))) append-first aloa] ;keep the word
    [else (unqiue (rest aloa))]))

;-----------------------------------------------------------------------------------------------

;l33t :: List-of-strings -> List-of-strings
;consumes a list of words and produces a list of the same words, in the same order..
;..but where some of the letters have been replaced by characters that stand for numbers
;#\A and #\a into #\4,#\E and #\e into #\3, #\I and #\i into #\1, and #\O and #\o into #\0


;-----------------------------------------------------------------------------------------------

;strip-vowels :: List-of-strings -> List-of-strings
;consumes a list of words and produces the same list of words, except with each vowel removed

