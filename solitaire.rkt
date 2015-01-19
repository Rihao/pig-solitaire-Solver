;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname solitaire) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;
;;*************************************************************
;;Rihao Kang (20560708)
;;CS 135 Fall 2014
;;Assignment 10, problem 2(solitaire)
;;*************************************************************


;; The following line is REQUIRED (do not remove)
(require "a10lib.rkt")


;; Place your Personal Identification here


;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Dimension is an Int
;; requires: 1 <= Dimension <= 9

;; A Peg [position] is an Int
;; requires: 11 <= Peg <= 99
;;           neither digit can be zero or greater than the
;;             Dimension (for the corresponding board)

;; A Board is a (list Dimension (listof Peg))
;; The list contains INVALID Peg positions

;; A State is a (listof Peg)
;; requires: list is non-emtpy
;;           each Peg is VALID for the corresponding board

;; A Solution is one of:
;; * 'any
;; * Peg


(define no-solution-text (list (list "No Solution Found")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the sample board from the assignment

(define sample (list 4 (list 41 42 43 44)))
#|
....
....
....
    
|#

(define sample/init (list 22 23))
#|
....
.OO.
....
    
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the traditional cross pattern with default init state cross/init
;; with some additional (easier) init states you can use

(define cross (list 7 (list 11 12 16 17 21 22 26 27 61 62 66 67 71 72 76 77)))
#|
  ...  
  ...  
.......
.......
.......
  ...  
  ...  
|#

(define cross/init (list 13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                         45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75))
#|
  OOO  
  OOO  
OOOOOOO
OOO.OOO
OOOOOOO
  OOO  
  OOO  
|#

(define cross/submarine (list 34 42 43 44 45 46))
#|
  ...  
  ...  
...O...
.OOOOO.
.......
  ...  
  ...  
|#

(define cross/greek (list 24 34 42 43 44 45 46 54 64))
#|
  ...  
  .O.  
...O...
.OOOOO.
...O...
  .O.  
  ...  
|#

(define cross/small-diamond (list 24 33 34 35 42 43 45 46 53 54 55 64))
#|
  ...  
  .O.  
..OOO..
.OO.OO.
..OOO..
  .O.  
  ...  
|#

(define cross/big-diamond (list 14 23 24 25 32 33 34 35 36 41 42 43
                                45 46 47 52 53 54 55 56 63 64 65 74))
#|
  .O.  
  OOO  
.OOOOO.
OOO.OOO
.OOOOO.
  OOO  
  .O.  
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; This is a provided function: no additional documentation required
;; Uncomment the following line after your neighbours is complete and tested
 (define (make-neighbours board) (lambda (state) (neighbours board state)))



;; try this when you are done: (but leave it commented out when you submit)
; (show (result->text cross (solitaire cross cross/init 'any)))



;; The purpose of this function is to build up the board
;; by inputting number of peggs are needed by row and column
;; (build-board) Nat -> (Listof (Listof Num)


(check-expect
 (build-board 2)
 (list (list 11 12)
       (list 21 22)))

(check-expect (build-board 3)
              (list '(11 12 13)
                    '(21 22 23)
                    '(31 32 33)))
                    
(define (build-board num-pegs)
  (build-list num-pegs (lambda (x)
                         (build-list num-pegs 
                                     (lambda (y)
                                       (+ (* 10 (+ 1 x))

                                          (+ 1 y)))))))
(check-expect 
 (build-board 5)
(list (list 11 12 13 14 15) 
      (list 21 22 23 24 25) 
      (list 31 32 33 34 35) 
      (list 41 42 43 44 45)
      (list 51 52 53 54 55)))

(check-expect (build-board 0)
              (list))

;; The purpose of this function is to produce the state of the board
;; by inputting a board and a state
;; (state->los) board state -> (listof Sym)
;; example:

(check-expect 
              (state->los (list 4 (list 41 42 43 44))
           (list 22 23))
              (list "...." ".OO." "...." "    "))

(check-expect (state->los (list 3 (list 11 12 13))
           (list 22 23))
              (list "   " ".OO" "..."))
              

(define (state->los board state)
  (local [(define (replace-member total-board unoccupied p-position)
  (cond [(empty? total-board) empty]
        [else (cons  (list->string (map (lambda (x)
                                          (cond 
                                            [(member? x unoccupied) #\space]
                                            [(member? x p-position) #\O]
                                            [else #\.])) (first total-board)))
                     (replace-member (rest total-board)
                                     unoccupied p-position))]))]
  
  (replace-member (build-board (first board))
                  (second board)
                  state)))

                    
(check-expect                     
(state->los (list 3 (list 11 12 13))
           (list 22 23))
(list "   " ".OO" "..."))

(check-expect
 (state->los (list 4 empty)
          (list 11 12 13))
(list "OOO." "...." "...." "...."))

;; The purpose of this function is to produce either true or false
;; by inputting a function that consume a peg, which stands for the
;; last peg situation, the number stands for the peg should be the same
;; as the solution. However, when the solution is any, any number of peg
;; is a solution, and produce true.
;; (make-solved?) solution -> bool
;; example:

(check-expect ((make-solved?  45) (list 45)) true)
(check-expect ((make-solved? 45) (list 44)) false)

(define (make-solved? solution)
  (lambda (x)
    (cond [(> (length x) 1) false]
          [(equal? solution 'any) true]
          [(= solution (first x)) true]
          [else false])))


(check-expect ((make-solved?  42) (list 42)) true)
(check-expect ((make-solved? 42) (list 44)) false)
(check-expect ((make-solved? 'any) (list 44 45)) false)
(check-expect ((make-solved? 'any) (list 45)) true)

;; The purpose of this function is to produce the possible moves
;; that the peg can make on the current state by inputting a board
;; and a state
;; (neighbours) board state -> (listof (listof Num))

;; example:

(check-expect (neighbours (list 5 empty) '(22 23))
              (list (list 21) (list 24)))
(check-expect (neighbours (list 5 empty) '(22 32))
              (list (list 12) (list 42)))

(define (neighbours board state)
  (remove-list 
   (append
    (local [(define (horizontal-check board state)
              (append 
               (local [(define (left-check board state
                                           original-state acc)
                         (cond 
                           [(empty? state) acc]
                           [(and (member? 
                                  (- (first state) 1)
                                  original-state)
                                 (local
                                   [(define (check-board num board)
                                      (cond [(empty? board) false]
                                            [(member? num (first board))
                                             true]
                                            [else 
                                             (check-board num (rest board))]))]
                                   (check-board 
                                    (- (first state) 2) board))
                                 (not (member
                                       (- (first state) 2)
                                       original-state)))
                            (left-check
                             board (rest state)
                             original-state 
                             (cons
                              (local
                                [(define
  (create-list original-state first-point second-point final-point)
                                   (local
                                     [(define (insert num lst)
  (cond
    [(empty? lst) (list num)]
    [(< num (first lst)) (cons num lst)]
    [else (cons (first lst) (insert num (rest lst)))]))]
  (insert final-point (remove second-point 
                              (remove first-point original-state)))))]
                              (create-list original-state 
                                           (first state)
                                           (- (first state) 1)
                                           (- (first state) 2)))
                              acc))]
                           [else (left-check board
                                             (rest state)
                                             original-state acc)]))]
                 (left-check board state state empty))
               (local [(define (right-check
                                board state
                                original-state acc)
                         (cond
                           [(empty? state) acc]
                           [(and (member? (+ (first state) 1)
                                          original-state)
                                 (local
                                   [(define (check-board num board)
  (cond [(empty? board) false]
        [(member? num (first board)) true]
        [else (check-board num (rest board))]))]
                                 (check-board (+ (first state) 2)
                                              board))
                                 (not (member (+ (first state) 2)
                                              original-state)))
                            (right-check board
                                         (rest state)
                                         original-state
                                         (cons
                                          (local
                                            [(define
  (create-list original-state first-point second-point final-point)
                                               (local
                                                 [(define (insert num lst)
  (cond
    [(empty? lst) (list num)]
    [(< num (first lst)) (cons num lst)]
    [else (cons (first lst) (insert num (rest lst)))]))]
  (insert final-point (remove second-point 
                              (remove first-point original-state)))))]
                                          (create-list original-state
                                                       (first state)
                                                       (+ 
                                                        (first state) 1)
                                                       (+
                                                        (first state) 2)))
                                          
                                          acc))]
                           [else (right-check board (rest state)
                                              original-state acc)]))
                       ]
                 (right-check board state state empty))))]
      (horizontal-check (remove (second board)
                                (build-board (first board)))
                        state))
    (local
      [(define (vertical-check board state)
         (append 
          (local [(define (up-check board state original-state acc)
                    (cond 
                      [(empty? state) acc]
                      [(and (member? (- (first state) 10) original-state)
                            (local [(define (check-board num board)
  (cond [(empty? board) false]
        [(member? num (first board)) true]
        [else (check-board num (rest board))]))]
                            (check-board
                             (- (first state) 20) board))
                            (not (member (- (first state) 20)
                                         original-state)))
                       (up-check board (rest state) original-state
                                 (cons
                                  (local
                                    [(define
  (create-list original-state first-point second-point final-point)
                                       (local
                                         [(define (insert num lst)
  (cond
    [(empty? lst) (list num)]
    [(< num (first lst)) (cons num lst)]
    [else (cons (first lst) (insert num (rest lst)))]))]
  (insert final-point (remove second-point 
                              (remove first-point original-state)))))]
                                  (create-list
                                   original-state (first state)
                                   (- (first state) 10)
                                   (- (first state) 20)))
                                  acc))]
                      [else (up-check board (rest state)
                                      original-state acc)]))]
            (up-check board state state empty))
          (local [(define (down-check
                           board state original-state acc)
                    (cond 
                      [(empty? state) acc]
                      [(and (member? (+ (first state) 10)
                                     original-state)
                            (local [(define (check-board num board)
  (cond [(empty? board) false]
        [(member? num (first board)) true]
        [else (check-board num (rest board))]))]
                            (check-board (+ (first state) 20) board))
                            (not (member (+ (first state) 20)
                                         original-state)))
                       (down-check board (rest state) original-state 
                                   (cons
                                    (local [(define
  (create-list original-state first-point second-point final-point)
                                              (local
                                                [(define (insert num lst)
  (cond
    [(empty? lst) (list num)]
    [(< num (first lst)) (cons num lst)]
    [else (cons (first lst) (insert num (rest lst)))]))]
  (insert final-point (remove second-point 
                              (remove first-point original-state)))))]
                                    (create-list
                                           original-state
                                           (first state)
                                           (+ (first state) 10)
                                           (+ (first state) 20)))
                                          acc))]
                      [else (down-check board
                                        (rest state)
                                        original-state acc)]))]
            (down-check board state state empty))))]
      (vertical-check (remove (second board) 
                              (build-board (first board)))
                      state)))
   ))

;; test:

(check-expect
 (neighbours (list 5 empty) '(22 23 32 33))
 (list (list 22 23 31)
       (list 21 32 33)
       (list 22 23 34) 
       (list 24 32 33) 
       (list 13 22 32) 
       (list 12 23 33) 
       (list 22 32 43) 
       (list 23 33 42)))
(check-expect (neighbours (list 5 empty) '(14 22 32 13))
(list (list 12 22 32) 
      (list 15 22 32) (list 12 14 13) (list 14 13 42)))
(check-expect 
(neighbours (list 4 (list 41 42 43 44)) (list 22 23))
(list (list 21) (list 24)))


(check-expect
 (neighbours (list 7 empty) '(24 25 34 35))
 (list (list 24 25 33)
       (list 23 34 35)
       (list 24 25 36)
       (list 26 34 35)
       (list 15 24 34) 
       (list 14 25 35)
       (list 24 34 45) (list 25 35 44)))

;; The purpose of this function is to remove the list already existed
;; in the fisrt element from the rest list
;; (remove-list)(listof Num) -> 

(check-expect 
 (remove-list '(1 1 2 3 4 1 1 2))
 (list 3 4 1 2))

(define (remove-list lst)
  (cond 
    [(empty? lst) empty]
    [(member? (first lst) (rest lst))
     (remove-list (remove (first lst) lst))]
    [else (cons (first lst) (remove-list (rest lst)))]))

(check-expect 
 (remove-list '(5 6 9 8 2 2 3))
 (list 5 6 9 8 2 3))











          
  
                            
                        
(define board1 (list (list 11 12 13 14 15)
                     (list 21 22 23 24 25)
                     (list 31 32 33 34 35)
                     (list 41 42 43 44 45)
                     (list 51 52 53 54 55)))
(define board2 (list (list 11 12 13 14)
                     (list 21 22 23 24)
                     (list 31 32 33 34)
                     (list 41 42 43 44)))




;;The function solitaire takes a board, a stae and a solution
;; to give the final route result. If there is a result, produce
;; the list, otherwise, it produces the false
;;(solitaire) board state solution -> (listof (listof Num)) or false
;;example:
(check-expect (solitaire '(3 ()) '(11 12) 'any)
              (list '(11 12) '(13)))
 


(define (solitaire board state solution)
  (find-route state (make-neighbours board) (make-solved? solution)))

(check-expect (solitaire (list 7 empty) '(24 25 34 35) 26)false)

;; The purpose of this function is to convert everything
;; above and produce the real situation.

(check-expect (result->text '(3 ()) (list '(11 12) '(13)))
              (list '("OO."
                      "..."
                      "...")
                    '("..O"
                      "..."
                      "...")))


(define (result->text board result)
  (map (lambda (x)
         (state->los board x))
         result))













