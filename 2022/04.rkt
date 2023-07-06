#lang racket

(require threading)

#; {String -> [List Number Number]}
;; Converts a string in the format of "%d-%d" to a pair of numbers.
(define (string->range s)
  (~> s
      (string-split "-")
      (map string->number _)))

#; {[List Number Number Number Number] -> Boolean}
;; Are either of the ranges contained in the other range?
(define (contained? ranges)
  (match ranges
    [(list r1f r1s r2f r2s)
     (or (<= r1f r2f r2s r1s)
         (<= r2f r1f r1s r2s))]))

#; {[List Number Number Number Number] -> Boolean}
;; Do the ranges overlap with one another?
(define (overlap? ranges)
  (match ranges
    [(list r1f r1s r2f r2s)
     (define min-right (min r1s r2s))
     (define max-left (max r1f r2f))
     (<= max-left min-right)]))

(call-with-input-file "04.txt"
  (lambda (in)
    (define elves
      (for/list ([line (in-lines in)])
        (define split (string-split line ","))
        (define range1 (string->range (car split)))
        (define range2 (string->range (cadr split)))
        (list (car range1)
              (cadr range1)
              (car range2)
              (cadr range2))))

    ;; Part 1
    (println (length (filter contained? elves)))

    ;; Part 2
    (println (length (filter overlap? elves)))))
