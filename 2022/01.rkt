#lang racket

(call-with-input-file "01.txt"
  (lambda (in)
    (define lines (in-lines in))
    (define sorted-elves
      (for/fold ([current-elf 0]
                 [elves '()]
                 #:result (sort (cons current-elf elves) >))
                ([line lines])
        (cond [(string=? line "")
               (values 0 (cons current-elf elves))]
              [else
               (define num (string->number line))
               (define elf-total (+ current-elf num))
               (values elf-total elves)])))
    ;; Part 1
    (println (car sorted-elves))
    ;; Part 2
    (println (foldr + 0 (take sorted-elves 3)))))
