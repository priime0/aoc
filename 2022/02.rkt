#lang racket

(call-with-input-file "02.txt"
  (lambda (in)
    (define lines (sequence->list (in-lines in)))
    (define (part1 elf you)
      (case (list elf you)
        [((A X)) (+ 3 1)]
        [((A Y)) (+ 6 2)]
        [((A Z)) (+ 0 3)]
        [((B X)) (+ 0 1)]
        [((B Y)) (+ 3 2)]
        [((B Z)) (+ 6 3)]
        [((C X)) (+ 6 1)]
        [((C Y)) (+ 0 2)]
        [((C Z)) (+ 3 3)]))

    (define (part2 elf you)
      (case (list elf you)
        [((A X)) (+ 0 3)]
        [((A Y)) (+ 3 1)]
        [((A Z)) (+ 6 2)]
        [((B X)) (+ 0 1)]
        [((B Y)) (+ 3 2)]
        [((B Z)) (+ 6 3)]
        [((C X)) (+ 0 2)]
        [((C Y)) (+ 3 3)]
        [((C Z)) (+ 6 1)]))

    (define (line->sym line idx)
      (string->symbol (string (string-ref line idx))))
    (define (line->elf line)
      (line->sym line 0))
    (define (line->you line)
      (line->sym line 2))

    (define (sum-scores part)
      (for/sum ([line lines])
        (part (line->elf line) (line->you line))))
    
    ;; Part 1
    (println (sum-scores part1))
    ;; Part 2
    (println (sum-scores part2))))
