#lang racket

(call-with-input-file "02.txt"
  (lambda (in)
    (define lines (in-lines in))

    #; {Symbol -> Symbol}
    (define (decision->shape decision)
      (hash-ref #hash((X . A) (Y . B) (Z . C))
                decision))
    #; {Symbol Symbol -> Symbol}
    (define (decision->you elf decision)
      (case (list decision elf)
        [((X A) (Y C) (Z B)) 'C]
        [((Y A) (Z C) (X B)) 'A]
        [((Z A) (X C) (Y B)) 'B]))
    (define shapes
      #hash((A . 1) (X . 1) (B . 2) (Y . 2) (C . 3) (Z . 3)))
    #; {Symbol -> Natural}
    (define (shape->score shape)
      (hash-ref shapes shape))
    #; {Symbol Symbol -> Natural}
    (define (round-result-score elf you)
      (case (list elf you)
        [((A A) (B B) (C C)) 3]
        [((A B) (B C) (C A)) 6]
        [else 0]))
    #; {Symbol Symbol -> Natural}
    (define (round-score elf you)
      (+ (round-result-score elf you)
         (shape->score you)))

    #; {(values [Listof Natural] [Listof Natural])}
    (define-values (scores1 scores2)
      (for/fold ([list1 '()]
                 [list2 '()])
                ([line lines])
        (define moves (string-split line " "))
        (define elf (string->symbol (car moves)))
        (define decision (string->symbol (cadr moves)))
        (values (cons (round-score elf (decision->shape decision)) list1)
                (cons (round-score elf (decision->you elf decision)) list2))))

    ;; Part 1
    (println (foldr + 0 scores1))
    ;; Part 2
    (println (foldr + 0 scores2))))
