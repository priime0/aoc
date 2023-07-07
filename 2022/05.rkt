#lang racket

(require threading)

#; {String -> [Listof String]}
(define (parse-crate-string line)
  (for/fold ([crate-layer '()]
             [chars (string->list line)]
             #:result (reverse crate-layer))
            ([_ (in-naturals)]
             #:break (null? chars))
    (match chars
      [(list #\[ c #\] rest ...)
       (values (cons c crate-layer)
               (if (pair? rest) (cdr rest) rest))]
      [(list #\space #\space #\space rest ...)
       (values (cons #\space crate-layer)
               (if (pair? rest) (cdr rest) rest))]
      ['() (values crate-layer chars)])))

#; {[Listof String] -> [Vectorof [Listof Symbol]]}
(define (parse-crates crates-strs)
  (define crate-layers
    (for/vector ([line crates-strs])
      (parse-crate-string line)))

  (for/fold ([crates (make-vector 9 '())])
            ([layer crate-layers])
    (for ([c layer]
          [i (in-naturals)])
      (unless (char=? c #\space)
        (define sym-char (string->symbol (string c)))
        (define current-stack (vector-ref crates i))
        (define new-stack (cons sym-char current-stack))
        (vector-set! crates i new-stack)))
    crates))

#; {[Listof String] -> [List Natural Natural Natural]}
(define (parse-moves moves-strs)
  (for/list ([s moves-strs])
    (define split (string-split s " "))
    (define ref (curry list-ref split))
    (map string->number (list (ref 1)
                              (ref 3)
                              (ref 5)))))

#; {[Vectorof [Listof Symbol]] [Listof [List Natural Natural Natural]] -> String}
(define (move-crates! crates moves
                      #:multi? [multi? #f])
  (for ([move moves])
    (match move
      [(list amt from^ to^)
       (define from (sub1 from^))
       (define to (sub1 to^))
       (define taken (take (vector-ref crates from) amt))
       (define left (drop (vector-ref crates from) amt))
       (define target-stack (vector-ref crates to))
       (define new-stack
         (append ((if multi? identity reverse) taken)
                 target-stack))
       (vector-set! crates from left)
       (vector-set! crates to new-stack)]))
  (define string-list-result
    (for/list ([stack crates])
      (if (pair? stack)
          (symbol->string (car stack))
          " ")))
  (string-join string-list-result ""))

(call-with-input-file "05.txt"
  (lambda (in)
    (define-values (crates-strs moves-strs)
      (for/fold ([crates-strs '()]
                 [moves-strs  '()]
                 [crates-done? #f]
                 #:result (values (cdr crates-strs)
                                  (reverse moves-strs)))
                ([line (in-lines in)])
        (cond [(string=? line "")
               (values crates-strs moves-strs #t)]
              [(not crates-done?)
               (values (cons line crates-strs) moves-strs crates-done?)]
              [else
               (values crates-strs (cons line moves-strs) crates-done?)])))

    (define crates1 (parse-crates crates-strs))
    (define crates2 (vector-copy crates1))
    (define moves  (parse-moves moves-strs))

    ;; Part 1
    (println (move-crates! crates1 moves))
    ;; Part 2
    (println (move-crates! crates2 moves #:multi? #t))))
