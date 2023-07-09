#lang racket

#; {HashTable String -> HashTable}
(define (update-parents! ht dir amt #:nf [nf #f])
  (define curr-dir-name (string-join (reverse dir) "/"))
  (define old-size (hash-ref ht curr-dir-name))
  (define new-size (+ old-size amt))
  (hash-set! ht curr-dir-name new-size)
  (unless (null? dir)
    (update-parents! ht (cdr dir) amt))
  ht)

#; {[Listof String] -> HashTable}
(define (get-dirs cmds)
  (for/fold ([ht (make-hash)]
             [dir '()]
             #:result ht)
            ([cmd cmds])
    (define curr-dir-name (string-join (reverse dir) "/"))
    (match (string-split cmd " ")
      ['() (values ht dir)]
      [(list "$" "cd" "/")
       (values ht '())]
      [(list "$" "cd" "..")
       (values ht (cdr dir))]
      [(list "$" "cd" target)
       (unless (hash-has-key? ht curr-dir-name)
         (hash-set! ht curr-dir-name 0))
       (values ht (cons target dir))]
      [(list "$" "ls")
       (values ht dir)]
      [(list "dir" _)
       (values ht dir)]
      [(list size^ filename)
       (define size (string->number size^))
       (cond [(hash-has-key? ht curr-dir-name)
              (define new-ht (update-parents! ht dir size #:nf filename))
              (values new-ht dir)]
             [else
              (hash-set! ht curr-dir-name 0)
              (define new-ht (update-parents! ht dir size #:nf filename))
              (values new-ht dir)])])))

#; {HashTable -> Natural}
(define (sum-small-directories ht)
  (define (small-directory? dir)
    (<= (hash-ref ht dir) 100000))
  (for/sum ([key (hash-keys ht)])
    (if (small-directory? key)
        (hash-ref ht key)
        0)))

#; {HashTable -> Natural}
(define (freed-space ht)
  (define used-space   (hash-ref ht ""))
  (define total-space  70000000)
  (define needed-space 30000000)
  (define unused-space (- total-space used-space))
  (define target-space (- needed-space unused-space))

  (for/fold ([min-amt total-space])
            ([key (hash-keys ht)]
             #:do [(define size (hash-ref ht key))])
    (if (< size target-space)
        min-amt
        (min min-amt size))))

(call-with-input-file "07.txt"
  (lambda (in)
    (define lines (in-lines in))

    (define dirs (get-dirs lines))

    ;; Part 1
    (println (sum-small-directories dirs))
    ;; Part 2
    (println (freed-space dirs))))
