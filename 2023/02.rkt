#lang racket

(require threading)

(define *max-red*   (make-parameter 12))
(define *max-green* (make-parameter 13))
(define *max-blue*  (make-parameter 14))

(define (line->sets line)
  (define game-info (second (string-split line ":")))
  (map string-trim (string-split game-info ";")))

(define (set->hash set-str)
  (define set^
    (~>> (string-split set-str ",")
         (map string-trim)
         (map (curryr string-split " "))))
  (for/hash ([s set^])
    (define amt (string->number (first s)))
    (define colour (string->symbol (second s)))
    (values colour amt)))

(define (max-of-colour sets colour)
  (for/fold ([max-amt 1])
            ([set sets])
    (define ht (set->hash set))
    (max max-amt (hash-ref ht colour 0))))

(define (part1 lines)
  (for/sum ([line lines] [i (in-naturals)])
    (define sets (line->sets line))

    (define possible?
      (for/and ([set sets])
        (define ht (set->hash set))
        (and (<= (hash-ref ht 'red 0)   (*max-red*))
             (<= (hash-ref ht 'green 0) (*max-green*))
             (<= (hash-ref ht 'blue 0)  (*max-blue*)))))

    (if possible? (add1 i) 0)))

(define (part2 lines)
  (for/sum ([line lines] [i (in-naturals)])
    (define sets (line->sets line))

    (define max-red   (max-of-colour sets 'red))
    (define max-green (max-of-colour sets 'green))
    (define max-blue  (max-of-colour sets 'blue))

    (* max-red max-green max-blue)))

(define (main)
  (define lines (sequence->list (in-lines (current-input-port))))

  (println (part1 lines))

  (println (part2 lines)))

(module+ main
  (main))
