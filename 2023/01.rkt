#lang racket

(require threading)

(define (line->nums line)
  (~>> line
       sanitize
       string->list
       (filter char-numeric?)
       (map string)
       (map string->number)))

(define (sanitize line)
  (~> line
      (regexp-replace* #rx"one"   _ "one1one")
      (regexp-replace* #rx"two"   _ "two2two")
      (regexp-replace* #rx"three" _ "three3three")
      (regexp-replace* #rx"four"  _ "four4four")
      (regexp-replace* #rx"five"  _ "five5five")
      (regexp-replace* #rx"six"   _ "six6six")
      (regexp-replace* #rx"seven" _ "seven7seven")
      (regexp-replace* #rx"eight" _ "eight8eight")
      (regexp-replace* #rx"nine"  _ "nine9nine")))

(define (part1 lines)
  (for/sum ([line lines])
    (define nums (line->nums line))
    (define num0 (first nums))
    (define num+ (last nums))
    (+ (* num0 10) num+)))

(define (main)
  (define lines (in-lines (current-input-port)))

  (part1 lines))

(module+ main
  (main))
