#lang racket

#; {[Listof Char] Natural -> Natural}
(define (find-start s n)
  (for/fold ([done? #f]
             [idx n]
             [chars s]
             #:result idx)
            ([_ (in-naturals)]
             #:break done?)
    (cond [(= (set-count (list->set (take chars n))) n)
           (values #t idx #f)]
          [else
           (values #f (add1 idx) (cdr chars))])))

(call-with-input-file "06.txt"
  (lambda (in)
    (define data (port->string in))

    ;; Part 1
    (println (find-start (string->list data) 4))
    ;; Part 2
    (println (find-start (string->list data) 14))))
