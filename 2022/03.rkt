#lang racket

(call-with-input-file "03.txt"
  (lambda (in)
    (define lines (sequence->list (in-lines in)))

    #; {String -> (values String String)}
    (define (compartments line)
      (define mid (floor (/ (string-length line) 2)))
      (values (substring line 0 mid)
              (substring line mid)))

    #; {Char -> Natural}
    (define (char->priority c)
      (if (char-lower-case? c)
          (+ (- (char->integer c) 97) 1)
          (+ (- (char->integer c) 65) 27)))

    #; {String -> [Setof Char]}
    (define (string->set s)
      (apply set (string->list s)))

    #; {String ... -> [Setof Char]}
    (define (strings->set . s)
      (apply set-intersect (map string->set s)))

    (define results1
      (for/list ([line lines])
        (define-values (first second) (compartments line))
        (define mutual-chars (strings->set first second))
        
        (char->priority (set-first mutual-chars))))

    (define results2
      (for/fold ([groups '()]
                 [current-group '()]
                 #:result groups)
                ([i (in-naturals)]
                 [line lines])
        (cond [(= (length current-group) 2)
               (define mutual-chars (strings->set (car current-group)
                                                  (cadr current-group)
                                                  line))
               (define new-group (char->priority (set-first mutual-chars)))
               (values (cons new-group groups) '())]
              [else (values groups (cons line current-group))])))

    ;; Part 1
    (println (foldr + 0 results1))
    ;; Part 2
    (println (foldr + 0 results2))))
