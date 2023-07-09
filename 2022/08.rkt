#lang racket

(define vref vector-ref)
(define vset! vector-set!)
(define mvec make-vector)

#; {[Listof String] -> [Vectorof [Vectorof Natural]]}
(define (make-heights lines)
  (define heights (mvec (length lines)))
  (for ([srow lines]
        [i (in-naturals)])
    (define curr-row (mvec (string-length (car lines))))
    (for ([scol (string->list srow)]
          [j (in-naturals)])
      (define height (string->number (string scol)))
      (vset! curr-row j height))
    (vset! heights i curr-row))
  heights)

#; {[Vectorof [Vectorof Natural]] -> [Vectorof [Vectorof Natural]]}
(define (make-maxes heights)
  (define maxes (mvec (vector-length heights)))
  (define vec-len (vector-length maxes))
  (define row-len (vector-length (vref heights 0)))

  (for ([r (in-range (vector-length maxes))])
    (vset! maxes r (mvec row-len #f)))

  (for ([i (in-range vec-len)]
        [row heights]
        [rowm maxes])
    (for/fold ([curr-max -1])
              ([col row]
               [j (in-range row-len)])
      (vset! rowm j curr-max)
      (max curr-max col))
    (for/fold ([curr-max -1])
              ([col (reverse (vector->list row))]
               [j   (in-inclusive-range (sub1 row-len) 0 -1)])
      (define existing (vref rowm j))
      (vset! rowm j (min existing curr-max))
      (max curr-max col)))

  (for ([c (in-range row-len)])
    (for/fold ([curr-max -1])
              ([r (in-range vec-len)])
      (define existing (vref (vref maxes r) c))
      (define col (vref (vref heights r) c))
      (vset! (vref maxes r) c (min existing curr-max))
      (max curr-max col))
    (for/fold ([curr-max -1])
              ([r (in-inclusive-range (sub1 vec-len) 0 -1)])
      (define existing (vref (vref maxes r) c))
      (define col (vref (vref heights r) c))
      (vset! (vref maxes r) c (min existing curr-max))
      (max curr-max col)))

  maxes)

#; {[Vectorof [Vectorof Natural]] [Vectorof [Vectorof Natural]] -> Natural}
(define (get-visible heights maxes)
  (for/fold ([total 0])
            ([rowh heights]
             [rowm maxes])
    (define count
      (for/fold ([c 0])
                ([colh rowh]
                 [colm rowm])
        (if (> colh colm)
            (add1 c)
            c)))
    (+ total count)))

#; {[Vectorof [Vectorof Natural]] -> [Vectorof [Vectorof Natural]]}
(define (get-scenic-scores heights)
  (define nrows (vector-length heights))
  (define ncols (vector-length (vref heights 0)))
  (define mrows (sub1 nrows))
  (define mcols (sub1 ncols))
  (define west (mvec nrows))
  (define east (mvec nrows))
  (define north (mvec nrows))
  (define south (mvec nrows))
  (define (diff x y)
    (abs (- x y)))

  (for ([r (in-range nrows)])
    (for ([target (list west east north south)])
      (vset! target r (mvec ncols #f))))

  (define (update! row c dv cell acc)
    (vset! row c (diff dv (vref acc cell)))
    (for ([k (in-range (add1 cell))])
      (vset! acc k dv))
    acc)

  (define make-acc (curry mvec 10))

  (define (range-d n) (in-inclusive-range n 0 -1))

  (for ([r (in-range nrows)]
        [row heights]
        [roww west])
    (for/fold ([acc (make-acc 0)])
              ([cell row]
               [c (in-range ncols)])
      (update! roww c c cell acc)))

  (for ([r (in-range nrows)]
        [row heights]
        [rowe east])
    (for/fold ([acc (make-acc mcols)])
              ([c (range-d mcols)])
      (define cell (vref row c))
      (update! rowe c c cell acc)))

  (for ([c (in-range ncols)])
    (for/fold ([acc (make-acc 0)])
              ([r (in-range nrows)])
      (define rown (vref north r))
      (define cell (vref (vref heights r) c))
      (update! rown c r cell acc)))

  (for ([c (in-range ncols)])
    (for/fold ([acc (make-acc mrows)])
              ([r (range-d mrows)])
      (define srow (vref south r))
      (define cell (vref (vref heights r) c))
      (update! srow c r cell acc)))

  (for/vector ([wr west]
               [er east]
               [nr north]
               [sr south])
    (for/vector ([w wr]
                 [e er]
                 [n nr]
                 [s sr])
      (* w e n s))))

#; {[Vectorof [Vectorof Natural]] -> Natural}
(define (vectors-max vec)
  (for/fold ([m 0])
            ([r vec])
    (max m
         (for/fold ([n 0])
                   ([c r])
           (max n c)))))

(call-with-input-file "08.txt"
  (lambda (in)
    (define heights (make-heights (sequence->list (in-lines in))))
    (define maxes (make-maxes heights))

    ;; Part 1
    (println (get-visible heights maxes))

    ;; Part 2
    (println (vectors-max (get-scenic-scores heights)))))
