#lang racket
(require "../aoc.rkt")

(year 2024)
(day 2)

#;(example "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")
(define line1 (car (input-lines)))

(define (parse-line line)
  (map string->number
       (string-split line " ")))

(define (parsed-data)
  (map parse-line (input-lines)))

(define (all-inc-dec list)
  (cond ((apply > list) #t)
        ((apply < list) #t)
        (else #f)))

(define (small-diff lst)
  (cond
    [(= (length lst) 1) #t]
    [else (if (> (abs(- (first lst) (second lst))) 3)
              #f
              (small-diff (rest lst)))]))
(define (fakeand bool1 bool2)
  (if (and bool1 bool2)
      #t
      #f))
(define (tru1 bool)
  (if bool
      1
      0))

(define (part1)
  (apply + (map tru1 (apply map fakeand (list (map all-inc-dec (parsed-data))
                                              (map small-diff (parsed-data)))))))
(part1)
                                  
;this one's gonna be funny
(define (get-sublists lst)
  (cons lst
        (for/list ([i (length lst)])
          (define-values (beg end) (split-at lst i))
          (append beg (rest end)))))
             

(define (all-inc-dec2 lst)
  (ormap all-inc-dec (get-sublists lst)))

(define (small-diff2 lst)
  (ormap small-diff (get-sublists lst)))

(define (satisfies-both lst)
  (and (all-inc-dec lst)
       (small-diff lst)))
(define (sub-satisfy lst)
  (ormap satisfies-both (get-sublists lst)))

(define (part2)
  (apply + (map tru1 (map sub-satisfy (parsed-data)))))
(part2)
    
