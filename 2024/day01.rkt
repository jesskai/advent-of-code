#lang racket
(require "../aoc.rkt")

(year 2024)
(day 1)

#;(example "3   4
4   3
2   5
1   3
3   9
3   3")

(define line1 (car (input-lines)))


(define (line-to-cons line)
  (string-split line "   "))
  

(define (parse-input input)
  (map line-to-cons input))
  

(define (get-first-list input)
  (sort (map string->number
             (map first (parse-input input)))
  <))

(define (get-second-list input)
  (sort (map string->number
             (map second (parse-input input)))
  <))

(define first-list (get-first-list (input-lines)))

(define second-list (get-second-list (input-lines)))

(define (diffs a b)
  (map abs (apply map - (list a b))))

(define (part1)
  (apply + (diffs first-list second-list)))

(define (similar? num1 num2)
  (if (= num1 num2)
      num1
      0))

(define (get-similarity number lst)
  (apply + (map (lambda (num2)
         (similar? number num2))
       lst)))

(define (part2)
  (apply + (map (lambda (num1)
         (get-similarity num1 second-list))
       first-list)))

(part2)


  






