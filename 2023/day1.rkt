#lang racket
(require "../aoc.rkt")

(year 2023)
(day 1)


#;(example "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
")

(define line1 (car (input-lines)))
;str -> list of numbers
(define (find-numbers str)
  (regexp-match* #px"\\d" str))

(define (find-numbers2 str) 
  (regexp-match* #px"(?=(one|two|three|four|five|six|seven|eight|nine|\\d))"
                 str #:match-select second))


(define (stringtonumber str)
  (cond
    [(string=? "one" str) 1]
    [(string=? "two" str) 2]
    [(string=? "three" str) 3]
    [(string=? "four" str) 4]
    [(string=? "five" str) 5]
    [(string=? "six" str) 6]
    [(string=? "seven" str) 7]
    [(string=? "eight" str) 8]
    [(string=? "nine" str) 9]
    [else (string->number str)]))


(define (get-calibration line)
  ;(newline)
  ;(displayln line)
  (define numbers (find-numbers line))
  (define res
  (+ (* 10 (string->number (first numbers))) (string->number (last numbers))))
  ;(displayln res)
  res)

(define (get-calibration2 line)
  (newline)
  (displayln line)
  (define numbers (find-numbers2 line))
  (define res
  (+ (* 10 (stringtonumber (first numbers))) (stringtonumber (last numbers))))
  (displayln res)
  res)




(define (part1 input)
  (apply + (map get-calibration input)))
(define (part2 input)
  (apply + (map get-calibration2 input)))

#;(submit2
(part2 (input-lines)))
(get-calibration2 "twone3")
(part1 (input-lines))



(submit2
(part2 (input-lines)))



;(submit1(part1 (input-lines)))


#|

(string-split line1 "")
(string->number "")
(string->number "1")
(string->number "meow")
|#