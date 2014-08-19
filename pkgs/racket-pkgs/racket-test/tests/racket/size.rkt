#lang racket/base

; Short version
(let ()
  #;(define (f x) (+ x x)); size => -3
  #;(define (f x) (+ x x 0)); size => -4
  #;(define (f x) (+ x x x)); size => -4
  #;(define (f x) (+ x x 0 0)); size => -5
  ; at least two x's
  ; add more x'r or 0's to get a more negative result
  (define (f x) (+ x x 0 0))
  (define h 0)
  (define (g (x 0)) (f x))
  (define (i (x 0)) (f x))
  (lambda () (f 0)))



; Expanded version
(let ()
  (let ([f (lambda (x) (+ x x 0 0))])
    (let ([g 0])
      (let ([h (let ([h2 (lambda (p q)
                           (let ([x (if q p 0)])
                             (f x)))])
                 (case-lambda
                   [() (h2 #f #f)]
                   [(x) (h2 x #t)]
                   ))])
        (let ([i (let ([i2 (lambda (p q)
                             (let ([x (if q p 0)])
                               (f x)))])
                   (case-lambda
                     [() (i2 #f #f)]
                     [(x) (i2 x #t)]
                     ))])
          (lambda () (f 0))
          )))))

