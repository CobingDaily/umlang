#lang racket
(module+ test (require rackunit))


;; An Expr represents an expression in umlang
;; It is one of:
;; - a (num Number) , representing a number
;; - a (plus Expr Expr) , denotes the addition of two expressions
(struct num [n] #:transparent)
(struct plus [left right] #:transparent)

;; Examples:
(define two (num 2))
(define ten (num 10))
(define two-plus-ten (plus two ten))

;; Template for Expr
;; F : Expr -> X
;; <purpose statement goes here>
#;(define (F expr)
  (match expr
    [(num n) ... n ...]
    [(plus left right) ... (F left) ... (F right) ...]))


;; calc : Expr -> Number
;; Calculate the value of an expression
(define (calc expr)
  (match expr
    [(num n) n]
    [(plus left right) (+ (calc left) (calc right))]))

(module+ test
  (check-equal? (calc (num 0)) 0)
  (check-equal? (calc ten) 10)
  (check-equal? (calc (plus (num 1) (num 0))) 1)
  (check-equal? (calc (plus (num 0) (num 1))) 1)
  (check-equal? (calc two-plus-ten) 12))

