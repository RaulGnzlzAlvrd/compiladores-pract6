#lang nanopass

(require rackunit
         "pract6.rkt")

;; Ejercicio 1: uncurry

;; Expresiones query, todas conforme a L10
(define e1 `(lambda ([x Int]) (primapp * x x)))
(define e2 `(lambda ([x Int]) (lambda ([y Int]) (primapp + x y x))))
(define e3 `(lambda ([a Int]) (lambda ([b Int]) ,e2)))
(define e4 `(lambda ([a Int]) (lambda ([b Int]) ((,e2 a) (,e1 b)))))

;; Expresiones esperadas, todas conforme a L11
(define r1 `(lambda ([x Int]) (primapp * x x)))
(define r2 `(lambda ([x Int] [y Int]) (primapp + x y x)))
(define r3 `(lambda ([a Int] [b Int] [x Int] [y Int]) (primapp + x y x)))
(define r4 `(lambda ([a Int] [b Int]) (,r2 a (,r1 b))))

(define (verify-uncurry query expected)
  (check-equal? (uncurry (parser-L10 query))
                (parser-L11 expected)))

(foldl (lambda (q e flag) (and flag (verify-uncurry q e))) #t
       (list e1 e2 e3 e4)
       (list r1 r2 r3 r4))
