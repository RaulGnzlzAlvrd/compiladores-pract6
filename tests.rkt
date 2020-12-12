#lang nanopass

(require rackunit
         "pract6.rkt")

;;
;; Ejercicio 1: uncurry
;;

;; Expresiones query, todas conforme a L10
(define e1 `(lambda ([x Int]) (primapp * x x)))
(define e2 `(lambda ([x Int]) (lambda ([y Int]) (primapp + x y x))))
(define e3 `(lambda ([a Int]) (lambda ([b Int]) ,e2)))
(define e4 `(foo x))
(define e5 `(((foo x) y) z))
(define e6 `(lambda ([a Int]) (lambda ([b Int]) ((,e2 a) (,e1 b)))))

;; Expresiones esperadas, todas conforme a L11
(define r1 `(lambda ([x Int]) (primapp * x x)))
(define r2 `(lambda ([x Int] [y Int]) (primapp + x y x)))
(define r3 `(lambda ([a Int] [b Int] [x Int] [y Int]) (primapp + x y x)))
(define r4 `(foo x))
(define r5 `(((foo x) y) z))
(define r6 `(lambda ([a Int] [b Int]) ((,r2 a) (,r1 b))))

(define (verify-uncurry query expected)
  (check-equal? (uncurry (parser-L10 query))
                (parser-L11 expected)))

(foldl (lambda (q e flag) (and flag (verify-uncurry q e))) #t
       (list e1 e2 e3 e4 e5 e6)
       (list r1 r2 r3 r4 r5 r6))

;;
;; Ejercicio 2: symbol-table-var
;;

;; Expresiones query, todas conforme a L11
(define e7 `(let ([x Int (const Int 1)]) (primapp + (let ([y Int (const Int 2)]) y) x)))
(define e8 `(primapp + ,e7 (letrec ([z Int (const Int 1)]) (primapp + z z))))
(define e9 `(letrec ([a Int ,e7]) a))

;; Resultados esperados, son tablas hash
(define r7 (make-hash (list
            (cons 'x (cons 'Int (parser-L11 `(const Int 1))))
            (cons 'y (cons 'Int (parser-L11 `(const Int 2)))))))
(define r8 (make-hash (list
            (cons 'x (cons 'Int (parser-L11 `(const Int 1))))
            (cons 'y (cons 'Int (parser-L11 `(const Int 2))))
            (cons 'z (cons 'Int (parser-L11 `(const Int 1)))))))
(define r9 (make-hash (list
            (cons 'x (cons 'Int (parser-L11 `(const Int 1))))
            (cons 'y (cons 'Int (parser-L11 `(const Int 2))))
            (cons 'a (cons 'Int (parser-L11 e7))))))

(define (verify-symbol-table-var query expected)
  (check-equal? (symbol-table-var (parser-L11 query))
                expected))

(foldl (lambda (q e flag) (and flag (verify-symbol-table-var q e))) #t
       (list e7 e8 e9)
       (list r7 r8 r9))