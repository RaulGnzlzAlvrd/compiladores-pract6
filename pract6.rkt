#lang nanopass

(provide (all-defined-out))

(define fun-count 0)

(define (variable? x) (and (symbol? x) (not (primitive? x)) (not (constant? x))))

(define (primitive? x) (memq x '(+ - * / length car cdr)))

(define (constant? x)
  (or (integer? x)
      (char? x)
      (boolean? x)))

;; SISTEMA DE TIPOS
;; Int | Char | Bool | Lambda | List | (List of T) | (T → T)
(define (type? x) (or (b-type? x) (c-type? x)))
(define (b-type? x) (memq x '(Bool Char Int List Lambda)))
(define (c-type? x) (if (list? x) 
	(let* (
		[f (car x)]
		[s (cadr x)]
		[t (caddr x)])
	(or (and (equal? f 'List) (equal? s 'of) (type? t)) 
		(and (type? f) (equal? s '→) (type? t))))
	#f))

(define (arit? x) (memq x '(+ - * /)))

(define (lst? x) (memq x '(length car cdr)))

(define-language L10
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t)))
  (Expr (e body)
    x
    (const t c)
    (begin e* ... e)
    (primapp pr e* ...)
    (if e0 e1 e2)
    (lambda ([x t]) body)
    (let ([x t e]) body)
    (letrec ([x t e]) body)
    (letfun ([x t e]) body)
    (list e* ...)
    (e0 e1)))

(define-parser parser-L10 L10)

(define-language L11
  (extends L10)
  (Expr (e body)
        (- (lambda ([x t]) body)
           (e0 e1))
        (+ (lambda ([x* t*] ...) body)
           (e e* ...))))

(define-parser parser-L11 L11)

(define (lambda? expr)
  (nanopass-case (L11 Expr) expr
                 [(lambda ([,x* ,t*] ...) ,body) #t]
                 [else #f]))

(define (saca-cosas expr)
  (nanopass-case (L11 Expr) expr
                 [(lambda ([,x* ,t*] ...) ,body) `((,x* ,t*) ,body)]
                 [else #f]))

(define-pass uncurry : L10 (ir) -> L11 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x ,t]) ,[body])
         (if (lambda? body)
             (let* ([procesado (saca-cosas body)]
                    [asignaciones (car procesado)])
               `(lambda ([,(cons x (car asignaciones))
                          ,(cons t (cadr asignaciones))] ...)
                  ,(cadr procesado)))
             `(lambda ([,x ,t]) ,body))]
        [(,[e0] ,[e1]) `(,e0 ,(list e1) ...)]))
