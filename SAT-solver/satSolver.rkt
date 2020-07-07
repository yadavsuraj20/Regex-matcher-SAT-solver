#lang racket

(require "utilities.rkt")

(define assign #hash())

; Fill in your code here. Should finally define a function
; called dpll which returns true or false. Should additionally
; store the satisfying assignment in the variable assign.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;empty-clause?
(define (empty-clause? l)
  (cond [(null? l) #f]
        [(equal? (car l) '()) #t]
        [else (empty-clause? (cdr l))]))
;unit-clause
(define (unit-clause l)
  (cond [(null? l) '()]
        [(null? (car l)) (unit-clause (cdr l))]
        [(null? (cdr (car l))) (car l)]
        [else (unit-clause (cdr l))]))
;is-present?
(define (is-present? x l)
  (cond [(null? l) #f]
        [(= x (car l)) #t]
        [else (is-present? x (cdr l))]))
;unit-improve
(define (unit-improve l x)
  (map (lambda (y) (remove* (list (- (car x))) y)) (removing-clauses l (car x))))
;removing-clauses
(define (removing-clauses l x)
  (filter (lambda (y) (not (is-present? x y))) l))
;;same-sign-element
;(define (same-sign-element l l1)
;  (cond [(null? l) '()]
;        [(not (is-present? (- (car l)) (cdr l))) (list (car l))]
;        ;[(is-present? (car l) l1) (same-sign-element (cdr l) l1)]
;        [else (same-sign-element (remove* (list (car l) (- (car l))) l) (cons (car l) l1))]))
;same-sign-element
(define (same-sign-element l l1)
  (cond [(null? l) '()]
        [(is-present? (car l) l1) (same-sign-element (cdr l) l1)]
        [(is-present? (- (car l)) (cdr l)) (same-sign-element (cdr l) (cons (car l) (cons (- (car l)) l1)))]
        [else (list (car l))]))
;Assign
(define (Assign val)
  (if (> val 0) (set! assign (dict-set assign val #t))
                (set! assign (dict-set assign (- val) #f))))
;DPLL
(define (DPLL l)
  (let* ([single (unit-clause l)]
         [same-sign (same-sign-element (append* l) '())])
    (cond [(null? l) #t]
          [(empty-clause? l) #f]
          [(not (null? single)) (let* ([val (car single)])
                                  (begin (Assign val)
                                         (DPLL (unit-improve l single))))]
          [(not (null? same-sign)) (let* ([val (car same-sign)])
                                     (begin (Assign val)
                                            (DPLL (filter (lambda (y) (not (is-present? (car same-sign) y))) l))))]
          [(DPLL (unit-improve l (list (caar l)))) (let* ([val (caar l)])
                                                     (begin (Assign val)
                                                            #t))]
          [(DPLL (unit-improve l (list (- (caar l))))) (let* ([val (- (caar l))])
                                                         (begin (Assign val)
                                                                #t))]
          [else #f])))
;inverse
(define (inverse t)
  (cond [(And? t) (append (inverse (And-x t)) (inverse (And-y t)))]
        [(Or? t) (list (append* (append (inverse (Or-x t)) (inverse (Or-y t)))))]
        [(Var? t) (list (list (Var-lit t)))]
        [(Not? t) (list (list (- (caar (inverse (Not-e t))))))]))
;dpll
(define (dpll t)
  (set! assign #hash())
  (if (DPLL (inverse t)) #t (begin (set! assign #hash()) #f)))
