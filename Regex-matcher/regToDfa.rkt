#lang racket
(require parser-tools/lex
         parser-tools/yacc)
(require "declarations.rkt")
(require "utilities.rkt")


;nullable
(define (nullable t)
  (cond [(or (Epsilon? t) (Star? t)) #t]
        [(Literal? t) #f]
        [(Or? t) (or (nullable (Or-t1 t)) (nullable (Or-t2 t)))]
        [(Then? t) (and (nullable (Then-t1 t)) (nullable (Then-t2 t)))]))
;buildNullable
(define (buildNullable t)
  (cond [(Epsilon? t) (list (cons (Epsilon-n t) (nullable t)))]
        [(Literal? t) (list (cons (Literal-n t) (nullable t)))]
        [(Or? t) (append (buildNullable (Or-t1 t)) (list (cons (Or-n t) (nullable t))) (buildNullable (Or-t2 t)))]
        [(Then? t) (append (buildNullable (Then-t1 t)) (list (cons (Then-n t) (nullable t))) (buildNullable (Then-t2 t)))]
        [(Star? t) (append (list (cons (Star-n t) (nullable t))) (buildNullable (Star-t t)))]))

;firstpos
(define (firstpos t)
  (cond [(Epsilon? t) '()]
        [(Literal? t) (list (Literal-n t))]
        [(Or? t) (Union (firstpos (Or-t1 t)) (firstpos (Or-t2 t)))]
        [(Then? t) (if (nullable (Then-t1 t))
                       (Union (firstpos (Then-t1 t)) (firstpos (Then-t2 t))) (firstpos (Then-t1 t)))]
        [(Star? t) (firstpos (Star-t t))]))
;buildFirst
(define (buildFirst t)
  (cond [(Epsilon? t) (list (cons (Epsilon-n t) (firstpos t)))]
        [(Literal? t) (list (cons (Literal-n t) (firstpos t)))]
        [(Or? t) (append (buildFirst (Or-t1 t)) (list (cons (Or-n t) (firstpos t))) (buildFirst (Or-t2 t)))]
        [(Then? t) (append (buildFirst (Then-t1 t)) (list (cons (Then-n t) (firstpos t))) (buildFirst (Then-t2 t)))]
        [(Star? t) (append (list (cons (Star-n t) (firstpos t))) (buildFirst (Star-t t)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lastpos
(define (lastpos t)
  (cond [(Epsilon? t) '()]
        [(Literal? t) (list (Literal-n t))]
        [(Or? t) (Union (lastpos (Or-t1 t)) (lastpos (Or-t2 t)))]
        [(Then? t) (if (nullable (Then-t2 t))
                       (Union (lastpos (Then-t1 t)) (lastpos (Then-t2 t))) (lastpos (Then-t2 t)))]
        [(Star? t) (lastpos (Star-t t))]))
;buildLast
(define (buildLast t)
  (cond [(Epsilon? t) (list (cons (Epsilon-n t) (lastpos t)))]
        [(Literal? t) (list (cons (Literal-n t) (lastpos t)))]
        [(Or? t) (append (buildLast (Or-t1 t)) (list (cons (Or-n t) (lastpos t))) (buildLast (Or-t2 t)))]
        [(Then? t) (append (buildLast (Then-t1 t)) (list (cons (Then-n t) (lastpos t))) (buildLast (Then-t2 t)))]
        [(Star? t) (append (list (cons (Star-n t) (lastpos t))) (buildLast (Star-t t)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;buildFollow
(define (buildFollow t)
  (define (helper T1 T2 )
    (cond [(Epsilon? T1) '()]
          [(Literal? T1) (list (cons (Literal-n T1) (followpos (Literal-n T1) T2 '())))]
          [(Then? T1) (append (helper (Then-t1 T1) T2) (helper (Then-t2 T1) T2))]
          [(Or? T1) (append (helper (Or-t1 T1) T2) (helper (Or-t2 T1) T2))]
          [(Star? T1) (helper (Star-t T1) T2)]))
  (filter (lambda (x) (> (length x) 1)) (helper t t)))
;followpos
(define (followpos i t l)
  (cond [(Epsilon? t) l]
        [(Literal? t) l]
        [(Then? t) (cond [(is-present? i (lastpos (Then-t1 t))) (followpos i (Then-t1 t) (append (firstpos (Then-t2 t)) l))]
                         [(is-there? i (Then-t2 t)) (followpos i (Then-t2 t) l)] 
                         [else (followpos i (Then-t1 t) l)])]
        ;[(Or? t) (cond [(is-there? i (Or-t2 t)) (followpos i (Or-t2 t) l)] 
        ;               [else (followpos i (Or-t1 t) l)])]
        [(Or? t) (Union (followpos i (Or-t1 t) l) (followpos i (Or-t2 t) l))]
        [(Star? t) (cond [(is-present? i (lastpos (Star-t t)))
                               (followpos i (Star-t t) (append (firstpos (Star-t t)) l))]
                         [else (followpos i (Star-t t) l)])]))
;is-there?
(define (is-there? i t)
  (cond [(Epsilon? t) #t]
        [(Literal? t) (if (= i (Literal-n t)) #t #f)]
        [(Or? t) (or (is-there? i (Or-t1 t)) (is-there? i (Or-t2 t)))]
        [(Then? t) (or (is-there? i (Then-t1 t)) (is-there? i (Then-t2 t)))]
        [(Star? t) (is-there? i (Star-t t))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Union
(define (Union l1 l2)
  (remove-duplicates (append l1 l2)))
;is-present
(define (is-present? x l)
  (cond [(null? l) #f]
        [(equal? x (car l)) #t]
        [else (is-present? x (cdr l))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;nodes
(define (nodes t)
  (define (helper l1)
    (define l2 (append* (map (lambda (l3) (next-node l3 t)) l1)))
    (if (all-lies l2 l1) l1 (helper (Union l1 l2))))
  (helper (list (firstpos t))))
;all-lies
(define (all-lies l1 l2)
  (cond [(null? l1) #t]
        [(is-present? (car l1) l2) (all-lies (cdr l1) l2)]
        [else #f]))
;trans
(define (trans t)
  (append* (map (lambda (l) (transitions l t)) (nodes t))))
(define (transitions l t)
  (define pack-l (remove* '(()) (arrange (map (lambda (x) (list x)) (symbols t)) l t)))
  ;(map (lambda (l1) (Trans l (symbol-of (car l1) t) (append* (next-node l1 t)))) pack-l))
  (define (helper l1)
    (cond [(null? l1) '()]
          [(not (equal? (symbol-of (caar l1) t) "#"))
                           (append (list (Trans l (symbol-of (caar l1) t) (append*(next-node (car l1) t))))
                                   (helper (cdr l1)))]
          [else (helper (cdr l1))]))
  (helper pack-l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;arrange
(define (Arrange l1 l2 t)
  (cond [(null? l2) l1]
        [else (Arrange (map (lambda (x) (if (equal? (symbol-of (car l2) t) (car x)) (append x (list (car l2))) x)) l1) (cdr l2) t)]))
(define (arrange l1 l2 t)
  (map (lambda (l3) (cdr l3)) (Arrange l1 l2 t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;rednodes
(define (rednodes t)
  (define nodes-list (nodes t))
  (filter (lambda (x) (is-present? (integer-of "#" t) x)) nodes-list))
;next-node
(define (next-node l t)
  (define (helper)
    (define pack-l (arrange (map (lambda (x) (list x)) (symbols t)) l t))
    (map (lambda (l1) (remove-duplicates (append*(map (lambda (x) (followpos x t '())) l1)))) pack-l))
  (remove* '(()) (helper)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;symbol-of
(define (symbol-of n t)
  (let* ((literal-list (literals t)))
    (define (search i l)
      (if (= i (caar l)) (cdar l) (search i (cdr l))))
    (search n literal-list)))
;integer-of
(define (integer-of x t)
  (let* ((literal-list (literals t)))
    (define (search x l)
      (if (equal? x (cdar l)) (caar l) (search x (cdr l))))
    (search x literal-list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;symbols
(define (symbols t)
  (remove-duplicates (map (lambda (x) (cdr x)) (literals t))))
;literals
(define (literals t)
  (define (helper t)
    (cond [(Epsilon? t) '()]
          [(Literal? t) (list (cons (Literal-n t) (Literal-c t)))]
          [(Or? t) (append (helper (Or-t1 t)) (helper (Or-t2 t)))]
          [(Then? t) (append (helper (Then-t1 t)) (helper (Then-t2 t)))]
          [(Star? t) (helper (Star-t t))]))
  (remove-duplicates (helper t)))
;buildGraph
(define (buildGraph reg)
  (define (buildGraph1 t)
    (Graph (firstpos t) (nodes t) (trans t) (rednodes t) (symbols t)))
  (buildGraph1 (maketree reg)))    

; Testcase
(define r "(a|b)*bba | cc*")
(define graph (buildGraph r))
(matches? graph "bbab")

