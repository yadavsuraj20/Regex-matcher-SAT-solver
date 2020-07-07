#lang racket
(require "regToDfa.rkt") ;Don't worry if you do not find regToDfa.rkt
(define r "(a|b)*bba | cc*")
(define t (maketree r))
(define nullable (buildNullable t))
(define firstpos (buildFirst t))
(define lastpos (buildLast t))
(define followpos (buildFollow t))
(define graph (buildGraph r))
;(printGraph graph)
(matches? graph "bbab")
(matches? graph "bba")
(matches? graph "")
(matches? graph "cc")
(define r1 "@|((a|b)*bba | cc*)")
(define t1 (maketree r1))

(define graph1 (buildGraph r1))
(matches? graph1 "")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; There were some problems with the buildFollow definition,
; which I have now fixed. I have essentially followed the
; following principle: Any leaf symbol which does not have
; a follow symbol does not appear in the output of buildFollow.
; Thus for the tree t above the number corresponding to #(16)
; does not appear in (buildFollow t). It now works correctly
; (buildFollow (Then-t1 t)) and (buildFollow (Or-t1 (Then-t1 t)))

(display #\newline)


"Printing (buildFollow t)"
(buildFollow t)

(display #\newline)


"Printing (buildFollow (Then-t1 t))"
(buildFollow (Then-t1 t))

(display #\newline)


"Printing (buildFollow (Or-t1 (Then-t1 t)))"
(buildFollow (Or-t1 (Then-t1 t)))

; ************************************************
; Another problem reported was that the same state was
; getting generated more than once, but in a different 
; order -- for example (1 14 9) and (14 1 9). This has been fixed
; by first sorting the elements of each state that is generated
; and only then examining whether the state was generated earlier.

(display #\newline)

"Printing the graph for ((abcdef)*|(@|d)*)*"

(buildGraph "((abcdef)*|(@|d)*)*")

; *************************************************

;  Yet another problem. There were duplicates in the numbering of the
;states. For example (buildFollow (maketree "(a*|b)*")) was returning
;((1 1 1 3 6) (3 1 3 6)). I am now removing the duplicates so that
;the returned value is ((1 1 3 6) (3 1 3 6)). Thanks Sagar Kalsaria

(display #\newline)

"Printing the result of (buildFollow (maketree \"(a*|b)*\"))"

(buildFollow (maketree "(a*|b)*" ))



