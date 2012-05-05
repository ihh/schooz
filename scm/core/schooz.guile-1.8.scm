;; Load this file to make schooz.scm (R6RS-compliant) work with guile 1.8 (R5RS)
(use-syntax (ice-9 syncase))

(define (make-eq-hashtable) (make-hash-table))
(define (hashtable-ref tab key default) (let ((val (hash-ref tab key))) (if val val default)))
(define (hashtable-set! tab key val) (hash-set! tab key val))

(define (random-integer n) (random n))
