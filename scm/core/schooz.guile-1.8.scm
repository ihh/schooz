;; Load this file to make schooz.scm (R6RS-compliant) work with guile 1.8 (R5RS)
(define (make-eq-hashtable) (make-hash-table))
(define (hashtable-ref tab key default) (hash-ref tab key))
(define (hashtable-set! tab key val) (hash-set! tab key val))
