#!/usr/local/bin/guile
-s
!#
;; Wrapper to make schooz.scm (R6RS-compliant) work with guile 1.8 (R5RS)
(define (make-eq-hashtable) (make-hash-table))
(load "./schooz.scm")
