#!/usr/local/bin/guile
-s
!#
;; (load "./schooz.scm")
(load "./guile-1.8-schooz.scm")

;; Basic terminal interface

(define action-text-list '())
(define action-func-list '())

(define (reset-action-list)
  (set! action-text-list '())
  (set! action-func-list '()))

;; push (action-text,action-func) onto (action-text-list,action-func-list)
(define (add-action action-text action-func)
  (set! action-text-list (cons action-text action-text-list))
  (set! action-func-list (cons action-func action-func-list)))

(define (link link-text action-text action-func)
  (add-action action-text action-func)
  link-text)

(define (menu link-text action-list)
  (if (null? action-list) link-text
      (let* ((action (car action-list))
	     (action-text (car action))
	     (action-func (car action))
	     (rest-of-list (cdr action-list)))
	(begin
	  (add-action action-text action-func)
	  (menu link-text rest-of-list)))))

(define (choice action-list) (menu "" action-list))

(define (ask X PROMPT)
  (display PROMPT)
  (now X (read)))

(define (main-loop)
  (if (game-over?)
      1
      (begin
	(reset-action-list)
	(display (look)) ((action-chosen-from-list))
	(main-loop))))

(define
  (action-chosen-from-list)
  (display action-text-list)  ;; should display action texts with numbers here
  (display "Choice? ")
  (let ((actions (length action-func-list)))
    (cond (= actions 0) (lambda () ((quit) ""))  ;; auto-quit if no available actions
	  (let ((n (read)))
	    (cond (and (number? n) (>= n 1) (<= n actions))
		  (list-ref action-func-list (- actions n)  ;; return n'th function from action-func-list
			    (action-chosen-from-list)))))))  ;; can't parse, try again (should print error message)
