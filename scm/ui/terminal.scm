;; Basic terminal interface
;; Defines a (schooz:main-loop) function that should be called to start the game.

;; dummy HTML
(define p (lambda args `("p" ,@args)))
(define span (lambda args `("span" ,@args)))
(define h1 (lambda args `("h1" ,@args)))
(define h2 (lambda args `("h2" ,@args)))
(define h3 (lambda args `("h3" ,@args)))
(define i (lambda args `("i" ,@args)))
(define b (lambda args `("b" ,@args)))

;; Highlighting of links (just to show where they would be)
(define
  (schooz:highlight text)
  (string-append
   (string (integer->char 27)) "[1m" text (string (integer->char 27)) "[0m"))

;; Interface implementation of hyperlinks & popups
(define (schooz:impl-link* link-text action-text action-func)
  (schooz:highlight link-text))

(define (schooz:impl-popup* link-text action-list)
  (schooz:highlight link-text))

(define (schooz:impl-menu* action-list)
  (schooz:impl-popup* "" action-list))

(define (schooz:impl-ask X PROMPT)
  (display PROMPT)
  (schooz:now X (read)))

;; Main entry point
(define (schooz:main-loop)
  (schooz:action-loop (schooz:initial-action)))

(define (schooz:action-loop action-func)
  (display (schooz:fold-strings (action-func)))
  (cond ((schooz:game-over?) (display "GAME OVER\n"))
	((schooz:no-actions?) (display "THE END\n"))
	(else (schooz:action-loop (schooz:action-chosen-from-list)))))

;; Menu
(define
  (schooz:action-chosen-from-list)
  (let ((actions (schooz:number-of-actions)))
    (if (= actions 0) (lambda () (schooz:return) "")  ;; auto-quit if no available actions
	(begin
	  (display "\nOptions:\n")
	  (schooz:display-action-text-list)
	  (display "Choice? ")
	  (let* ((l (read))
		 (n (if (integer? l) (inexact->exact l) -1)))
	    (if (and (>= n 1) (<= n actions))
		(begin
		  (display "\n")
		  (schooz:get-action-func n))
		(schooz:action-chosen-from-list)))))))  ;; can't parse, try again (should print error message)

(define (schooz:display-action-text-list)
  (schooz:display-action-text 1))

(define (schooz:display-action-text n)
  (if (<= n (schooz:number-of-actions))
      (begin
	(display n)
	(display ". ")
	(display (schooz:get-action-text n))
	(display "\n")
	(schooz:display-action-text (+ n 1)))))

