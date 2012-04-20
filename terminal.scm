;; Basic terminal interface
;; Defines a (schooz:main-loop) function that should be called to start the game.

;; Highlighting of links (just to show where they would be)
(define
  (schooz:highlight text)
  (string-append
   (string (integer->char 27)) "[1m" text (string (integer->char 27)) "[0m"))

;; Interface methods
(define (schooz:impl-link* link-text action-text action-func)
  (schooz:highlight link-text))

(define (schooz:impl-menu* link-text action-list)
  (schooz:highlight link-text))

(define (schooz:impl-explicit-menu* action-list)
  (schooz:impl-menu* "" action-list))

(define (schooz:impl-ask X PROMPT)
  (display PROMPT)
  (schooz:now X (read)))

;; Main entry point
(define (schooz:main-loop)
  (schooz:action-loop (schooz:initial-action)))

(define (schooz:action-loop action-func)
  (display (schooz:fold-strings (action-func)))
  (if (schooz:game-over?)
      (display "GAME OVER\n")
      (schooz:action-loop (schooz:action-chosen-from-list))))

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

