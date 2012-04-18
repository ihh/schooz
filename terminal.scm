;; (load "./schooz.scm")
(load "./guile-1.8-schooz.scm")

;; Basic terminal interface
;; Defines a (main-loop) function that should be called to start the game.

(define schooz:action-text-list '())
(define schooz:action-func-list '())

(define (schooz:reset-action-list)
;; Uncomment to debug
;;  (display "Resetting action list\n")
  (set! schooz:action-text-list '())
  (set! schooz:action-func-list '()))

;; push (action-text,action-func) onto (schooz:action-text-list,schooz:action-func-list)
(define (schooz:add-action action-text action-func)
;; Uncomment to debug
;;  (display "Adding action text: ") (display action-text) (display " function: ") (display action-func) (display "\n")
  (set! schooz:action-text-list (cons action-text schooz:action-text-list))
  (set! schooz:action-func-list (cons action-func schooz:action-func-list)))

;; Highlighting of links (just to show where they would be)
(define
  (schooz:highlight text)
  (string-append
   (string (integer->char 27)) "[1m" text (string (integer->char 27)) "[0m"))

;; Interface methods
(define (schooz:link* link-text action-text action-func)
  (schooz:add-action action-text action-func)
  (schooz:highlight link-text))

(define (schooz:menu* link-text action-list)
  (if (null? action-list) (schooz:highlight link-text)
      (let* ((action (car action-list))
	     (action-text (car action))
	     (action-func (cadr action))
	     (rest-of-list (cdr action-list)))
	(begin
	  (schooz:add-action action-text action-func)
	  (schooz:menu* link-text rest-of-list)))))

(define (schooz:explicit-menu* action-list) (menu* "" action-list))

(define (schooz:ask X PROMPT)
  (display PROMPT)
  (schooz:now X (read)))

;; Main entry point
(define (main-loop)
  (if (schooz:game-over?)
      (display "GAME OVER\n")
      (begin
	(schooz:reset-action-list)
	(display (schooz:fold-strings (schooz:look)))
	(display "\n")
	(display (schooz:fold-strings (schooz:eval-or-return (schooz:action-chosen-from-list))))
	(display "\n")
	(main-loop))))

;; Menu
(define
  (schooz:action-chosen-from-list)
  (display "\nOptions:\n")
  (schooz:display-action-text-list)
  (let ((actions (length schooz:action-func-list)))
    (display "Choice? ")
    (if (= actions 0) (lambda () ((return) ""))  ;; auto-quit if no available actions
	(let* ((l (read))
	       (n (if (integer? l) (inexact->exact l) -1)))
	  (if (and (>= n 1) (<= n actions))
	      (begin
		(display "\n")
		(list-ref schooz:action-func-list (- actions n)))  ;; return n'th function from schooz:action-func-list
	      (schooz:action-chosen-from-list))))))  ;; can't parse, try again (should print error message)

(define (schooz:display-action-text-list)
  (schooz:display-action-text 0))

(define (schooz:display-action-text n)
  (let ((actions (length schooz:action-text-list)))
    (if (>= n actions) 1
	(begin
	  (display (+ n 1))
	  (display ". ")
	  (display (list-ref schooz:action-text-list (- actions n 1)))
	  (display "\n")
	  (schooz:display-action-text (+ n 1))))))

