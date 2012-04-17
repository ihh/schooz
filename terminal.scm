;; (load "./schooz.scm")
(load "./guile-1.8-schooz.scm")

;; Basic terminal interface
;; Defines a (main-loop) function that should be called to start the game.

(define action-text-list '())
(define action-func-list '())

(define (reset-action-list)
;; Uncomment to debug
;;  (display "Resetting action list\n")
  (set! action-text-list '())
  (set! action-func-list '()))

;; push (action-text,action-func) onto (action-text-list,action-func-list)
(define (add-action action-text action-func)
;; Uncomment to debug
;;  (display "Adding action text: ") (display action-text) (display " function: ") (display action-func) (display "\n")
  (set! action-text-list (cons action-text action-text-list))
  (set! action-func-list (cons action-func action-func-list)))

(define (link link-text action-text action-func)
  (add-action action-text action-func)
  link-text)

(define (menu link-text action-list)
  (if (null? action-list) link-text
      (let* ((action (car action-list))
	     (action-text (car action))
	     (action-func (cadr action))
	     (rest-of-list (cdr action-list)))
	(begin
	  (add-action action-text action-func)
	  (menu link-text rest-of-list)))))

(define (explicit-menu action-list) (menu "" action-list))

(define (ask X PROMPT)
  (display PROMPT)
  (now X (read)))

(define (main-loop)
  (if (game-over?)
      (display "GAME OVER\n")
      (begin
	(reset-action-list)
	(display (schooz-flatten-strings (look)))
	(display "\n")
	(display (schooz-flatten-strings (eval-or-return (action-chosen-from-list))))
	(display "\n")
	(main-loop))))

(define
  (action-chosen-from-list)
  (display "\nOptions:\n")
  (display-action-text-list)
  (let ((actions (length action-func-list)))
    (display "Choice? ")
    (if (= actions 0) (lambda () ((return) ""))  ;; auto-quit if no available actions
	  (let ((n (read)))
	    (if (and (number? n) (>= n 1) (<= n actions))
		(begin
		  (display "\n")
		  (list-ref action-func-list (- actions n)))  ;; return n'th function from action-func-list
		(action-chosen-from-list))))))  ;; can't parse, try again (should print error message)

(define (display-action-text-list)
  (display-action-text 0))

(define (display-action-text n)
  (let ((actions (length action-text-list)))
    (if (>= n actions) 1
	(begin
	  (display (+ n 1))
	  (display ". ")
	  (display (list-ref action-text-list (- actions n 1)))
	  (display "\n")
	  (display-action-text (+ n 1))))))

