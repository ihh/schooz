
;; Sketch of minimal implementation:
(define (link TEXT ACTIONTEXT FUNC)
  ...  ;; push (ACTIONTEXT,FUNC) onto (action-text-list,action-func-list)
  TEXT)
(define (menu TEXT ACTIONTEXT-FUNC-LIST)
  ...  ;; push each (ACTIONTEXT,FUNC) onto (action-text-list,action-func-list)
  TEXT)
(define (choice ACTIONTEXT-FUNC-LIST) (menu "" ACTIONTEXT-FUNC-LIST))
(define (ask X PROMPT)
  (display PROMPT)
  (now X (read)))
(define (main-loop) (if (game-over?) 1 (display-scene)))
(define (display-scene)
  ...  ;; clear (action-text-list,action-func-list)
  (display (look) ((action-chosen-from-list)))
  (main-loop))
(define
  (action-chosen-from-list)
  (display action-text-list)  ;; should display action texts with numbers here
  (display "Choice: ")
  (let ((n (read)))
      (cond ((number? n) ...)  ;; should do better test, return n'th function from action-func-list
	    (action-chosen-from-list))))  ;; can't parse, try again (should print error message)
