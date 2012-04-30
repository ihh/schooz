
;; The first part of this file just defines shortcuts for the most commonly-used functions,
;; dropping the "schooz:" prefix; e.g. (link...) instead of (schooz:link...)

;; (machine-names)  ... returns the list of names of all state-machine objects
(define
  (machine-names)
  (schooz:machine-names))

;; (machine-states x)  ... returns the possible states of the object named x
(define
  (machine-states x)
  (schooz:machine-states x))


;; (link-goto link-text state action-text result-text)
(define
  (link-goto link state action result)
  (schooz:link-goto link state action result))

;; (link-gosub link-text state action-text result-text)
(define
  (link-gosub link state action result)
  (schooz:link-gosub link state action result))

;; (link-return link-text action-text result-text)
(define
  (link-return link action result)
  (schooz:link-return link action result))

;; (schooz:choice-goto state action-text result-text)
(define
  (choice-goto state action result)
  (schooz:choice-goto state action result))

;; (schooz:choice-gosub state action-text result-text)
(define
  (choice-gosub state action result)
  (schooz:choice-gosub state action result))

;; (schooz:choice-return action-text result-text)
(define
  (choice-return action result)
  (schooz:choice-return action result))

;; (now x state)  ... places object x in state state
(define
  (now x state)
  (schooz:now x state))

;; (state x)  ... returns the current state (typically a string) of object named x, where x is an atom
(define
  (state x)
  (schooz:state x))

;; (description* x state func)  ... set object x's descriptor function for state state to func
(define
  (description* x state func)
  (schooz:description* x state func))

;; (description x state func-body)  ... macro to avoid writing (lambda () ...)
(define-macro
  (description x state func-body)
  `(schooz:description ,x ,state ,func-body))

;; (describe x)  ... looks up the descriptor function for current state of object x, calls it
(define
  (describe x)
  (schooz:describe x))

;; (push x state)  ... pushes state onto x's stack
(define
  (push x state)
  (schooz:push x state))

;; (pop x)  ... pops state off x's stack
(define
  (pop x)
  (schooz:pop x))

;; (finish x)  ... clears x's stack
(define (finish x) (schooz:finish x))

;; (finished? x)  ... test to see if x's stack is empty
(define (finished? x) (schooz:finished? x))

;; (story* state func)
(define (story* state func) (schooz:story* state func))

;; (story state func-body)
(define-macro
  (story state func-body)
  `(schooz:story ,state ,func-body))

;; (look)
(define (look) (schooz:look))

;; (goto state)
(define (goto state) (schooz:goto state))

;; (gosub state)
(define (gosub state) (schooz:gosub state))

;; (return)
(define (return) (schooz:return))

;; (chapter)
(define (chapter) (schooz:chapter))

;; (game-over)
(define (game-over?) (schooz:game-over?))

;; (quit)
(define (quit) (schooz:quit))



;; Now come the actual data structures and function definitions.

;; Core data structures.
;; Object->state->descriptor hashtable
(define schooz:desc (make-eq-hashtable))
;; Object->stack hashtable
(define schooz:stack (make-eq-hashtable))

;; (schooz:ensure-object x)  ... ensures that x has valid entries in hashtables
(define
  (schooz:ensure-object x state)
  (if
   (not (hashtable-ref schooz:desc x #f))
   (hashtable-set! schooz:desc x (make-eq-hashtable)))
  (if
   (not (hashtable-ref schooz:stack x #f))
   (hashtable-set! schooz:stack x (list state))))

;; clear stack of object x
(define (schooz:clear-stack x)
  (hashtable-set! schooz:stack x '()))


;; API functions.
;; (schooz:machine-names)  ... returns the list of names of all state-machine objects
(define (schooz:machine-names) (hashtable-keys schooz:desc))

;; (schooz:machine-states x)  ... returns the possible states of the object named x
(define (schooz:machine-states x) (hashtable-keys (hashtable-ref schooz:desc x #f)))

;; (schooz:now x state)  ... places object x in state state
(define
  (schooz:now x state)
  (let ((old-stack (hashtable-ref schooz:stack x '())))
    (hashtable-set! schooz:stack x
;		    (if (or (null? old-stack) (not (pair? old-stack)))
		    (if (null? old-stack)
			(list state)
			(cons state (cdr old-stack))))))

;; (schooz:state x)  ... returns the current state (typically a string) of object named x, where x is an atom
(define
  (schooz:state x)
  (let ((stack (hashtable-ref schooz:stack x '())))
    (if (null? stack) #f (car stack))))

;; (schooz:description* x state func)  ... set object x's descriptor function for state state to func
(define
  (schooz:description* x state func)
  (schooz:ensure-object x state)
  (hashtable-set! (hashtable-ref schooz:desc x #f) state func))

;; (schooz:description x state func-body)  ... macro to avoid writing (lambda () ...)
(define-macro
  (schooz:description x state func-body)
  `(schooz:description* ,x ,state (lambda () ,func-body)))

;; (schooz:describe x)  ... looks up the descriptor function for current state of object x, calls it
(define
  (schooz:describe x)
  (let ((desc (hashtable-ref (hashtable-ref schooz:desc x #f) (schooz:state x) #f)))
    (schooz:eval-or-return desc)))

;; (schooz:push x state)  ... pushes state onto x's stack
(define
  (schooz:push x state)
  (hashtable-set! schooz:stack x (cons state (hashtable-ref schooz:stack x '()))))

;; (schooz:pop x)  ... pops state off x's stack
(define
  (schooz:pop x)
  (let ((stack (hashtable-ref schooz:stack x '())))
    (if (null? stack)
	#f
	(begin
	  (hashtable-set! schooz:stack x (cdr stack))
	  (car stack)))))

;; (schooz:finish x)  ... clears x's stack
(define (schooz:finish x) (schooz:clear-stack x))

;; (schooz:finished? x)  ... test to see if x's stack is empty
(define (schooz:finished? x) (equal? (schooz:state x) #f))


;; (schooz:link-goto link-text state action-text result-text)
(define (schooz:link-goto link state action result)
  (schooz:link* link action (lambda () (begin (goto state) result))))

;; (schooz:link-gosub link-text state action-text result-text)
(define (schooz:link-gosub link state action result)
  (schooz:link* link action (lambda () (begin (gosub state) result))))

;; (schooz:link-return link-text action-text result-text)
(define (schooz:link-return link action result)
  (schooz:link* link action (lambda () (begin (return) result))))

;; (schooz:choice-goto state action-text result-text)
(define (schooz:choice-goto state action result)
  (schooz:choice* action (lambda () (begin (goto state) result))))

;; (schooz:choice-gosub state action-text result-text)
(define (schooz:choice-gosub state action result)
  (schooz:choice* action (lambda () (begin (gosub state) result))))

;; (schooz:choice-return action-text result-text)
(define (schooz:choice-return action result)
  (schooz:choice* action (lambda () (begin (return) result))))


;; Main story object
(define schooz:narrative "narrative")

;; Syntactic-sugar shortcuts for working with the main story graph
;; (schooz:story* state func)
(define (schooz:story* state func) (schooz:description* schooz:narrative state func))
;; (schooz:story state func-body)
(define-macro
  (schooz:story state func-body)
  `(schooz:story* ,state (lambda () ,func-body)))
;; (schooz:look)
(define (schooz:look) (schooz:describe schooz:narrative))
;; (schooz:goto state)
(define (schooz:goto state) (schooz:now schooz:narrative state))
;; (schooz:gosub state)
(define (schooz:gosub state) (schooz:push schooz:narrative state))
;; (schooz:return)
(define (schooz:return) (schooz:pop schooz:narrative))
;; (schooz:chapter)
(define (schooz:chapter) (schooz:state schooz:narrative))
;; (schooz:game-over)
(define (schooz:game-over?) (schooz:finished? schooz:narrative))
;; (schooz:quit)
(define (schooz:quit) (schooz:finish schooz:narrative))

;; look after every action
(define (schooz:look-after-every-action)
  (schooz:after-every-action schooz:look))

