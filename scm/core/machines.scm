
;; (machine-names)  ... returns the list of names of all state-machine objects
(define
  (machine-names)
  (schooz:machine-names))

;; (machine-states X)  ... returns the possible states of the object named X
(define
  (machine-states X)
  (schooz:machine-states X))


;; (link-goto LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define
  (link-goto LINK STATE ACTION RESULT)
  (schooz:link-goto LINK STATE ACTION RESULT))

;; (link-gosub LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define
  (link-gosub LINK STATE ACTION RESULT)
  (schooz:link-gosub LINK STATE ACTION RESULT))

;; (link-return LINK-TEXT ACTION-TEXT RESULT-TEXT)
(define
  (link-return LINK ACTION RESULT)
  (schooz:link-return LINK ACTION RESULT))

;; (schooz:choice-goto STATE ACTION-TEXT RESULT-TEXT)
(define
  (choice-goto STATE ACTION RESULT)
  (schooz:choice-goto STATE ACTION RESULT))

;; (schooz:choice-gosub STATE ACTION-TEXT RESULT-TEXT)
(define
  (choice-gosub STATE ACTION RESULT)
  (schooz:choice-gosub STATE ACTION RESULT))

;; (schooz:choice-return ACTION-TEXT RESULT-TEXT)
(define
  (choice-return ACTION RESULT)
  (schooz:choice-return ACTION RESULT))

;; (now X STATE)  ... places object X in state STATE
(define
  (now X STATE)
  (schooz:now X STATE))

;; (state X)  ... returns the current state (typically a string) of object named X, where X is an atom
(define
  (state X)
  (schooz:state X))

;; (description* X STATE FUNC)  ... set object X's descriptor function for state STATE to FUNC
(define
  (description* X STATE FUNC)
  (schooz:description* X STATE FUNC))

;; (description X STATE FUNC-BODY)  ... macro to avoid writing (lambda () ...)
(define-macro
  (description X STATE FUNC-BODY)
  `(schooz:description ,X ,STATE ,FUNC-BODY))

;; (describe X)  ... looks up the descriptor function for current state of object X, calls it
(define
  (describe X)
  (schooz:describe X))

;; (push X STATE)  ... pushes STATE onto X's stack
(define
  (push X STATE)
  (schooz:push X STATE))

;; (pop X)  ... pops state off X's stack
(define
  (pop X)
  (schooz:pop X))

;; (finish X)  ... clears X's stack
(define (finish X) (schooz:finish X))

;; (finished? X)  ... test to see if X's stack is empty
(define (finished? X) (schooz:finished? X))

;; (story* STATE FUNC)
(define (story* STATE FUNC) (schooz:story* STATE FUNC))

;; (story STATE FUNC-BODY)
(define-macro
  (story STATE FUNC-BODY)
  `(schooz:story ,STATE ,FUNC-BODY))

;; (look)
(define (look) (schooz:look))

;; (goto STATE)
(define (goto STATE) (schooz:goto STATE))

;; (gosub STATE)
(define (gosub STATE) (schooz:gosub STATE))

;; (return)
(define (return) (schooz:return))

;; (chapter)
(define (chapter) (schooz:chapter))

;; (game-over)
(define (game-over?) (schooz:game-over?))

;; (quit)
(define (quit) (schooz:quit))


;; Core data structures.
;; Object->state->descriptor hashtable
(define schooz:desc (make-eq-hashtable))
;; Object->stack hashtable
(define schooz:stack (make-eq-hashtable))

;; (schooz:ensure-object X)  ... ensures that X has valid entries in hashtables
(define
  (schooz:ensure-object X STATE)
  (if
   (not (hashtable-ref schooz:desc X #f))
   (hashtable-set! schooz:desc X (make-eq-hashtable)))
  (if
   (not (hashtable-ref schooz:stack X #f))
   (hashtable-set! schooz:stack X (list STATE))))

;; clear stack of object X
(define (schooz:clear-stack X)
  (hashtable-set! schooz:stack X '()))


;; API functions.
;; (schooz:machine-names)  ... returns the list of names of all state-machine objects
(define (schooz:machine-names) (hashtable-keys schooz:desc))

;; (schooz:machine-states X)  ... returns the possible states of the object named X
(define (schooz:machine-states X) (hashtable-keys (hashtable-ref schooz:desc X #f)))

;; (schooz:now X STATE)  ... places object X in state STATE
(define
  (schooz:now X STATE)
  (let ((old-stack (hashtable-ref schooz:stack X '())))
    (hashtable-set! schooz:stack X
	       (if (null? old-stack)
		   (list STATE)
		   (cons STATE (cdr old-stack))))))

;; (schooz:state X)  ... returns the current state (typically a string) of object named X, where X is an atom
(define
  (schooz:state X)
  (let ((stack (hashtable-ref schooz:stack X '())))
    (if (null? stack) #f (car stack))))

;; (schooz:description* X STATE FUNC)  ... set object X's descriptor function for state STATE to FUNC
(define
  (schooz:description* X STATE FUNC)
  (schooz:ensure-object X STATE)
  (hashtable-set! (hashtable-ref schooz:desc X #f) STATE FUNC))

;; (schooz:description X STATE FUNC-BODY)  ... macro to avoid writing (lambda () ...)
(define-macro
  (schooz:description X STATE FUNC-BODY)
  `(schooz:description* ,X ,STATE (lambda () ,FUNC-BODY)))

;; (schooz:describe X)  ... looks up the descriptor function for current state of object X, calls it
(define
  (schooz:describe X)
  (let ((desc (hashtable-ref (hashtable-ref schooz:desc X #f) (schooz:state X) #f)))
    (schooz:eval-or-return desc)))

;; (schooz:push X STATE)  ... pushes STATE onto X's stack
(define
  (schooz:push X STATE)
  (hashtable-set! schooz:stack X (cons STATE (hashtable-ref schooz:stack X '()))))

;; (schooz:pop X)  ... pops state off X's stack
(define
  (schooz:pop X)
  (let ((stack (hashtable-ref schooz:stack X '())))
    (if (null? stack)
	#f
	(begin
	  (hashtable-set! schooz:stack X (cdr stack))
	  (car stack)))))

;; (schooz:finish X)  ... clears X's stack
(define (schooz:finish X) (schooz:clear-stack X))

;; (schooz:finished? X)  ... test to see if X's stack is empty
(define (schooz:finished? X) (equal? (schooz:state X) #f))


;; (schooz:link-goto LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:link-goto LINK STATE ACTION RESULT)
  (schooz:link* LINK ACTION (lambda () (begin (goto STATE) RESULT))))

;; (schooz:link-gosub LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:link-gosub LINK STATE ACTION RESULT)
  (schooz:link* LINK ACTION (lambda () (begin (gosub STATE) RESULT))))

;; (schooz:link-return LINK-TEXT ACTION-TEXT RESULT-TEXT)
(define (schooz:link-return LINK ACTION RESULT)
  (schooz:link* LINK ACTION (lambda () (begin (return) RESULT))))

;; (schooz:choice-goto STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:choice-goto STATE ACTION RESULT)
  (schooz:choice* ACTION (lambda () (begin (goto STATE) RESULT))))

;; (schooz:choice-gosub STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:choice-gosub STATE ACTION RESULT)
  (schooz:choice* ACTION (lambda () (begin (gosub STATE) RESULT))))

;; (schooz:choice-return ACTION-TEXT RESULT-TEXT)
(define (schooz:choice-return ACTION RESULT)
  (schooz:choice* ACTION (lambda () (begin (return) RESULT))))


;; Main story object
(define schooz:narrative "narrative")

;; Syntactic-sugar shortcuts for working with the main story graph
;; (schooz:story* STATE FUNC)
(define (schooz:story* STATE FUNC) (schooz:description* schooz:narrative STATE FUNC))
;; (schooz:story STATE FUNC-BODY)
(define-macro
  (schooz:story STATE FUNC-BODY)
  `(schooz:story* ,STATE (lambda () ,FUNC-BODY)))
;; (schooz:look)
(define (schooz:look) (schooz:describe schooz:narrative))
;; (schooz:goto STATE)
(define (schooz:goto STATE) (schooz:now schooz:narrative STATE))
;; (schooz:gosub STATE)
(define (schooz:gosub STATE) (schooz:push schooz:narrative STATE))
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

