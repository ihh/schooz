
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
