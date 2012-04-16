;; Minimal Scheme-based CYOA framework.

;; Main objects.
;; Object->state hashtable
(define schooz-state (make-eq-hashtable))
;; Object->state->descriptor hashtable
(define schooz-desc (make-eq-hashtable))
;; Object->stack hashtable
(define schooz-stack (make-eq-hashtable))

;; Internal functions.
;; (ensure-object X)  ... ensures that X has valid entries in hashtables
(define
  (ensure-object X)
  (if
   (not (hash-ref schooz-state X))
   (hash-set! schooz-state X "start"))
  (if
   (not (hash-ref schooz-desc X))
   (hash-set! schooz-desc X (make-eq-hashtable)))
  (if
   (not (hash-ref schooz-stack X))
   (hash-set! schooz-stack X '())))

;; API functions.
;; (now X STATE)  ... places object X in state STATE
(define
  (now X STATE)
  (hash-set! schooz-state X STATE))

;; (state X)  ... returns the current state (typically a string) of object named X, where X is an atom
(define
  (state X)
  (hash-ref schooz-state X))

;; (describe X STATE FUNC)  ... set object X's descriptor function for state STATE to FUNC
(define
  (describe X STATE FUNC)
  (ensure-object X)
  (hash-set! (schooz-desc X) STATE FUNC))

;; (tell X)  ... looks up the descriptor function for current state of object X, calls it
(define
  (tell X)
  ((hash-ref schooz-desc (state X))))


;; (push X STATE)  ... pushes the current state of X onto X's stack, places X into state STATE
(define
  (push X STATE)
  (hash-set! (schooz-stack X) (cons STATE (schooz-stack X))))

;; (pop X)  ... pops state off X's state stack, places X into popped state
(define
  (pop X)
  (hash-set! (schooz-stack X) (cdr (schooz-stack X))))

;; Main story object
(define narrative "narrative")
;; Syntactic-sugar shortcuts for working with the main story graph
;; (story STATE FUNC)
(define (story STATE FUNC) (describe narrative STATE FUNC))
;; (look)
(define (look) (tell narrative))
;; (goto STATE)
(define (goto STATE) (now narrative STATE))
;; (gosub STATE)
(define (gosub STATE) (push narrative STATE))
;; (return)
(define (return) (pop narrative))
;; (chapter)
(define (chapter) (state narrative))

;; Interface-dependent methods.
;; The following functions must return a string, list, or nested list
;; (link TEXT ACTION FUNC)  ... returns a hyperlink with text TEXT that calls FUNC, with mouseover text ACTION
;; (menu TEXT ((ACTIONTEXT1 FUNC1) (ACTIONTEXT2 FUNC2) ...))  ... returns text hyperlinked to a popup menu
;; (choice ((ACTIONTEXT1 FUNC1) (ACTIONTEXT2 FUNC2) ...))  ... returns a menu (rendered as a list)

;; The following functions do I/O in an implementation-dependent manner.
;; (ask X PROMPT)  ... outputs PROMPT; blocks until user responds; sets state of X directly.
