;; Minimal Scheme-based CYOA framework.
;; R6RS compliant

(define schooz:version 1)

;; Core data structures.
;; Object->state->descriptor hashtable
(define schooz:desc (make-eq-hashtable))
;; Object->stack hashtable
(define schooz:stack (make-eq-hashtable))

;; Internal functions.
;; (schooz:ensure-object X)  ... ensures that X has valid entries in hashtables
(define
  (schooz:ensure-object X STATE)
  (if
   (not (hash-ref schooz:desc X))
   (hash-set! schooz:desc X (make-eq-hashtable)))
  (if
   (not (hash-ref schooz:stack X))
   (hash-set! schooz:stack X (list STATE))))

;; (schooz:eval-or-return f)  ... if f is a function, evaluate; otherwise, return
(define (schooz:eval-or-return f)
  (if (procedure? f) (f) f))

;; (schooz:as-function f)  ... if f is a function, return f; otherwise, return function returning f
(define (schooz:as-function f)
  (if (procedure? f) f (lambda () f)))

;; clear stack of object X
(define (schooz:clear-stack X)
  (hash-set! schooz:stack X '()))

;; convert an SXML S-expression to an XML string
;; Can't yet handle attributes...
(define (schooz:fold-sxml-outer str lst)
  (cond ((string? lst) (string-append str lst))
	((null? lst) str)
	((pair? lst)
	 (let ((tag (car lst))
	       (rest (cdr lst)))
	   (if (string? tag)
	       (if (null? rest)
		   (string-append str "<" tag "/>")
		   (string-append str "<" tag ">" (schooz:fold-sxml-inner "" rest) "</" tag ">"))
	       (schooz:fold-sxml-inner str lst))))
	(else str)))

(define (schooz:fold-sxml-inner str lst)
  (cond ((string? lst) (string-append str lst))
	((null? lst) str)
	((pair? lst)
	 (let ((elem (car lst))
	       (rest (cdr lst)))
	   (schooz:fold-sxml-inner (schooz:fold-sxml-outer str elem) rest)))
	(else str)))

(define (schooz:fold-strings-sxml lst)
  (schooz:fold-sxml-inner "" lst))

;; alternate (non-SXML) output adapter: just flattens everything
(define (schooz:fold-plain str lst)
  (cond ((string? lst) (string-append str lst))
	((null? lst) str)
	((pair? lst)
	 (let ((elem (car lst))
	       (rest (cdr lst)))
	   (schooz:fold-plain (schooz:fold-plain str elem) rest)))))

(define (schooz:fold-strings-plain lst)
  (schooz:fold-plain "" lst))

;; define default output adapter, and ways to change output adapter
(define schooz:fold-strings schooz:fold-strings-sxml)

(define (schooz:output-plain)
  (set! schooz:fold-strings schooz:fold-strings-plain))

(define (schooz:output-sxml)
  (set! schooz:fold-strings schooz:fold-strings-sxml))

;; list helpers
(define (schooz:as-list lst) (if (pair? lst) lst (list lst)))

(define (schooz:append-as-lists lst1 lst2)
  (append (schooz:as-list lst1) (schooz:as-list lst2)))

;; API functions.
;; (schooz:now X STATE)  ... places object X in state STATE
(define
  (schooz:now X STATE)
  (let ((old-stack (hash-ref schooz:stack X)))
    (hash-set! schooz:stack X
	       (if (null? old-stack)
		   (list STATE)
		   (cons STATE (cdr old-stack))))))

;; (schooz:state X)  ... returns the current state (typically a string) of object named X, where X is an atom
(define
  (schooz:state X)
  (let ((stack (hash-ref schooz:stack X)))
    (if (null? stack) #f (car stack))))

;; (schooz:description* X STATE FUNC)  ... set object X's descriptor function for state STATE to FUNC
(define
  (schooz:description* X STATE FUNC)
  (schooz:ensure-object X STATE)
  (hash-set! (hash-ref schooz:desc X) STATE FUNC))

;; (schooz:description X STATE FUNC-BODY)  ... macro to avoid writing (lambda () ...)
(define-macro
  (schooz:description X STATE FUNC-BODY)
  `(schooz:description* ,X ,STATE (lambda () ,FUNC-BODY)))

;; (schooz:describe X)  ... looks up the descriptor function for current state of object X, calls it
(define
  (schooz:describe X)
  (let ((desc (hash-ref (hash-ref schooz:desc X) (schooz:state X))))
    ;; Uncomment to debug
    ;; (display "(describe ") (display X) (display ") -> ") (display (schooz:state X)) (display " -> ") (display desc) (display "\n")
    (schooz:eval-or-return desc)))

;; (schooz:push X STATE)  ... pushes STATE onto X's stack
(define
  (schooz:push X STATE)
  (hash-set! schooz:stack X (cons STATE (hash-ref schooz:stack X))))

;; (schooz:pop X)  ... pops state off X's stack
(define
  (schooz:pop X)
  (let ((stack (hash-ref schooz:stack X)))
    (if (null? stack)
	#f
	(begin
	  (hash-set! schooz:stack X (cdr stack))
	  (car stack)))))

;; (schooz:finish X)  ... clears X's stack
(define (schooz:finish X) (schooz:clear-stack X))

;; (schooz:finished? X)  ... test to see if X's stack is empty
(define (schooz:finished? X) (equal? (schooz:state X) #f))

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


;; (schooz:link* TEXT ACTION-TEXT FUNC-BODY)
(define (schooz:link* link-text action-text action-func)
  (schooz:impl-link* link-text action-text (schooz:transform-action action-func)))

;; (schooz:link TEXT ACTION-TEXT FUNC-BODY)
(define-macro (schooz:link LINK ACTION FUNC-BODY)
  `(schooz:link* ,LINK ,ACTION (lambda () ,FUNC-BODY)))

;; (schooz:link-goto LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:link-goto LINK STATE ACTION RESULT)
  (schooz:link* LINK ACTION (lambda () (begin (goto STATE) RESULT))))

;; (schooz:link-gosub LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:link-gosub LINK STATE ACTION RESULT)
  (schooz:link* LINK ACTION (lambda () (begin (gosub STATE) RESULT))))

;; (schooz:link-return LINK-TEXT ACTION-TEXT RESULT-TEXT)
(define (schooz:link-return LINK ACTION RESULT)
  (schooz:link* LINK ACTION (lambda () (begin (return) RESULT))))

;; (schooz:menu* TEXT ACTION-LIST)
(define (schooz:menu* link-text action-list)
  (schooz:impl-menu* link-text (schooz:transform-action-list action-list)))

;; (schooz:menu TEXT ACTION-LIST)
(define-macro
  (schooz:menu TEXT ACTION-LIST)
  `(schooz:menu* ,TEXT (list ,@ACTION-LIST)))

;; (schooz:explicit-menu* ACTION-LIST)
(define (schooz:explicit-menu* action-list)
  (schooz:impl-explicit-menu* (schooz:transform-action-list action-list)))

;; (schooz:explicit-menu ACTION-LIST)
(define-macro
  (schooz:explicit-menu ACTION-LIST)
  `(schooz:explicit-menu* (list ,@ACTION-LIST)))

;; (schooz:choice* ACTIONTEXT FUNC)  ... simple helper/wrapper
(define (schooz:choice* ACTIONTEXT FUNC) (list ACTIONTEXT FUNC))

;; (schooz:choice ACTIONTEXT FUNC-BODY)
(define-macro
  (schooz:choice ACTIONTEXT FUNC-BODY)
  `(schooz:choice* ,ACTIONTEXT (lambda () ,FUNC-BODY)))

;; (schooz:choice-goto STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:choice-goto STATE ACTION RESULT)
  (schooz:choice* ACTION (lambda () (begin (goto STATE) RESULT))))

;; (schooz:choice-gosub STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:choice-gosub STATE ACTION RESULT)
  (schooz:choice* ACTION (lambda () (begin (gosub STATE) RESULT))))

;; (schooz:choice-return ACTION-TEXT RESULT-TEXT)
(define (schooz:choice-return ACTION RESULT)
  (schooz:choice* ACTION (lambda () (begin (return) RESULT))))

;; (schooz:ask X PROMPT)
(define (schooz:ask X PROMPT)
  (display PROMPT)
  (schooz:now X (read)))


;; UI implementation should call this procedure to fire the initial action
(define (schooz:dummy-action) '())
(define schooz:initial-action* schooz:dummy-action)  ;; untransformed
(define (schooz:initial-action) (schooz:transform-action schooz:initial-action*))  ;; transformed

;; Action transformations, applied automatically by (link...), (menu...), (explicit-menu...), (fire-action...)
;; Default action transformation just ensures that all actions are functions
(define schooz:transform-action schooz:as-function)

;; method to compose a new action transformation
(define (schooz:compose-transform-action new-transform)
  (let ((old-transform schooz:transform-action))
    (set! schooz:transform-action (lambda (f) (new-transform (old-transform f))))))

;; action transformation: do something after every action
(define (schooz:after-every-action g)
  (schooz:compose-transform-action
   (lambda (f) (lambda () (schooz:append-as-lists (f) (g))))))

;; action transformation: do something before every action
(define (schooz:before-every-action g)
  (schooz:compose-transform-action
   (lambda (f) (lambda () (schooz:append-as-lists (g) (f))))))

;; look after every action
(define (schooz:look-after-every-action)
  (schooz:after-every-action schooz:look))

;; application of transform-action to list of the form ((ACTIONTEXT1 FUNC1) (ACTIONTEXT2 FUNC2)...)
(define (schooz:transform-action-list lst)
  (if
   (null? lst)
   lst
   (let* ((action (car lst))
	  (rest (cdr lst))
	  (action-text (car action))
	  (action-func (cadr action)))
     (cons (list action-text (schooz:transform-action action-func)) (schooz:transform-action-list rest)))))

;; Interface implementation-dependent methods.
;; The following functions must return a string, list, or nested list
;; (schooz:impl-link* TEXT ACTIONTEXT FUNC)  ... returns hyperlinked text TEXT that calls FUNC with mouseover text ACTIONTEXT
;; (schooz:impl-menu* TEXT ((ACTIONTEXT1 FUNC1) (ACTIONTEXT2 FUNC2) ...))  ... returns text hyperlinked to a popup menu
;; (schooz:impl-explicit-menu* ((ACTIONTEXT1 FUNC1) (ACTIONTEXT2 FUNC2) ...))  ... returns a menu (rendered as a list)

;; The following functions do I/O in an implementation-dependent manner.
;; (schooz:impl-ask X PROMPT)  ... outputs PROMPT; blocks until user responds; sets state of X directly.

;; The interface must also implement the logic of the main loop.
;; Roughly speaking, this is as follows:
;; Loop:
;;  Evaluate and display results of current action (starting with (schooz:initial-action))
;;  If not (game-over?):
;;   Acquire next action; continue to Loop.
