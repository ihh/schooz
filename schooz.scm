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
	   (if (null? rest)
	       (string-append str "<" tag "/>")
	       (string-append str "<" tag ">" (schooz:fold-sxml-inner "" rest) "</" tag ">"))))))

(define (schooz:fold-sxml-inner str lst)
  (cond ((string? lst) (string-append str lst))
	((null? lst) str)
	((pair? lst)
	 (let ((elem (car lst))
	       (rest (cdr lst)))
	   (schooz:fold-sxml-inner (schooz:fold-sxml-outer str elem) rest)))))

;; default output adapter: SXML
(define (schooz:fold-strings lst)
  ;; Uncomment to debug
  ;; (write lst)
  (schooz:fold-sxml-inner "" lst))

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

;; (schooz:choice-goto STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:choice-goto STATE ACTION RESULT)
  (schooz:choice* ACTION (lambda () (begin (goto STATE) RESULT))))

;; (schooz:choice-gosub STATE ACTION-TEXT RESULT-TEXT)
(define (schooz:choice-gosub STATE ACTION RESULT)
  (schooz:choice* ACTION (lambda () (begin (gosub STATE) RESULT))))

;; (schooz:choice-return ACTION-TEXT RESULT-TEXT)
(define (schooz:choice-return ACTION RESULT)
  (schooz:choice* ACTION (lambda () (begin (return) RESULT))))

;; (schooz:choice* ACTIONTEXT FUNC)  ... simple helper/wrapper
(define (schooz:choice* ACTIONTEXT FUNC) (list ACTIONTEXT FUNC))

;; (schooz:choice ACTIONTEXT FUNC-BODY)
(define-macro
  (schooz:choice ACTIONTEXT FUNC-BODY)
  `(schooz:choice* ,ACTIONTEXT (lambda () ,FUNC-BODY)))

;; (schooz:menu TEXT ACTION-LIST)
(define-macro
  (schooz:menu TEXT ACTION-LIST)
  `(schooz:menu* ,TEXT (list ,@ACTION-LIST)))

;; (schooz:explicit-menu ACTION-LIST)
(define-macro
  (schooz:explicit-menu ACTION-LIST)
  `(schooz:explicit-menu* (list ,@ACTION-LIST)))

;; Interface implementation-dependent methods.
;; The following functions must return a string, list, or nested list
;; (schooz:link* TEXT ACTIONTEXT FUNC)  ... returns hyperlinked text TEXT that calls FUNC with mouseover text ACTIONTEXT
;; (schooz:menu* TEXT ((ACTIONTEXT1 FUNC1) (ACTIONTEXT2 FUNC2) ...))  ... returns text hyperlinked to a popup menu
;; (schooz:explicit-menu* ((ACTIONTEXT1 FUNC1) (ACTIONTEXT2 FUNC2) ...))  ... returns a menu (rendered as a list)

;; The following functions do I/O in an implementation-dependent manner.
;; (schooz:ask X PROMPT)  ... outputs PROMPT; blocks until user responds; sets state of X directly.

;; The interface must also implement the logic of the main loop.
;; Roughly speaking, this is as follows:
;;  While not (game-over?):
;;  The current scene is given by (look).
;;   Optionally the interface can render available action texts (link,menu,choice) in a separate menu,
;;    but embedded hyperlinks are preferred.
;;  After an action function is triggered:
;;   The results of the action function are displayed, and the current scene (look) is refreshed.
