;; Minimal Scheme-based CYOA framework.
;; R6RS compliant

(define schooz:version 1)

;; Core data structures.
;; Object->state->descriptor hashtable
(define schooz:desc (make-eq-hashtable))
;; Object->stack hashtable
(define schooz:stack (make-eq-hashtable))

;; Internal functions.
;; Simple map
(define (schooz:map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (schooz:map f (cdr lst)))))

;; (schooz:ensure-object X)  ... ensures that X has valid entries in hashtables
(define
  (schooz:ensure-object X STATE)
  (if
   (not (hashtable-ref schooz:desc X #f))
   (hashtable-set! schooz:desc X (make-eq-hashtable)))
  (if
   (not (hashtable-ref schooz:stack X #f))
   (hashtable-set! schooz:stack X (list STATE))))

;; (schooz:eval-or-return f)  ... if f is a function, evaluate; otherwise, return
(define (schooz:eval-or-return f)
  (if (procedure? f) (f) f))

;; (schooz:as-function f)  ... if f is a function, return f; otherwise, return function returning f
(define (schooz:as-function f)
  (if (procedure? f) f (lambda () f)))

;; clear stack of object X
(define (schooz:clear-stack X)
  (hashtable-set! schooz:stack X '()))

;; convert an SXML S-expression to an XML string
;; Can't yet handle attributes...
(define (schooz:fold-sxml-outer str lst)
  (cond ((string? lst) (string-append str lst))
	((null? lst) str)
	((pair? lst)
	 (let ((tag (car lst))
	       (rest (cdr lst)))
	   (if (string? tag)
	       (string-append
		str "<" tag
		(if (null? rest)
		    "/>"
		    (let ((fold-rest (lambda (r) (string-append ">" (schooz:fold-sxml-inner "" r) "</" tag ">"))))
		      (if (and
			   (pair? rest)
			   (pair? (car rest))
			   (string? (caar rest))
			   (string=? (caar rest) "@"))  ;; SXML attribute list?
			  (string-append (schooz:fold-sxml-attrs "" (cdar rest)) (fold-rest (cdr rest)))
			  (fold-rest rest)))))
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

(define (schooz:fold-sxml-attrs str attr-list)
  (if (null? attr-list) str
      (let* ((attr (car attr-list))
	     (name (car attr))
	     (value (cadr attr))
	     (rest (cdr attr-list)))
	(schooz:fold-sxml-attrs
	 (string-append str " " name "=\"" value "\"")
	 rest))))

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
  (let* ((transformed-action-func (schooz:transform-action action-func))
	 (return-value (schooz:impl-link* link-text action-text transformed-action-func)))
    (schooz:register-action action-text transformed-action-func)
    return-value))

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
  (let* ((transformed-action-list (schooz:transform-action-list action-list))
	 (return-value (schooz:impl-menu* link-text transformed-action-list)))
    (schooz:register-action-list transformed-action-list)
    return-value))

;; (schooz:menu TEXT ACTION-LIST)
(define-macro
  (schooz:menu TEXT ACTION-LIST)
  `(schooz:menu* ,TEXT (list ,@ACTION-LIST)))

;; (schooz:explicit-menu* ACTION-LIST)
(define (schooz:explicit-menu* action-list)
  (let* ((transformed-action-list (schooz:transform-action-list action-list))
	 (return-value (schooz:impl-explicit-menu* transformed-action-list)))
    (schooz:register-action-list transformed-action-list)
    return-value))

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


;; We keep track of (link,menu,explicit-menu) actions during calls to descriptors.
(define schooz:action-text-list '())
(define schooz:action-func-list '())

(define (schooz:get-action-text n)
  (list-ref schooz:action-text-list (- (schooz:number-of-actions) n)))

(define (schooz:get-action-func n)
  (list-ref schooz:action-func-list (- (schooz:number-of-actions) n)))

(define (schooz:reset-action-list)
  (set! schooz:action-text-list '())
  (set! schooz:action-func-list '()))

(define (schooz:register-action action-text action-func)
  (set! schooz:action-text-list (cons action-text schooz:action-text-list))
  (set! schooz:action-func-list (cons action-func schooz:action-func-list)))

(define (schooz:register-action-list action-list)
  (if (not (null? action-list))
      (let* ((text-func (car action-list))
	     (text (car text-func))
	     (func (cadr text-func))
	     (rest (cdr action-list)))
      (begin (schooz:register-action text func)
	     (schooz:register-action-list rest)))))

(define (schooz:number-of-actions)
  (length schooz:action-func-list))

(define (schooz:no-actions?)
  (= (schooz:number-of-actions) 0))

;; UI implementation should call (schooz:initial-action) to fire the initial action
(define (schooz:dummy-action) '())
(define schooz:initial-action-untransformed schooz:dummy-action)  ;; untransformed
(define (schooz:initial-action) (schooz:transform-action schooz:initial-action-untransformed))  ;; transformed

;; Action transformations, applied automatically by (link...), (menu...), (explicit-menu...), (fire-action...)
;; Before any part of the action, we reset action-text-list and action-func-list,
;; so we can track links to the next action.
(define (schooz:transform-action action-func)
  (lambda ()
    (schooz:reset-action-list)
    ((schooz:action-transformation action-func))))

;; Default action transformation just ensures that all actions are functions
(define schooz:action-transformation schooz:as-function)

;; method to compose a new action transformation
(define (schooz:compose-action-transformation new-transform)
  (let ((old-transform schooz:action-transformation))
    (set! schooz:action-transformation (lambda (f) (new-transform (old-transform f))))))

;; action transformation: do something after every action
(define (schooz:after-every-action g)
  (schooz:compose-action-transformation
   (lambda (f) (lambda () (let* ((fres (f)) (gres (g))) (schooz:append-as-lists fres gres))))))

;; action transformation: do something before every action
(define (schooz:before-every-action g)
  (schooz:compose-action-transformation
   (lambda (f) (lambda () (let* ((gres (g)) (fres (f))) (schooz:append-as-lists gres fres))))))

;; look after every action
(define (schooz:look-after-every-action)
  (schooz:after-every-action schooz:look))

;; newline after every action
(define (schooz:newline-after-every-action)
  (schooz:after-every-action (lambda () "\n")))

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
