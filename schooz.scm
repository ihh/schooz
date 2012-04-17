;; Minimal Scheme-based CYOA framework.
;; R6RS compliant

(define schooz:version 1)

;; Main objects.
;; Object->state->descriptor hashtable
(define schooz:desc (make-eq-hashtable))
;; Object->stack hashtable
(define schooz:stack (make-eq-hashtable))

;; Internal functions.
;; (schooz:ensure-object X)  ... ensures that X has valid entries in hashtables
(define
  (schooz:ensure-object X)
  (if
   (not (hash-ref schooz:desc X))
   (hash-set! schooz:desc X (make-eq-hashtable)))
  (if
   (not (hash-ref schooz:stack X))
   (hash-set! schooz:stack X '("start"))))

;; (schooz:eval-or-return f)  ... if f is a function, evaluate; otherwise, return
(define (schooz:eval-or-return f)
  (if (procedure? f) (f) f))

;; clear stack of object X
(define (schooz:clear-stack X)
  (hash-set! schooz:stack X '()))

;; convert an SXML S-expression to an XML string
;; Can't yet handle attributes...
(define (schooz:fold-sxml-outer str lst)
  (cond ((null? lst) str)
	((not (pair? lst)) (string-append str lst))
	(else
	 (let ((tag (car lst))
	       (rest (cdr lst)))
	   (if (null? rest)
	       (string-append str "<" tag "/>")
	       (string-append str "<" tag ">" (schooz:fold-sxml-inner "" rest) "</" tag ">"))))))

(define (schooz:fold-sxml-inner str lst)
  (cond ((null? lst) str)
	((not (pair? lst)) (string-append str lst))
	(else
	 (let ((elem (car lst))
	       (rest (cdr lst)))
	   (schooz:fold-sxml-inner (schooz:fold-sxml-outer str elem) rest)))))

;; default output adapter: SXML
(define (schooz:fold-strings lst) (schooz:fold-sxml-inner "" lst))

;; API functions.
;; (now X STATE)  ... places object X in state STATE
(define
  (now X STATE)
  (let ((old-stack (hash-ref schooz:stack X)))
    (hash-set! schooz:stack X
	       (if (null? old-stack)
		   (list STATE)
		   (cons STATE (cdr old-stack))))))

;; (state X)  ... returns the current state (typically a string) of object named X, where X is an atom
(define
  (state X)
  (let ((stack (hash-ref schooz:stack X)))
    (if (null? stack) #f (car stack))))

;; (description X STATE FUNC)  ... set object X's descriptor function for state STATE to FUNC
(define
  (description X STATE FUNC)
  (schooz:ensure-object X)
  (hash-set! (hash-ref schooz:desc X) STATE FUNC))

;; (describe X)  ... looks up the descriptor function for current state of object X, calls it
(define
  (describe X)
  (let ((desc (hash-ref (hash-ref schooz:desc X) (state X))))
;; Uncomment to debug
;;    (display "describe ") (display X) (display ": ") (display desc) (display "\n")
    (schooz:eval-or-return desc)))

;; (push X STATE)  ... pushes STATE onto X's stack
(define
  (push X STATE)
  (hash-set! schooz:stack X (cons STATE (hash-ref schooz:stack X))))

;; (pop X)  ... pops state off X's stack
(define
  (pop X)
  (let ((stack (hash-ref schooz:stack X)))
    (if (null? stack)
	#f
	(begin
	  (hash-set! schooz:stack X (cdr stack))
	  (car stack)))))

;; Main story object
(define narrative "narrative")
;; Syntactic-sugar shortcuts for working with the main story graph
;; (story STATE FUNC)
(define (story STATE FUNC) (description narrative STATE FUNC))
;; (look)
(define (look) (describe narrative))
;; (goto STATE)
(define (goto STATE) (now narrative STATE))
;; (gosub STATE)
(define (gosub STATE) (push narrative STATE))
;; (return)
(define (return) (pop narrative))
;; (chapter)
(define (chapter) (state narrative))
;; (game-over)
(define (game-over?) (equal? (chapter) #f))
;; (quit)
(define (quit) (schooz:clear-stack narrative))

;; (link-goto LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define (link-goto LINK STATE ACTION RESULT)
  (link LINK ACTION (lambda () (begin (goto STATE) RESULT))))

;; (link-gosub LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define (link-gosub LINK STATE ACTION RESULT)
  (link LINK ACTION (lambda () (begin (gosub STATE) RESULT))))

;; (link-return LINK-TEXT ACTION-TEXT RESULT-TEXT)
(define (link-return LINK ACTION RESULT)
  (link LINK ACTION (lambda () (begin (return) RESULT))))

;; (choice-goto STATE ACTION-TEXT RESULT-TEXT)
(define (choice-goto STATE ACTION RESULT)
  (choice ACTION (lambda () (begin (goto STATE) RESULT))))

;; (choice-gosub STATE ACTION-TEXT RESULT-TEXT)
(define (choice-gosub STATE ACTION RESULT)
  (choice ACTION (lambda () (begin (gosub STATE) RESULT))))

;; (choice-return ACTION-TEXT RESULT-TEXT)
(define (choice-return ACTION RESULT)
  (choice ACTION (lambda () (begin (return) RESULT))))

;; (choice ACTIONTEXT FUNC)  ... simple helper/wrapper
(define (choice ACTIONTEXT FUNC) (list ACTIONTEXT FUNC))

;; Interface implementation-dependent methods.
;; The following functions must return a string, list, or nested list
;; (link TEXT ACTIONTEXT FUNC)  ... returns a hyperlink with text TEXT that calls FUNC, with mouseover text ACTIONTEXT
;; (menu TEXT ((ACTIONTEXT1 FUNC1) (ACTIONTEXT2 FUNC2) ...))  ... returns text hyperlinked to a popup menu
;; (explicit-menu ((ACTIONTEXT1 FUNC1) (ACTIONTEXT2 FUNC2) ...))  ... returns a menu (rendered as a list)

;; The following functions do I/O in an implementation-dependent manner.
;; (ask X PROMPT)  ... outputs PROMPT; blocks until user responds; sets state of X directly.

;; The interface must also implement the logic of the main loop.
;; Roughly speaking, this is as follows:
;;  While not (game-over?):
;;  The current scene is given by (look).
;;   Optionally the interface can render available action texts (link,menu,choice) in a separate menu,
;;    but embedded hyperlinks are preferred.
;;  After an action function is triggered:
;;   The results of the action function are displayed, and the current scene (look) is refreshed.
