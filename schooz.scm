;; Minimal Scheme-based CYOA framework.
;; R6RS compliant

(define schooz-version 1)

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

;; (eval-or-return f)  ... if f is a function, evaluate; otherwise, return
(define (eval-or-return f)
  (if (procedure? f) (f) f))

;; concatenate a (nested) list of strings
(define (flatten-strings str lst)
  (cond ((null? lst) str)
	((not (pair? lst)) (string-append str lst))
	(else (flatten-strings
	       (flatten-strings str (car lst))
	       (cdr lst)))))

;; default list->string conversion
(define (schooz-flatten-strings lst) (flatten-strings "" lst))

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
  (hash-set! (hash-ref schooz-desc X) STATE FUNC))

;; (tell X)  ... looks up the descriptor function for current state of object X, calls it
(define
  (tell X)
  (let ((desc (hash-ref (hash-ref schooz-desc X) (state X))))
;; Uncomment to debug
;;    (display "tell ") (display X) (display ": ") (display desc) (display "\n")
    (eval-or-return desc)))

;; (push X STATE)  ... pushes the current state of X onto X's stack, places X into state STATE
(define
  (push X STATE)
  (hash-set! (schooz-stack X) (cons (state X) (schooz-stack X)))
  (now X STATE))

;; (pop X)  ... pops state off X's state stack, places X into popped state
(define
  (pop X)
  (now X (car (schooz-stack X)))
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
;; (game-over)
(define (game-over?) (equal? (chapter) "end"))
;; (quit)
(define (quit) (goto "end"))

;; (link-goto STATE LINK-TEXT ACTION-TEXT RESULT-TEXT)
(define (link-goto STATE LINK ACTION RESULT)
  (link LINK ACTION (lambda () (begin (goto STATE) RESULT))))

;; (link-gosub STATE LINK-TEXT ACTION-TEXT RESULT-TEXT)
(define (link-gosub STATE LINK ACTION RESULT)
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
;; Use schooz-flatten-strings to convert look & action results to simple strings,
;;  or define your own handler (e.g. SXML).
