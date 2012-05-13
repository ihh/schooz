;; Minimal Scheme-based CYOA framework.
;; R6RS compliant

(define schooz:version 1)

;; The first part of this file just defines shortcuts for the most commonly-used functions,
;; dropping the "schooz:" prefix; e.g. (link...) instead of (schooz:link...)

;; (link* text action-text func)
(define
  (link* link-text action func)
  (schooz:link* link-text action func))

;; (link text action-text func-body)
(define-macro
  (link link-text action func-body)
  `(schooz:link ,link-text ,action ,func-body))

;; (choice* action-text func)  ... simple helper/wrapper
(define
  (choice* action-text func)
  (schooz:choice* action-text func))

;; (choice action-text func-body)
(define-macro
  (choice action-text func-body)
  `(schooz:choice ,action-text ,func-body))

;; (popup link-text (text1 action1) (text2 action2) ...)
(define popup (lambda args (apply schooz:popup args)))

;; (menu (text1 action1) (text2 action2) ...)
(define menu (lambda args (apply schooz:menu args)))


;; (ask X PROMPT)
;; currently unimplemented by JavaScript client
(define (ask X PROMPT)
  (schooz:ask X PROMPT))


;; (main-loop)  ... terminal only
(define (main-loop) (schooz:main-loop))


;; Now come the actual function definitions.

;; Internal functions.
;; Simple fold
(define (schooz:fold-right binary-func init lst)
  (cond
   ((null? lst) init)
   ((pair? lst) (let ((fcar (binary-func init (car lst))))  ;; force this first
		  (schooz:fold-right binary-func fcar (cdr lst))))
   (else init)))

;; Simple map
(define (schooz:map unary-func lst)
  (schooz:fold-right (lambda (x y) (append x (list (unary-func y)))) '() lst))

;; Spliced map
(define (schooz:map-spliced unary-func lst)
  (schooz:fold-right (lambda (x y) (append x (unary-func y))) '() lst))

;; Simple grep
(define (schooz:grep predicate lst)
  (let* ((map-function (lambda (x) (if (predicate x) (list x) '()))))
    (schooz:map-spliced map-function lst)))

;; (schooz:eval-or-return f)  ... if f is a function, evaluate; otherwise, return
(define (schooz:eval-or-return f)
  (if (procedure? f) (f) f))

;; (schooz:as-function f)  ... if f is a function, return f; otherwise, return function returning f
(define (schooz:as-function f)
  (if (procedure? f) f (lambda () f)))

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



;; (schooz:link* text action-text func-body)
(define (schooz:link* link-text action-text action-func)
  (let* ((transformed-action-func (schooz:transform-action action-func))
	 (return-value (schooz:impl-link* link-text action-text transformed-action-func)))
    (schooz:register-action action-text transformed-action-func)
    return-value))

;; (schooz:link text action-text func-body)
(define-macro (schooz:link link-text action func-body)
  `(schooz:link* ,link-text ,action (lambda () ,func-body)))

;; (schooz:popup link-text (text1 action1) (text2 action2) ...)
(define schooz:popup
  (lambda args
    (let* ((link-text (car args))
	   (action-list (cdr args))
	   (transformed-action-list (schooz:transform-action-list action-list))
	   (return-value (schooz:impl-popup* link-text transformed-action-list)))
    (schooz:register-action-list transformed-action-list)
    return-value)))

;; (schooz:menu (text1 action1) (text2 action2) ...)
(define schooz:menu
  (lambda action-list
    (let* ((transformed-action-list (schooz:transform-action-list action-list))
	   (return-value (schooz:impl-menu* transformed-action-list)))
      (schooz:register-action-list transformed-action-list)
      return-value)))

;; (schooz:choice* action-text func)  ... simple helper/wrapper
(define (schooz:choice* action-text func) (list action-text func))

;; (schooz:choice action-text func-body)
(define-macro
  (schooz:choice action-text func-body)
  `(schooz:choice* ,action-text (lambda () ,func-body)))

;; (schooz:ask X prompt)
(define (schooz:ask X prompt)
  (display prompt)
  (schooz:now X (read)))


;; We keep track of (link,popup,menu) actions during calls to descriptors.
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
(define (schooz:set-initial-action a) (set! schooz:initial-action-untransformed a))
(define (schooz:initial-action) (schooz:transform-action schooz:initial-action-untransformed))  ;; transformed

;; Action transformations, applied automatically by (link...), (popup...), (menu...), (fire-action...)
;; Before any part of the action, we reset action-text-list and action-func-list,
;; so we can track links to the next action.
(define (schooz:transform-action action-func)
  (lambda ()
    (schooz:reset-action-list)
    ((schooz:action-transformation action-func))))

;; Default action transformation is the identity
(define schooz:action-transformation (lambda (f) f))

;; method to compose a new action transformation
(define (schooz:compose-action-transformation new-transform)
  (let ((old-transform schooz:action-transformation))
    (set! schooz:action-transformation (lambda (f) (new-transform (old-transform f))))))

(define (schooz:wrap-string-actions wrap-func)
  (schooz:compose-action-transformation
   (lambda (a)
     (if (procedure? a)
	 (lambda () (wrap-func (a)))
	 (schooz:as-function (wrap-func a))))))

(define (schooz:interpret-actions-as-functions)
  (schooz:compose-action-transformation schooz:as-function))

(define (schooz:after-every-terminal-action g)
  (schooz:compose-action-transformation
   (lambda (action)
     (lambda ()
       (let ((evaluated-action (action)))
	 (if (> (schooz:number-of-actions) 0)
	     evaluated-action
	     (append (schooz:as-list evaluated-action) (g))))))))

;; action transformation: do something after every action
(define (schooz:after-every-action g)
  (schooz:compose-action-transformation
   (lambda (f) (lambda () (let* ((fres (f)) (gres (g))) (schooz:append-as-lists fres gres))))))

;; action transformation: do something before every action
(define (schooz:before-every-action g)
  (schooz:compose-action-transformation
   (lambda (f) (lambda () (let* ((gres (g)) (fres (f))) (schooz:append-as-lists gres fres))))))

;; newline after every action
(define (schooz:newline-after-every-action)
  (schooz:after-every-action (lambda () "\n")))

;; new paragraph after every action
(define (schooz:p-element-after-every-action)
  (schooz:after-every-action (lambda () '(("p")))))

;; application of transform-action to list of the form ((action-text1 func1) (action-text2 func2)...)
(define (schooz:transform-action-list lst)
  (if
   (null? lst)
   lst
   (let* ((action (car lst))
	  (rest (cdr lst)))
     (append
      (if (null? action)
	  '()
	  (let ((action-text (car action))
		(action-func (cadr action)))
	    (list (list action-text (schooz:transform-action action-func)))))
      (schooz:transform-action-list rest)))))



;; Utilities
(define nl "\n")
(define (dnl) (display nl))
