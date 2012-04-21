(define schooz:onclick-element-id-prefix "schoozLink")
(define schooz:view-element-id "schoozView")
(define schooz:js-notify-function "schoozNotify")

(define schooz:onclick-binding (make-eq-hashtable))

(define (schooz:reset-onclick-bindings)
  (hashtable-clear! schooz:onclick-binding))

(define (schooz:bound-id func)
  (let* ((n (+ (hashtable-size schooz:onclick-binding) 1))
	 (name (schooz:onclick-element-id n)))
    (hashtable-set! schooz:onclick-binding name func)
    name))

(define (schooz:js-wrap-action action-func)
  (js-closure
   (lambda ()
     (display "In wrapped action\n")
     (schooz:reset-onclick-bindings)
     (let ((action-result (action-func)))
       (display "action-result: ") (write action-result) (display "\n")
       (schooz:js-set-view action-result)
       (schooz:js-bind-funcs)
       (schooz:js-notify)))))

(define (schooz:js-notify)
  (js-call (js-eval schooz:js-notify-function)))

(define (schooz:js-set-element-property id property value)
  (let ((js-expr (string-append "document.getElementById(\"" id "\")")))
    (display (string-append js-expr "." property "=\"" (if (string? value) value "<value>") "\";\n"))
    (js-set!
     (js-eval js-expr)
     property
     value)))

(define (schooz:js-set-view data)
  (schooz:js-set-element-property schooz:view-element-id "innerHTML" (schooz:fold-strings data)))

(define (schooz:js-set-onclick id func)
  (schooz:js-set-element-property id "onclick" func))

(define (schooz:onclick-element-id n)
  (string-append schooz:onclick-element-id-prefix (number->string n)))

(define (schooz:js-bind-funcs)
  (let ((ids (vector->list (hashtable-keys schooz:onclick-binding))))
    (schooz:js-bind-func-ids ids)))

(define (schooz:js-bind-func-ids ids)
;  (write ids)
;  (display "(list? ids)=") (display (list? ids)) (display "\n")
  (if (pair? ids)
      (let ((id (car ids))
	    (rest (cdr ids)))
	(display (string-append "Binding " id "\n"))
	(schooz:js-set-onclick id (hashtable-ref schooz:onclick-binding id #f))
	(schooz:js-bind-func-ids rest))))

;; Interface implementation of hyperlinks & menus
(define (schooz:anchor link-text mouseover-text func)
  `("a" ("@"
	 ("id" ,(schooz:bound-id func))
	 ("href" "#")
	 ("title" ,mouseover-text))
    ,link-text))

(define (schooz:impl-link* link-text action-text action-func)
  (schooz:anchor link-text action-text action-func))

(define (schooz:impl-menu* link-text action-list)
  `("div"
    ,link-text
    ,(schooz:impl-explicit-menu* action-list)))

(define (schooz:impl-explicit-menu* action-list)
  `("ul" ,(schooz:map
	   (lambda (n)
	     (let* ((action (list-ref action-list n))
		    (text (car action))
		    (func (cadr action)))
	       `("li" ,(schooz:anchor text text func))))
	   (iota (length action-list)))))

;; Initial action
(define (schooz:js-call-initial-action)
  (js-call (schooz:js-wrap-action (schooz:initial-action))))
