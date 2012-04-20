(define schooz:element-id-prefix "schoozLink")  ;; <a id="schoozLink1" href="#">...</a>
(define schooz:js-stash-function-name "schoozStash")  ;; schoozNotify(xml) { ... stash xml in page somewhere ... }
(define schooz:js-bind-function-name "schoozBind") ;; schoozBind(id,f) { document.getElementById(id) = f }
(define schooz:js-reveal-function-name "schoozReveal") ;; schoozReveal() { ... make xml visible ... }

(define (schooz:js-stash action-result)
  (js-call (js-eval schooz:js-stash-function-name) action-result))

(define (schooz:js-reveal)
  (js-call (js-eval schooz:js-reveal-function-name)))

(define (schooz:js-wrap-action action-func)
  (js-closure
   (lambda ()
     (schooz:js-stash (schooz:fold-strings (action-func)))
     (schooz:js-bind-funcs)
     (schooz:js-reveal))))

(define (schooz:element-id n)
  (string-append schooz:element-id-prefix (number->string n)))

(define (schooz:current-link-element-id)
  (schooz:js-element-id (+ (schooz:number-of-actions) 1)))

(define (schooz:current-menu-element-id menu-pos)
  (schooz:js-element-id (+ (schooz:number-of-actions) menu-pos 1)))

(define (schooz:js-bind-funcs)
  (schooz:js-bind-func 1))
(define (schooz:js-bind-func n)
  (if (<= n (schooz:number-of-actions))
      (begin
	(js-call
	 (js-eval schooz:js-bind-function-name)
	 (schooz:element-id n)
	 (schooz:js-wrap-action (schooz:get-action-func n)))
	(schooz:js-bind-func (+ n 1)))))

;; Interface implementation of hyperlinks & menus
(define (schooz:impl-link* link-text action-text action-func)
  `(a (@ ("id" ,(schooz:current-link-element-id)) ("href" "#")) ,action-text))

(define (schooz:impl-menu* link-text action-list)
;; write me
)

(define (schooz:impl-explicit-menu* action-list)
;; write me
)

;; Initial action
(define (schooz:js-call-initial-action)
  (js-call (schooz:js-wrap-action (schooz:initial-action))))
