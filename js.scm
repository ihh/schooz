(define schooz:js-notify-function-name "schoozNotify")  ;; schoozNotify(xml) { ... stash xml in page somewhere ... }
(define schooz:js-bind-function-name "schoozBind") ;; schoozBind(id,f) { document.getElementById(id) = f }

(define (schooz:js-notify action-result)
  (js-call (js-eval schooz:js-notify-function-name) action-result))

(define (schooz:js-wrap-action action-func)
  (js-closure
   (lambda ()
     (schooz:js-notify (action-func))
     (schooz:js-bind-functions))))

(define schooz:dom-element-id-prefix "schoozLink")

(define (schooz:dom-element-id n)
  (string-append schooz:dom-element-id-prefix (number->string n)))

(define (schooz:current-dom-element-id)
  (schooz:js-dom-element-id (schooz:number-of-actions)))

(define (schooz:js-bind-functions)
  (schooz:js-bind-function 1))
(define (schooz:js-bind-function n)
  (if (<= n (schooz:number-of-actions))
      (begin
	(js-call
	 (js-eval schooz:js-bind-function-name)
	 (schooz:dom-element-id n)
	 (schooz:js-wrap-action (schooz:get-action-func n)))
	(schooz:js-bind-function (+ n 1)))))

;; Interface methods
(define (schooz:impl-link* link-text action-text action-func)
  `(a (@ ("id" ,(schooz:current-dom-element-id))) ,action-text))

(define (schooz:impl-menu* link-text action-list)
;; write me
)

(define (schooz:impl-explicit-menu* action-list)
;; write me
)