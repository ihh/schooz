(define schooz:onclick-element-id-prefix "schoozLink")
(define schooz:view-element-id "schoozView")
(define schooz:js-notify-function "schoozNotify")

(define (schooz:js-wrap-action action-func)
  (js-closure
   (lambda ()
     (let ((action-result (action-func)))
       (schooz:js-set-view action-result)
       (schooz:js-bind-funcs)
       (schooz:js-notify)))))

(define (schooz:js-notify)
  (js-call (js-eval schooz:js-notify-function)))

(define (schooz:js-set-element-property id property value)
  (js-set!
   (js-call (js-eval (string-append "document.getElementById(\"" id "\")")))
   property
   value))

(define (schooz:js-set-view data)
  (schooz:js-set-element-property schooz:view-element-id "innerHTML" (schooz:fold-strings data)))

(define (schooz:js-set-onclick n func)
  (schooz:js-set-element-property (schooz:onclick-element-id n) "onclick" func))

(define (schooz:onclick-element-id n)
  (string-append schooz:onclick-element-id-prefix (number->string n)))

(define (schooz:current-link-element-id)
  (schooz:onclick-element-id (+ (schooz:number-of-actions) 1)))

(define (schooz:current-menu-element-id menu-pos)
  (schooz:onclick-element-id (+ (schooz:number-of-actions) menu-pos 1)))

(define (schooz:js-bind-funcs)
  (schooz:js-bind-func 1))

(define (schooz:js-bind-func n)
  (if (<= n (schooz:number-of-actions))
      (begin
	(schooz:js-set-onclick n (schooz:js-wrap-action (schooz:get-action-func n)))
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
