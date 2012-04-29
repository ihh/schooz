;; Constants
(define schooz:onclick-element-id-prefix "schoozLink")
(define schooz:popup-element-id-prefix "schoozPopup")
(define schooz:view-element-id "schoozText")
(define schooz:js-notify-function "schoozUpdate")

(define schooz:paragraph-css-class "paragraph")
(define schooz:popup-css-class "popup")
(define schooz:js-popup-function "makePopup")
(define schooz:choice-list-css-class "choice-list")

(define schooz:js-hide-popups-function "hideAllPopups")
(define schooz:js-delete-popups-function "deleteAllPopups")

;; Constants visible to player
(define schooz:cancel-text "Cancel")  ;; text to cancel a menu choice

;; HTML
(define p (lambda args `("p" ("@" ("class" ,schooz:paragraph-css-class)) ,@args)))

;; Functions
(define schooz:onclick-binding (make-eq-hashtable))

(define (schooz:reset-onclick-bindings)
  (hashtable-clear! schooz:onclick-binding))

(define (schooz:bound-id func hint)
  (let ((name (schooz:next-onclick-id))
	(wrapped-func (schooz:js-wrap-action func hint)))
    (hashtable-set! schooz:onclick-binding name wrapped-func)
    name))

(define schooz:popup-count 0)
(define (schooz:next-popup-id)
  (set! schooz:popup-count (+ schooz:popup-count 1))
  (schooz:popup-element-id schooz:popup-count))

(define (schooz:popup-element-id n)
  (string-append schooz:popup-element-id-prefix (number->string n)))

(define schooz:onclick-count 0)
(define (schooz:next-onclick-id)
  (set! schooz:onclick-count (+ schooz:onclick-count 1))
  (schooz:onclick-element-id schooz:onclick-count))

(define (schooz:onclick-element-id n)
  (string-append schooz:onclick-element-id-prefix (number->string n)))

(define (schooz:js-wrap-action action-func label)
  (js-closure
   (lambda ()
     (display (string-append "Action: " label "\n"))
     (schooz:js-delete-popups)
     (schooz:reset-onclick-bindings)
     (let ((action-result (action-func)))
       (schooz:js-set-view action-result)
       (schooz:js-bind-funcs)
       (schooz:js-notify)))))

(define (schooz:js-notify)
  (js-call (js-eval schooz:js-notify-function)))

(define (schooz:js-delete-popups)
  (js-call (js-eval schooz:js-delete-popups-function)))

(define (schooz:js-document-getElementById id)
  (string-append "document.getElementById('" id "')"))

(define (schooz:js-set-element-property id property value)
  (let ((js-expr (schooz:js-document-getElementById id)))
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
  (if (pair? ids)
      (let ((id (car ids))
	    (rest (cdr ids)))
	(schooz:js-set-onclick
	 id
	 (hashtable-ref schooz:onclick-binding id #f))
	(schooz:js-bind-func-ids rest))))

;; Interface implementation of hyperlinks & menus
(define (schooz:anchor link-text mouseover-text func)
  (let ((id (schooz:bound-id func mouseover-text)))
    `("a" ("@"
	   ("id" ,id)
	   ("href" "#")
	   ("title" ,mouseover-text))
      ,link-text)))

(define (schooz:button button-text func)
  (let ((id (schooz:bound-id func button-text)))
    `("button" ("@"
		("type" "button")
		("id" ,id))
      ,button-text)))

(define (schooz:hide-button button-text container-id)
  `("button" ("@"
	      ("type" "button")
	      ("onclick" ,(string-append schooz:js-hide-popups-function "()")))
    ,button-text))

(define (schooz:popup link-text popup-id popup-content)
  (let ((elt (js-eval "document.createElement('div');")))
    (js-invoke elt "setAttribute" "class" schooz:popup-css-class)
    (js-set! elt "id" popup-id)
    (js-set! elt "innerHTML" (schooz:fold-strings (list popup-content)))
    (js-invoke (js-eval "document.body") "appendChild" elt)
    `(("a" ("@" ("href" "#") ("onclick" ,(string-append schooz:js-popup-function "('" popup-id "',this);")))
       ,link-text))))

(define (schooz:choice-list container-id action-list)
  `("ul" ("@" ("class" ,schooz:choice-list-css-class))
    ,(schooz:map
      (lambda (action)
	(let ((action-text (car action))
	      (action-func (cadr action)))
	  `("li" ,(schooz:button action-text action-func))))
      action-list)
    ("li" ,(schooz:hide-button schooz:cancel-text container-id))))

(define (schooz:impl-link* link-text action-text action-func)
;  (schooz:anchor link-text action-text action-func))
  (let ((popup-id (schooz:next-popup-id)))
    (schooz:popup link-text popup-id (schooz:choice-list popup-id (list (list action-text action-func))))))

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
  (js-call (schooz:js-wrap-action (schooz:initial-action) "Let's get this started")))
