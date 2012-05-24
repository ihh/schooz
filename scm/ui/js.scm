;; Constants
(define schooz:onclick-element-id-prefix "schoozLink")
(define schooz:popup-element-id-prefix "schoozPopup")
(define schooz:popup-parent-element-id-suffix "Link")
(define schooz:view-element-id "schoozText")
(define schooz:js-notify-function "schoozUpdate")

(define schooz:paragraph-css-class "paragraph")
(define schooz:popup-css-class "popup")
(define schooz:popup-link-css-class "popup-link")
(define schooz:button-css-class "choice-button")
(define schooz:choice-list-css-class "choice-list")

(define schooz:js-popup-function "makePopup")
(define schooz:js-hide-popups-function "hideAllPopups")
(define schooz:js-attach-popups-function "attachPopups")

;; String constants shown to player
(define schooz:cancel-text "Cancel")  ;; text to cancel a popup choice
(define schooz:popup-mouseover-hint "Click for options")  ;; mouseover text for popup popup links
(define schooz:button-mouseover-hint "Click to select")  ;; mouseover text for choice buttons
(define schooz:cancel-mouseover-hint "Click to hide")  ;; mouseover text for Cancel buttons

;; HTML
(define p (lambda args `("p" ("@" ("class" ,schooz:paragraph-css-class)) ,@args)))
(define span (lambda args `("span" ,@args)))
(define h1 (lambda args `("h1" ,@args)))
(define h2 (lambda args `("h2" ,@args)))
(define h3 (lambda args `("h3" ,@args)))
(define i (lambda args `("i" ,@args)))
(define b (lambda args `("b" ,@args)))

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
;     (display (string-append "Action: " label "\n"))
     (schooz:reset-onclick-bindings)
     (let ((action-result (action-func)))
       (schooz:js-set-view action-result)
       (schooz:js-bind-funcs)
       (schooz:js-notify)
       (schooz:js-attach-popups)))))

(define (schooz:js-notify)
  (js-call (js-eval schooz:js-notify-function)))

(define (schooz:js-attach-popups)
  (js-call (js-eval schooz:js-attach-popups-function)))

(define (schooz:js-element-by-id id)
  (string-append "document.getElementById('" id "')"))

(define (schooz:js-set-element-property id property value)
  (let ((js-expr (schooz:js-element-by-id id)))
    (js-set!
     (js-eval js-expr)
     property
     value)))

(define (schooz:js-set-element-content id data)
  (schooz:js-set-element-property id "innerHTML" (schooz:fold-strings data)))

(define (schooz:js-set-view data)
  (schooz:js-set-element-content schooz:view-element-id data))

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

;; Interface implementation of hyperlinks & popups
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
		("class" ,schooz:button-css-class)
		("title" ,schooz:button-mouseover-hint)
		("id" ,id))
      ,button-text)))

(define (schooz:hide-button button-text container-id)
  `("button" ("@"
	      ("type" "button")
	      ("class" ,schooz:button-css-class)
	      ("title" ,schooz:cancel-mouseover-hint)
	      ("onclick" ,(string-append schooz:js-hide-popups-function "()")))
    ,button-text))

(define (schooz:js-popup link-text popup-id popup-content)
  (let ((elt (js-eval "document.createElement('div');")))
    (js-invoke elt "setAttribute" "class" schooz:popup-css-class)
    (js-set! elt "id" popup-id)
    (js-set! elt "innerHTML" (schooz:fold-strings (list popup-content)))
    (js-invoke (js-eval "document.body") "appendChild" elt)
    `(("span" ("@"
	       ("id" ,(string-append popup-id schooz:popup-parent-element-id-suffix))
	       ("class" ,schooz:popup-link-css-class)
	       ("onclick" ,(string-append schooz:js-popup-function "('" popup-id "',1);"))
	       ("onmouseover" ,(string-append schooz:js-popup-function "('" popup-id "',0);")))
       ,link-text))))

(define (schooz:choice-list-wrapper choice-list)
  `("ul" ("@" ("class" ,schooz:choice-list-css-class))
    ,(schooz:map
      (lambda (choice)
	`("li" ,choice))
      choice-list)))

(define (schooz:choice-list action-list)
  (schooz:choice-list-wrapper (schooz:action-choices action-list)))

(define (schooz:choice-list-with-cancel container-id action-list)
  (schooz:choice-list-wrapper (append (schooz:action-choices action-list) (list (schooz:cancel-choice)))))

(define (schooz:action-choices action-list)
  (schooz:map
   (lambda (action)
     (let ((action-text (car action))
	   (action-func (cadr action)))
       (schooz:button action-text action-func)))
   action-list))

(define (schooz:cancel-choice container-id)
  (schooz:hide-button schooz:cancel-text container-id))

(define (schooz:impl-link* link-text action-text action-func)
;  (schooz:anchor link-text action-text action-func))
  (let ((popup-id (schooz:next-popup-id)))
    (schooz:js-popup link-text popup-id (schooz:choice-list `((,action-text ,action-func))))))

(define (schooz:impl-popup* link-text action-list)
  (let ((popup-id (schooz:next-popup-id)))
    (schooz:js-popup link-text popup-id (schooz:choice-list action-list))))

(define (schooz:impl-menu* action-list)
  (p (schooz:choice-list action-list)))

;; Initial action
(define (schooz:js-call-initial-action)
  (js-call (schooz:js-wrap-action (schooz:initial-action) "Let's get this started")))
