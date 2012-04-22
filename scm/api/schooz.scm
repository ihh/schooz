
;; (link* TEXT ACTION-TEXT FUNC)
(define
  (link* LINK ACTION FUNC)
  (schooz:link* LINK ACTION FUNC))

;; (link TEXT ACTION-TEXT FUNC-BODY)
(define-macro
  (link LINK ACTION FUNC-BODY)
  `(schooz:link ,LINK ,ACTION ,FUNC-BODY))

;; (link-goto LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define
  (link-goto LINK STATE ACTION RESULT)
  (schooz:link-goto LINK STATE ACTION RESULT))

;; (link-gosub LINK-TEXT STATE ACTION-TEXT RESULT-TEXT)
(define
  (link-gosub LINK STATE ACTION RESULT)
  (schooz:link-gosub LINK STATE ACTION RESULT))

;; (link-return LINK-TEXT ACTION-TEXT RESULT-TEXT)
(define
  (link-return LINK ACTION RESULT)
  (schooz:link-return LINK ACTION RESULT))

;; (schooz:choice-goto STATE ACTION-TEXT RESULT-TEXT)
(define
  (choice-goto STATE ACTION RESULT)
  (schooz:choice-goto STATE ACTION RESULT))

;; (schooz:choice-gosub STATE ACTION-TEXT RESULT-TEXT)
(define
  (choice-gosub STATE ACTION RESULT)
  (schooz:choice-gosub STATE ACTION RESULT))

;; (schooz:choice-return ACTION-TEXT RESULT-TEXT)
(define
  (choice-return ACTION RESULT)
  (schooz:choice-return ACTION RESULT))

;; (schooz:choice* ACTIONTEXT FUNC)  ... simple helper/wrapper
(define
  (choice* ACTIONTEXT FUNC)
  (schooz:choice* ACTIONTEXT FUNC))

;; (schooz:choice ACTIONTEXT FUNC-BODY)
(define-macro
  (choice ACTIONTEXT FUNC-BODY)
  `(schooz:choice ,ACTIONTEXT ,FUNC-BODY))

;; (menu* TEXT ACTION-LIST)
(define
  (menu* TEXT ACTION-LIST)
  (schooz:menu* TEXT ACTION-LIST))

;; (menu TEXT ACTION-LIST)
(define-macro
  (menu TEXT ACTION-LIST)
  `(schooz:menu ,TEXT ,ACTION-LIST))

;; (explicit-menu* TEXT ACTION-LIST)
(define
  (explicit-menu* TEXT ACTION-LIST)
  (schooz:explicit-menu* TEXT ACTION-LIST))

;; (explicit-menu ACTION-LIST)
(define-macro
  (explicit-menu ACTION-LIST)
  `(schooz:explicit-menu ,ACTION-LIST))

;; (ask X PROMPT)
(define (ask X PROMPT)
  (schooz:ask X PROMPT))


;; (main-loop)  ... terminal only
(define (main-loop) (schooz:main-loop))
