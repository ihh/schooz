
;; (link* TEXT ACTION-TEXT FUNC)
(define
  (link* LINK ACTION FUNC)
  (schooz:link* LINK ACTION FUNC))

;; (link TEXT ACTION-TEXT FUNC-BODY)
(define-macro
  (link LINK ACTION FUNC-BODY)
  `(schooz:link ,LINK ,ACTION ,FUNC-BODY))

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
