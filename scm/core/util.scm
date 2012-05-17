

;; shortcuts
(define map schooz:map)
(define grep schooz:grep)

;; grep -v
(define (grep-not-equal x list)
  (grep (lambda (y) (not (equal? x y))) list))

;; Extract random element from list
(define random-element
  (lambda list
    (let ((len (length list)))
      (list-ref list (random-integer len)))))

;; Emily Short-style "One of..." alias for random-element
(define one-of random-element)

;; Helper functions (move these to core libraries?)
;; Expandable objects
(define (expandable-machine name link-text post-link-text action-text expanded-description)
  (description
   name
   #f
   `(,(link link-text action-text (begin (now name #t) (look))) ,post-link-text))

  (description
   name
   #t
   `(,(span link-text post-link-text expanded-description))))

;; Simple one-way switches
(define (one-way-switch name link-text action-text)
  (expandable-machine name link-text "" action-text ""))

;; Consistent "Next" links
(define
  (next-goto state)
  `(,(p (menu (choice-goto state "Next" "")))))

(define
  (next-action action)
  `(,(p (menu (choice* "Next" action)))))

(define
  (next-look)
  (next-action look))

(define
  (next-return)
  (next-action (lambda () (return) (look))))


;; Simple buttons
(define
  (simple-button* text action)
  `(,(p (menu (choice* text action)))))

(define
  (simple-button text action-body)
  (menu (choice text action-body)))

;; Self-propelled state machines (generic)
(define (auto-machine name next-state-function descriptor-list)
  (let ((states (length descriptor-list)))
    (map
     (lambda (s)
       (let ((descriptor (list-ref descriptor-list s)))
	 (description*
	  name
	  s
	  (lambda ()
	    (let ((next-state (next-state-function s)))
	      (now name next-state)
	      ((schooz:as-function descriptor)))))))
     (iota states))))

;; Cyclic increment function
(define (inc-modulo modulus)
  (lambda (x) (% (+ x 1) modulus)))

;; Cycler
(define (cyclic-machine name descriptor-list)
  (auto-machine name (inc-modulo (length descriptor-list)) descriptor-list))

;; Random cycler (never repeats)
(define (random-machine name descriptor-list)
  (let* ((states (length descriptor-list))
	 (mutator-function
	  (lambda (x)
	    (let ((other-states (grep-not-equal x (iota states))))
	      (apply random-element other-states)))))
    (auto-machine name mutator-function descriptor-list)))

;; Timer (delay fuse)
(define (fuse-machine name descriptor-list)
  (let* ((states (length descriptor-list))
	 (inc-function (lambda (x) (if (< x (- states 1)) (+ x 1) x))))
  (auto-machine name inc-function descriptor-list)))


;; Machines whose state the player can select
(define 
  selectable-machine
  (lambda args
    (let* ((name (car args))
	   (state-info-list (cdr args))
	   (states (length state-info-list))
	   (state-list (iota states))
	   (state-action-text (map
			       (lambda (state-info)
				 (car state-info))
			       state-info-list)))
      (map
       (lambda (s)
	 (let* ((state-info (list-ref state-info-list s))
		(action-text (car state-info))
		(pre-text (cadr state-info))
		(link-text (caddr state-info))
		(post-text (cadddr state-info))
		(next-state-list (if (>= (length state-info) 5)  ;; optional argument
				     (caddddr state-info)
				     (grep-not-equal s state-list))))
	   (description*
	    name
	    s
	    (lambda ()
	      (p pre-text
		 " "
		 (if (equal? next-state-list '())
		     link-text
		     (let ((link-and-choices (cons
				     link-text
				     (map (lambda (d)
					    (choice*
					     (list-ref state-action-text d)
					     (lambda () (now name d) (look))))
					  next-state-list))))
		       (apply schooz:popup link-and-choices)))
		 " "
		 post-text)))))
       state-list))))

;; Wrapper for simple "Choose Your Own" actions with some text and a popup
;; (cyoa text (text1 choice1) (text2 choice2) ...)
(define cyoa
  (lambda args
    (let ((text (car args))
	  (choice-list (cdr args)))
      `(,text ,(apply menu choice-list)))))

;; Wrapper for simple "Choose Your Own" actions that uses the story-machine mechanism
;; (cyo text (text1 choice1) (text2 choice2) ...)
(define cyo
  (lambda args
    (let ((state (unique-narrative-state))
	  (text (car args))
	  (choice-list (cdr args)))
      (story
       state
       `(,text ,(apply menu choice-list)))  ;; putting declaration inside action is a sucky hack, to prevent symbol binding errors by delaying evaluation of choice-list. Note this may give unexpected behavior if the state is re-entered (e.g. embedded initialization code will be re-run every time)
      (goto state)
      (look))))


;; Helpers for the cyo storymachine wrapper
(define (make-unique-id hash clue)
  (if (eq? (hashtable-ref hash clue #f) #f)
      clue
      (make-unique-id hash (+ clue 1))))

(define (unique-state machine)
  (let ((desc (hashtable-ref schooz:desc machine #f)))
    (number->string (make-unique-id desc (hashtable-size desc)))))

(define (unique-narrative-state) (unique-state schooz:narrative))

