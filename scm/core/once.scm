(define schooz:once-cache (make-eq-hashtable))

;; (first first-sxml subsequent-sxml)
(define
  (first first-sxml subsequent-sxml)
  (let* ((tag (schooz:fold-strings first-sxml))
	 (val (hashtable-ref schooz:once-cache tag #f)))
    (if
     val
     subsequent-sxml
     (begin
       (hashtable-set! schooz:once-cache tag 1)
       first-sxml))))

(define
  (once sxml)
  (first sxml '()))

(define (once-choice text-func)
  (let* ((text (car text-func))
	 (func (cadr text-func))
	 (val (hashtable-ref schooz:once-cache text #f)))
    (if
     val
     '()
     (choice*
      text
      (lambda ()
	(let ((result (func)))
	  (hashtable-set! schooz:once-cache text 1)
	  result))))))
