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
  (first sxml ""))