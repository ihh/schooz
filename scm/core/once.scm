(define schooz:once-cache (make-eq-hashtable))

;; (once sxml)
(define
  (once sxml)
  (if
   (hashtable-ref schooz:once-cache sxml)
   #f
   (begin
     (hashtable-set! schooz:once-cache sxml 1)
     sxml)))
