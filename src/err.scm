
(define (condition-message x)
  (get-condition-property x 'exn 'message))

(define (condition-arguments x)
  (get-condition-property x 'exn 'arguments))
