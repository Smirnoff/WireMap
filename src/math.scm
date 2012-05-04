(use int-limits) ;; note, the egg for this is number-limits

(define +number-of-digits+
  (let iter ((test-num 9) (digit-count 1))
    (if (> test-num most-positive-integer32)
        digit-count
        (iter (+ (* test-num 10) 9) (add1 digit-count)))))

(define (large-modulo a b)
  (let ((number-a (if (number? a) a (string->number a)))
        (string-a (if (number? a) (number->string a) a)))
    (if (< (string-length string-a) +number-of-digits+)
        (modulo number-a b)
        (let* ((prefix (substring string-a 0 (sub1 +number-of-digits+)))
               (rest (substring string-a (sub1 +number-of-digits+))))
          (large-modulo
           (string-append
            (number->string (modulo (string->number prefix) b)) rest)
           b)))))
