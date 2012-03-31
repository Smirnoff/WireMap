(use srfi-1 srfi-13 srfi-14)

(load-relative "math")

(define (valid-iban? iban)

  (define (letter->iban-number x)
    (if (and (char-set-contains? char-set:ascii x)
             (char-set-contains? char-set:upper-case x))
        (string->list
         (number->string
          (+ (- (char->integer x) (char->integer #\A)) 10)))
        (list x)))

  (define (valid-modulus?)
    (let* ((spaceless (string-concatenate (string-split iban)))
           (rearranged (string-append (substring spaceless 4)
                                      (substring spaceless 0 4)))
           (letters-as-numbers
            (list->string
             (concatenate
              (map letter->iban-number (string->list rearranged))))))
      (= 1 (large-modulo letters-as-numbers 97))))

  (and (valid-modulus?)))

(define (swift-code-bank/raw x) (substring x 0 4))
(define (swift-code-country/raw x) (substring x 4 6))
(define (swift-code-location/raw x) (substring x 6 8))
(define (swift-code-branch/raw x) (substring x 8))

(define (valid-swift-code? code)
  (and ; (string? code) ; do i need this?
       (or (= 8 (string-length code)) (= 11 (string-length code)))
       (every (lambda (x) (and (char-set-contains? char-set:ascii x)
                               (char-set-contains? char-set:letter x)))
              (string->list (swift-code-bank/raw code)))
       (every (lambda (x) (and (char-set-contains? char-set:ascii x)
                               (char-set-contains? char-set:letter x)))
              (string->list (swift-code-country/raw code)))
       (every (lambda (x) (and (char-set-contains? char-set:ascii x)
                               (char-set-contains? char-set:letter+digit x)))
              (string->list (swift-code-location/raw code)))
       (every (lambda (x) (and (char-set-contains? char-set:ascii x)
                               (char-set-contains? char-set:letter+digit x)))
              (string->list (swift-code-location/raw code)))
       #t))
