(use srfi-1 srfi-13 srfi-14 irregex)

(load-relative "math")
(load-relative "data")

(define (iban-sans-spaces x)
  (string-concatenate (string-split x)))

(define (iban-modulus iban)
  (let* ((spaceless (iban-sans-spaces iban))
         (rearranged (string-append (substring spaceless 4)
                                    (substring spaceless 0 4))))
    (list->string
     (concatenate
      (map (lambda (x)
             (if (and (char-set-contains? char-set:ascii x)
                      (char-set-contains? char-set:upper-case x))
                 (string->list
                  (number->string
                   (+ (- (char->integer x) (char->integer #\A)) 10)))
                 (list x)))
             (string->list rearranged))))))

(define (valid-iban-modulus? iban)
  (= 1 (large-modulo (iban-modulus iban) 97)))

(define (iban-country-code iban)
  (and (string? iban)
       (<= 2 (string-length iban))
       (let ((res (string->symbol (substring (iban-sans-spaces iban) 0 2))))
         (and (valid-country-code? res)
              res))))

(define (iban-country-format iban)
  (alist-ref 'IBAN-structure
             (hash-table-ref/default
              (iban-registry/country) (iban-country-code iban) '())))

(define (iban-format-pieces x)
  (define (iban-format-next-piece x pos)
    (let ((next-char (string-ref x pos)))
      (cond
       ((and (char-set-contains? char-set:ascii next-char)
             (char-set-contains? char-set:upper-case next-char))
        (string next-char))
       ((irregex-search "[0-9]+![nac]" x pos) =>
        irregex-match-substring)
       (else (error "Unknown iban format" x pos)))))
  (let iter ((res '()) (cur-pos 0))
    (if (>= cur-pos (string-length x))
        (reverse res)
        (let ((next (iban-format-next-piece x cur-pos)))
          (iter (cons next res)
                (+ cur-pos (string-length next)))))))

(define (iban-country-format/split iban)
  (let ((res (iban-country-format iban)))
    (and res
         (iban-format-pieces res))))

(define (iban-piece-matches spaceless piece current-pos)
  (if (= 1 (string-length piece))
      (list (equal? (string-ref spaceless current-pos)
                    (string-ref piece 0))
            (add1 current-pos))
      (let* ((split-piece (string-split piece "!"))
             (count (and (pair? split-piece)
                         (string->number (car split-piece))))
             (type (and (pair? split-piece)
                        (pair? (cdr split-piece))
                        (cadr split-piece)))
             (charset (cond
                       ((equal? type "n")
                        (string->char-set "0123456789"))
                       ((equal? type "a")
                        (string->char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                       ((equal? type "c")
                        (string->char-set
                         (string-append "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "abcdefghijklmnopqrstuvwxyz"
                                        "0123456789")))
                       (else #f))))
        (unless (and count type)
                (error "Unknown iban parsing instruction" piece))
        (list (and (every
                    (lambda (char) (char-set-contains? charset char))
                    (string->list
                     (substring spaceless current-pos (+ current-pos count))))
                   #t)
              (+ current-pos count)))))

(define (iban-country-format-matches? iban format)
  (let ((spaceless (iban-sans-spaces iban)))
    (and format
         (let iter ((format-pieces format)
                    (current-pos 0))
           (if (null? format-pieces)
               #t
               (let* ((res (iban-piece-matches
                            spaceless (car format-pieces) current-pos))
                      (next-pos (cadr res))
                      (matches? (car res)))
                 (if matches?
                     (iter (cdr format-pieces) next-pos)
                     #f))))
         #t)))

(define (valid-iban? iban)
  (and (valid-iban-modulus? iban)
       (iban-country-format-matches? iban (iban-country-format/split iban))))

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
