(use srfi-1 srfi-13 srfi-14 irregex)

(load-relative "math")
(load-relative "data")

;; TODO: think up some standard for which functions take spaceless ibans and
;;       which ones don't
;; TODO: Make the code that splits up and handles iban format codes able to
;;       support non-fixed lengths

(define +numeric+ (string->char-set "0123456789"))
(define +capital-letters+ (string->char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define +alpha-numeric+
  (string->char-set (string-append "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                   "abcdefghijklmnopqrstuvwxyz"
                                   "0123456789")))

(define (iban-alphanumeric? x)
  (and (every (lambda (char) (char-set-contains? +alpha-numeric+ char))
              (string->list x))
       #t))

(define (string-sans-spaces x)
  (string-concatenate (string-split x)))

(define (string-all-caps x)
  (list->string (map char-upcase (string->list x))))

(define (iban-sans-spaces x) (string-sans-spaces x))

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
  (handle-exceptions
   exn
   #f
   (= 1 (large-modulo (iban-modulus iban) 97))))

(define (iban-country-code iban)
  (and (string? iban)
       (<= 2 (string-length iban))
       (let ((res (string->symbol
                   (substring (iban-sans-spaces (string-all-caps iban)) 0 2))))
         (and (valid-country-code? res)
              res))))

(define (iban-country-name iban)
  (let ((code (iban-country-code iban)))
    (and code (country-code->country-name code))))

(define (iban-info iban)
  (hash-table-ref/default (iban-registry/country) (iban-country-code iban) '()))

(define (iban-country-format iban)
  (alist-ref 'IBAN-structure (iban-info iban)))

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

(define (iban-format-piece-split x)
  (let ((match (irregex-match "([0-9]+)(!?)([nac])" x)))
    (and match
         (list (string->number (irregex-match-substring match 1))
               (if (equal? "!" (irregex-match-substring match 2))
                   'fixed
                   'less-than)
               (let ((val (irregex-match-substring match 3)))
                 (cond
                  ((equal? val "n") 'numeric)
                  ((equal? val "c") 'capital-letters)
                  ((equal? val "a") 'alpha-numeric)
                  (else (error "Shouldn't get here"))))))))

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
                       ((equal? type "n") +numeric+)
                       ((equal? type "a") +capital-letters+)
                       ((equal? type "c") +alpha-numeric+)
                       (else #f))))
        (unless (and count type)
                (error "Unknown iban parsing instruction" piece))
        (list (and (every
                    (lambda (char) (char-set-contains? charset char))
                    (string->list
                     (substring spaceless current-pos (+ current-pos count))))
                   #t)
              (+ current-pos count)))))

(define (iban-country-format-matches/where iban format)
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
                     (list current-pos
                           (car format-pieces)
                           (iban-format-piece-split
                            (car format-pieces))))))))))

(define (iban-country-format-matches? iban format)
  (eqv? #t (iban-country-format-matches/where iban format)))

(define (valid-iban? iban)
  (and (valid-iban-modulus? iban)
       (iban-country-format-matches? iban (iban-country-format/split iban))))

(define (looks-like-an-iban? iban)
  (and (iban-alphanumeric? (string-sans-spaces iban))
       (let ((len (string-length (string-sans-spaces iban))))
         (and (>= len 15) (<= len 31)))
       (iban-country-code iban)
       #t))

(define (iban-bban iban)
  (substring iban 4))

(define (iban-format-getter name confirmer)
  (lambda (iban)
    (let* ((format (iban-info iban))
           (format-info (alist-ref name format))
           (format-validation (and confirmer (alist-ref confirmer format)))
           (res (and format-info
                     (substring (iban-bban iban)
                                (car format-info)
                                (+ (car format-info) (cadr format-info))))))
      (when (and confirmer format-validation
                 (not (car (iban-piece-matches res format-validation 0))))
            (error "Bad bank-identifier" iban res))
      (and format-info
           res))))

(define iban-bank-identifier/no-validate
  (iban-format-getter 'bank-identifier #f))
(define iban-bank-identifier
  (iban-format-getter 'bank-identifier 'bank-identifier-length))

(define iban-branch-identifier/no-validate
  (iban-format-getter 'branch-identifier #f))
(define iban-branch-identifier
  (iban-format-getter 'branch-identifier 'branch-identifier-length))

(define iban-whole-bank-identifier/no-validate
  (iban-format-getter 'whole-bank-identifier #f))
(define iban-whole-bank-identifier
  (iban-format-getter 'whole-bank-identifier 'whole-bank-identifier-length))

(define (swift-code-bank/raw x) (substring x 0 4))
(define (swift-code-country/raw x) (substring x 4 6))
(define (swift-code-location/raw x) (substring x 6 8))
(define (swift-code-branch/raw x) (substring x 8))

(define (swift-code-branch x)
  (let ((res (swift-code-branch/raw x)))
    (if (string-null? res)
        "XXX"
        res)))

(define (swift-normalize x)
  (and (string? x)
       (string-all-caps (string-sans-spaces x))
       (let ((caps+no-spaces (string-all-caps (string-sans-spaces x))))
         (cond
           ((= (string-length caps+no-spaces) 8)
            (string-append caps+no-spaces (swift-code-branch caps+no-spaces)))
           ((= (string-length caps+no-spaces) 11)
            caps+no-spaces)
           (else #f)))))

(define (swift-main-code x) (substring (swift-normalize x) 0 8))

(define (swift-hash-key x)
  (let ((x (swift-normalize x)))
    (if x
        (list (swift-main-code x) (swift-code-branch x))
        #f)))

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

(define (looks-like-a-swift-code? code)
  (and (swift-normalize code)
       #t))

(define (clabe-valid-length? x) (= (string-length x) 18))
(define (clabe-is-numeric? x)
  (and (every (lambda (x) (char-set-contains? +numeric+ x))
              (string->list x))
       #t))

(define (clabe-bank-code/raw x) (substring x 0 3))
(define (clabe-branch-code/raw x) (substring x 3 6))
(define (clabe-account-number/raw x) (substring x 6 17))
(define (clabe-sans-control-digit/raw x) (substring x 0 17))
(define (clabe-control-digit/raw x) (substring x 17 18))

(define (char->digit x) (- (char->integer x) 48))

(define (weight-factor idx)
  (cond
   ((= (modulo idx 3) 0) 3)
   ((= (modulo idx 3) 1) 7)
   ((= (modulo idx 3) 2) 1)
   (else (error "What" idx (modulo idx 3)))))

(define (calculate-clabe-control-digit x)
  (modulo
   (- 10
      (modulo
       (fold +
             0
             (map (lambda (digit weight) (modulo (* digit weight) 10))
                  (map char->digit
                       (string->list (clabe-sans-control-digit/raw x)))
                  (map weight-factor (iota 17))))
       10))
   10))

(define (clabe-valid-control-digit? x)
  (equal? (clabe-control-digit/raw x)
          (->string (calculate-clabe-control-digit x))))

(define (clabe-bank-info x)
  (abm-bank-number-ref (clabe-bank-code/raw x)))

(define (clabe-valid-bank-code? x) (and (clabe-bank-info x) #t))

(define (clabe-normalize x) (string-trim-both x))

(define (valid-clabe? x)
  (let ((x (clabe-normalize x)))
    (and (clabe-valid-length? x)
         (clabe-is-numeric? x)
         (clabe-valid-control-digit? x)
         (clabe-valid-bank-code? x)
         #t)))

(define (looks-like-a-clabe? x)
  (clabe-is-numeric? (clabe-normalize x)))
