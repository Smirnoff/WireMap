(use srfi-1 srfi-69
;tokyocabinet
)

;; filenames
(define relative-path
  (let ((path ##sys#current-load-path))
    (lambda (filename)
      (string-append path filename))))

(define +iso-3166+ (relative-path "../data/iso_3166-1_alpha-2.scm"))
(define +iso-4217+ (relative-path "../data/iso_4217.scm"))
(define +bank-details+ (relative-path "../data/bank_details.scm"))
(define +bank-details-db+ (relative-path "../data/bank_details.db"))
(define +payment-templates+ (relative-path "../data/payment_templates.scm"))
(define +iban-registry+ (relative-path "../data/iban_registry.scm"))

(define (file-data filename) (with-input-from-file filename read-file))

(define (run-once func)
  (let ((res #f) (ran? #f))
    (lambda args
      (if ran?
          res
          (begin
            (set! res (apply func args))
            (set! ran? #t)
            res)))))

(define-syntax define/run-once
  (syntax-rules ()
    ((define/run-once (name . args) body . rest)
     (define name (run-once (lambda args body . rest))))))

(define/run-once (country-code-hash)
  (alist->hash-table (file-data +iso-3166+)))

(define/run-once (reverse-country-code-hash)
  (alist->hash-table
   (map (lambda (x) (cons (cdr x) (car x)))
        (hash-table->alist (country-code-hash)))))

(define (country-code->country-name code)
  (hash-table-ref/default
    (country-code-hash) (if (string? code) (string->symbol code) code) #f))

(define (country-name->country-code name)
  (hash-table-ref/default (reverse-country-code-hash) name #f))

(define (valid-country-code? code) (and (country-code->country-name code) #t))
(define/run-once (currency-data) (file-data +iso-4217+))
(define/run-once (transaction-templates) (file-data +payment-templates+))

(define (with-tokyocabinet file func
         #!optional (flags (bitwise-ior
;                              TC_HDBOWRITER
                              TC_HDBOREADER
;                              TC_HDBOCREAT
;                              TC_HDBOTRUNC
;                              TC_HDBONOLCK
                              TC_HDBOLCKNB 
)))
  (let ((db (tc-hdb-open file flags: flags))
        (res #f))
    (unless db (error "Failed to open tc database" file flags))
    (handle-exceptions
      exn
      (begin
        (tc-hdb-close db)
        (abort exn))
      (lambda () (set! res (func db))))
    (tc-hdb-close db)
(printf "debug ~s\n" res)
    res))

(define (swift+branch-key swift branch)
  (with-output-to-string (lambda () (write (list swift branch)))))

(define (tc-hdb->alist db)
  (tc-hdb-fold db
               alist-cons
               '()))

(define (alist->tc-hdb! lst db)
  (for-each
    (lambda (x) (tc-hdb-put! db (car x) (cdr x)))
    lst))

(define (alist->tokyocabinet/file lst filename)
  (with-tokyocabinet filename (lambda (db) (alist->tc-hdb! lst db))))

(define (tokyocabinet->alist/file filename)
  (with-tokyocabinet filename tc-hdb->alist))

(define (lookup-banks swift branch-code)
  (with-tokyocabinet
    +bank-details-db+
    (lambda (db)
      (or (tc-hdb-get db (swift+branch-key swift branch-code))
          '()))))

(define/run-once (transaction-templates/currency)
  (fold (lambda (item res)
          (hash-table-update!
             res (alist-ref 'currency (alist-ref 'matcher item))
             (lambda (x) (cons item x))
             (lambda () (list item)))
          res)
        (make-hash-table)
        (transaction-templates)))

(define/run-once (iban-registry) (file-data +iban-registry+))

(define/run-once (iban-registry/country)
  (alist->hash-table
    (map (lambda (x) (cons (alist-ref 'country-code x) x))
         (iban-registry))))
