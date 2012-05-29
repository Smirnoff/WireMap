(use srfi-1 srfi-69)
(use berkeleydb posix)

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
(define +abm-banks+ (relative-path "../data/abm_bank_numbers.scm"))

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

(define (obj->string x)
  (with-output-to-string (lambda () (write x))))

(define (string->obj x)
  (with-input-from-string x read))

(define (swift+branch-key swift branch)
  (obj->string (list swift branch)))

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

(define (redo-bank-details-db?)
  (or (not (file-exists? +bank-details-db+))
      (< (file-modification-time +bank-details-db+)
         (file-modification-time +bank-details+))))

(define/run-once (bank-details) (file-data +bank-details+))

(define (db-append-obj! db key value)
  (let ((key/str (obj->string key)))
    (db-put!
     db
     key/str
     (obj->string
      (cons value
            (string->obj (db-get/default db key/str (obj->string '()))))))))

(define (ensure-bank-details-db!)
  (when (redo-bank-details-db?)
    (call-with-fresh-db
     +bank-details-db+
     (lambda (db)
       (for-each
        (lambda (x)
          (let ((swift (alist-ref 'swift x))
                (branch (alist-ref 'swift-branch x)))
            (db-put!
             db
             (obj->string (list swift branch))
             (obj->string x))
            (db-append-obj! db (list swift #f) (list swift branch))))
        (bank-details))))))

(define (bank-details-db-ref swift #!optional (branch "XXX"))
  (ensure-bank-details-db!)
  (call-with-db
   +bank-details-db+
   (lambda (db)
     (string->obj
      (db-get/default db (obj->string (list swift branch)) (obj->string #f))))))

(define (bank-swifts-db-ref swift)
  (ensure-bank-details-db!)
  (call-with-db
   +bank-details-db+
   (lambda (db)
     (string->obj
      (db-get/default db (obj->string (list swift #f)) (obj->string #f))))))

(define/run-once (abm-banks) (file-data +abm-banks+))
(define/run-once (abm-number-hash)
  (alist->hash-table
   (map (lambda (x) (cons (alist-ref 'number x) x)) (abm-banks))))

(define (abm-bank-number-ref num)
  (hash-table-ref/default (abm-number-hash) num #f))
