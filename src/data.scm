(use srfi-1 srfi-69)

(define relative-path
  (let ((path ##sys#current-load-path))
    (lambda (filename)
      (string-append path filename))))

(define (relative-file-data filename)
  (with-input-from-file (relative-path filename) read-file))

(define (run-once func)
  (let ((res #f) (ran? #f))
    (lambda args
      (if ran?
          res
          (begin
            (set! res (apply func args))
            (set! ran? #t)
            res)))))

(define country-code-hash
  (run-once
   (lambda ()
     (alist->hash-table
      (relative-file-data "../data/iso_3166-1_alpha-2.scm")))))

(define reverse-country-code-hash
  (run-once
   (lambda ()
     (alist->hash-table
      (map (lambda (x) (cons (cdr x) (car x)))
           (hash-table->alist (country-code-hash)))))))

(define (country-code->country-name code)
  (hash-table-ref/default
    (country-code-hash) (if (string? code) (string->symbol code) code) #f))

(define (country-name->country-code name)
  (hash-table-ref/default (reverse-country-code-hash) name #f))

(define (valid-country-code? code)
  (and (country-code->country-name code) #t))

(define currency-data
  (run-once
   (lambda ()
     (relative-file-data "../data/iso_4217.scm"))))

(define transaction-templates
  (run-once
   (lambda ()
     (relative-file-data "../data/payment_templates.scm"))))

(define transaction-templates/currency
  (run-once
   (lambda ()
     (fold (lambda (item res)
             (hash-table-update!
              res (alist-ref 'currency (alist-ref 'matcher item))
              (lambda (x) (cons item x))
              (lambda () (list item))))
           (make-hash-table)
           (transaction-templates)))))

(define iban-registry
  (run-once
   (lambda ()
     (relative-file-data "../data/iban_registry.scm"))))

(define iban-registry/country
  (run-once
   (lambda ()
     (alist->hash-table
      (map (lambda (x) (cons (alist-ref 'country-code x) x))
           (iban-registry))))))
