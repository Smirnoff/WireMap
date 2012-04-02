(use srfi-1 srfi-69)

(load-relative "codes")

;; templates are alists containing two elements, matcher and template
;; both matcher's value and template's value are alists
(define (template-matcher x) (alist-ref 'matcher x))
(define (template-template x) (alist-ref 'template x))

;; transactions are alists
(define (transaction-currency x) (alist-ref 'currency x))
(define (transaction-beneficiary-bank-country x)
  (alist-ref 'beneficiary-bank-country x))

;; account number template info
(define (account-number-type x)
  (or (alist-ref 'type x)
      (error "Missing account number type" x)))

(define (template-matches-transaction? template transaction)
  (and (every (lambda (x) (equal? (cdr x) (alist-ref (car x) transaction)))
              (template-matcher template))
       #t))

(define (relevant-transaction-template transaction)
  (find
   (lambda (x) (template-matches-transaction? x transaction))
   (hash-table-ref/default
    (transaction-templates/currency) (transaction-currency transaction) '())))

(define *template-verifiers* (make-hash-table))

(define (add-template-verifier! name value)
  (hash-table-set! *template-verifiers* name value))

(add-template-verifier!
 'beneficiary-account-number
 (lambda (verifier-data transaction-element)
   (unless (eq? 'IBAN (account-number-type verifier-data))
           (error "Only IBAN account types handled thus far"
                  verifier-data transaction-element))
   (let ((account-number (and transaction-element (cdr transaction-element))))
     (cond
      ((not transaction-element)
       `(beneficiary-account-number . "Missing account number"))
      ((not (valid-iban? account-number))
       `(beneficiary-account-number . "IBAN didn't validate"))
      (())
    ))
   ))

;; this returns a list of things the transaction is missing or has incorrect
;; so it finds the right transaction template for the given template
;; then it iterates over elements of the template and it verifies each one
;; from the given template
;; if the element fails to verify, it adds the element's name and proper
;; implementation to the result list
(define (validate-transaction transaction)
  (let ((template (relevant-transaction-template transaction)))
    (unless template
            (error "Failed to find relevant transaction template" transaction))
    (filter-map
     (lambda (x)
       (let ((validator
              (hash-table-ref/default *template-verifiers* (car x) #f))
             (transaction-element (assoc (car x) transaction)))
         (unless validator
                 (error "Missing a template verifier" transaction template x))
         (validator x transaction-element)))
     template)))
