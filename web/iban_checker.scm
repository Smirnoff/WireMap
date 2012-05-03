#!/usr/bin/csi -ss

(use utils json)
(load-relative "../src/err.scm")
(load-relative "../src/codes.scm")

(define-syntax var-alist
  (syntax-rules ()
    ((var-alist) '())
    ((var-alist name . rest)
     `((name . ,name) . ,(var-alist . rest)))))

(define-syntax let-alist
  (syntax-rules ()
    ((let-alist lst () body . rest)
     (begin body . rest))
    ((let-alist lst (var-name . var-rest) body . rest)
     (let ((var-name (alist-ref 'var-name lst)))
       (let-alist lst var-rest body . rest)))))

(define (res-to-browser result message data)
  (list->vector
   `((res . ,result)
     (message . ,message)
     . ,data)))

(define (approx-substring x start len)
  (let ((temp (substring x start)))
    (substring temp 0 (min (string-length temp) len))))

(define (user-message res)
  (res-to-browser
   'info
   (let-alist res (alpha-numeric? valid-modulus? format-match country-name
                   iban/spaceless bank-id-correct? branch-id-correct?)
    (cond
     ((not alpha-numeric?)
      "IBANs are alphanumeric characters only.")
     ((or (not valid-modulus?) (not bank-id-correct?) (not branch-id-correct?))
      "Your IBAN has a mistake somewhere. Check your transcription for errors.")
     ((list? format-match)
      (format
       (string-append
        "The IBAN format requirements for ~a require that the ~a ~a characters"
        " starting from the ~a character be ~a only."
        "<br />In your IBAN, this is the section reads ~s.")
       country-name
       (car (caddr format-match))
       (if (eq? (cadr (caddr format-match)) 'fixed) "" "(or less)")
       (car format-match)
       (let ((type (caddr (caddr format-match))))
         (cond ((eq? type 'numeric) "numeric characters")
               ((eq? type 'capital-letters) "capital letters")
               ((eq? type 'alpha-numeric) "alpha-numeric characters")
               (else "something")))
       (approx-substring iban/spaceless
                         (car format-match) (car (caddr format-match)))))
     (else #f)))
   res))

(define (check-iban iban iban/spaceless)
  (let* (
         ;; checking
         (alpha-numeric? (iban-alphanumeric? iban/spaceless))
         (valid-modulus? (valid-iban-modulus? iban/spaceless))
         (format-match (iban-country-format-matches/where
                        iban/spaceless
                        (iban-country-format/split iban/spaceless)))
         (bank-id-correct?
          (handle-exceptions
           exn #f
           (begin (iban-bank-identifier iban/spaceless) #t)))
         (branch-id-correct?
          (handle-exceptions
           exn #f
           (begin (iban-branch-identifier iban/spaceless) #t)))
         (valid? (valid-iban? iban/spaceless))

         ;; information
         (country-code (iban-country-code iban/spaceless))
         (country-name (iban-country-name iban/spaceless))
         (bank-identifier (iban-bank-identifier/no-validate iban/spaceless))
         (branch-identifier 
          (iban-branch-identifier/no-validate iban/spaceless)))

    (user-message
     (var-alist alpha-numeric? valid-modulus? format-match
                bank-id-correct? branch-id-correct? valid?
                country-code country-name bank-identifier branch-identifier
                iban/spaceless))))

(define (handle-args args)
  (let* ((iban (alist-ref "iban" (vector->list args) equal? #f))
         (iban/spaceless (and (string? iban) (iban-sans-spaces iban))))
    (unless iban (error "No iban provided" args))
    (unless iban/spaceless (error "Not a string" iban))
    (check-iban iban iban/spaceless)))

(define (main args)
  (printf "Content-type: text/plain\n\n")
  (json-write
   (handle-exceptions
    exn
    `#((res . error)
       (message . ,(condition-message exn))
       (arguments . ,(condition-arguments exn)))
    (handle-args (json-read))))
  (newline))
