#!/usr/bin/csi -ss

(use utils json)
(load-relative "../src/err.scm")
(load-relative "../src/codes.scm")
(load-relative "../src/codes.scm")

(define-syntax var-alist
  (syntax-rules ()
    ((var-alist) '())
    ((var-alist name . rest)
     `((name . ,name) . ,(var-alist . rest)))))

(define (res-to-browser result message data)
  (list->vector
   `((res . ,result)
     (message . ,message)
     . ,data)))

(define (check-swift swift swift/normalized)
  (let* (
         (valid? (valid-swift-code? swift/normalized))
         (code-bank (and valid? (swift-code-bank/raw swift/normalized)))
         (code-country (and valid? (swift-code-country/raw swift/normalized)))
         (code-location (and valid? (swift-code-location/raw swift/normalized)))
         (code-branch (and valid? (swift-code-branch swift/normalized)))
         (hash-key (swift-hash-key swift/normalized))
         (details (and valid?
                       (list->vector
                         (lookup-banks (car hash-key) (cadr hash-key)))))
         )
    (res-to-browser
     'info
     (cond
      (else #f)
     )
     (let ((details (and (list? details) (list->vector details))))
       (var-alist valid? code-bank code-country code-location code-branch
                  details swift swift/normalized))
    )
  )
)

(define (handle-args args)
  (let* ((swift (alist-ref "swift" (vector->list args) equal? #f))
         (swift/normalized (and (string? swift) (swift-normalize swift))))
    (unless swift (error "No iban provided" args))
    (unless swift/normalized (error "Not a string" swift))
    (check-swift swift swift/normalized)))

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
