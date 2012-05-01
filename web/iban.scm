#!/usr/bin/csi -ss

(use srfi-1 sxml-transforms)

(define (main args)
  (printf "Content-type: text/html\n\n")
  (SXML->HTML
   `(html
     (head (title "IBAN Checking")
           (script (@ (src "iban.js") (type "application/javascript"))))
     (body
      (h1 "IBAN Checking")
      (form (@)
            "blah"))
     ))
  (newline))
