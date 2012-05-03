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
      (form
       (@ (onsubmit "return false;"))
       (p (input (@ (size 40) (type "text") (name "iban") (id "iban"))))
       (p (input (@ (value "Check") (type "button")
                    (onclick "click_check_iban();")))))
      (hr)
      (h2 "Result")
      (p (@ (id "res_note")) "")
      (p (@ (id "res_country_code")) "")
      (p (@ (id "res_country_name")) "")
      (p (@ (id "res_bank_id")) "")
      (p (@ (id "res_branch_id")) "")
)
     ))
  (newline))
