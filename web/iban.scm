#!/usr/bin/csi -ss

(use srfi-1 sxml-transforms)

(define (main args)
  (printf "Content-type: text/html\n\n")
  (SXML->HTML
   `(html
     (head (title "IBAN Checking")
           (link (@ (href "css/bootstrap.min.css") (rel "stylesheet")))
           (style " body { padding-top: 60px; } ")
           (link (@ (href "css/bootstrap-responsive.min.css")
                    (rel "stylesheet")))
           (script (@ (src "iban.js") (type "application/javascript")))
     )
     (body

      (div (@ (class "navbar navbar-fixed-top"))
       (div (@ (class "navbar-inner"))
        (div (@ (class "container"))
         (a (@ (class "btn btn-navbar")
               (data-toggle "collapse") (data-target ".nav-collapse"))
            (span (@ (class "icon-bar")) "")
            (span (@ (class "icon-bar")) "")
            (span (@ (class "icon-bar")) ""))
         (a (@ (class "brand") (href "#")) "Wiremap")
         (div (@ (class "nav-collapse"))
          (ul (@ (class "nav"))
           (li (@ (class "active")) (a (@ (href "#")) "Home"))
           (li (a (@ (href "#about")) "About"))
           (li (a (@ (href "#contact")) "Contact")))))))

      (div (@ (class "container"))

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
       (script (@ (src "js/jquery.js")))
       (script (@ (src "js/bootstrap.min.js"))))
      ))
  (newline))
