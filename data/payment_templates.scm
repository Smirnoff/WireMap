;; this file is a list of transaction templates

;; template members:
;; * matcher - this is a list of transaction members that must match.
;;     they all must match. if you need different matchers, use different
;;     transactions
;; * template - this is the transaction template. it includes all the things
;;     you need or are optional for a transaction that matches the matcher

;; specifying an IBAN:
;; * digits - number of digits
;; * format - a string specifying the format. everything is a literal unless
;;     specified otherwise
;; * check-digits - a list of lists, each list has the first element as the
;;     start position, second element is the length
;; * bank-code - same format as check-digits
;; * account-number - samfe format as bank-code and check-digits


;; one element is the matcher which matches the template to the transaction
;; one element is the transaction requirements

((matcher (currency . AED) (beneficiary-bank-country . AE))
 (template
  (swift-branch-details-field . blank)
  (beneficiary-bank-swift (digits . 8))
  (beneficiary-bank-country . AE)
  (beneficary-account-number
   (type . IBAN)
   (format . "AEkk AAAB BBBB BBBB BBBB BBB")
   (digits . 23)
   (check-digits (2 2))
   (bank-code (5 3))
   (account-number (8 1) (10 4) (15 4) (20 4) (25 3)))
  (additional-considerations (client-references . additional-bank-details)
                             optional)))

((matcher (currency . ALL) (beneficiary-bank-country . AL))
 (template
  (beneficiary-account-number
   (type . IBAN)
   (format . "AL2!n8!n16!c")
   (format . "ALkk AAAB BBBD CCCC CCCC CCCC CCCC")
   (digits . 28)
   (check-digits (2 2))
   (bank-code (5 3))
   (branch-code (8 1) (10 3))
   (account-check-digit (13 1))
   (account-number (15 4) (20 4) (25 4) (30 4))
)
  (beneficiary-bank-BIC/SWIFT (digits . 8))
  (country . AL)
  (purpose-for-payment)
  (beneficiary-full-name)
  (beneficiary-address)))

;end
