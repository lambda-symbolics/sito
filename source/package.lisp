(defpackage #:sito
  (:documentation "Fuzzy matching of a short query against short strings.")
  (:use #:cl)
  (:export #:*score-length-limit*
           #:positions
           #:rank
           #:score
           #:terms))
