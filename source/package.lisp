(defpackage #:sito
  (:documentation "Library for fuzzy matching a short query against short strings.")
  (:use #:cl)
  (:export #:*score-length-limit*
           #:positions
           #:rank
           #:score
           #:terms))
