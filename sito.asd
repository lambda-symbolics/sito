(asdf:defsystem #:sito
  :description "A fast fuzzy matcher for short strings: scores, match positions and ranking"
  :author      "Lukáš Hozda"
  :license     "COLL-Attribution"
  :version     "0.1.0"
  :serial      t
  :components  ((:module     "source"
                 :serial     t
                 :components ((:file "package")
                              (:file "score")
                              (:file "rank"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:sito/tests))))

(asdf:defsystem #:sito/tests
  :description "Tests for sito"
  :depends-on  (#:sito)
  :serial      t
  :components  ((:module     "tests"
                 :serial     t
                 :components ((:file "package")
                              (:file "tests"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (uiop:symbol-call '#:sito/tests '#:run-tests)))
