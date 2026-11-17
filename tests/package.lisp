(defpackage #:sito/tests
  (:documentation "Regression tests for sito.")
  (:use #:cl)
  (:import-from #:sito
                #:*score-length-limit*
                #:positions
                #:rank
                #:score
                #:terms)
  (:export #:run-tests))

(in-package #:sito/tests)

(defvar *test-count* 0
  "The number of assertions executed by the current test run.")

(defun test-assert (condition description)
  "Record and require CONDITION for DESCRIPTION."
  (incf *test-count*)
  (unless condition
    (error "sito test failed: ~A" description))
  t)
