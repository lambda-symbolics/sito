(in-package #:sito)

;;;; -- Scores --

;;; The scoring is fzy's: a query matches a string that contains its
;;; characters in order, and the best alignment wins. Consecutive matches and
;;; matches at the start of a word score high, gaps cost a little. Scores are
;;; integers, a thousand to one matched character.

(defparameter *score-length-limit* 1024
  "The longest string aligned character by character; a longer match scores lowest.")

(deftype score ()
  "An alignment score, or its lower bound."
  'fixnum)

(defparameter *score-floor* (floor most-negative-fixnum 4)
  "The score of an impossible alignment, low enough that adding to it never wraps.")

(defparameter *score-exact* (floor most-positive-fixnum 4)
  "The score of a string equal to the query but for case.")

(defparameter *gap-leading* -5
  "The cost of each character before the first match.")

(defparameter *gap-inner* -10
  "The cost of each character between two matches.")

(defparameter *gap-trailing* -5
  "The cost of each character after the last match.")

(defparameter *bonus-consecutive* 1000
  "The score of a match directly after another.")

(defparameter *bonus-start* 900
  "The score of a match at the start of the string or after a slash.")

(defparameter *bonus-word* 800
  "The score of a match after a hyphen, an underscore or a space.")

(defparameter *bonus-capital* 700
  "The score of a capital that follows a lowercase letter.")

(defparameter *bonus-dot* 600
  "The score of a match after a dot.")

(declaim (ftype (function (simple-string fixnum) fixnum) score--bonus))

(defun score--bonus (string index)
  "Return what a match at INDEX of STRING earns for where it stands."
  (if (zerop index)
      *bonus-start*
      (let ((previous (schar string (1- index))))
        (cond
          ((char= previous #\/)
           *bonus-start*)
          ((find previous "-_ ")
           *bonus-word*)
          ((char= previous #\.)
           *bonus-dot*)
          ((and (lower-case-p previous) (upper-case-p (schar string index)))
           *bonus-capital*)
          (t
           0)))))

(declaim (ftype (function (simple-string simple-string) boolean) score--subsequence-p))

(defun score--subsequence-p (query string)
  "Return true when STRING contains QUERY's characters in order, ignoring case."
  (let ((start 0))
    (loop for character across query
          do (let ((found (position character string :start start :test #'char-equal)))
               (unless found
                 (return nil))
               (setf start (1+ found)))
          finally (return t))))

(declaim (ftype (function (string string) boolean) match-p))

(defun match-p (query string)
  "Return true when QUERY is a case-insensitive subsequence of STRING."
  (score--subsequence-p (coerce query 'simple-string)
                        (coerce string 'simple-string)))

(declaim (ftype (function (simple-string simple-string &key (:ending simple-vector) (:best simple-vector))
                          score)
                score--align))

(defun score--align (query string &key ending best)
  "Return the best alignment score of QUERY in STRING.

ENDING and BEST, one row a query character, receive the score of the best
alignment that ends with a match at each string index and the best up to it;
without them two rows are kept."
  (let* ((length  (length string))
         (floor   *score-floor*)
         (above-d (make-array length :element-type 'fixnum :initial-element floor))
         (above-m (make-array length :element-type 'fixnum :initial-element floor))
         (row-d   (make-array length :element-type 'fixnum :initial-element floor))
         (row-m   (make-array length :element-type 'fixnum :initial-element floor)))
    (declare (type fixnum length floor)
             (type (simple-array fixnum (*)) above-d above-m row-d row-m))
    (loop for character across query
          for row fixnum from 0
          do (let ((gap     (if (= row (1- (length query))) *gap-trailing* *gap-inner*))
                   (running floor))
               (declare (type fixnum gap running))
               (dotimes (index length)
                 (let ((matched
                         (if (char-equal character (schar string index))
                             (cond
                               ((zerop row)
                                (+ (* index *gap-leading*) (score--bonus string index)))
                               ((zerop index)
                                floor)
                               (t
                                (max (+ (aref above-m (1- index)) (score--bonus string index))
                                     (+ (aref above-d (1- index)) *bonus-consecutive*))))
                             floor)))
                   (declare (type fixnum matched))
                   (setf running (max matched (+ running gap))
                         (aref row-d index) matched
                         (aref row-m index) running)))
               (when ending
                 (setf (svref ending row) (copy-seq row-d)
                       (svref best row) (copy-seq row-m)))
               (rotatef above-d row-d)
               (rotatef above-m row-m)))
    (aref above-m (1- length))))

(declaim (ftype (function (string string) (or null score)) score))

(defun score (query string)
  "Return how well QUERY matches STRING, or NIL when it does not match.

QUERY matches when STRING contains its characters in order, ignoring case. A
higher score is a better match; an empty QUERY matches everything with zero."
  (let ((query  (coerce query 'simple-string))
        (string (coerce string 'simple-string)))
    (cond
      ((zerop (length query))
       0)
      ((not (score--subsequence-p query string))
       nil)
      ((= (length query) (length string))
       *score-exact*)
      ((> (length string) *score-length-limit*)
       *score-floor*)
      (t
       (score--align query string)))))

(declaim (ftype (function (string string) list) positions))

(defun positions (query string)
  "Return the indices of STRING the best alignment of QUERY matches, in order, or NIL.

A string longer than *SCORE-LENGTH-LIMIT* reports its first matching characters."
  (let ((query  (coerce query 'simple-string))
        (string (coerce string 'simple-string)))
    (cond
      ((or (zerop (length query)) (not (score--subsequence-p query string)))
       nil)
      ((> (length string) *score-length-limit*)
       (let ((start 0))
         (loop for character across query
               collect (let ((found (position character string :start start :test #'char-equal)))
                         (setf start (1+ found))
                         found))))
      (t
       (let ((ending (make-array (length query)))
             (best   (make-array (length query)))
             (result nil)
             (index  (1- (length string)))
             (run-p  nil))
         (score--align query string :ending ending :best best)
         (loop for row from (1- (length query)) downto 0
               do (loop
                    (let ((here (aref (svref ending row) index)))
                      (when (and (/= here *score-floor*)
                                 (or run-p (= here (aref (svref best row) index))))
                        ;; A match that continues a run is preferred over the
                        ;; equal score of a later word start.
                        (setf run-p (and (plusp row) (plusp index)
                                         (= (aref (svref best row) index)
                                            (+ (aref (svref ending (1- row)) (1- index))
                                               *bonus-consecutive*))))
                        (push index result)
                        (decf index)
                        (return)))
                    (decf index)))
         result)))))
