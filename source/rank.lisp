(in-package #:sito)

;;;; -- Ranking --

(declaim (ftype (function (string) list) terms))
(defun terms (query)
  "Return QUERY's words, the parts whitespace separates."
  (loop with start = 0
        for end = (position-if (lambda (character) (find character '(#\Space #\Tab #\Newline)))
                               query :start start)
        when (< start (or end (length query)))
          collect (subseq query start end)
        while end
        do (setf start (1+ end))))

(declaim (ftype (function (list (or string list)) (or null integer)) rank--score))
(defun rank--score (terms fields)
  "Return the sum of each of TERMS' best score over FIELDS, or NIL when one matches none.

FIELDS is a string or a list of strings; a NIL field is skipped."
  (let ((total 0))
    (dolist (term terms total)
      (let ((best nil))
        (dolist (field (if (listp fields) fields (list fields)))
          (let ((score (and field (score term field))))
            (when (and score (or (null best) (> score best)))
              (setf best score))))
        (unless best
          (return nil))
        (incf total best)))))

(declaim (ftype (function (string list &key (:key function) (:limit (or null (integer 0)))) list) rank))
(defun rank (query items &key (key #'identity) limit)
  "Return the ITEMS that QUERY matches, best first, at most LIMIT of them.

KEY returns the string an item is matched by, or a list of strings of which the
best one counts. Every word of QUERY must match; an empty QUERY keeps every
item in its order. Equal scores keep the order of ITEMS."
  (let* ((terms  (terms query))
         (ranked (if terms
                     (mapcar #'rest
                             (stable-sort
                              (loop for item in items
                                    for score = (rank--score terms (funcall key item))
                                    when score
                                      collect (cons score item))
                              #'> :key #'first))
                     items)))
    (if (and limit (< limit (length ranked)))
        (subseq ranked 0 limit)
        ranked)))
