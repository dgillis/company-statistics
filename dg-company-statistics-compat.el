;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions added for backwards compatibility below.                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dg-company-statistics-compat-score-reducer (cand score-components)
  (let ((score 0))
    (dolist (comp score-components)
      (let ((comp-score (or (cdr (assoc :score comp)) 0)))
        (setq score (+ score comp-score))))
    score))

(defun dg-company-statistics-capture-context-heavy (&optional _manual)
  "Calculate some context, once for the whole completion run."
  (let ((dg-company-statistics-features dg-company-statistics-default-features-heavy))
    (dg-company-statistics-capture-context _manual)))

(defun dg-company-statistics-score-change-heavy (_cand)
  "Count for global score, mode context, last keyword, parent symbol,
buffer file name."
  (let ((dg-company-statistics-features dg-company-statistics-default-features-heavy))
    (dg-company-statistics-score-change _cand)))

(defun dg-company-statistics-score-calc-heavy (cand)
  "Global score, and bonus for matching major mode, last keyword, parent
symbol, buffer file name."
  (let ((dg-company-statistics-features dg-company-statistics-default-features-heavy)
        (dg-company-statistics-score-reducer 'dg-company-statistics-compat-score-reducer))
    (dg-company-statistics-score-calc cand)))

(defun dg-company-statistics-score-change-light (_cand)
  "Count for global score and mode context."
  (let ((dg-company-statistics-features dg-company-statistics-default-features-light))
    (dg-company-statistics-score-change _cand)))

(defun dg-company-statistics-score-calc-light (cand)
  "Global score, and bonus for matching major mode."
  (let ((dg-company-statistics-features dg-company-statistics-default-features-light)
        (dg-company-statistics-score-reducer 'dg-company-statistics-compat-score-reducer))
    (dg-company-statistics-score-calc cand)))

(provide 'dg-company-statistics-compat)