;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions added for backwards compatibility below.                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-statistics-capture-context-heavy (&optional _manual)
  "Calculate some context, once for the whole completion run."
  (let ((company-statistics-features company-statistics-default-features-heavy))
    (company-statistics-capture-context _manual)))

(defun company-statistics-score-change-heavy (_cand)
  "Count for global score, mode context, last keyword, parent symbol,
buffer file name."
  (let ((company-statistics-features company-statistics-default-features-heavy))
    (company-statistics-score-change _cand)))

(defun company-statistics-score-calc-heavy (cand)
  "Global score, and bonus for matching major mode, last keyword, parent
symbol, buffer file name."
  (let ((company-statistics-features company-statistics-default-features-heavy))
    (company-statistics-score-calc cand)))

(defun company-statistics-score-change-light (_cand)
  "Count for global score and mode context."
  (let ((company-statistics-features company-statistics-default-features-light))
    (company-statistics-score-change _cand)))

(defun company-statistics-score-calc-light (cand)
  "Global score, and bonus for matching major mode."
  (let ((company-statistics-features company-statistics-default-features-light))
    (company-statistics-score-calc cand)))

(provide 'company-statistics-compat)