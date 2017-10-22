;; The sort by statistics transformer can be slow since it will repeatedly call
;; `dg-company-statistics-score-calc'. These calls are mostly redundant as only
;; one call per candidate is needed since the scores remain unchanged while a
;; sort is in progress. The following advice makes use of this by caching the
;; scores during sorting.
(defvar dg-company-statistics--score-calc-cache-enabled t)

(defvar dg-company-statistics--sort-size-limit 1000)

(defvar dg-company-statistics--score-calc-cache-size 1000)

(defvar dg-company-statistics--score-calc-cache
  (make-hash-table :test 'equal
                   :size dg-company-statistics--score-calc-cache-size))

(defvar dg-company-statistics--non-cached-score-calc-func nil)

(defun dg-company-statistics--score-calc-reset-hash ()
  (setq dg-company-statistics--score-calc-cache
        (make-hash-table :test 'equal
                         :size dg-company-statistics--score-calc-cache-size)))

(defun dg-company-statistics--score-calc-using-cache (cand)
  (let* ((cache-key cand)
         (score
          (when dg-company-statistics--score-calc-cache-enabled
            (gethash cache-key dg-company-statistics--score-calc-cache))))
    (unless score
      (setq score (funcall dg-company-statistics--non-cached-score-calc-func cand))
      (puthash cache-key score dg-company-statistics--score-calc-cache))
    score))

(defun dg--company-sort-by-statistics-advice (fn cands)
  (if (and dg-company-statistics--sort-size-limit
           (> (length cands) dg-company-statistics--sort-size-limit))
      (progn
        (message "dg-company-statistics - sort by passed, too many candidates (%s)"
                 (length cands))
        cands)
    (let* ((dg-company-statistics--non-cached-score-calc-func
            dg-company-statistics-score-calc)
           (dg-company-statistics-score-calc
            'dg-company-statistics--score-calc-using-cache))
      (funcall fn cands))))

(defun dg-company-statistics---finished-advice (fn result)
  (let ((retval (funcall fn result)))
    (dg-company-statistics--score-calc-reset-hash)
    ;;(remhash result dg-company-statistics--score-calc-cache)
    ;;(clrhash dg-company-statistics--score-calc-cache)
    retval))

(defun dg-company-statistics-score-calc-cache-advice-enable ()
  (advice-add 'dg-company-statistics-sort-transformer :around 'dg--company-sort-by-statistics-advice)
  (advice-add 'dg-company-statistics--finished :around 'dg-company-statistics---finished-advice))

(defun dg-company-statistics-score-calc-cache-advice-disable ()
  (advice-remove 'dg-company-statistics-sort-transformer 'dg--company-sort-by-statistics-advice)
  (advice-remove 'dg-company-statistics--finished 'dg-company-statistics---finished-advice))

(provide 'dg-company-statistics-score-calc-cache-advice)