;;; dg-company-statistics.el --- Sort candidates using completion history  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017  Free Software Foundation, Inc.

;; Author: Ingo Lohmar <i.lohmar@gmail.com>
;; URL: https://github.com/company-mode/company-statistics
;; Version: 0.2.3
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "24.3") (company "0.8.5"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Package installed from elpa.gnu.org:
;;
;;   (add-hook 'after-init-hook #'dg-company-statistics-mode)
;;
;; Manually installed: make sure that this file is in load-path, and
;;
;;   (require 'dg-company-statistics)
;;   (dg-company-statistics-mode)
;;
;; Every time a candidate is chosen using company-mode, we keep track of this
;; (for a limited amount of recent choices).  When presenting completion
;; candidates next time, they are sorted according to the score thus acquired.
;;
;; The same candidate might occur in different modes, projects, files etc., and
;; possibly has a different meaning each time.  Therefore along with the
;; completion, we store some context information.  In the default (heavy)
;; configuration, we track the overall frequency, the major-mode of the buffer,
;; the last preceding keyword, the parent symbol, and the filename (if it
;; applies), and the same criteria are used to score all possible candidates.

;;; Code:

(require 'cl)
(require 'company)

(defgroup dg-company-statistics nil
  "Completion candidates ranking by historical statistics."
  :group 'company)

(defcustom dg-company-statistics-size 400
  "Number of completion choices that `dg-company-statistics' keeps track of.
As this is a global cache, making it too small defeats the purpose."
  :type 'integer
  :initialize #'custom-initialize-default
  :set #'dg-company-statistics--log-resize)

(defcustom dg-company-statistics-file
  (concat user-emacs-directory "dg-company-statistics-cache.el")
  "File to save dg-company-statistics state."
  :type 'string)

(defcustom dg-company-statistics-auto-save t
  "Whether to save the statistics when leaving emacs."
  :type 'boolean)

(defcustom dg-company-statistics-auto-restore t
  "Whether to restore statistics when dg-company-statistics is enabled and has
not been used before."
  :type 'boolean)

(defcustom dg-company-statistics-capture-context #'dg-company-statistics-capture-context
  "Function called with single argument (t if completion started manually).
This is the place to store any context information for a completion run."
  :type 'function)

(defcustom dg-company-statistics-score-change #'dg-company-statistics-score-change
  "Function called with completion choice.  Using arbitrary other info,
it should produce an alist, each entry labeling a context and the
associated score update: ((ctx-a . 1) (\"str\" . 0.5) (nil . 1)).  Nil is
the global context."
  :type 'function)

(defcustom dg-company-statistics-score-calc #'dg-company-statistics-score-calc
  "Function called with completion candidate.  Using arbitrary other info,
eg, on the current context, it should evaluate to the candidate's score (a
number)."
  :type 'function)

(defvar dg-company-statistics-features nil)

(defconst dg-company-statistics-default-features-heavy
  '((keyword (:get-context . (dg-company-statistics--last-keyword-ctx)))
    (symbol (:get-context . (dg-company-statistics--parent-symbol-ctx)))
    (file (:get-context . buffer-file-name))
    (major-mode (:get-context . major-mode))
    (global (:get-context . t))))

(defconst dg-company-statistics-default-features-light
  '((global (:get-context . t))
    (major-mode (:get-context . major-mode))))

(setq dg-company-statistics-features dg-company-statistics-default-features-heavy)

;; internal vars, persistence

(defvar dg-company-statistics-feature-scores-alist nil
  "This variable will be found to an alist of (FEATURE . SCORE) elements
during the evaluation of `dg-company-statistics-score-calc-expr'.")

(defvar dg-company-statistics--scores nil
  "Store selection frequency of candidates in given contexts.")

(defvar dg-company-statistics--log nil
  "Ring keeping a log of statistics updates.")

(defvar dg-company-statistics--index nil
  "Index into the log.")

(defun dg-company-statistics--init ()
  "Initialize dg-company-statistics."
  (setq dg-company-statistics--scores
        (make-hash-table :test #'equal :size dg-company-statistics-size))
  (setq dg-company-statistics--log (make-vector dg-company-statistics-size nil)
        dg-company-statistics--index 0))

(defun dg-company-statistics--initialized-p ()
  (hash-table-p dg-company-statistics--scores))

(defun dg-company-statistics--log-resize (_option new-size)
  (when (dg-company-statistics--initialized-p)
    ;; hash scoresheet auto-resizes, but log does not
    (let ((new-hist (make-vector new-size nil))
          ;; use actual length, to also work for freshly restored stats
          (dg-company-statistics-size (length dg-company-statistics--log)))
      ;; copy newest entries (possibly nil) to new-hist
      (dolist (i (number-sequence 0 (1- (min new-size dg-company-statistics-size))))
        (let ((old-i (mod (+ (- dg-company-statistics--index new-size) i)
                          dg-company-statistics-size)))
          (aset new-hist i (aref dg-company-statistics--log old-i))))
      ;; remove discarded log entry (when shrinking) from scores
      (when (< new-size dg-company-statistics-size)
        (dolist (i (number-sequence
                    dg-company-statistics--index
                    (+ dg-company-statistics-size
                       dg-company-statistics--index
                       (1- new-size))))
          (dg-company-statistics--log-revert (mod i dg-company-statistics-size))))
      (setq dg-company-statistics--log new-hist)
      (setq dg-company-statistics--index (if (<= new-size dg-company-statistics-size)
                                          0
                                        dg-company-statistics-size))))
  (setq dg-company-statistics-size new-size))

(defun dg-company-statistics--save ()
  "Save statistics."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let (print-level print-length)
      (encode-coding-string
       (format
        "%S"
        `(setq
          dg-company-statistics--scores ,dg-company-statistics--scores
          dg-company-statistics--log ,dg-company-statistics--log
          dg-company-statistics--index ,dg-company-statistics--index))
       'utf-8 nil (current-buffer))
      (let ((coding-system-for-write 'binary))
        (write-region nil nil dg-company-statistics-file)))))

(defun dg-company-statistics--maybe-save ()
  (when (and (dg-company-statistics--initialized-p)
             dg-company-statistics-auto-save)
    (dg-company-statistics--save)))

(defun dg-company-statistics--load ()
  "Restore statistics."
  (load dg-company-statistics-file 'noerror nil 'nosuffix))

;; score calculation for insert/retrieval --- can be changed on-the-fly

(defvar dg-company-statistics--context nil
  "Current completion context, a list of entries searched using `assoc'.")

(defvar dg-company-statistics--override-context nil
  "Optional context intended to be manually set for debugging/testing purposes.")

;; NOTE: There seem to be cases where dg-company-statistics--context will not be
;; updated in time and the score calc will be on the basis of an older context.
;; Therefore, we'll use a function to access it, which will recapture it if it
;; looks like an update is needed.
(defvar dg-company-statistics--previous-context-id nil)

(defun dg-company-statistics--get-context-current-id ()
  "Identifies the position with which the current context is associated."
  (point))

(defun dg-company-statistics--get-context (&optional force-refresh)
  (or dg-company-statistics--override-context
      (let ((curid (dg-company-statistics--get-context-current-id)))
        (when (or force-refresh
                  (not (equal curid dg-company-statistics--previous-context-id)))
          (dg-company-statistics-capture-context)
          (setq dg-company-statistics--previous-context-id curid))
        dg-company-statistics--context)))

(defun dg-company-statistics--last-keyword-ctx ()
  "Return last keyword, ie, text of region fontified with the
font-lock-keyword-face up to point, or nil."
  (let ((face-pos (point)))
    (while (and (number-or-marker-p face-pos)
                (< (point-min) face-pos)
                (not (eq (get-text-property (1- face-pos) 'face)
                         'font-lock-keyword-face)))
      (setq face-pos
            (previous-single-property-change face-pos 'face nil (point-min))))
    (when (and (number-or-marker-p face-pos)
               (eq (get-text-property (max (point-min) (1- face-pos)) 'face)
                   'font-lock-keyword-face))
      (buffer-substring-no-properties
       (previous-single-property-change face-pos 'face nil (point-min))
       face-pos))))

(defun dg-company-statistics--parent-symbol-ctx ()
  "Return symbol immediately preceding current completion prefix, or nil.
May be separated by punctuation, but not by whitespace."
  ;; expects to be at start of company-prefix; little sense for lisps
  (let ((preceding (save-excursion
                     (unless (zerop (skip-syntax-backward "."))
                       (substring-no-properties (symbol-name (symbol-at-point)))))))
    preceding))

(defun dg-company-statistics-score-change (_cand)
  "Score for candidate determined using the current `dg-company-statistics-features'."
  (let* ((context (dg-company-statistics--get-context))
         (changed (mapcar (lambda (feat)
                            (let* ((key (car feat))
                                   (cfg (cdr feat))
                                   (incr-expr (cdr (assoc :incr cfg)))
                                   (incr (or (eval incr-expr) 1))
                                   (ctx (assoc key context)))
                              (when (and ctx (> incr 0))
                                (cons ctx incr))))
                          dg-company-statistics-features))
         (changed-no-nils (delq nil changed))
         (changed-totals (mapcar (lambda (z)
                                   (let* ((pair (car z))
                                          (name (car pair))
                                          (incr (cdr z))
                                          (name-total-key
                                           (list name 'TOTAL)))
                                     (cons name-total-key incr)))
                                 changed-no-nils)))
    (append changed-no-nils changed-totals)))

(defun dg-company-statistics-capture-context (&optional _manual)
  (save-excursion
    (backward-char (length company-prefix))
    (let* ((ctx (mapcar (lambda (feat)
                          (let* ((key (car feat))
                                 (get-ctx-form (cdr (assoc :get-context feat)))
                                 (value (eval get-ctx-form)))
                            (when value
                              (list key value))))
                        dg-company-statistics-features))
           (ctx-no-nils (delq nil ctx)))
      (setq dg-company-statistics--context ctx-no-nils)
      ctx-no-nils)))

(defun dg-company-statistics--score-calc-components (cand &optional using-company-prefix)
  "Helper function for `dg-company-statistics-score-calc' that constructs a list containing
score information for each feature."
  (setq using-company-prefix (or using-company-prefix company-prefix))
  (let* ((company-prefix using-company-prefix)
         (context (dg-company-statistics--get-context))
         (scores (gethash cand dg-company-statistics--scores))
         (cand-score 0)
         (retval nil))
    (dolist (f dg-company-statistics-features)
      (let* ((feat-sym (car f))
             (feat-config (cdr f))
             (feat-weight (or (cdr (assoc :weight feat-config)) 1))
             (feat-value-assoc (assoc feat-sym context))
             (feat-value (cadr feat-value-assoc))
             (feat-score-key (list feat-sym feat-value))
             (feat-score (cdr (assoc feat-score-key scores)))
             (feat-total-score-key (list feat-sym 'TOTAL))
             (feat-total-score (cdr (assoc feat-total-score-key scores)))
             (feat-percent-score (if (and feat-total-score
                                          feat-score
                                          (> feat-total-score 0))
                                     (/ feat-score (float feat-total-score))
                                   0.0))
             (comp `((:feature . ,feat-sym)
                     (:config . ,feat-config)
                     (:score . ,feat-score)
                     (:total . ,feat-total-score)
                     (:percent . ,feat-percent-score))))
        (push comp retval)))
    retval))

(defun dg-company-statistics-score-calc (cand &optional using-company-prefix)
  (let ((retval 0)
        (score-components (dg-company-statistics--score-calc-components
                           cand using-company-prefix)))
    (dolist (comp score-components)
      (let ((comp-score (or (cdr (assoc :score comp)))))
        (setq retval (+ retval (or comp-score 0)))))
    retval))

;; score manipulation in one place --- know about hash value alist structure

(defun dg-company-statistics--alist-update (alist updates merger &optional filter)
  "Return new alist with conses from ALIST.  Their cdrs are updated
to (merger cdr update-cdr) if the UPDATES alist contains an entry with an
equal-matching car.  If FILTER called with the result is non-nil, remove
the cons from the result.  If no matching cons exists in ALIST, add the new
one.  ALIST structure and cdrs may be changed!"
  (let ((filter (or filter 'ignore))
        (updated alist)
        (new nil))
    (mapc
     (lambda (upd)
       (let ((found (assoc (car upd) alist)))
         (if found
             (let ((result (funcall merger (cdr found) (cdr upd))))
               (if (funcall filter result)
                   (setq updated (delete found updated))
                 (setcdr found result)))
           (push upd new))))
     updates)
    (nconc updated new)))

(defun dg-company-statistics--scores-add (cand score-updates)
  (puthash cand
           (dg-company-statistics--alist-update
            (gethash cand dg-company-statistics--scores)
            score-updates
            #'+)
           dg-company-statistics--scores))

(defun dg-company-statistics--log-revert (&optional index)
  "Revert score updates for log entry.  INDEX defaults to
`dg-company-statistics--index'."
  (let ((hist-entry
         (aref dg-company-statistics--log
               (or index dg-company-statistics--index))))
    (when hist-entry                    ;ignore nil entry
      (let* ((cand (car hist-entry))
             (score-updates (cdr hist-entry))
             (new-scores
              (dg-company-statistics--alist-update
               (gethash cand dg-company-statistics--scores)
               score-updates
               #'-
               #'zerop)))
        (if new-scores                    ;sth left
            (puthash cand new-scores dg-company-statistics--scores)
          (remhash cand dg-company-statistics--scores))))))

(defun dg-company-statistics--log-store (result score-updates)
  "Insert/overwrite result and associated score updates."
  (aset dg-company-statistics--log dg-company-statistics--index
        (cons result score-updates))
  (setq dg-company-statistics--index
        (mod (1+ dg-company-statistics--index) dg-company-statistics-size)))

;; core functions: updater, actual sorting transformer, minor-mode

(defun dg-company-statistics--start (manual)
  (funcall dg-company-statistics-capture-context manual))

(defun dg-company-statistics--finished (result)
  "After completion, update scores and log."
  (let* ((score-updates (funcall dg-company-statistics-score-change result))
         (result (substring-no-properties result)))
    (dg-company-statistics--scores-add result score-updates)
    (dg-company-statistics--log-revert)
    (dg-company-statistics--log-store result score-updates)))

(defun dg-company-statistics-sort-by-statistics (candidates)
  "Sort candidates by historical statistics.  Stable sort, so order is only
changed for candidates distinguishable by score."
  (setq candidates
        (sort candidates
              (lambda (cand1 cand2)
                (>  (funcall dg-company-statistics-score-calc cand1)
                    (funcall dg-company-statistics-score-calc cand2))))))

(defun dg-company-statistics--get-scores-alist (&optional scores-hash)
  "Returns the scores hash table as an alist. This function is intended to aid
debugging by making the scores easier to inspect."
  (setq scores-hash (or scores-hash dg-company-statistics--scores))
  (let ((scores-alist nil))
    (and scores-hash (maphash (lambda (key val)
                                (add-to-list 'scores-alist `(,key . ,val)))
                              scores-hash))
    scores-alist))

;;;###autoload
(define-minor-mode dg-company-statistics-mode
  "Statistical sorting for company-mode.  Ranks completion candidates by
the frequency with which they have been chosen in recent (as given by
`dg-company-statistics-size') history.

Turning this mode on and off preserves the statistics.  They are also
preserved automatically between Emacs sessions in the default
configuration.  You can customize this behavior with
`dg-company-statistics-auto-save', `dg-company-statistics-auto-restore' and
`dg-company-statistics-file'."
  nil nil nil
  :global t
  (if dg-company-statistics-mode
      (progn
        (unless (dg-company-statistics--initialized-p)
          (if (and dg-company-statistics-auto-restore
                   (dg-company-statistics--load))
              ;; maybe of different size
              (dg-company-statistics--log-resize nil dg-company-statistics-size)
            (dg-company-statistics--init)))
        (add-to-list 'company-transformers
                     'dg-company-statistics-sort-by-statistics 'append)
        (add-hook 'company-completion-started-hook
                  'dg-company-statistics--start)
        (add-hook 'company-completion-finished-hook
                  'dg-company-statistics--finished))
    (setq company-transformers
          (delq 'dg-company-statistics-sort-by-statistics company-transformers))
    (remove-hook 'company-completion-started-hook
                 'dg-company-statistics--start)
    (remove-hook 'company-completion-finished-hook
                 'dg-company-statistics--finished)))

(add-hook 'kill-emacs-hook 'dg-company-statistics--maybe-save)

(provide 'dg-company-statistics)
;;; dg-company-statistics.el ends here
