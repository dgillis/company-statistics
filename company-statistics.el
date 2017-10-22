;;; company-statistics.el --- Sort candidates using completion history  -*- lexical-binding: t -*-

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
;;   (add-hook 'after-init-hook #'company-statistics-mode)
;;
;; Manually installed: make sure that this file is in load-path, and
;;
;;   (require 'company-statistics)
;;   (company-statistics-mode)
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

(defgroup company-statistics nil
  "Completion candidates ranking by historical statistics."
  :group 'company)

(defcustom company-statistics-size 400
  "Number of completion choices that `company-statistics' keeps track of.
As this is a global cache, making it too small defeats the purpose."
  :type 'integer
  :initialize #'custom-initialize-default
  :set #'company-statistics--log-resize)

(defcustom company-statistics-file
  (concat user-emacs-directory "company-statistics-cache.el")
  "File to save company-statistics state."
  :type 'string)

(defcustom company-statistics-auto-save t
  "Whether to save the statistics when leaving emacs."
  :type 'boolean)

(defcustom company-statistics-auto-restore t
  "Whether to restore statistics when company-statistics is enabled and has
not been used before."
  :type 'boolean)

(defcustom company-statistics-capture-context #'company-statistics-capture-context
  "Function called with single argument (t if completion started manually).
This is the place to store any context information for a completion run."
  :type 'function)

(defcustom company-statistics-score-change #'company-statistics-score-change
  "Function called with completion choice.  Using arbitrary other info,
it should produce an alist, each entry labeling a context and the
associated score update: ((ctx-a . 1) (\"str\" . 0.5) (nil . 1)).  Nil is
the global context."
  :type 'function)

(defcustom company-statistics-score-calc #'company-statistics-score-calc
  "Function called with completion candidate.  Using arbitrary other info,
eg, on the current context, it should evaluate to the candidate's score (a
number)."
  :type 'function)

(defvar company-statistics-features nil)

(defconst company-statistics-default-features-heavy
  '((keyword (:get-context . (dg-company-statistics--last-keyword-ctx)))
    (symbol (:get-context . (dg-company-statistics--parent-symbol-ctx)))
    (file (:get-context . buffer-file-name))
    (major-mode (:get-context . major-mode))
    (global (:get-context . t))))

(defconst company-statistics-default-features-light
  '((global (:get-context . t))
    (major-mode (:get-context . major-mode))))

(setq company-statistics-features company-statistics-default-features-heavy)

;; internal vars, persistence

(defvar company-statistics-feature-scores-alist nil
  "This variable will be found to an alist of (FEATURE . SCORE) elements
during the evaluation of `company-statistics-score-calc-expr'.")

(defvar company-statistics--scores nil
  "Store selection frequency of candidates in given contexts.")

(defvar company-statistics--log nil
  "Ring keeping a log of statistics updates.")

(defvar company-statistics--index nil
  "Index into the log.")

(defun company-statistics--init ()
  "Initialize company-statistics."
  (setq company-statistics--scores
        (make-hash-table :test #'equal :size company-statistics-size))
  (setq company-statistics--log (make-vector company-statistics-size nil)
        company-statistics--index 0))

(defun company-statistics--initialized-p ()
  (hash-table-p company-statistics--scores))

(defun company-statistics--log-resize (_option new-size)
  (when (company-statistics--initialized-p)
    ;; hash scoresheet auto-resizes, but log does not
    (let ((new-hist (make-vector new-size nil))
          ;; use actual length, to also work for freshly restored stats
          (company-statistics-size (length company-statistics--log)))
      ;; copy newest entries (possibly nil) to new-hist
      (dolist (i (number-sequence 0 (1- (min new-size company-statistics-size))))
        (let ((old-i (mod (+ (- company-statistics--index new-size) i)
                          company-statistics-size)))
          (aset new-hist i (aref company-statistics--log old-i))))
      ;; remove discarded log entry (when shrinking) from scores
      (when (< new-size company-statistics-size)
        (dolist (i (number-sequence
                    company-statistics--index
                    (+ company-statistics-size
                       company-statistics--index
                       (1- new-size))))
          (company-statistics--log-revert (mod i company-statistics-size))))
      (setq company-statistics--log new-hist)
      (setq company-statistics--index (if (<= new-size company-statistics-size)
                                          0
                                        company-statistics-size))))
  (setq company-statistics-size new-size))

(defun company-statistics--save ()
  "Save statistics."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let (print-level print-length)
      (encode-coding-string
       (format
        "%S"
        `(setq
          company-statistics--scores ,company-statistics--scores
          company-statistics--log ,company-statistics--log
          company-statistics--index ,company-statistics--index))
       'utf-8 nil (current-buffer))
      (let ((coding-system-for-write 'binary))
        (write-region nil nil company-statistics-file)))))

(defun company-statistics--maybe-save ()
  (when (and (company-statistics--initialized-p)
             company-statistics-auto-save)
    (company-statistics--save)))

(defun company-statistics--load ()
  "Restore statistics."
  (load company-statistics-file 'noerror nil 'nosuffix))

;; score calculation for insert/retrieval --- can be changed on-the-fly

(defvar company-statistics--context nil
  "Current completion context, a list of entries searched using `assoc'.")

(defvar company-statistics--override-context nil
  "Optional context intended to be manually set for debugging/testing purposes.")

;; NOTE: There seem to be cases where company-statistics--context will not be
;; updated in time and the score calc will be on the basis of an older context.
;; Therefore, we'll use a function to access it, which will recapture it if it
;; looks like an update is needed.
(defvar company-statistics--previous-context-id nil)

(defun company-statistics--get-context-current-id ()
  "Identifies the position with which the current context is associated."
  (point))

(defun company-statistics--get-context (&optional force-refresh)
  (or company-statistics--override-context
      (let ((curid (company-statistics--get-context-current-id)))
        (when (or force-refresh
                  (not (equal curid company-statistics--previous-context-id)))
          (company-statistics-capture-context)
          (setq company-statistics--previous-context-id curid))
        company-statistics--context)))

(defun company-statistics--last-keyword-ctx ()
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

(defun company-statistics--parent-symbol-ctx ()
  "Return symbol immediately preceding current completion prefix, or nil.
May be separated by punctuation, but not by whitespace."
  ;; expects to be at start of company-prefix; little sense for lisps
  (let ((preceding (save-excursion
                     (unless (zerop (skip-syntax-backward "."))
                       (substring-no-properties (symbol-name (symbol-at-point)))))))
    preceding))

(defun company-statistics-score-change (_cand)
  "Score for candidate determined using the current `company-statistics-features'."
  (let* ((context (company-statistics--get-context))
         (changed (mapcar (lambda (feat)
                            (let* ((key (car feat))
                                   (cfg (cdr feat))
                                   (incr-expr (cdr (assoc :incr cfg)))
                                   (incr (or (eval incr-expr) 1))
                                   (ctx (assoc key context)))
                              (when (and ctx (> incr 0))
                                (cons ctx incr))))
                          company-statistics-features)))
    (delq nil changed)))

(defun company-statistics-capture-context (&optional _manual)
  (save-excursion
    (backward-char (length company-prefix))
    (let* ((ctx (mapcar (lambda (feat)
                          (let* ((key (car feat))
                                 (get-ctx-form (cdr (assoc :get-context feat)))
                                 (value (eval get-ctx-form)))
                            (when value
                              (list key value))))
                        company-statistics-features))
           (ctx-no-nils (delq nil ctx)))
      (setq company-statistics--context ctx-no-nils)
      ctx-no-nils)))

(defun company-statistics-score-calc (cand &optional using-company-prefix)
  (setq using-company-prefix (or using-company-prefix company-prefix))
  (let* ((company-prefix using-company-prefix)
         (context (company-statistics--get-context))
         (scores (gethash cand company-statistics--scores))
         (cand-score 0))
    (dolist (f company-statistics-features)
      (let* ((feat-sym (car f))
             (feat-config (cdr f))
             (feat-weight (or (cdr (assoc :weight feat-config)) 1))
             (feat-value-assoc (assoc feat-sym context))
             (feat-value (cadr feat-value-assoc))
             (feat-score-key (list feat-sym feat-value))
             (feat-score (cdr (assoc feat-score-key scores))))
        (when feat-score
          (setq cand-score (+ cand-score (* feat-score feat-weight))))))
    cand-score))

;; score manipulation in one place --- know about hash value alist structure

(defun company-statistics--alist-update (alist updates merger &optional filter)
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

(defun company-statistics--scores-add (cand score-updates)
  (puthash cand
           (company-statistics--alist-update
            (gethash cand company-statistics--scores)
            score-updates
            #'+)
           company-statistics--scores))

(defun company-statistics--log-revert (&optional index)
  "Revert score updates for log entry.  INDEX defaults to
`company-statistics--index'."
  (let ((hist-entry
         (aref company-statistics--log
               (or index company-statistics--index))))
    (when hist-entry                    ;ignore nil entry
      (let* ((cand (car hist-entry))
             (score-updates (cdr hist-entry))
             (new-scores
              (company-statistics--alist-update
               (gethash cand company-statistics--scores)
               score-updates
               #'-
               #'zerop)))
        (if new-scores                    ;sth left
            (puthash cand new-scores company-statistics--scores)
          (remhash cand company-statistics--scores))))))

(defun company-statistics--log-store (result score-updates)
  "Insert/overwrite result and associated score updates."
  (aset company-statistics--log company-statistics--index
        (cons result score-updates))
  (setq company-statistics--index
        (mod (1+ company-statistics--index) company-statistics-size)))

;; core functions: updater, actual sorting transformer, minor-mode

(defun company-statistics--start (manual)
  (funcall company-statistics-capture-context manual))

(defun company-statistics--finished (result)
  "After completion, update scores and log."
  (let* ((score-updates (funcall company-statistics-score-change result))
         (result (substring-no-properties result)))
    (company-statistics--scores-add result score-updates)
    (company-statistics--log-revert)
    (company-statistics--log-store result score-updates)))

(defun company-sort-by-statistics (candidates)
  "Sort candidates by historical statistics.  Stable sort, so order is only
changed for candidates distinguishable by score."
  (setq candidates
        (sort candidates
              (lambda (cand1 cand2)
                (>  (funcall company-statistics-score-calc cand1)
                    (funcall company-statistics-score-calc cand2))))))

(defun company-statistics--get-scores-alist (&optional scores-hash)
  "Returns the scores hash table as an alist. This function is intended to aid
debugging by making the scores easier to inspect."
  (setq scores-hash (or scores-hash company-statistics--scores))
  (let ((scores-alist nil))
    (and scores-hash (maphash (lambda (key val)
                                (add-to-list 'scores-alist `(,key . ,val)))
                              scores-hash))
    scores-alist))

;;;###autoload
(define-minor-mode company-statistics-mode
  "Statistical sorting for company-mode.  Ranks completion candidates by
the frequency with which they have been chosen in recent (as given by
`company-statistics-size') history.

Turning this mode on and off preserves the statistics.  They are also
preserved automatically between Emacs sessions in the default
configuration.  You can customize this behavior with
`company-statistics-auto-save', `company-statistics-auto-restore' and
`company-statistics-file'."
  nil nil nil
  :global t
  (if company-statistics-mode
      (progn
        (unless (company-statistics--initialized-p)
          (if (and company-statistics-auto-restore
                   (company-statistics--load))
              ;; maybe of different size
              (company-statistics--log-resize nil company-statistics-size)
            (company-statistics--init)))
        (add-to-list 'company-transformers
                     'company-sort-by-statistics 'append)
        (add-hook 'company-completion-started-hook
                  'company-statistics--start)
        (add-hook 'company-completion-finished-hook
                  'company-statistics--finished))
    (setq company-transformers
          (delq 'company-sort-by-statistics company-transformers))
    (remove-hook 'company-completion-started-hook
                 'company-statistics--start)
    (remove-hook 'company-completion-finished-hook
                 'company-statistics--finished)))

(add-hook 'kill-emacs-hook 'company-statistics--maybe-save)

(provide 'company-statistics)
;;; company-statistics.el ends here
