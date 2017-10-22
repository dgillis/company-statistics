;;; dg-company-statistics-tests.el --- dg-company-statistics tests  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015  Free Software Foundation, Inc.

;; Author: Ingo Lohmar

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
;; emacs -batch -L . -L ../company-mode/ -l ert -l dg-company-statistics-tests.el  -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(require 'dg-company-statistics)
(require 'dg-company-statistics-compat)
(setq dg-company-statistics-auto-restore nil
      dg-company-statistics-auto-save nil)

(dg-company-statistics-mode)

;;; Core

(defun my/hash-compare (h1 h2 &optional pred)
  "Check that hashes H1 and H2 use the same test, contain the same keys (as
per that test), and that their stored values agree (as per PRED, which
defaults to `equal')."
  (let ((key-test (hash-table-test h1))
        (pred (or pred 'equal)))
    (and (eq key-test (hash-table-test h2))
         (eq (hash-table-count h1) (hash-table-count h2))
         (let ((keys nil))
           (maphash (lambda (k v) (push k keys)) h1) ;get keys
           (null                                     ;expect no mismatch
            (catch 'mismatch
              (while keys               ;if this finishes, it's nil
                (let* ((k (car keys))
                       (v1 (gethash k h1))
                       (v2 (gethash k h2)))
                  (setq keys (cdr keys))
                  (unless (funcall pred v1 v2)
                    (throw 'mismatch k))))))))))

(defun my/vector-slice-compare (v1 i1 v2 i2 count &optional pred)
  "Check that COUNT vector entries of V1 (starting at index I1) and
V2 (starting at index I2) satisfy the binary predicate PRED, default
`equal'.  Wraps around if index exceeds corresponding vector length."
  (let ((pred (or pred 'equal)))
    (null
     (let ((l1 (length v1))
           (l2 (length v2)))
       (catch 'mismatch
         (dolist (i (number-sequence 0 (1- count)))
           (unless (funcall pred
                            (aref v1 (mod (+ i1 i) l1))
                            (aref v2 (mod (+ i2 i) l2)))
             (throw 'mismatch t))))))))

(defmacro cs-fixture (&rest body)
  "Set up a completion history."
  `(unwind-protect
       ;; some setup to get a completion history
       (let ((dg-company-statistics-size 5)
             (dg-company-statistics-features dg-company-statistics-default-features-heavy))
         (dg-company-statistics--init)
         (let ((dg-company-statistics--override-context
                '((global t)
                  (major-mode foo-mode)
                  (keyword "if")
                  (symbol "parent")
                  (file "foo-file"))))
           (dg-company-statistics--finished "foo"))
         (let ((dg-company-statistics--override-context
                '((global t)
                  (major-mode foo-mode)
                  (symbol "statistics")
                  (file "bar-file"))))
           (dg-company-statistics--finished "bar"))
         (let ((dg-company-statistics--override-context
                '((global t)
                  (major-mode baz-mode)
                  (keyword "unless")
                  (symbol "company"))))
           (dg-company-statistics--finished "baz"))
         (let ((dg-company-statistics--override-context
                '((global t)
                  (major-mode baz-mode)
                  (keyword "when")
                  (file "quux-file"))))
           (dg-company-statistics--finished "quux"))
         ,@body)
     ;; tear down to clean slate
     (dg-company-statistics--init)))

(defmacro cs-persistence-fixture (&rest body)
  "Check and prepare for persistence, clean up."
  `(let ((dg-company-statistics-file "./cs-test-tmp"))
     (when (and (file-exists-p dg-company-statistics-file)
                (file-writable-p dg-company-statistics-file))
       (unwind-protect
           (progn ,@body)
         ;; clean up file system
         (when (file-exists-p dg-company-statistics-file)
           (delete-file dg-company-statistics-file))))))

;; tests themselves

(ert-deftest c-s-history-resize ()
  "Test history-resize for shrinking and enlarging."
  (cs-fixture
   ;; resize several times
   (let ((cs-scores (copy-tree dg-company-statistics--scores))
         (cs-history (copy-tree dg-company-statistics--log 'vecp)))
     (dg-company-statistics--log-resize 'dummy 10)
     ;; scores unaffected?
     (should (my/hash-compare dg-company-statistics--scores cs-scores))
     ;; find all 4 old entries
     (should (my/vector-slice-compare dg-company-statistics--log
                                      (- dg-company-statistics--index 4)
                                      cs-history 0
                                      4))
     ;; index at "old-size"
     (should (equal dg-company-statistics--index 5))
     (dg-company-statistics--log-resize 'dummy 5)
     (should (my/hash-compare dg-company-statistics--scores cs-scores))
     (should (my/vector-slice-compare dg-company-statistics--log
                                      (- dg-company-statistics--index 4)
                                      cs-history 0
                                      4))
     ;; after shrink: index at 0
     (should (equal dg-company-statistics--index 0))
     ;; lose oldest entry "foo"
     (dg-company-statistics--log-resize 'dummy 3)
     ;; score should be removed
     (should-not (gethash "foo" dg-company-statistics--scores))
     ;; find *3* latest entries
     (should (my/vector-slice-compare dg-company-statistics--log
                                      (- dg-company-statistics--index 3)
                                      cs-history 1
                                      3))
     (should (equal dg-company-statistics--index 0)))))

(ert-deftest c-s-persistence ()
  "Test that all statistics are properly saved and restored."
  (cs-persistence-fixture
   (cs-fixture
    (let ((cs-scores (copy-sequence dg-company-statistics--scores))
          (cs-history (copy-sequence dg-company-statistics--log))
          (cs-index dg-company-statistics--index))
      (dg-company-statistics--save)
      (dg-company-statistics--init)        ;hence shallow copies suffice
      (dg-company-statistics--load)
      ;; (should (equal dg-company-statistics--scores cs-scores))
      (should (my/hash-compare dg-company-statistics--scores cs-scores))
      (should (equal dg-company-statistics--log cs-history))
      (should (equal dg-company-statistics--index cs-index))))))

;; (ert-deftest c-s-score-change-heavy ()
;;   "Test a few things about the heavy score updates."
;;   (let ((major-mode 'foobar-mode))
;;     (should (equal (dg-company-statistics-score-change-heavy "dummy")
;;                    '((nil . 1) (foobar-mode . 1))))
;;     (let ((dg-company-statistics--override-context
;;            '((keyword "kwd")
;;              nil                        ;deliberately omit parent symbol
;;              (file "test-file.XYZ"))))
;;       (should (equal (dg-company-statistics-score-change-heavy "dummy")
;;                      '((nil . 1) (foobar-mode . 1)
;;                        ((keyword "kwd") . 1)
;;                        ((file "test-file.XYZ") . 1)))))))

(ert-deftest c-s-score-calc-heavy ()
  "Test heavy score calculation."
  (cs-fixture
   (let ((dg-company-statistics--override-context
          '((global t)
            (major-mode foo-mode)
            (keyword nil)
            (symbol "company")
            (file "foo-file"))))
     (should (eq (dg-company-statistics-score-calc-heavy "dummy") 0))
     (should (eq (dg-company-statistics-score-calc-heavy "foo") 3))
     (should (eq (dg-company-statistics-score-calc-heavy "bar") 2))
     (should (eq (dg-company-statistics-score-calc-heavy "baz") 2))
     (should (eq (dg-company-statistics-score-calc-heavy "quux") 1)))
   (let ((dg-company-statistics--override-context
          '((global t)
            (major-mode foo-mode)
            (keyword "unless")
            (symbol "parent")
            (file "quux-file"))))
     (should (eq (dg-company-statistics-score-calc-heavy "dummy") 0))
     (should (eq (dg-company-statistics-score-calc-heavy "foo") 3))
     (should (eq (dg-company-statistics-score-calc-heavy "bar") 2))
     (should (eq (dg-company-statistics-score-calc-heavy "baz") 2))
     (should (eq (dg-company-statistics-score-calc-heavy "quux") 2)))
   (let ((dg-company-statistics--override-context
          '((global t)
            (major-mode baz-mode)
            (keyword "when")
            (symbol nil)
            (file "baz-file"))))
     (should (eq (dg-company-statistics-score-calc-heavy "dummy") 0))
     (should (eq (dg-company-statistics-score-calc-heavy "foo") 1))
     (should (eq (dg-company-statistics-score-calc-heavy "bar") 1))
     (should (eq (dg-company-statistics-score-calc-heavy "baz") 2))
     (should (eq (dg-company-statistics-score-calc-heavy "quux") 3)))
   (let ((dg-company-statistics--override-context
          '((global t)
            (major-mode baz-mode)
            (keyword "if")
            (symbol "statistics")
            (file "quux-file"))))
     (should (eq (dg-company-statistics-score-calc-heavy "dummy") 0))
     (should (eq (dg-company-statistics-score-calc-heavy "foo") 2))
     (should (eq (dg-company-statistics-score-calc-heavy "bar") 2))
     (should (eq (dg-company-statistics-score-calc-heavy "baz") 2))
     (should (eq (dg-company-statistics-score-calc-heavy "quux") 3)))))

(ert-deftest c-s-alist-update ()
  "Test central helper function for context/score alist update."
  (let ((alist '((nil . 0) ("a" . 1) ("b" . 2) ("d" . some-symbol)))
        (updates '(("a" . 1) ("c" . 3))))
    (should (equal (dg-company-statistics--alist-update alist updates '+)
                   '((nil . 0) ("a" . 2) ("b" . 2) ("d" . some-symbol) ("c" . 3)))))
  ;; filter only checks on merged, so nil entry remains, and symbol should not pose a problem:
  (let ((alist '((nil . 0) ("a" . 1) ("b" . 2) ("d" . some-symbol)))
        (updates '(("a" . 1) ("c" . 3))))
    (should (equal (dg-company-statistics--alist-update alist updates '+ 'zerop)
                   '((nil . 0) ("a" . 2) ("b" . 2) ("d" . some-symbol) ("c" . 3)))))
  (let ((alist '((nil . 0) ("a" . 1) ("b" . 2) ("d" . some-symbol)))
        (updates '(("a" . 1) ("c" . 3))))
    (should (equal (dg-company-statistics--alist-update alist updates '-)
                   '((nil . 0) ("a" . 0) ("b" . 2) ("d" . some-symbol) ("c" . 3)))))
  (let ((alist '((nil . 0) ("a" . 1) ("b" . 2) ("d" . some-symbol)))
        (updates '(("a" . 1) ("c" . 3))))
    (should (equal (dg-company-statistics--alist-update alist updates '- 'zerop)
                   '((nil . 0) ("b" . 2) ("d" . some-symbol) ("c" . 3))))))

(ert-deftest c-s-scores-add ()
  "Test adding scores."
  (cs-fixture
   ;; new entry
   (dg-company-statistics--scores-add "zufpah" '(((global t) . 27)))
   (should (equal (gethash "zufpah" dg-company-statistics--scores)
                  '(((global t) . 27))))
   ;; update existing entry
   (dg-company-statistics--scores-add "foo" '(((global t) . 2)))
   (let ((h (gethash "foo" dg-company-statistics--scores)))
     (should (equal (assoc '(global t) h) '((global t) . 3)))
     (should (equal (assoc '(major-mode foo-mode) h) '((major-mode foo-mode) . 1))))))

(ert-deftest c-s-history-revert ()
  "Test reverting a score update stored in history."
  ;; deep copies throughout!
  (cs-fixture
   ;; pointing to nil, should not change anything
   (let ((cs-scores (copy-tree dg-company-statistics--scores))
         (cs-history (copy-tree dg-company-statistics--log 'vecp))
         (cs-index dg-company-statistics--index))
     (dg-company-statistics--log-revert)
     (should (my/hash-compare dg-company-statistics--scores cs-scores))
     (should (equal dg-company-statistics--log cs-history))
     (should (equal dg-company-statistics--index cs-index))))
  (cs-fixture
   ;; remove existing item 2: should vanish from scores
   (let ((cs-scores (copy-tree dg-company-statistics--scores))
         (cs-history (copy-tree dg-company-statistics--log 'vecp))
         (cs-index dg-company-statistics--index))
     (dg-company-statistics--log-revert 2)
     (should-not (gethash "baz" dg-company-statistics--scores))
     (should (equal dg-company-statistics--log cs-history))
     (should (equal dg-company-statistics--index cs-index))))
  (cs-fixture
   ;; remove just inserted item 3 (scores should be same)
   (let ((cs-scores (copy-tree dg-company-statistics--scores))
         (cs-history (copy-tree dg-company-statistics--log 'vecp))
         (cs-index dg-company-statistics--index))
     (let ((dg-company-statistics--override-context
            '((global t)
              (major-mode extra-mode))))
       (dg-company-statistics--finished "foo")) ;adds to scores, history, index
     (dg-company-statistics--log-revert 4) ;reverts scores only, so...
     (aset cs-history 4 `("foo" ((major-mode extra-mode) . 1) ((global t) . 1) ((major-mode TOTAL) . 1) ((global TOTAL) . 1)))
     (setq cs-index (mod (1+ cs-index) dg-company-statistics-size))
     (should (my/hash-compare dg-company-statistics--scores cs-scores))
     (should (equal dg-company-statistics--log cs-history))
     (should (equal dg-company-statistics--index cs-index)))))

(ert-deftest c-s-history-store ()
  "Test insert/overwrite of history item."
  (cs-fixture
   (let ((cs-history (copy-tree dg-company-statistics--log 'vecp))
         (cs-index dg-company-statistics--index))
     ;; only changes history and index
     (dg-company-statistics--log-store "foo" '((nil . 27)))
     (aset cs-history cs-index '("foo" (nil . 27)))
     (setq cs-index 0)                  ;wraps around
     (should (equal dg-company-statistics--log cs-history))
     (should (equal dg-company-statistics--index cs-index))
     ;; now wrap around to overwrite an entry
     (dg-company-statistics--log-store "tagyok" '((bla . 42)))
     (aset cs-history cs-index '("tagyok" (bla . 42)))
     (setq cs-index 1)
     (should (equal dg-company-statistics--log cs-history))
     (should (equal dg-company-statistics--index cs-index)))))

;; test finished and sort functions?  if the above is ok, they are trivial...
