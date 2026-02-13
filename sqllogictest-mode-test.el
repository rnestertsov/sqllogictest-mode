;;; sqllogictest-mode-test.el --- Tests for sqllogictest-mode -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests verifying font-lock highlighting for sqllogictest-mode.
;; Run with: emacs --batch -l ert -l sqllogictest-mode.el -l sqllogictest-mode-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'sqllogictest-mode)

;; ---------------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------------

(defmacro sqllogictest-with-fontified-buffer (content &rest body)
  "Insert CONTENT into a temp buffer, enable sqllogictest-mode, fontify, run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (sqllogictest-mode)
     (font-lock-ensure)
     ,@body))

(defun sqllogictest-face-at (pos)
  "Return the face at POS in the current buffer."
  (get-text-property pos 'face))

;; ---------------------------------------------------------------------------
;; Query command tests
;; ---------------------------------------------------------------------------

(ert-deftest sqllogictest-test-query-basic ()
  "Test that `query T` highlights query as function-name, T as type."
  (sqllogictest-with-fontified-buffer "query T\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-function-name-face))
    (should (eq (sqllogictest-face-at 7) 'font-lock-type-face))))

(ert-deftest sqllogictest-test-query-with-sort ()
  "Test that `query II rowsort` highlights all three parts."
  (sqllogictest-with-fontified-buffer "query II rowsort\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-function-name-face))
    (should (eq (sqllogictest-face-at 7) 'font-lock-type-face))
    (should (eq (sqllogictest-face-at 10) 'font-lock-type-face))))

(ert-deftest sqllogictest-test-query-with-label ()
  "Test that `query I nosort label1` highlights all four parts."
  (sqllogictest-with-fontified-buffer "query I nosort label1\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-function-name-face))
    (should (eq (sqllogictest-face-at 7) 'font-lock-type-face))
    (should (eq (sqllogictest-face-at 9) 'font-lock-type-face))
    (should (eq (sqllogictest-face-at 16) 'font-lock-variable-name-face))))

(ert-deftest sqllogictest-test-query-no-abort ()
  "Test that multiple query blocks all get highlighted (regression test)."
  (sqllogictest-with-fontified-buffer "query T\nSELECT 1\n----\n1\n\nquery II rowsort\nSELECT 1, 2\n----\n1 2\n"
    ;; First query block
    (should (eq (sqllogictest-face-at 1) 'font-lock-function-name-face))
    ;; Second query block — find the second "query"
    (goto-char (point-min))
    (search-forward "\nquery II")
    (let ((second-query-start (1+ (match-beginning 0))))
      (should (eq (sqllogictest-face-at second-query-start) 'font-lock-function-name-face))
      (should (eq (sqllogictest-face-at (+ second-query-start 6)) 'font-lock-type-face)))))

;; ---------------------------------------------------------------------------
;; Statement command tests
;; ---------------------------------------------------------------------------

(ert-deftest sqllogictest-test-statement-ok ()
  "Test that `statement ok` highlights both parts."
  (sqllogictest-with-fontified-buffer "statement ok\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-function-name-face))
    (should (eq (sqllogictest-face-at 11) 'font-lock-type-face))))

(ert-deftest sqllogictest-test-statement-count ()
  "Test that `statement count 5` highlights count number as constant."
  (sqllogictest-with-fontified-buffer "statement count 5\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-function-name-face))
    (should (eq (sqllogictest-face-at 11) 'font-lock-type-face))
    (should (eq (sqllogictest-face-at 17) 'font-lock-constant-face))))

;; ---------------------------------------------------------------------------
;; Conditional directive tests
;; ---------------------------------------------------------------------------

(ert-deftest sqllogictest-test-skipif ()
  "Test that `skipif postgresql` highlights as preprocessor + string."
  (sqllogictest-with-fontified-buffer "skipif postgresql\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-preprocessor-face))
    (should (eq (sqllogictest-face-at 8) 'font-lock-string-face))))

(ert-deftest sqllogictest-test-onlyif ()
  "Test that `onlyif mysql` highlights as preprocessor + string."
  (sqllogictest-with-fontified-buffer "onlyif mysql\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-preprocessor-face))
    (should (eq (sqllogictest-face-at 8) 'font-lock-string-face))))

;; ---------------------------------------------------------------------------
;; Control record tests
;; ---------------------------------------------------------------------------

(ert-deftest sqllogictest-test-halt ()
  "Test that `halt` highlights as warning."
  (sqllogictest-with-fontified-buffer "halt\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-warning-face))))

(ert-deftest sqllogictest-test-hash-threshold ()
  "Test that `hash-threshold 100` highlights as preprocessor + constant."
  (sqllogictest-with-fontified-buffer "hash-threshold 100\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-preprocessor-face))
    (should (eq (sqllogictest-face-at 16) 'font-lock-constant-face))))

;; ---------------------------------------------------------------------------
;; Separator test
;; ---------------------------------------------------------------------------

(ert-deftest sqllogictest-test-separator ()
  "Test that `----` highlights as builtin."
  (sqllogictest-with-fontified-buffer "----\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-builtin-face))))

;; ---------------------------------------------------------------------------
;; SQL keyword tests
;; ---------------------------------------------------------------------------

(ert-deftest sqllogictest-test-sql-keywords ()
  "Test that SQL keywords are highlighted as keyword face."
  (sqllogictest-with-fontified-buffer "SELECT * FROM foo WHERE x = 1\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-keyword-face))
    ;; FROM starts at position 10
    (goto-char (point-min))
    (search-forward "FROM")
    (should (eq (sqllogictest-face-at (match-beginning 0)) 'font-lock-keyword-face))
    ;; WHERE
    (search-forward "WHERE")
    (should (eq (sqllogictest-face-at (match-beginning 0)) 'font-lock-keyword-face))))

(ert-deftest sqllogictest-test-sql-keywords-case-insensitive ()
  "Test that lowercase SQL keywords are also highlighted."
  (sqllogictest-with-fontified-buffer "select * from foo where x = 1\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-keyword-face))
    (goto-char (point-min))
    (search-forward "from")
    (should (eq (sqllogictest-face-at (match-beginning 0)) 'font-lock-keyword-face))))

;; ---------------------------------------------------------------------------
;; SQL function tests
;; ---------------------------------------------------------------------------

(ert-deftest sqllogictest-test-sql-functions ()
  "Test that SQL functions are highlighted as function-name face."
  (sqllogictest-with-fontified-buffer "SELECT COUNT(*) FROM t\n"
    (goto-char (point-min))
    (search-forward "COUNT")
    (should (eq (sqllogictest-face-at (match-beginning 0)) 'font-lock-function-name-face)))
  (sqllogictest-with-fontified-buffer "SELECT ABS(x), CEIL(y), FLOOR(z) FROM t\n"
    (goto-char (point-min))
    (search-forward "ABS")
    (should (eq (sqllogictest-face-at (match-beginning 0)) 'font-lock-function-name-face))
    (search-forward "CEIL")
    (should (eq (sqllogictest-face-at (match-beginning 0)) 'font-lock-function-name-face))
    (search-forward "FLOOR")
    (should (eq (sqllogictest-face-at (match-beginning 0)) 'font-lock-function-name-face))))

;; ---------------------------------------------------------------------------
;; Number tests
;; ---------------------------------------------------------------------------

(ert-deftest sqllogictest-test-numbers ()
  "Test that numbers are highlighted as constant face."
  (sqllogictest-with-fontified-buffer "SELECT 42, 3.14\n"
    (goto-char (point-min))
    (search-forward "42")
    (should (eq (sqllogictest-face-at (match-beginning 0)) 'font-lock-constant-face))
    (search-forward "3.14")
    (should (eq (sqllogictest-face-at (match-beginning 0)) 'font-lock-constant-face))))

;; ---------------------------------------------------------------------------
;; Comment tests
;; ---------------------------------------------------------------------------

(ert-deftest sqllogictest-test-comments ()
  "Test that lines starting with # are highlighted as comments.
The # delimiter itself gets `font-lock-comment-delimiter-face',
while the rest of the comment text gets `font-lock-comment-face'."
  (sqllogictest-with-fontified-buffer "# this is a comment\n"
    (should (eq (sqllogictest-face-at 1) 'font-lock-comment-delimiter-face))
    (should (eq (sqllogictest-face-at 3) 'font-lock-comment-face))))

;;; sqllogictest-mode-test.el ends here
