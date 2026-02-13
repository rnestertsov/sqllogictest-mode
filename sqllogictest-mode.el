;;; sqllogictest-mode.el --- Major mode for sqllogictest files -*- lexical-binding: t -*-

;; Author: Roman Nestertsov
;; Version: 1.0
;; Keywords: languages, sql, testing

;;; Commentary:

;; This mode provides syntax highlighting for sqllogictest files (.slt).
;; sqllogictest files contain:
;; - Comments starting with #
;; - Query commands: "query <types>"
;; - SQL statements
;; - Expected results separated by ----
;; - Result data

;;; Code:

(defvar sqllogictest-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments start with #
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; SQL string literals
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    table)
  "Syntax table for sqllogictest-mode.")

(defconst sqllogictest-font-lock-keywords
  `(
    ;; Comments (lines starting with #)
    ("^#.*$" . font-lock-comment-face)

    ;; Query command lines: query <types> [sort] [label]
    ("^\\(query\\)\\s-+\\([A-Za-z]+\\)\\(?:\\s-+\\([a-zA-Z]+\\)\\)?\\(?:\\s-+\\(\\S-+\\)\\)?\\s-*$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-type-face nil t)
     (4 font-lock-variable-name-face nil t))

    ;; Statement command lines: statement <ok|error|count> [N]
    ("^\\(statement\\)\\s-+\\([A-Za-z]+\\)\\(?:\\s-+\\([0-9]+\\)\\)?\\s-*$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-constant-face nil t))


    ;; Conditional directives: skipif/onlyif <engine>
    ("^\\(skipif\\|onlyif\\)\\s-+\\(\\S-+\\)\\s-*$"
     (1 font-lock-preprocessor-face)
     (2 font-lock-string-face))

    ;; Control records
    ("^\\(halt\\)\\s-*$" (1 font-lock-warning-face))
    ("^\\(hash-threshold\\)\\s-+\\([0-9]+\\)\\s-*$"
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))

    ;; Result separator
    ("^----\\s-*$" . font-lock-builtin-face)

    ;; SQL keywords (case insensitive, with proper word boundaries)
    (,(concat "\\(?:^\\|[^a-zA-Z0-9_]\\)\\("
              (regexp-opt '("SELECT" "FROM" "WHERE" "ORDER" "BY" "GROUP" "HAVING"
                           "JOIN" "LEFT" "RIGHT" "INNER" "OUTER" "CROSS" "ON" "AS"
                           "INSERT" "UPDATE" "DELETE" "CREATE" "DROP" "ALTER"
                           "TABLE" "INDEX" "VIEW" "DATABASE" "SCHEMA"
                           "AND" "OR" "NOT" "NULL" "IS" "IN" "EXISTS"
                           "BETWEEN" "LIKE" "DISTINCT" "ALL" "ANY" "UNION"
                           "EXCEPT" "INTERSECT" "CASE" "WHEN" "THEN" "ELSE" "END"
                           "LIMIT" "OFFSET" "ASC" "DESC"))
              "\\)\\(?:[^a-zA-Z0-9_]\\|$\\)")
     (1 font-lock-keyword-face))

    ;; SQL functions (with proper word boundaries)
    (,(concat "\\(?:^\\|[^a-zA-Z0-9_]\\)\\("
              (regexp-opt '("COUNT" "SUM" "AVG" "MIN" "MAX" "COALESCE" "NULLIF"
                           "CAST" "CONVERT" "SUBSTRING" "LOWER" "UPPER" "TRIM"
                           "LENGTH" "ROUND" "ABS" "CEIL" "FLOOR" "CEILING" "NOW" "DATE"
                           "YEAR" "MONTH" "DAY"))
              "\\)\\(?:[^a-zA-Z0-9_]\\|$\\)")
     (1 font-lock-function-name-face))

    ;; Numbers
    ("\\b[0-9]+\\.?[0-9]*\\b" . font-lock-constant-face)

    ;; String literals
    ("'[^']*'" . font-lock-string-face)
    ("\"[^\"]*\"" . font-lock-string-face)

    )
  "Keyword highlighting specification for sqllogictest-mode.")

(defvar sqllogictest-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for sqllogictest-mode.")

(defun sqllogictest-extend-region ()
  "Extend the font-lock region to include complete test blocks.
This ensures that multiline constructs (query/statement + SQL + results)
are fontified as a unit."
  (save-excursion
    (goto-char font-lock-beg)
    ;; Go back to the previous comment line (test boundary)
    (while (and (not (bobp))
                (not (looking-at "^#")))
      (forward-line -1))
    (setq font-lock-beg (point))

    (goto-char font-lock-end)
    ;; Go forward to the next comment line or end of buffer
    (while (and (not (eobp))
                (not (looking-at "^#")))
      (forward-line 1))
    (setq font-lock-end (point)))
  nil)

;;;###autoload
(define-derived-mode sqllogictest-mode fundamental-mode "SQLLogicTest"
  "Major mode for editing sqllogictest files.

sqllogictest files contain test cases for SQL engines with:
- Comments starting with #
- Query commands specifying expected column types
- SQL SELECT statements
- Expected results separated by ----

\\{sqllogictest-mode-map}"
  :syntax-table sqllogictest-mode-syntax-table

  ;; Font lock
  (setq-local font-lock-defaults '(sqllogictest-font-lock-keywords nil t))

  ;; Add multiline font-lock support
  (add-hook 'font-lock-extend-region-functions
            #'sqllogictest-extend-region nil t)

  ;; Comment syntax
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")

  ;; Indentation (simple, no special indentation rules for now)
  (setq-local indent-line-function 'indent-relative)

  ;; Case folding for SQL keywords
  (setq-local font-lock-keywords-case-fold-search t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slt\\'" . sqllogictest-mode))

(provide 'sqllogictest-mode)

;;; sqllogictest-mode.el ends here
