# SQLLogicTest Mode for Emacs

A major mode for editing sqllogictest (.slt) files with syntax highlighting.

## Features

- **Syntax highlighting** for SQL keywords, functions, strings, numbers, and comments
- **Query command highlighting** for `query <types>` lines
- **Result separator highlighting** for `----` lines
- **Comment support** for lines starting with `#`
- **Auto-detection** of `.slt` files

## Installation

### Manual Installation

1. Copy `sqllogictest-mode.el` to your Emacs load path (e.g., `~/.emacs.d/`)

2. Add to your Emacs configuration (`~/.emacs` or `~/.emacs.d/init.el`):

```elisp
(load "sqllogictest-mode")
```

### Using package managers

#### use-package
```elisp
(use-package sqllogictest-mode
  :load-path "path/to/sqllogictest-mode.el"
  :mode "\\.slt\\'")
```

## Usage

The mode will automatically activate when opening `.slt` files. You can also manually activate it with:

```
M-x sqllogictest-mode
```

## Syntax Highlighting

The mode provides highlighting for:

- **Comments**: Lines starting with `#`
- **Query commands**: `query TTTT`, `query ITT`, etc.
- **SQL keywords**: SELECT, FROM, WHERE, ORDER BY, etc.
- **SQL functions**: COUNT, SUM, COALESCE, etc.
- **Result separators**: `----` lines
- **String literals**: Single and double quoted strings
- **Numbers**: Integer and decimal numbers
- **Database identifiers**: Table and column names

## Example

```sql
# Test basic retail data access

# Select * from dim_persona
query TTTT
SELECT * FROM dim_persona ORDER BY persona_key
----
CUST001 John Smith
CUST002 Jane Johnson
```
