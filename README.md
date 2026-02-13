# SQLLogicTest Mode for Emacs

A major mode for editing sqllogictest (.slt) files with syntax highlighting.

## Features

- **Syntax highlighting** for SQL keywords, functions, strings, numbers, and comments
- **Query command highlighting** for `query <types> [sort] [label]` lines
- **Statement command highlighting** for `statement ok|error|count` lines
- **Conditional directives** for `skipif` and `onlyif`
- **Control records** for `halt` and `hash-threshold`
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
- **Query commands**: `query TTTT`, `query ITT rowsort`, `query I nosort label1`, etc.
- **Statement commands**: `statement ok`, `statement error`, `statement count 5`
- **Conditional directives**: `skipif postgresql`, `onlyif mysql`
- **Control records**: `halt`, `hash-threshold 100`
- **SQL keywords**: SELECT, FROM, WHERE, ORDER BY, CREATE TABLE, INSERT INTO, etc.
- **SQL functions**: COUNT, SUM, ABS, CEIL, FLOOR, COALESCE, etc.
- **Result separators**: `----` lines
- **String literals**: Single and double quoted strings
- **Numbers**: Integer and decimal numbers

## Testing

The project includes ERT tests for all font-lock rules. Run them with:

```bash
make test
```

This runs Emacs in batch mode and executes all tests defined in `sqllogictest-mode-test.el`.

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
