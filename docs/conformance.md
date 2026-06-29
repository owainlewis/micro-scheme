# Conformance

This file tracks progress toward R7RS-small support.

Status values:

- `todo`: not started
- `partial`: some behavior exists
- `done`: implemented and tested
- `blocked`: needs a design decision or standard clarification

## Reader

- `done`: booleans
- `partial`: numbers
  Exact integers and inexact decimal floats are implemented.
  The full R7RS numeric syntax is not implemented.
- `partial`: characters
  Single characters, `space`, `newline`, and `tab` are implemented.
- `partial`: strings
  Common escapes are implemented.
  Full R7RS escape syntax is not implemented.
- `partial`: symbols
  Plain symbols and vertical-bar symbols are implemented.
  Full identifier syntax is not implemented.
- `done`: lists and dotted lists
- `done`: vectors
- `done`: bytevectors
- `done`: quote syntax
- `done`: quasiquote syntax
- `done`: comments
  Line comments, nested block comments, and datum comments are implemented.
- `todo`: datum labels if implemented

## Core Evaluation

- `done`: variable references
- `done`: literal evaluation
- `done`: procedure calls
- `done`: lexical environments
- `done`: closures
- `partial`: proper tail recursion
  Tail-recursive Scheme loops are tested with 20,000 calls.
  The evaluator is not yet proven against all R7RS tail contexts.
- `partial`: internal definitions
  Sequential internal `define` works.
  R7RS internal definition semantics are not complete.
- `done`: mutation with `set!`

## Special Forms And Derived Expressions

- `done`: `quote`
- `done`: `lambda`
- `done`: `if`
- `done`: `set!`
- `done`: `define`
- `done`: `begin`
- `done`: `cond`
- `done`: `case`
- `done`: `and`
- `done`: `or`
- `done`: `when`
- `done`: `unless`
- `done`: `let`
- `done`: `let*`
- `done`: `letrec`
- `done`: `letrec*`
- `todo`: `let-values`
- `todo`: `let*-values`
- `done`: `do`
- `done`: `delay`
- `partial`: `delay-force`
  It currently behaves like `delay`.
- `todo`: `parameterize`
- `todo`: `guard`
- `partial`: quasiquote
  Basic unquote and list splicing are implemented.

## Macros

- `todo`: `define-syntax`
- `todo`: `let-syntax`
- `todo`: `letrec-syntax`
- `todo`: `syntax-rules`
- `todo`: hygiene
- `todo`: ellipsis handling

## Libraries

- `todo`: `define-library`
- `partial`: `import`
  It is accepted as a no-op for currently built-in procedures.
- `todo`: `export`
- `partial`: `(scheme base)`
  A useful subset is built into the initial environment.
- `todo`: `(scheme case-lambda)`
- `partial`: `(scheme char)`
  Character values and `char?` are implemented.
- `todo`: `(scheme complex)`
- `partial`: `(scheme cxr)`
  Only `car` and `cdr` are implemented.
- `todo`: `(scheme eval)`
- `todo`: `(scheme file)`
- `todo`: `(scheme inexact)`
- `todo`: `(scheme lazy)`
- `partial`: `(scheme load)`
  File execution exists in the CLI.
- `todo`: `(scheme process-context)`
- `todo`: `(scheme read)`
- `todo`: `(scheme repl)`
- `todo`: `(scheme time)`
- `partial`: `(scheme write)`
  `write`, `display`, and `newline` are implemented.

## Runtime Values

- `done`: booleans
- `partial`: numbers
- `done`: characters
- `done`: strings
- `done`: symbols
- `done`: pairs
- `done`: lists
- `done`: vectors
- `done`: bytevectors
- `done`: procedures
- `todo`: ports
- `partial`: promises
- `partial`: errors

## Standard Procedures

- `partial`: equivalence predicates
  `eq?`, `eqv?`, and `equal?` are implemented.
- `partial`: numbers
  Basic arithmetic, comparisons, `number?`, `integer?`, and `zero?` are implemented.
- `partial`: booleans
  `not` and `boolean?` are implemented.
- `partial`: pairs and lists
  Common constructors, selectors, mutation, predicates, and list utilities are implemented.
- `partial`: symbols
  `symbol?`, `symbol->string`, and `string->symbol` are implemented.
- `partial`: characters
  `char?` is implemented.
- `partial`: strings
  `string?`, `string-length`, `string-append`, and `string-ref` are implemented.
- `partial`: vectors
  Basic vector creation, conversion, length, ref, and mutation are implemented.
- `partial`: bytevectors
  Basic bytevector creation, length, ref, and mutation are implemented.
- `partial`: control features
  `force` is implemented.
- `todo`: exceptions
- `todo`: environments and evaluation
- `partial`: input and output
  Simple output procedures are implemented.
- `todo`: system interface

## Notes

The current implementation is a useful tested subset, not full R7RS-small.

Verification:

```bash
dune test
```
