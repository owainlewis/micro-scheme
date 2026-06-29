# micro-scheme

`micro-scheme` is an OCaml project to build a complete, tested Scheme interpreter from scratch.

The first target is R7RS-small Scheme.

The project is agent-first.
Agents should work from the standard, add tests for each language feature, implement the feature, and update the conformance record.

## Start Here

- [Mission](docs/mission.md)
- [Standards](docs/standards.md)
- [Conformance](docs/conformance.md)
- [Implementation Plan](docs/implementation-plan.md)

## Setup

Install OCaml and Dune.

```bash
opam install dune
dune build
dune test
```

## Usage

Run one expression.

```bash
dune exec micro-scheme -- -e "(+ 1 2 3)"
```

Run a Scheme file.

```bash
dune exec micro-scheme -- examples/factorial.scm
```

Start the REPL.

```bash
dune exec micro-scheme
```

## Examples

```scheme
(define (fact n acc)
  (if (= n 0)
      acc
      (fact (- n 1) (* acc n))))

(fact 6 1)
```

```scheme
(let loop ((n 10) (acc 0))
  (if (= n 0)
      acc
      (loop (- n 1) (+ acc n))))
```

## Current Status

The repository has a working OCaml and Dune interpreter.

Implemented areas include:

- reader support for booleans, numbers, characters, strings, symbols, lists, dotted lists, vectors, bytevectors, quote syntax, quasiquote syntax, and comments
- evaluation for variables, literals, procedure calls, lexical closures, mutation, and a tested tail-recursive loop
- core forms including `quote`, `if`, `lambda`, `define`, `set!`, `begin`, `let`, `let*`, `letrec`, `cond`, `case`, `and`, `or`, `when`, `unless`, `delay`, `delay-force`, `quasiquote`, and `do`
- useful base procedures for numbers, booleans, pairs, lists, symbols, strings, vectors, bytevectors, promises, and simple output
- CLI, REPL, examples, and a no-dependency test suite

Not implemented yet:

- hygienic macros and `syntax-rules`
- full R7RS library loading and `define-library`
- complete numeric tower, ports, exceptions, parameters, records, and exact R7RS procedure coverage

See [Conformance](docs/conformance.md) for the current checklist.
