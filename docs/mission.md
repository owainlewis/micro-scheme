# Mission

`micro-scheme` exists to build a fully tested Scheme interpreter from scratch using agentic development loops.

The goal is not to write a toy Lisp.

The goal is to implement a real standard with a public conformance record, useful tests, and a codebase that can run meaningful Scheme programs.

## Primary Target

The primary target is R7RS-small Scheme.

R7RS-small is small enough to implement end to end, but real enough to force proper language design.

It includes lexical scope, procedures, proper tail recursion, derived expressions, libraries, macros, numbers, strings, vectors, ports, and standard procedures.

## Why This Project Exists

This project is a better demo than another CRUD app.

It tests whether agents can:

- read a real standard
- turn prose into tests
- implement semantics incrementally
- keep a conformance document current
- compare behavior against reference implementations
- improve a codebase over many autonomous loops

## Success Criteria

The project is successful when:

- `micro-scheme` can run R7RS-small programs from files
- the REPL is useful for interactive work
- the conformance document tracks every major standard section
- each implemented feature has tests
- proper tail calls are tested
- hygienic macros are implemented or clearly marked incomplete
- standard libraries are loadable
- limitations are documented plainly

## Non-Goals For The First Milestone

- Native code generation.
- A bytecode VM.
- Full Common Lisp.
- Full R6RS.
- Performance leadership.
- A package manager.

Those can come later if the interpreter core is solid.
