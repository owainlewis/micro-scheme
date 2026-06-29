# Implementation Plan

This is a plan.

It should change as the interpreter becomes real.

## Architecture

Use this pipeline:

```text
source text
  -> reader
  -> datum
  -> expander
  -> core AST
  -> evaluator
  -> runtime value
```

## Suggested OCaml Layout

```text
lib/
  datum.ml
  reader.ml
  value.ml
  env.ml
  ast.ml
  expand.ml
  eval.ml
  primitives.ml
  library.ml
  ports.ml

bin/
  micro_scheme.ml

test/
  reader_tests.ml
  eval_tests.ml
  conformance_tests.ml
```

## Milestones

### M0: Project Skeleton

- Dune project.
- CLI executable.
- Test runner.
- Minimal README.
- CI.

### M1: Reader

- Parse R7RS datums.
- Print datums.
- Add golden tests.

### M2: Core Evaluator

- Evaluate literals.
- Resolve variables.
- Apply primitive procedures.
- Create closures.
- Support lexical scope.

### M3: Essential Forms

- `quote`
- `if`
- `lambda`
- `define`
- `set!`
- `begin`
- procedure calls

### M4: Data And Procedures

- pairs
- lists
- symbols
- booleans
- numbers
- strings
- vectors
- equality

### M5: Tail Calls

- Make proper tail recursion explicit in the evaluator design.
- Add tests that would overflow without tail calls.

### M6: Derived Forms

- Implement derived forms directly or through expansion.
- Cover `let`, `let*`, `letrec`, `cond`, `case`, `and`, `or`, `do`, and quasiquote.

### M7: Macros

- Implement `syntax-rules`.
- Preserve hygiene.
- Add macro expansion tests.

### M8: Libraries

- Implement `define-library`.
- Load R7RS-small standard libraries.
- Add import and export tests.

### M9: Useful Programs

- Run real Scheme examples.
- Add examples under `examples/`.
- Document what works.

## Testing Strategy

Every feature starts with tests.

Prefer small tests tied to a standard section.

Use reference Scheme implementations for behavior checks when useful.

Keep tests deterministic.

## PR Strategy

Open small PRs for early milestones.

After the skeleton is stable, each PR should map to a section of `docs/conformance.md`.
