# AGENTS.md

This is the canonical instruction file for agents working in `micro-scheme`.

## Mission

Build a complete, tested R7RS-small Scheme interpreter in OCaml.

The project should be able to run real Scheme programs, not only toy examples.

## Start Here

1. Read `docs/mission.md`.
2. Read `docs/standards.md`.
3. Read `docs/conformance.md`.
4. Read `docs/implementation-plan.md`.
5. Check the current code and tests before editing.

## Hard Rules

- Treat R7RS-small as the primary source of truth.
- Do not invent language behavior when the standard specifies it.
- If the standard is unclear, write down the ambiguity in `docs/conformance.md`.
- Add tests with every language feature.
- Prefer small, reviewable PRs.
- Keep documentation and conformance status in sync with code.
- Do not claim compliance until tests prove it.
- Use simple prose.
- Do not use em dashes in docs or comments.

## Implementation Defaults

- Use OCaml.
- Use Dune.
- Put library code under `lib/`.
- Put CLI or REPL code under `bin/`.
- Put tests under `test/`.
- Keep the interpreter architecture explicit:
  - reader
  - datum model
  - expander
  - core AST
  - evaluator
  - runtime values
  - standard libraries

## Verification

Before opening a PR, run the relevant local checks.

At minimum:

```bash
dune test
```

When the implementation exists, add targeted tests for:

- reader behavior
- evaluation semantics
- standard procedures
- tail calls
- macro expansion
- libraries
- error cases

## Agent Loop

For each feature:

1. Pick one unchecked item from `docs/conformance.md`.
2. Read the relevant standard section.
3. Add failing tests.
4. Implement the smallest complete behavior.
5. Run tests.
6. Update `docs/conformance.md`.
7. Open a PR.
