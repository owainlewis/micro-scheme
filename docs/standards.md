# Standards

This project should implement Scheme from standards, not from memory.

## Primary Standard

### R7RS-small

R7RS-small is the primary implementation target.

References:

- R7RS-small homepage: https://small.r7rs.org/
- Corrected R7RS HTML: https://standards.scheme.org/corrected-r7rs/r7rs.html
- Corrected R7RS PDF: https://standards.scheme.org/corrected-r7rs/r7rs.pdf

Use this standard for language behavior, syntax, libraries, and terminology.

## Comparison Standards

### R5RS

R5RS is useful for classic Scheme semantics and historical comparison.

Reference:

- R5RS HTML: https://schemers.org/Documents/Standards/R5RS/HTML/

### R6RS

R6RS is larger and more library-heavy.

It is not the first implementation target.

It can guide future work after R7RS-small is stable.

Reference:

- R6RS homepage: https://www.r6rs.org/

## Reference Implementations

Use mature Scheme implementations to compare behavior when the standard is subtle.

Suggested references:

- Chibi Scheme
- Gauche
- Chez Scheme
- Racket

Reference behavior is useful evidence, but the standard wins when there is a conflict.

## Compliance Policy

Do not mark a feature complete unless:

- the relevant standard behavior is understood
- tests cover normal behavior
- tests cover at least one error or edge case when meaningful
- implementation limitations are documented

If behavior differs from a reference implementation, record the reason in `docs/conformance.md`.
