# Knotty Constitution

> Principles adapted from the [Spec-Driven Development methodology](https://github.com/github/spec-kit/blob/main/spec-driven.md).
> Articles II (CLI interface) and IX (integration databases) are not applicable to this library project and are omitted.

## Core Principles

### I. Library-First
Every feature lands in the core `knotty` crate (`src/`) before any GUI or CLI surface is updated.
New public API must be independently useful to a downstream consumer of the library.

### II. WASM-Compatible (NON-NEGOTIABLE)
All code in `src/` must compile for `wasm32-unknown-unknown`.
No `std`-only crate dependencies may be added without explicit justification.
Verify with `cargo check --target wasm32-unknown-unknown` before marking a task done.

### III. Test-First
New behaviour in `src/` requires `#[test]` coverage before or alongside the implementation — no untested public API.
New diagram operations require `insta` snapshot tests specifically.
Run `cargo insta review` to accept new snapshots before committing.
Regression tests must be added for any bug fix in `diagram.rs`, `rotate.rs`, or `moves.rs`.

### IV. Notation Fidelity
Features must preserve or extend the abbreviated knot notation format.
The verbose format and ASCII rendering are derived outputs — the abbreviated notation is the source of truth.
Specs must include example abbreviated-notation inputs and expected outputs.

### V. Minimal Dependencies
No new entry in `Cargo.toml` without justification in the spec.
Prefer `std`, `itertools`, or `regex` (already present) over new crates.
GUI-only deps belong in `examples/knot-so-good/Cargo.toml`, not the root.

## Development Workflow

### Branching
Feature branches use the `-story` suffix: `claude/<feature-name>-story`.
CI runs automatically on these branches (`test.yml` trigger).
Spec branches (pre-implementation) may use any name; switch to a `-story` branch before writing code.

### Conventional Commits
Prefix every commit: `feat:`, `fix:`, `ci:`, `build:`, `refactor:`, `doc:`.
One logical change per commit. Clean up history with `--force-with-lease` rather than piling on fixup commits.

### Action Pin Policy
All GitHub Actions must be pinned to a full commit SHA with a `# vX.Y.Z` or `# master` comment.

## Governance

This constitution supersedes ad-hoc conventions. Amendments require updating this file and the `Last Amended` date below.

**Version**: 1.0.0 | **Ratified**: 2026-06-18 | **Last Amended**: 2026-06-18
