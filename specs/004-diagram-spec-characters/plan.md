# Implementation Plan: Revised Diagram Text Format Symbol Table

**Branch**: `claude/diagram-spec-characters-rqm0ye` | **Date**: 2026-09-02 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `/specs/004-diagram-spec-characters/spec.md`

**Note**: This template is filled in by the `/speckit-plan` command. See `.specify/templates/plan-template.md` for the execution workflow.

## Summary

Remap seven of the sixteen `Horiz` cell characters used by the diagram text format
(`Horiz::as_byte` / `Horiz::from_byte` in `src/render.rs`): Empty `_`→`.`, Line `-`→`_`,
CrossDownOver `\`→`x`, CrossDownUnder `/`→`y`, OpenedAbove `.`→`'`, TransferUp `i`→`/`,
TransferDown `k`→`\`. The nine other characters are untouched. Both functions are the single
source of truth the reader, writer, and the example app's in-app symbol table already draw from
(`examples/knot-so-good/src/main.rs`'s `SYMBOL_TABLE` builds itself from `Horiz::as_byte`), so no
separate documentation surface needs a matching edit. The work is a pure character-constant swap
plus updating every hard-coded diagram-text-format fixture in `src/render.rs`'s test module that
used one of the seven retired characters.

## Technical Context

**Language/Version**: Rust, channel pinned in `rust-toolchain.toml` (1.94.0)

**Primary Dependencies**: None new — change is confined to `src/render.rs`, an existing module with no new crate dependencies

**Storage**: N/A

**Testing**: `cargo test` (existing `#[test]` module in `src/render.rs`); `insta` snapshot tests are unaffected since they cover `AbbreviatedDiagram` → ASCII-art rendering, not the compact byte format

**Target Platform**: Library (`src/`) must remain `wasm32-unknown-unknown`-compatible; example app (`examples/knot-so-good`) consumes the library unchanged

**Project Type**: Library (`knotty` crate) with a downstream example GUI app

**Performance Goals**: N/A — constant-time character lookup, unchanged in shape

**Constraints**: Must not change any format rule other than the seven character assignments (whitespace rejection, ragged-row padding, round-trip fidelity, etc. all continue to hold per spec FR-006)

**Scale/Scope**: Two functions (`Horiz::as_byte`, `Horiz::from_byte`) in one file, plus their existing unit tests and every literal diagram-text fixture string in that file's test module that uses a retired character

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- **I. Library-First**: PASS. The mapping lives in `src/render.rs` (core crate); the example app
  consumes it via `Horiz::as_byte`/`from_byte` with no app-side duplication to update.
- **II. WASM-Compatible**: PASS. No new dependency, no `std`-only API introduced; `as_byte`/
  `from_byte` remain `const fn` over `u8`.
- **III. Test-First**: PASS, with an action item. Existing tests (`byte_mapping_round_trips`,
  `byte_mapping_is_distinct`, `unrecognized_bytes_have_no_mapping`, and the fixture-based tests
  using `UNKNOT`/`TREFOIL`/ragged/inline literals) already exercise exactly the properties this
  change must preserve; they need their literal strings updated to the new characters, and
  `unrecognized_bytes_have_no_mapping` should gain the now-retired bytes (`-`, `i`, `k`) as cases.
  No snapshot tests are affected (see Testing above), so no `cargo insta review` step is needed.
- **IV. Notation Fidelity**: PASS / not applicable. This feature touches only the diagram *text*
  format (a rendering-picture serialization); it does not touch abbreviated knot notation, which
  remains the unmodified source of truth.
- **V. Minimal Dependencies**: PASS. No `Cargo.toml` change in either the root crate or the example
  app.

No violations to record in Complexity Tracking.

## Project Structure

### Documentation (this feature)

```text
specs/004-diagram-spec-characters/
├── plan.md              # This file (/speckit-plan command output)
├── research.md          # Phase 0 output (/speckit-plan command)
├── data-model.md         # Phase 1 output (/speckit-plan command)
├── quickstart.md        # Phase 1 output (/speckit-plan command)
├── contracts/           # Phase 1 output (/speckit-plan command)
└── tasks.md             # Phase 2 output (/speckit-tasks command - NOT created by /speckit-plan)
```

### Source Code (repository root)

```text
src/
└── render.rs        # Horiz::as_byte / Horiz::from_byte (the symbol table) and its #[test] module

examples/knot-so-good/
└── src/main.rs       # SYMBOL_TABLE — reads Horiz::as_byte directly, no separate edit needed
```

**Structure Decision**: Single project (existing `knotty` library crate at `src/`, with the
`examples/knot-so-good` app as a downstream consumer). This feature is a self-contained edit to
`src/render.rs`; no new modules, files, or directories are introduced.

## Complexity Tracking

*No violations — table omitted.*
