# Phase 0 Research: Revised Diagram Text Format Symbol Table

No `NEEDS CLARIFICATION` markers were present in the Technical Context — the feature is a fully
specified character remapping within one existing module. This document records the decisions made
while confirming the implementation surface.

## Decision: Where the mapping lives

**Decision**: The mapping is the pair of `const fn`s `Horiz::as_byte` and `Horiz::from_byte` in
`src/render.rs` (lines ~423-469). No other location encodes the mapping independently.

**Rationale**: Confirmed by reading `src/render.rs` and `examples/knot-so-good/src/main.rs`. The
example app's in-app symbol table (`SYMBOL_TABLE` constant, `examples/knot-so-good/src/main.rs:209`)
is built by calling `horiz.as_byte()` for each `Horiz` variant rather than hard-coding characters,
so it updates automatically once `as_byte` changes — satisfying spec FR-007 with no separate edit.

**Alternatives considered**: A separate lookup table shared between reader/writer/docs was
considered unnecessary — one already exists as `as_byte`/`from_byte` and nothing outside
`render.rs` duplicates it.

## Decision: Scope of the character swap

**Decision**: Only the seven mappings named in the spec's FR-001 change value; the other nine
match arms are left byte-for-byte identical. The change is a straight edit to the literal bytes in
both functions' match arms — no restructuring of the functions.

**Rationale**: Spec FR-002/FR-003/FR-004 require exactly this: the untouched characters keep their
meaning, and no character maps to two cells or a cell to two characters. Editing the literals
in-place preserves that guarantee by construction (each match arm still names exactly one byte and
one variant).

**Alternatives considered**: None — the mapping is a fixed, exhaustive match; there's no
alternative representation to weigh.

## Decision: Test fixtures requiring updates

**Decision**: Every literal diagram-text string in `src/render.rs`'s `#[test]` module that uses one
of the seven retired characters (`_`, `-`, `\`, `/`, `.`, `i`, `k` in their *old* meanings) must be
rewritten with the new characters so it still describes the same picture. Identified fixtures:
`UNKNOT`, `TREFOIL`, the `ragged_rows_are_padded_on_the_right` literal, and the inline literals in
`trailing_newline_is_optional`, `carriage_returns_terminate_lines`, and
`blank_line_past_the_terminator_is_an_empty_row`. `unrecognized_bytes_have_no_mapping` should add
the newly-unrecognized bytes (`-`, `i`, `k`) to its case list, since it currently doesn't test them
and they are meaningful negative cases post-change.

**Rationale**: Found by grepping `src/render.rs` for the byte-mapping tests and the compact-text
literals feeding `parse(...)`. The other literal strings in the file (e.g. `r#"___"#, r#"\ /"#`
inside the `display`/rendering tests) are multi-character ASCII-art *display* output, not the
one-byte-per-cell compact format, and are unaffected by this change.

**Alternatives considered**: Leaving old fixtures in place and adding new ones alongside was
rejected — spec FR-005/FR-006 require the writer to emit only the new characters and the round-trip
guarantee to hold under the new mapping; keeping stale fixtures around would test a mapping that no
longer exists and risks masking a regression.

## Decision: No dependency, snapshot, or app-code changes needed

**Decision**: No `Cargo.toml` change, no `insta` snapshot update, and no `examples/knot-so-good`
source edit beyond what falls out of `Horiz::as_byte` changing (which the app reads dynamically).

**Rationale**: `insta` snapshots in `src/diagram/snapshots/` cover `AbbreviatedDiagram` → ASCII-art
rendering (multi-character cells with borders), not the one-byte-per-cell compact format this
feature touches. The example app has no other hard-coded reference to the mapping (confirmed via
grep for the retired characters and for "symbol table" in `examples/knot-so-good/src/main.rs`).

**Alternatives considered**: N/A.
