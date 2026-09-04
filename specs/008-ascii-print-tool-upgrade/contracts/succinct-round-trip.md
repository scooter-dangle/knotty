# Contract: Succinct Text Round-Trip (library-internal seam)

This documents the guarantee between `AbbreviatedDiagram::ascii_print_compact`
(producer) and the new `AbbreviatedDiagram::try_from_succinct_text`
(consumer), independent of the CLI. See `research.md` R5–R7 for the
reasoning behind each guarantee.

```text
   AbbreviatedDiagram ──ascii_print_compact──▶ succinct text ──try_from_succinct_text──▶ AbbreviatedDiagram
        (notation)                                (ASCII)                                    (notation)
```

## Guarantees

- **G1 (topology)**: For any `AbbreviatedDiagram` `d`, parsing
  `d.ascii_print_compact::<false>()` with `try_from_succinct_text` returns an
  `AbbreviatedDiagram` whose sequence of `(element, index)` pairs is
  identical to `d`'s. This holds regardless of which `PlacementMode` `d` was
  in when printed — `scan_row` recovers notation, never rendered row
  position (R5).
- **G2 (placement, indirect)**: Re-rendering the notation `G1` returns, under
  the *same* `PlacementMode` `d` was in, reproduces `d`'s exact rendered
  output byte-for-byte. Placement fidelity is achieved by re-deriving it from
  recovered notation + mode, not by reading it off the ASCII art (R5/R6).
- **G3 (collapse safety)**: Collapsing a maximal all-blank column run to
  exactly two placeholder columns (rather than deleting it, today's
  behavior) never changes any row's character within that run — each row's
  character is constant across the whole run by construction (`Grid::column`
  only changes a row's output in response to an event at that column; R7).
  This guarantee is what makes G1 hold for the *succinct* (not just
  fully-spaced) form specifically.
- **G4 (`GRID_BORDERS`)**: G1–G3 hold for both `ascii_print_compact::<true>`
  and `::<false>`. `try_from_succinct_text` does not need to know which was
  used — `scan_row` matches glyph shapes, not border decoration — but the
  CLI always parses without borders drawn in mind (border cells are outside
  `scan_row`'s matched shapes already, per existing rotation behavior which
  only ever scans `display::<false>()`).
- **G5 (malformed input)**: If a row cannot be interpreted (unrecognized
  glyph sequence, row/column count inconsistent with any valid diagram),
  `try_from_succinct_text` returns `Err(String)` describing the failure. It
  never returns `Ok` with a partially-correct or guessed notation.

## Non-Guarantees (explicitly out of scope)

- Exact original column width is **not** recoverable from succinct text —
  only topology and (given matching placement mode) rendered placement. See
  `spec.md` Assumptions.
- `try_from_succinct_text` does not validate that its input was actually
  produced by this tool; syntactically-matching hand-written ASCII art is
  accepted the same way rotation already accepts any validly-shaped
  fully-spaced text today.

## Test Obligations (carried into `tasks.md`)

- Round-trip test: for every fixture already used by the existing
  `ascii_print`/`ascii_print_compact`/`precalculated_heights*` snapshot tests
  (`src/diagram/tests.rs`), assert
  `try_from_succinct_text(d.ascii_print_compact::<false>())`'s tuples equal
  `d.to_tuples()` (G1).
- Regenerate the 15 snapshots named in `research.md` R9 via `cargo insta
  review` after R7's collapse-to-two change, and confirm each diff is
  exactly "wider blank run", nothing else.
- Add at least one new `scan_row` unit test in `src/rotate.rs` fed
  collapsed-to-two-column input directly (not derived from a full-width
  fixture), per R7's residual-risk note.
