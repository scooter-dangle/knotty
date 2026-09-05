# Contract: Succinct Text Round-Trip

**Revised during implementation.** The original version of this contract
(reconstructing notation via a `try_from_succinct_text` library addition
reusing `scan_row`) was falsified by direct experiment — see `research.md`
R5 for what happened and R10/R11 for what replaced it. This is the corrected
contract; it involves **no library change**.

```text
   AbbreviatedDiagram ──from_abbreviated──▶ VerboseDiagram ──to_text()──▶ trailer text
                                                   │                           │
                                          display::<B>()             (embedded in
                                                   │                   succinct output,
                                                   ▼                   CLI-only, R10)
                                          visible succinct/                    │
                                          full-spaced art                      │
                                                                                ▼
                                                                     str::parse::<VerboseDiagram>()
                                                                                │
                                                                                ▼
                                                                     display::<B>() (full-spaced)
```

All arrows in this diagram are **existing, already-`pub`, already-tested**
library functions (`AbbreviatedDiagram::from_abbreviated`,
`VerboseDiagram::to_text`, `impl FromStr for VerboseDiagram`,
`VerboseDiagram::display`). The only new work is the CLI-side trailer
embedding/extraction described in `data-model.md`.

## Guarantees

- **G1 (lossless grid recovery)**: For any `AbbreviatedDiagram` `d` and its
  `VerboseDiagram` `g = VerboseDiagram::from_abbreviated(&d)`, extracting the
  `# ascii_print-grid: ` trailer built from `g.to_text()` and parsing it back
  with `VerboseDiagram`'s `FromStr` yields a `VerboseDiagram` equal to `g`.
  This is not a new guarantee this feature must prove — it already holds,
  covered by `src/render.rs`'s existing round-trip tests
  (`text_settles_in_one_pass` and friends). This feature's only obligation is
  to embed and extract the trailer without corrupting it (no line-splitting
  bugs, no truncation).
- **G2 (placement fidelity, exact)**: Because G1 recovers the exact grid —
  not notation re-rendered under a guessed mode — the fully-spaced output
  from succinct input is **exactly** `g.display::<GRID_BORDERS>()`, for the
  same `g` the succinct output was built from. This is strictly stronger
  than "the same placement mode reproduces the same picture" (the original,
  now-superseded G2): there is no re-derivation step at all.
- **G3 (visible art unaffected)**: `--style succinct`'s visible diagram art
  is byte-for-byte `AbbreviatedDiagram::ascii_print_compact::<GRID_BORDERS>()`,
  unchanged from today. The trailer is additive only.
- **G4 (`GRID_BORDERS`)**: `--grid-borders` affects only how a grid (fresh or
  recovered) is displayed (`display::<GRID_BORDERS>()`); it has no effect on
  the trailer's content, which always encodes the border-independent
  `to_text()` format.
- **G5 (malformed input)**: succinct input with no `# ascii_print-grid: `
  lines, or whose extracted trailer fails `VerboseDiagram`'s `FromStr`,
  returns an `Err` (surfaced by the CLI as a non-zero exit with a descriptive
  message) rather than a partially-correct render.

## Non-Guarantees (explicitly out of scope)

- Notation is never recovered from succinct input. `--placement` and
  `--echo-diagram` are both rejected with `--input-format succinct` (R11) —
  neither has meaning without notation.
- The visible diagram art in a succinct file is not validated against its
  trailer on input; the trailer is authoritative and the visible art is
  decorative once the file is being *read* (it exists for a human to look
  at, not for the tool to re-derive anything from).

## Test Obligations (carried into `tasks.md`)

- Since this feature adds no library code, there is no new `#[test]`
  obligation in `src/` under constitution Principle III — the guarantees
  above are already covered by `src/render.rs`'s existing tests. What
  remains is the CLI-only obligation to embed/extract the trailer correctly,
  validated manually via `quickstart.md`'s Scenario 3 (a `diff` against
  direct full-spaced output must be empty).
- Confirm, at the end of implementation, that the 15 snapshot files
  `research.md` originally worried about (R9, since withdrawn) are
  byte-for-byte unchanged from the `tasks.md` T002 baseline — this is a
  cheap, high-value regression check that "succinct/full-spaced output for
  encoded input really is untouched."
