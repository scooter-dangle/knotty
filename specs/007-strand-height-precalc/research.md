# Phase 0 Research: Height-Precalculated Strand Placement

Rewritten 2026-09-03 against `origin/main` at `37b7c09`, after PRs #38–#44 and
seven clarifications. Supersedes the pre-rebase revision, whose R3 and R5 were
written around `raw_lines::{append, expand_above, contract_above}` — functions
PR #42 deleted.

Product-level decisions are settled in the spec's Clarifications. What follows
are the technical decisions, several of which are now derived from the five
golden fixtures in [fixtures/](./fixtures/) rather than reasoned in the abstract.

## R1. How is the placement mode represented and threaded?

**Decision**: `pub enum PlacementMode { #[default] IndexAligned, PrecalculatedHeights }`
(`Clone, Copy, PartialEq, Eq, Debug, Default`), stored as a field on
`AbbreviatedDiagram`. Convert the tuple struct
`AbbreviatedDiagram(pub(crate) Vec<AbbreviatedItem>)` (`src/diagram.rs:115`) to
`AbbreviatedDiagram { items, mode }`. Add `mode()`, `set_mode()`, `with_mode()`.
Re-export from `lib.rs`.

**Naming**: it is a *placement* mode, not a rendering mode — spec FR-014 makes
the orthogonality with the opening-centered grid mapping a testable requirement.
`IndexAligned` names the existing behavior by its defining property: a feature's
notation index and its rendered row are the same number. The word *legacy* is
retired — the legacy (split-cell) **rendering** was removed by #42 and is a
different axis entirely. Final variant names remain implementer discretion.

**Rationale**: reading `self.mode` means no existing public signature changes, so
`ascii_print*`, `try_rotate_90_ccw`, `try_apply*` keep their shapes and the
default (`IndexAligned`) preserves today's behavior by construction (FR-005,
FR-013, SC-004). Rotation dispatches at runtime through
`DiagramMove::Rotate90CounterClockwise`, so a field — not a const generic — is
required for that path to honor the mode.

**Alternatives**: const generic (cannot be selected by a runtime `DiagramMove`);
threaded parameter (ripples into both example crates and makes consistency the
caller's burden, contradicting the operating-context model of FR-012).

**Cost**: mechanical `self.0` → `self.items` rename, 37 sites in `src/diagram.rs`
plus `knot.0.iter()` at `:123`. Compiler-checked, low risk.

## R2. How are per-strand maxima calculated?

**Decision**: two linear passes over the abbreviated sequence.

1. **Count nesting.** Walk the sequence maintaining an ordered stack of live
   strands. When an opening inserts at logical index `N`, every currently-open
   pair whose two strands straddle `N` gains 2 to its nested-strand count.
2. **Assign rows.** Walk again, now knowing each pair's required gap, assigning
   each strand the row its ordering and gap demand.

**The governing invariant**, derived from the fixtures and verified against all
23 pairs in all five of them:

```text
upper_max − lower_max − 1  ==  number of strands ever opened between the pair
```

A pair's two strands are adjacent unless something opens between them, and the
gap is exactly wide enough for everything that ever does. This is what makes the
calculation tractable: the gap is a *count*, obtainable in one pass, not a
geometric search.

**Why two passes and not one**: a pair's gap depends on openings that occur
after it, so rows cannot be assigned during the first walk. This refines the
2026-09-03 clarification, which established that a strand's maximum covers its
flat run only and therefore involves **no fixpoint iteration**. That still
holds — two deterministic linear passes are not a fixpoint — but the earlier
"single forward pass" phrasing was optimistic.

**Alternatives**: cost-aware placement weighing crossing-alignment against
displacement — out of scope per the 2026-06-18 clarification.

## R3. Where does the new placement path live?

**Decision**: separate placement from glyph emission inside `src/raw_lines.rs`.

Today `OpeningCentered` (`src/raw_lines.rs:8`) does both:

| Concern | Members | Axis |
|---|---|---|
| Glyph emission | `column()`, the `Horiz` values | grid mapping (#40's axis) |
| Placement | `live`, `raise_once()`, `lower_once()`, `append()` | this feature's axis |

`column()` is already mode-agnostic: given `(row, glyph)` pairs it fills the rest
of the column from `live`, and computes the shadow rule. Extract the grid state
(`lines`, `live`) and `column()` into a small shared inner struct, then let two
placement builders drive it — `OpeningCentered` unchanged, and a new
`PrecalculatedHeights` builder implementing the midpoint rules.

**Rationale**: this is the minimum structure that satisfies FR-014. One
extraction, no trait, no generics — consistent with the repo's standing guidance
against abstractions beyond what is needed. It also means glyph output is
literally shared, so the two modes cannot drift on the grid-mapping axis.

**Expected glyph set**: unchanged. The fixtures use only `OpenedBelow`,
`ClosedBelow`, `CrossDownOver`, `CrossDownUnder`, `TransferUp`, `TransferDown`,
`Line`, `Empty` — exactly what `Horiz` already provides. No new glyph is
anticipated.

## R4. How are non-adjacent crossings handled?

**Decision**: the uniform rule, now fully specified by the spec and confirmed by
fixture. A cap, cup, or crossing is drawn at `floor((a+b)/2)` of the two strands
it joins; the convergence is split as evenly as the separation allows, with an
odd separation giving the lower strand the extra step (FR-002); after a
*crossing* — unlike a cap or cup — both strands return to their maxima (FR-011).

This was the highest-uncertainty area in the pre-rebase revision. It no longer
is: [fixtures/non-adjacent-crossing.md](./fixtures/non-adjacent-crossing.md)
supplies expected output for three separate crossings whose partners sit 3 rows
apart, each drawn at the midpoint with an explicit return column. The
construction is now specified by example rather than left to the implementer.

**Cost**: alignment dominates column count. That fixture spends 9 of its 19
columns on transfers.

## R5. How is default-mode parity guaranteed?

**Decision**: `PlacementMode::IndexAligned` routes through `OpeningCentered`
unchanged. The gate is that **all 24 existing snapshots stay byte-for-byte
identical** (SC-004); new snapshots are added only for `PrecalculatedHeights`.

**Note**: the pre-rebase revision said 16 snapshots. `origin/main` has 24 —
#40 and #42 added the opening-centered set and removed the split-cell set.

## R6. Rotation

**Decision**: no `scan_row` change anticipated. Its regexes match local glyph
shapes (`/_*\`, ` _+ `, ` / `, ` \ `, `\/`, `\ /`) and its indices come from
counters advanced along a scan line; neither depends on a row number. Per the
feature owner these patterns were written deliberately to work for any generally
well-formed ASCII knot diagram, this rendering included.

**Rotation results will change**, and that is the intent, not a regression:
scanning a cleaner picture yields different but equivalent notation. Only
default-mode output is frozen (SC-004, C1).

SC-006 — that removing reversed-direction transfers stops repeated rotation from
compounding artifacts — is the feature's **central hypothesis**, measured by
comparing scanned feature counts before/after a rotation and across a full
four-rotation cycle. Implementing rotation is what exposed the limit in the
older placement; SC-006 is what establishes whether this placement removes it.

## R7. Grid dimensions change in both directions

**Decision**: accept it, and derive the grid height from Component A's output
rather than from `AbbreviatedDiagram::height()`.

Measured, default versus fixture:

| Fixture | Default | New mode | Δ rows | Δ cols |
|---|---|---|---|---|
| rotated-5_1 | 8 × 19 | 8 × 18 | 0 | −1 |
| square-knot | 6 × 18 | 6 × 12 | 0 | −6 |
| non-adjacent-crossing | 6 × 18 | 6 × 19 | 0 | **+1** |
| little-dumb-link | 6 × 20 | **8** × 15 | **+2** | −5 |
| square-knot-links-encircled | 12 × 48 | **16** × 36 | **+4** | −12 |

**Height can grow.** `height()` returns `max simultaneous depth × 2`, which is
correct when strands are packed adjacently. Under precalculated placement a
divergent pair holds its gap open for its whole life, so the diagram spans more
rows than are ever occupied at once — the encircled fixture needs 16 rows while
never having more than 12 strands live. The required height is

```text
height = max(all strand maxima) + 1
```

which reduces to `height()` exactly when no pair diverges. **This invalidates the
pre-rebase R2 claim that total diagram height is unchanged.** Component B must
size the grid from A's output.

**Width usually shrinks but can grow.** Removing displacement transfers removes
columns — dramatically so for the encircled fixture (48 → 36). But crossing
alignment adds them, and `non-adjacent-crossing` comes out one column *wider*
than the default. Consistent with SC-002's refusal to guarantee a reduction in
total transfers.

## Resolved unknowns

No `NEEDS CLARIFICATION` remains. The two items the pre-rebase revision left
open — the height-map shape (per-pair vs per-strand) and the crossing-alignment
construction — are settled by clarification and by fixture respectively.
