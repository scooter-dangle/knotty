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

**Decision** (owner's rule, 2026-09-03 — **verified**): a strand's height is one
more than the tallest thing ever beneath it.

```text
height(s) = 0                                     if no strand is ever below s
height(s) = 1 + max{ height(t) : t ever below s }  otherwise
```

"Ever below" means: at some moment when both are live, `t` sits at a lower level
than `s`. This is a longest-path computation over the "ever below" DAG, and the
owner's description of it is the natural evaluation order — assign 0 to the
strands with nothing beneath them, then repeatedly assign any strand all of whose
subordinates are already assigned.

### Algorithm

1. Walk the sequence maintaining the ordered stack of live strands. `(N` inserts
   a pair at logical index `N`; `)N` removes two; **crossings do not reorder
   levels**.
2. After each mutation, record the *immediately-below* relation for adjacent
   stack neighbours.
3. Compute `height` by memoized longest path over that relation.

**Only adjacent edges are needed.** Recording every below-pair gives identical
heights (verified on 11 cases) because at any instant the live strands are
totally ordered, so any "ever below" relation is realized by a chain of
adjacencies at that same instant. That reduces edge collection from O(depth²) to
O(depth) per feature.

**Acyclic** because co-live strands never change relative order: openings and
closings shift blocks without reordering, and crossings do not touch the stack.

**Grid height** is `max(all heights) + 1`.

### Verification

Reproduces all five fixtures **exactly** — 23 pairs, 63 features — and returns
the correct gap of 2 for `(0 (1 )1 (1 )1 )0`, the counterexample that falsified
the previous formulation. It also handles the stacked case: for
`(0 (1 (2 )2 (3 )3 )1 )0` it yields `A=(0,7)`, `B=(1,4)`, `C=(2,3)`, `D=(5,6)` —
`B` holds its gap for `C`, and `D` stacks above `B` rather than reusing `C`'s
vacated rows.

Two fixtures are still worth having as regression tests, since no supplied
fixture covers them: **sequential siblings** and **a divergent pair with a
sibling stacked above it**.

### History: two formulations that failed

Recorded because the first was convincing and wrong, and rederiving it would be
easy.

#### Retracted: gap as a nesting count

An earlier revision of this section claimed

```text
upper_max − lower_max − 1  ==  number of strands ever opened between the pair
```

and noted it held for all 23 pairs in all five fixtures. **It is wrong**, and the
fixtures do not contain the case that breaks it.

Counterexample — `(0 (1 )1 (1 )1 )0` (valid, 6 features):

```text
A opens.  B opens between A's strands, closes.  C opens between them, closes.  A closes.
```

Four strands are opened between A's two strands, so the formula demands a gap of
4. But B and C never coexist: C reuses the rows B vacated, so the gap is 2. The
formula counts cumulatively where the quantity is a maximum over time.

### Also insufficient: maximum simultaneous span

The obvious repair — take the maximum, over the pair's lifetime, of the span
consumed by strands between it — is **also wrong**, and the encircled fixture
proves it. Computing that quantity for pair `A` there gives 12; the fixture has
14.

The reason is ordering. `H` sits *below* `B`, while `C`, `D`, `E`, `F` all sit
*above* `B`. `H` is live only after `C`, `D` and `E` have closed, so a purely
temporal argument says it may reuse their rows — but it may not, because it must
stay below `B`. Rows 1–2 are reserved for `H` alone and stand empty for most of
the diagram.

#### Why both failed, and why the accepted rule does not

Both attempts tried to compute a pair's gap *directly*, from a quantity measured
between its two strands. That framing is the mistake:

- **Reuse requires two conditions.** Two pairs may share rows only if they are
  disjoint in time *and* occupy the same position in the vertical order. The
  count formula ignored the first; the span formula ignored the second.
- **A nested pair contributes its span, not its strand count**, because it holds
  its own gap open for its whole life even where nothing occupies it — see
  [little-dumb-link](./fixtures/little-dumb-link.md), gap empty at column 6.

The accepted rule sidesteps both by never computing a gap at all. It assigns each
strand an absolute height from what lies beneath it, and the gap simply falls out
as `upper − lower − 1`. Both conditions are then automatic: two strands get the
same height exactly when neither is ever below the other, which is precisely
"disjoint in time or unordered", and a tall nested pair raises everything above it
by its full extent without that extent being counted anywhere.

**Note on FR-001**: the 2026-09-03 clarification remains correct and untouched.
Defining a maximum over the strand's **flat run only** removes the *fixpoint* —
a strand's maximum does not depend on a cap/cup row derived from that same
maximum. That is a different question from how heights are assigned, and the
longest-path computation introduces no fixpoint either.

**Alternatives**: cost-aware placement weighing crossing-alignment against
displacement — out of scope per the 2026-06-18 clarification, and unrelated.

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
