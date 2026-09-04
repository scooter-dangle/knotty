# Phase 1 Data Model: Height-Precalculated Strand Placement

Rewritten 2026-09-03 against `origin/main` at `37b7c09`. All types are in-memory
Rust types in the `knotty` crate; no persistence.

## New public types

### `PlacementMode` (enum)

The active operating context governing how strand heights are calculated.

| Variant | Meaning |
|---------|---------|
| `IndexAligned` *(default)* | Existing behavior: a feature's notation index and its rendered row are the same number. Output byte-for-byte identical to today. |
| `PrecalculatedHeights` | Each strand placed at its precalculated maximum; caps, cups and crossings drawn at the floored midpoint of the two strands they join. |

- Derives `Clone, Copy, PartialEq, Eq, Debug, Default`; `IndexAligned` is `#[default]`.
- Re-exported from `lib.rs`. Final variant names are implementer discretion.
- **Not** a rendering mode: orthogonal to the opening-centered grid mapping (FR-014).

## Changed public types

### `AbbreviatedDiagram`

Tuple struct → named struct so it can carry the active mode.

| Field | Type | Notes |
|-------|------|-------|
| `items` | `Vec<AbbreviatedItem>` | The ordered features (was the unnamed field `.0`). |
| `mode` | `PlacementMode` | The active operating context. `IndexAligned` for every existing constructor. |

New accessors: `mode(&self) -> PlacementMode`, `set_mode(&mut self, PlacementMode)`,
`with_mode(self, PlacementMode) -> Self`.

Invariants:

- All existing constructors (`FromStr`, `new_from_tuples`, rotation output) yield
  `IndexAligned`, preserving current behavior (FR-005, FR-013, SC-004).
- Mode is the only thing distinguishing how `items` is rendered: equal `items`
  plus equal `mode` implies equal output (FR-008).

## New internal types

### Strand maxima (Component A's output)

Two maxima per opening, in opening order — the lower strand's and the upper
strand's maximum rows over their flat runs (FR-001). Carried across the seam
described in [contracts/strand-heights.md](./contracts/strand-heights.md).

Not public. The derived opening row `floor((lower + upper) / 2)` is *not* part of
this structure — Component B recomputes it, since it needs both maxima anyway to
emit the two boundary transfers.

**How heights are assigned** (research R2) — a strand's height is one more than
the tallest thing ever beneath it:

```text
height(s) = 0                                     if no strand is ever below s
height(s) = 1 + max{ height(t) : t ever below s }  otherwise
```

Heights are absolute; a pair's gap is a *consequence* (`upper − lower − 1`), never
an input. Attempting to compute the gap directly is what produced two wrong
formulations — see R2's history section.

**Derived grid height**:

```text
height = max(all maxima) + 1
```

which equals `AbbreviatedDiagram::height()` exactly when no pair diverges, and
exceeds it otherwise (research R7). Component B sizes the grid from this, **not**
from `height()`.

### Grid state (shared by both placement modes)

Extracted from today's `OpeningCentered` (`src/raw_lines.rs:8`) so both placement
builders emit glyphs through identical code (research R3, FR-014):

| Member | Type | Role |
|--------|------|------|
| `lines` | `Vec<Vec<Horiz>>` | The grid being built, one row per level. |
| `live` | `Vec<bool>` | Which rows currently carry a strand. |
| `column()` | fn | Emits one column: places the given `(row, glyph)` pairs, fills live rows with `Line`, applies the shadow rule. |

`live` keeps its meaning under both modes. What changes is *which rows* are live
— under precalculated placement they need not be contiguous, and a divergent
pair's gap stays un-live for the pair's whole lifetime.

## Unchanged types

### `VerboseDiagram`

Structurally unchanged: `Vec<VerboseLine>` of `Horiz` cells. What differs by mode
is which cells are produced. `from_abbreviated` (`src/diagram.rs:118`) gains mode
awareness and dispatches to the matching placement builder.

### `Horiz`

Unchanged. The fixtures use only the existing eight variants; no new glyph is
anticipated (research R3).

## Relationships & flow

```text
AbbreviatedDiagram { items, mode }
      │
      ├─ IndexAligned ────────────────▶ OpeningCentered ──┐
      │                                                   ├─▶ Grid.column() ─▶ VerboseDiagram
      └─ PrecalculatedHeights ─▶ [A] maxima ─▶ [B] ───────┘
                                                            │
              display::<GRID_BORDERS>() ◀───────────────────┤
              full_render_lines → scan_row → rotation ◀─────┘
```

- Rendering and rotation both consult `self.mode`.
- Notation-only moves (`Swap`, `WrapAround`, `ChangeCrossing`, Reidemeister,
  `Bulge`, `Collapse*`) touch `items` only and are mode-independent (FR-012, C9).
- **Logical level ≠ rendered row** under `PrecalculatedHeights`. A notation index
  names a level among currently-live strands; Component B maintains that mapping.
  Component A never sees rendered rows.

## Validation rules

| Rule | Source |
|---|---|
| Default-mode output unchanged; 24 existing snapshots frozen | FR-005, SC-004, R5 |
| Maxima per strand, over the flat run only | FR-001 |
| Cap/cup/crossing at the floored midpoint; odd separation favors the lower strand | FR-002, FR-011, FR-016 |
| Each strand transfers between cap/cup and its own maximum | FR-015 |
| Both strands return to their maxima after a crossing | FR-011 |
| Crossing never drawn between non-adjacent rows | FR-007, FR-011 |
| Both modes represent the same knot | FR-006, SC-003 |
| Output deterministic for a given `(items, mode)` | FR-008, SC-005 |
| Empty and degenerate diagrams handled | FR-010 |
| Placement independent of grid mapping | FR-014 |
| Rotation feature count non-increasing | SC-006 |
