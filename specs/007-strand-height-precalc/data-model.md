# Phase 1 Data Model: Height-Precalculated Strand Placement

The feature adds a small amount of state and one derived (transient) structure.
No persistence is involved; all types are in-memory Rust types in the `knotty`
crate.

## New types

### `RenderMode` (enum)

The active operating context that governs how a diagram is rendered (and,
through rotation, how its notation is re-derived).

| Variant | Meaning |
|---------|---------|
| `Legacy` *(default)* | Existing renderer: each opening placed at the lowest free row; passing strands bumped up/down via transfers. Output is byte-for-byte identical to today. |
| `PrecalculatedHeights` | New renderer: each opening placed at its precalculated maximum row so passing strands run flat; only boundary and crossing-alignment transfers are emitted. |

- Derives: `Clone, Copy, PartialEq, Eq, Debug, Default` (`Legacy` is `#[default]`).
- Re-exported from `lib.rs`.
- Final variant names are at implementer discretion; `Legacy` / `PrecalculatedHeights` are used throughout these docs.

## Changed types

### `AbbreviatedDiagram`

Source of truth for a diagram. Changes from a tuple struct to a named struct so
it can carry the active mode.

| Field | Type | Notes |
|-------|------|-------|
| `items` | `Vec<AbbreviatedItem>` | The ordered diagram features (was the unnamed tuple field `.0`). |
| `mode` | `RenderMode` | The active operating context. Defaults to `Legacy` for every existing constructor. |

Accessors / builders (new):
- `fn mode(&self) -> RenderMode`
- `fn set_mode(&mut self, mode: RenderMode)`
- `fn with_mode(self, mode: RenderMode) -> Self`

Invariants:
- All existing constructors (`FromStr`, `new_from_tuples`, rotation output) yield
  `mode = Legacy`, preserving current behavior (FR-005, FR-013, SC-004).
- The mode is the *only* thing that distinguishes how `items` is rendered; two
  diagrams with equal `items` and equal `mode` are equivalent.

## Derived / transient structures

### Peak-row map (internal, not public)

Produced by the precalculation pass (research R2): a mapping from each opening
event to the maximum vertical row its strand pair occupies over its lifetime.
Consumed by the `PrecalculatedHeights` build path; never stored on the diagram.

### `VerboseDiagram` (unchanged shape)

The rendered grid (`Vec<VerboseLine>` of `Horiz` cells) remains structurally
unchanged. What differs by mode is *which* `Horiz` cells are produced for a
given `AbbreviatedDiagram`. `from_abbreviated` gains awareness of the mode (read
from the `AbbreviatedDiagram`) to choose the placement path.

## Relationships & flow

```text
AbbreviatedDiagram { items, mode }
        │  from_abbreviated (mode-aware)
        ▼
   VerboseDiagram (grid of Horiz)
        │  display::<GRID_BORDERS>()        │  full_render_lines → scan_row
        ▼                                   ▼
   ASCII string                        rotation → new AbbreviatedDiagram (mode carried)
```

- Rendering (`ascii_print*`) and rotation (`try_rotate_90_ccw`, via `try_apply`/
  `try_apply_all`) both consult `self.mode`.
- Notation-only moves (`Swap`, `WrapAround`, `ChangeCrossing`, Reidemeister,
  `Bulge`, `Collapse*`) operate on `items` only and are independent of `mode`
  (US3 acceptance scenario 3).

## Validation rules (from requirements)

- Default mode output unchanged (FR-005, SC-004).
- `PrecalculatedHeights`: opening placed at precalculated max row (FR-001/FR-002);
  unchanged-row strands render flat (FR-003); open/close displacement transfers
  reduced (FR-004); boundary diagonals retained (FR-009); crossings kept adjacent
  via crossing-alignment transfers, never drawn between non-adjacent rows
  (FR-007, FR-011).
- Both modes represent the same knot (FR-006, SC-003); output deterministic
  (FR-008, SC-005); empty/degenerate handled (FR-010).
- Rotation scanned-feature count non-increasing / reduced (SC-006).
