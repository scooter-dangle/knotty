# Phase 1 Data Model: Retire the Split-Cell Rendering

**Feature**: [spec.md](./spec.md) | **Research**: [research.md](./research.md)

This feature removes types rather than adding them. Each entity below is given as it stands today and
as it stands after the last phase.

## `RenderMode` — removed

```rust
pub enum RenderMode { #[default] Standard, OpeningCentered }   // src/render.rs
```

Two values, `Standard` the default, threaded as an explicit parameter through every drawing entry
point. **Removed entirely in Phase 3.** No replacement type; the parameter simply goes.

Spec: FR-009, FR-010, SC-001.

## `Horiz` — 16 variants down to 8

One cell of a rendered diagram. Each variant maps to exactly one character of diagram text
(`as_byte`/`from_byte`) and to one three-by-three tile (`display`).

| Variant | Character | Tile (opening-centered) | Fate |
|---|---|---|---|
| `Empty` | `.` | blank | keeps |
| `Line` | `_` | `"   " "   " "___"` | keeps |
| `CrossDownOver` | `x` | `"\ /" " \ " "/ \"` | keeps |
| `CrossDownUnder` | `y` | `"\ /" " / " "/ \"` | keeps |
| `OpenedBelow` | `(` | `"  /" " ( " "  \"` | keeps |
| `ClosedBelow` | `)` | `"\  " " ) " "/  "` | keeps |
| `TransferUp` | `/` | `"  /" " / " "/  "` | keeps |
| `TransferDown` | `\` | `"\  " " \ " "  \"` | keeps |
| `CrossUpOver` | `A` | never drawn | **removed, Phase 4** |
| `CrossUpUnder` | `a` | never drawn | **removed, Phase 4** |
| `OpenedAbove` | `'` | never drawn | **removed, Phase 4** |
| `ClosedAbove` | `,` | never drawn | **removed, Phase 4** |
| `TransferUpStart` | `j` | never drawn | **removed, Phase 4** |
| `TransferUpFinish` | `r` | never drawn | **removed, Phase 4** |
| `TransferDownStart` | `2` | never drawn | **removed, Phase 4** |
| `TransferDownFinish` | `L` | never drawn | **removed, Phase 4** |

**Validation rules after Phase 4**: `from_byte` returns `None` for the eight freed characters, so
`VerboseDiagram::from_str` reports each through its existing unrecognised-character path — the
character named, with one-based row and column, first offender only. `as_byte`/`from_byte` remain
total and mutually inverse over the surviving eight (FR-021).

**Methods**: `in_mode` is removed in Phase 3 (its only job was folding the eight into `Empty` under
one mode). `subsequent` is removed in Phase 3 (it served only the split-cell grid builder — R2).

Spec: FR-020, FR-020a, FR-021, FR-022, SC-011, SC-012.

## `VerboseDiagram` / `VerboseLine` — unchanged in shape, narrower in signature

The grid itself is unchanged: rows bottom-up, each row a `Vec<Horiz>`, one character of diagram text
per cell, three display lines per row. What changes is that no method takes a `RenderMode`:

| Method | Today | After Phase 3 |
|---|---|---|
| `VerboseDiagram::from_abbreviated` | `(&AbbreviatedDiagram, RenderMode)` | `(&AbbreviatedDiagram)` |
| `VerboseDiagram::display::<B>` | `(&self, RenderMode)` | `(&self)` |
| `VerboseLine::display::<B>` | `(&self, RenderMode)` | `(&self)` |
| `VerboseDiagram::to_text` | `(&self, RenderMode)` | `(&self)` |
| `Horiz::display` / `display_with_borders` | `(&self, RenderMode)` | `(&self)` |

**State transition** — how a grid is built, after Phase 3: only the opening-centered builder
(`raw_lines::OpeningCentered`) remains. It tracks which levels are live rather than deriving the next
column from the previous cell, because opening-centered leaves the cell above a feature empty. A
feature at abbreviated index `idx` occupies row `idx` alone. A climb of one level costs one column
(FR-024).

The split-cell builder — `raw_lines::{append, advance, expand_above, contract_above, is_empty_above}`
— is removed in Phase 3.

Spec: FR-009, FR-024.

## `PersistedRenderMode` (app) — removed

```rust
enum PersistedRenderMode { #[default] Standard, OpeningCentered, #[serde(other)] Other }
```

Serialized under the `render_mode` key of `PersistedState`. **Removed in Phase 3**, along with the
`render_mode` field, `Msg::SetRenderMode`, `Model::render_mode` and `render_mode_toggle`.

**Backward compatibility**: `PersistedState` does not set `serde(deny_unknown_fields)`, so a state
saved before this feature — carrying `"render_mode": "standard"` or `"opening_centered"` — still
deserializes, keeping every other setting. This is asserted by a test rather than assumed (FR-012,
SC-008).

## Entities explicitly unchanged

`AbbreviatedDiagram`, `AbbreviatedItem`, `DiagramMoves`, `Lean`, and the app's `PersistedSnapshot` /
`PersistedManualSnapshot` / `PersistedMode` / `PersistedDisplayMode`. The abbreviated notation, the
meaning of a diagram, moves, bulge detection and snapshots are all untouched (FR-023).
