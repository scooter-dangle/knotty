# Contract: `knotty` public API

**Feature**: [spec.md](./spec.md) | **Consumers**: `examples/ascii_print`, `examples/knot-so-good`, any downstream crate

Every change below is a breaking source change to the library's public surface, all of it the same
change: the `RenderMode` argument goes away. Signatures are otherwise identical, so a caller updates
by deleting one argument per call.

## Removed from `knotty`'s exports

```rust
pub use render::{Horiz, RenderMode, VerboseDiagram, VerboseLine};   // before
pub use render::{Horiz, VerboseDiagram, VerboseLine};               // after
```

`RenderMode` ceases to exist (Phase 3). Eight `Horiz` variants cease to exist (Phase 4):
`CrossUpOver`, `CrossUpUnder`, `OpenedAbove`, `ClosedAbove`, `TransferUpStart`, `TransferUpFinish`,
`TransferDownStart`, `TransferDownFinish`.

## Changed signatures

| Before | After |
|---|---|
| `VerboseDiagram::from_abbreviated(&AbbreviatedDiagram, RenderMode) -> Result<Self, String>` | `VerboseDiagram::from_abbreviated(&AbbreviatedDiagram) -> Result<Self, String>` |
| `VerboseDiagram::display::<const B: bool>(&self, RenderMode) -> impl Iterator<Item = String>` | `VerboseDiagram::display::<const B: bool>(&self) -> impl Iterator<Item = String>` |
| `VerboseDiagram::to_text(&self, RenderMode) -> String` | `VerboseDiagram::to_text(&self) -> String` |
| `VerboseLine::display::<const B: bool>(&self, RenderMode) -> impl Iterator<Item = String>` | `VerboseLine::display::<const B: bool>(&self) -> impl Iterator<Item = String>` |
| `Horiz::display(&self, RenderMode) -> [&'static str; 3]` | `Horiz::display(&self) -> [&'static str; 3]` |
| `Horiz::display_with_borders(&self, RenderMode) -> [&'static str; 4]` | `Horiz::display_with_borders(&self) -> [&'static str; 4]` |
| `AbbreviatedDiagram::ascii_print::<B>(&self, RenderMode) -> String` | `AbbreviatedDiagram::ascii_print::<B>(&self) -> String` |
| `AbbreviatedDiagram::ascii_print_compact::<B>(&self, RenderMode) -> String` | `AbbreviatedDiagram::ascii_print_compact::<B>(&self) -> String` |
| `AbbreviatedDiagram::try_ascii_print::<B>(&self, RenderMode) -> Result<String, String>` | `AbbreviatedDiagram::try_ascii_print::<B>(&self) -> Result<String, String>` |
| `AbbreviatedDiagram::try_ascii_print_compact::<B>(&self, RenderMode) -> Result<String, String>` | `AbbreviatedDiagram::try_ascii_print_compact::<B>(&self) -> Result<String, String>` |
| free `ascii_print::<B>(Vec<(u8, usize)>, RenderMode)` and its three siblings | same, without the mode |

## Unchanged

`AbbreviatedDiagram` construction, parsing and `Display`; `try_rotate_90_ccw`; every move; bulge
detection; `VerboseDiagram: FromStr`; `Display for VerboseDiagram` (which now delegates to the
one-argument `to_text`). The `GRID_BORDERS` const parameter and the compact variants stay exactly as
they are — all four combinations of compact × borders remain reachable (FR-013, SC-009).

## Behavioural guarantees

- **Pictures**: every rendering the library produces after this feature is byte-for-byte what
  `RenderMode::OpeningCentered` produces before it (FR-009, SC-002).
- **Rotation**: `try_rotate_90_ccw` produces identical notation before and after, and fails
  identically where it fails today (FR-006, FR-007, SC-003). Evidence: [research.md](../research.md) R1.
- **Diagram text**: text over the surviving eight characters round-trips byte for byte (FR-021).
  From Phase 4, text naming one of the eight freed characters is rejected by `VerboseDiagram::from_str`
  with the character and its one-based row and column (FR-020a).

## Command-line contract — `examples/ascii_print`

| Variable | Before | After |
|---|---|---|
| `KNOTTY_OPENING_CENTERED` | `true` selects the opening-centered rendering | **removed** — there is one rendering |
| `KNOTTY_GRID` | `true` draws cell boundaries | unchanged |
| `KNOTTY_COMPACT` | `true` strips columns | unchanged |
| `KNOTTY_PRINT_ABBREV` | `true` also prints the notation | unchanged |

Setting `KNOTTY_OPENING_CENTERED` afterwards is inert, not an error — it is an unread environment
variable like any other.

## App contract — `examples/knot-so-good`

- The "switch to opening-centered view" / "switch to standard view" button is removed from both
  notation mode and manual diagram mode.
- The compact toggle (notation mode) and the cell-boundary toggle (manual mode) are unchanged.
- **Persisted state**: the `render_mode` key is no longer written. A state that still carries it —
  with any value, known or unknown — loads without error and keeps every other setting (FR-012).
- **Symbol table**: shrinks from sixteen rows to eight in Phase 4, listing exactly the characters the
  parser accepts (FR-022).
