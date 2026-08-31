# Phase 0 Research: Opening-Centered Rendering Mode

Findings marked *verified* were checked by porting `src/raw_lines.rs` and `Horiz::display` to a
scratch prototype, running it beside the proposed opening-centered rules, and comparing against the
checked-in snapshots. The port reproduces all eight `snapshot_ascii_print` snapshots byte for byte,
so results measured with it are trustworthy. The prototype was scratch-only and is not part of the
delivery.

## R1 — The mode is a runtime parameter, not a second const generic

**Decision**: Add `pub enum RenderMode { #[default] Standard, OpeningCentered }` and pass it by value.
`GRID_BORDERS` stays the const generic it already is.

```rust
impl Horiz     { pub const fn display(&self, mode: RenderMode) -> [&'static str; DISPLAY_LINES]; }
impl VerboseDiagram {
    pub fn from_abbreviated(knot: &AbbreviatedDiagram, mode: RenderMode) -> Result<Self, String>;
    pub fn display<'a, const GRID_BORDERS: bool>(&'a self, mode: RenderMode) -> impl 'a + Iterator<Item = String>;
}
```

**Rationale**: Spec 002 R1 decided *not* to add runtime dispatch to the library for `GRID_BORDERS`,
and branching at the call site was right there — the const generic already existed and the choice was
whether to paper over it. This case is different in two ways that flip the answer:

- The mode must also reach `VerboseDiagram::from_abbreviated`, which is not generic. Expressing the
  mode as a const generic on `display` and as a separate constructor for building would say one thing
  two ways, and nothing would stop a caller pairing a `Standard` grid with an `OpeningCentered` draw.
- Two const generic `bool`s multiply. FR-010 requires all eight combinations of mode × compact ×
  borders from the library and the CLI, which is literally an eight-arm `match` in
  `examples/ascii_print.rs` and a four-arm one in the app's `render_manual`. A runtime parameter keeps
  every consumer at the branch count it has today.

There is no monomorphisation to lose: the tile lookup is a `match` returning `&'static str` either way.

**Alternatives considered**:

- *Second const generic* `display::<MODE, GRID_BORDERS>()`. Rejected for the combinatorics above.
- *Parallel methods* (`display_opening_centered::<B>()`). Rejected: it doubles every entry point
  (`try_ascii_print`, `try_ascii_print_compact`, `from_abbreviated`, `display`) and still leaves the
  pairing between builder and drawer unstated.
- *Store the mode inside `VerboseDiagram`*. Rejected: `FromStr` (spec 001) parses diagram text that
  carries no mode, and FR-009 requires the same parsed text to be drawable both ways.

## R2 — Opening-centered needs its own grid, not a re-skin *(verified)*

**Decision**: Build a separate grid for the mode; do not reuse the current-mode grid with a different
tile table.

**Rationale**: Measured. Applying the spec's cell table to the *existing* trefoil grid produces a
picture whose arms do not meet — the opening's upper arm is left dangling a full cell above the strand
it should join:

```
     /         \        <- the opening's upper arm
    (           )
     \_________/
      \ /   \ /         <- the row above's strand, one whole cell too low
```

The cause is the placement law in R3: the current grid puts a feature's main glyph at row `idx + 1`,
while opening-centered needs it at row `idx`. No per-cell substitution fixes that, because the same
row also carries `Line` cells that must not move.

## R3 — The placement law *(verified)*

**Decision**: In both modes a strand at level `L` lives in grid row `L`. What differs is where a
feature goes and how it draws:

| | current | opening-centered |
|---|---|---|
| feature at abbreviated index `idx` | rows `idx` (partner half) and `idx + 1` (main glyph) | row `idx` only |
| strand at level `L` | `Line` at row `L`, drawn on the cell's **top** line | `Line` at row `L`, drawn on the cell's **bottom** line |
| cell directly above a feature | carries the feature's other half | **must be empty** |

The last row is the one that is easy to get wrong. A feature at row `idx` joins level `idx` at its
bottom-right corner and level `idx + 1` at its top-right corner, both inside its own cell. A `Line` in
the cell above would draw underscores along that whole top edge — a strand a full cell too long, and
overlapping the feature's own marks. Prototyping without this rule put a stray `__` above the unknot.
Call row `idx + 1` the feature's *shadow row*: it is blank in the feature's column.

Grid height is unchanged: `AbbreviatedDiagram::height()` serves both modes. The top row of an
opening-centered grid only ever holds `Line` and `Empty`, since the highest feature sits at row
`height - 2`.

## R4 — Filler cells need explicit liveness *(verified)*

**Decision**: The opening-centered builder tracks a `Vec<bool>` of live levels and fills each new
column from it, rather than reading the previous column.

**Rationale**: This is the shortcut the user description warned about. `raw_lines::advance` fills a
column with `line.last().subsequent()`, which works today because a feature's two halves both imply
`Line`. Opening-centered, the cell above an opening is the shadow row — `Empty` — while the level it
represents is live, so `subsequent()` would end the strand right where it starts.

**Alternatives considered**:

- *Diagonal lookup*: filler at `(r, c)` is `Line` if `(r, c-1)` carries level `r` on its right edge or
  `(r-1, c-1)` carries level `r` on its top-right corner. Rejected: it recovers liveness from geometry
  every column, which is both slower and harder to read than keeping the two lines of state that the
  builder already has in hand. Worth remembering as the rule to fall back on if the builder is ever
  reworked to stream.

## R5 — Transfers cost one cell per level *(verified)*

**Decision**: `TransferUp` (`i`) raises a strand from level `r` to `r + 1` in the single cell at row
`r`; `TransferDown` (`k`) lowers level `r + 1` to `r` in the cell at row `r`. A whole stack can move
in one column, since each strand's cell is in its own row. `TransferUpStart`, `TransferUpFinish`,
`TransferDownStart` and `TransferDownFinish` are never emitted.

Making room for an opening inside an occupied stack is therefore two rise columns followed by the
opening's own column. Closing is the closing's own column followed by two fall columns. Neither can
share a column with the feature, because of the shadow row (R3).

**Measured consequence, and a correction to the spec as first drafted**: this does *not* make the
picture narrower. The current rendering also spends three columns there — it raises two levels over
three columns using half-cells that begin and end a rise part way through a cell, where
opening-centered raises one whole level per cell and spends the third column on the opening. Across
all eight sample knots the uncompacted widths are identical in both modes. After compaction the
opening-centered picture is a few columns *wider* (donut 20 vs 18, terrace 64 vs 58), because the
columns the compact pass can strip are not the same ones. The spec was corrected to say this.

## R6 — Trimming is mirrored *(verified)*

**Decision**: `VerboseDiagram::display` keeps its current trim for `Standard` — the last row emits only
its first line — and for `OpeningCentered` the *first* row emits only its last content line, plus the
border line when `GRID_BORDERS` is on.

**Rationale**: An opening-centered picture sits exactly two text lines lower inside its grid than the
current one, so the blank lines land at the top instead of the bottom. With borders the trim is not a
plain `skip(2)`: the four lines of a bordered cell are `[border, l0, l1, l2]` and the top row must keep
indices `0` and `3`, so the picture keeps its top rule. Trimming stays unconditional in both modes,
matching how the current mode already treats a hand-written diagram whose bottom row holds a feature.

## R7 — Transfer-free knots render identically *(verified)*

**Decision**: Assert byte equality between the two modes for every sample knot whose current-mode grid
contains no transfer cell, deriving that set in the test rather than hard-coding it.

**Measured**, over the eight knots in `snapshot_ascii_print`:

| knot | has transfers | full renders equal | compact renders equal |
|---|---|---|---|
| unknot | no | yes | yes |
| trefoil | no | yes | yes |
| donut, c_thingy, terrace, basket, ugly_trefoil, weird | yes | no | no |

This is the sharp regression test FR-007 and SC-003 ask for: for the transfer-free class any
difference at all is a defect, and the test keeps working as sample knots are added.

## R8 — Retired characters normalise on serialisation, not on parse

**Decision**: `Horiz::from_byte` and `fmt::Display` are unchanged. Normalisation is a mode-aware
serializer, `VerboseDiagram::to_text(&self, mode: RenderMode) -> String`, which maps the eight retired
variants to `Empty` when the mode is `OpeningCentered`. `Display` keeps emitting the spec 001 canonical
text for all sixteen.

**Rationale**: FR-005 makes the eight synonyms of `_` *under the new mode*, and FR-009 forbids the mode
from changing the diagram text. Normalising at parse time would do both wrong: it would lose
information the current mode still needs, and it would depend on a mode the parser does not have. The
retired cells already draw blank for free — the second tile table returns `"   "` for them — so the
only observable place normalisation is needed is serialisation, which is exactly FR-016's fixed-point
claim: `parse(t).to_text(OpeningCentered)` settles in one pass.

## R9 — Rotation stays on the current rendering

**Decision**: `AbbreviatedDiagram::full_render_lines` keeps calling `display::<false>(RenderMode::Standard)`
regardless of what the caller has selected, and `try_rotate_90_ccw` is unchanged.

**Rationale**: This closes the item `/speckit-clarify` deferred. `rotate::scan_row` recovers notation
from rendered ASCII with regexes — `r"/_*\\"`, `r" _+ "`, `r"\\ /"` — that encode the current tile
shapes and the current strand-at-the-top-of-the-cell geometry. Rotation is a diagram *manipulation*,
not a display, and the spec's Assumptions already say manipulation is unaffected by the mode. Feeding
it opening-centered output would silently produce wrong notation.

## R10 — The app's downstream rendering needs no change

**Decision**: Leave `ascii_diagram_to_html` and the svgbob path alone.

**Rationale**: The opening-centered tiles draw from exactly the glyph set the current ones do —
`space`, `_`, `/`, `\`, `(`, `)`, plus `+` and `|` with borders — all already on the allow-list that
`ascii_diagram_to_html` panics outside of. The SVG display renders whatever ASCII it is handed, so it
follows the selected mode with no change at all.

## R11 — Naming

**Decision**: `RenderMode::Standard` (the `Default`) and `RenderMode::OpeningCentered`.

**Rationale**: `Standard` is neutral and stable; `Current` would read as a lie the moment a third mode
appears, and naming the existing mode after its geometry (`SplitCell`, `TopAligned`) would rename an
established behaviour for the benefit of a new one. `Default` on `Standard` makes
`RenderMode::default()` the existing behaviour, which is what FR-008 wants.
