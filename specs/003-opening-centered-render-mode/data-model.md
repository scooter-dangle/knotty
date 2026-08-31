# Phase 1 Data Model: Opening-Centered Rendering Mode

## `RenderMode` (new, public)

```rust
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum RenderMode {
    #[default]
    Standard,
    OpeningCentered,
}
```

| Field | Meaning |
|---|---|
| `Standard` | The rendering the crate has always produced. Default, so `RenderMode::default()` preserves existing behaviour (FR-008). |
| `OpeningCentered` | Parentheses on the middle line of their cell; every feature whole inside one cell. |

No state, no lifecycle. It selects two things that must agree: **how a grid is built** from
abbreviated notation, and **how a grid is drawn**. Pairing a grid built in one mode with a draw in the
other is meaningless and the type system does not prevent it — see the contract.

## `Horiz` (existing, extended)

Unchanged as a type: the same sixteen variants, the same bytes, the same `FromStr`/`Display`. What is
added is a second tile table, selected by `RenderMode`.

| byte | variant | `Standard` cell | `OpeningCentered` cell |
|---|---|---|---|
| `_` | `Empty` | blank | blank |
| `-` | `Line` | `___` on the top line | `___` on the bottom line |
| `\` | `CrossDownOver` | `\ /` / ` \ ` on lines 1–2 | `\ /` / ` \ ` / `/ \` |
| `/` | `CrossDownUnder` | `\ /` / ` / ` on lines 1–2 | `\ /` / ` / ` / `/ \` |
| `A` | `CrossUpOver` | `/ \` on line 0 | blank |
| `a` | `CrossUpUnder` | `/ \` on line 0 | blank |
| `(` | `OpenedBelow` | `  /` / ` ( ` on lines 1–2 | `  /` / ` ( ` / `  \` |
| `.` | `OpenedAbove` | `  \` on line 0 | blank |
| `)` | `ClosedBelow` | `\  ` / ` ) ` on lines 1–2 | `\  ` / ` ) ` / `/  ` |
| `,` | `ClosedAbove` | `/  ` on line 0 | blank |
| `j` | `TransferUpStart` | `__/` on line 0 | blank |
| `i` | `TransferUp` | `  /` / ` / ` / `/  ` | identical |
| `r` | `TransferUpFinish` | `  _` / ` / ` / `/  ` | blank |
| `2` | `TransferDownStart` | `_  ` / ` \ ` / `  \` | blank |
| `k` | `TransferDown` | `\  ` / ` \ ` / `  \` | identical |
| `L` | `TransferDownFinish` | `\__` on line 0 | blank |

The eight drawn blank are the *retired* variants. Under `OpeningCentered` they are synonyms of
`Empty`: accepted, drawn blank, and serialised as `_` (FR-005).

### Derived classification

Two groupings the builder and the tests need, both derivable from the variant:

- **retired** — `CrossUpOver`, `CrossUpUnder`, `OpenedAbove`, `ClosedAbove`, `TransferUpStart`,
  `TransferUpFinish`, `TransferDownStart`, `TransferDownFinish`.
- **transfer** — `TransferUp*`, `TransferDown*` (all six). Used to derive the transfer-free sample set
  for the cross-mode equality test (R7) rather than hard-coding it.

## `VerboseDiagram` grid semantics

The type is unchanged — `Vec<VerboseLine>`, row 0 at the bottom of the picture. What the rows *mean*
depends on the mode the grid was built in.

| | `Standard` | `OpeningCentered` |
|---|---|---|
| strand at level `L` | `Line` at row `L` | `Line` at row `L` |
| feature at abbreviated index `idx` | main glyph at row `idx + 1`, partner half at row `idx` | glyph at row `idx` only |
| cell above a feature (`idx + 1`) | the partner half | **empty** — the *shadow row* |
| raising a stack two levels | three columns (`j`, `i`, `r` staggered) | two columns of `i`, one level each |
| making room for an opening | three columns, opening written into the third | two rise columns, then the opening's own column |
| closing into an occupied stack | three columns, closing written into the first | the closing's own column, then two fall columns |
| grid height | `AbbreviatedDiagram::height()` | identical |
| top row | may hold features | only `Line` and `Empty` |

### Validation rules

1. A feature's shadow row is `Empty` in that feature's column. Violating it draws a strand one cell
   too long, overlapping the feature.
2. A filler cell is `Line` exactly when its level is live at that column. Liveness is tracked, not
   inferred from the cell to the left (R4).
3. Under `OpeningCentered` the builder never emits a retired variant.

### Builder state

The opening-centered builder carries one piece of state the current one does not:

- `live: Vec<bool>`, one entry per level, length `height()`. `(` at `idx` sets `idx` and `idx + 1`;
  `)` at `idx` clears them; a crossing leaves them alone; a rise column shifts every live level at or
  above `idx` up by one; a fall column shifts them down by one.

## App persisted state (example app only)

```rust
enum PersistedRenderMode { Standard, OpeningCentered, #[serde(other)] Other }
```

One new field on `PersistedState` and one on `Model`, following `PersistedDisplayMode` exactly,
including the `Other` fallback so an unknown stored value degrades to the default rather than
discarding the whole saved state. The field is **shared by both app modes** — notation and manual read
and write the same one (FR-012, FR-013).
