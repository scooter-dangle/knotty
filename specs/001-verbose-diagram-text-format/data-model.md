# Phase 1 Data Model: Verbose Diagram Text Format

The feature adds **no new core data type**. It adds two conversions over types that already exist,
plus three fields to the example app's persisted state.

## Core library (`src/render.rs`) — existing types, new conversions

### `Horiz` (existing enum, 16 variants)

Gains a bidirectional byte mapping. The table is fixed and one-to-one (spec FR-002, FR-003).

| Byte | Variant | Byte | Variant |
|------|---------|------|---------|
| `_` | `Empty` | `,` | `ClosedAbove` |
| `-` | `Line` | `j` | `TransferUpStart` |
| `\` | `CrossDownOver` | `i` | `TransferUp` |
| `/` | `CrossDownUnder` | `r` | `TransferUpFinish` |
| `A` | `CrossUpOver` | `2` | `TransferDownStart` |
| `a` | `CrossUpUnder` | `k` | `TransferDown` |
| `(` | `OpenedBelow` | `L` | `TransferDownFinish` |
| `.` | `OpenedAbove` | | |
| `)` | `ClosedBelow` | | |

**Validation rules**

- Case-sensitive: `A` ≠ `a`, and `l` is unrecognized where `L` is valid (FR-003).
- Every byte outside this table is rejected, **whitespace included** (FR-004).
- `\r` is recognized only as part of a line terminator, never as a cell (FR-013).

### `VerboseLine` (existing) — one row

A row of cells. Rows are *not* individually parsed or written; they exist only inside a diagram,
because a row's canonical width depends on the diagram's widest row.

### `VerboseDiagram` (existing) — the diagram

Gains `FromStr` and `Display`.

**Field ordering — the one thing to get right**: `VerboseDiagram.0[0]` renders at the **bottom** of
the picture (`display()` iterates `.rev()`). The text format is top-first. Both conversions reverse.

**Validation and normalization on read**

1. Strip one trailing `\n`; split on `\n`; strip a trailing `\r` per line.
2. Map each byte through the table, failing on the first unrecognized one with its **input** line and
   column, both 1-based (FR-009 — see research.md, Trap 1).
3. Pad every row on the right with `Empty` to the widest row's width (FR-010).
4. Reverse the row order and store.

**Invariants after read**

- All rows have equal length.
- Empty input yields zero rows (FR-012).
- A blank line yields a row that is entirely `Empty` — preserved, not dropped (FR-013).
- No validation that the picture describes a real knot (FR-014).

**Canonical form on write**

Full padded rectangle, nothing trimmed — no trailing empty cells, no empty edge rows or columns
(FR-007). Rows emitted in reverse of storage, each terminated by `\n`.

**Round-trip guarantees** (FR-008)

- `parse(write(d)) == d` for every diagram the library can render.
- `write(parse(write(d))) == write(d)` byte for byte.
- `write(parse(t))` is the canonical text for any accepted `t`, canonical or ragged.

### Worked instance — the unknot

`(0 )0` renders as a 2-row diagram. Stored order (index 0 first) versus text order (top first):

| Stored index | Cells | Text line |
|---|---|---|
| `1` | `OpenedBelow, ClosedBelow` | line 1: `()` |
| `0` | `OpenedAbove, ClosedAbove` | line 2: `.,` |

```text
()
.,
```

renders as

```text

  /\
 (  )
  \/
```

## Example app (`examples/knot-so-good/src/main.rs`)

### `Mode` (new, in-memory)

`Notation | Manual`. Selects which state and which view are live. Not a display style — the existing
`DisplayMode` (`Ascii | Svg`) stays as-is and applies only to notation mode (FR-020).

### `Model` (existing, restructured)

Holds both modes' state simultaneously so neither is disturbed by a switch (FR-024):

| Field group | Fields | Notes |
|---|---|---|
| shared | `mode`, `storage_error` | |
| notation | existing fields, plus `compact_text` | `compact_text` feeds the collapsed readout (FR-028) |
| manual | `manual_text`, `manual_parsed: Result<VerboseDiagram, String>`, `last_valid_render: Option<String>`, `manual_snapshots` | three-state render logic per research.md |

**State transitions**

- Notation → Manual: seed `manual_text` from the current diagram's compact text **only if
  `manual_text` is empty** (FR-030, FR-031).
- Manual → Notation: nothing is copied back. The bridge is one-directional (spec Assumptions).
- Text edited: re-parse. Ok → render and store as `last_valid_render`. Err → keep
  `last_valid_render`, mark stale, show error (FR-017); if `None`, show error alone (FR-018).

### `PersistedState` (existing, three new fields)

| Field | Serde | Purpose |
|---|---|---|
| `mode` | `#[serde(default)]`, enum with `#[serde(other)] Other` | FR-025; `Other` → `Notation` |
| `manual_diagram` | `#[serde(default)]` | the manual text (FR-025) |
| `manual_snapshots` | `#[serde(default)]` | separate list (FR-021, FR-025) |

All three default, so state written before this feature loads as notation mode with no manual data
(FR-026) — the same pattern the existing `missing_fields_use_defaults` test already covers.

### `PersistedManualSnapshot` (new)

Single field: the diagram text it was taken from. The ASCII preview is re-rendered on display rather
than stored, so a preview can never disagree with its text (FR-022). Kept in a list distinct from
`snapshots`; neither catalog shows the other's entries (FR-021).
