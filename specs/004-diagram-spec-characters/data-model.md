# Phase 1 Data Model: Revised Diagram Text Format Symbol Table

This feature introduces no new types, fields, or state transitions. It changes the values of an
existing fixed mapping. This document records that mapping as the feature's one relevant "entity."

## Entity: Symbol Table (`Horiz` character mapping)

A one-to-one correspondence between a single ASCII byte and a `Horiz` cell kind, implemented as the
paired `const fn`s `Horiz::as_byte` (cell → byte) and `Horiz::from_byte` (byte → cell, `Option`) in
`src/render.rs`. There are sixteen `Horiz` variants; every variant has exactly one byte, and every
mapped byte has exactly one variant (enforced today by the existing `byte_mapping_round_trips` and
`byte_mapping_is_distinct` tests, which this feature keeps passing under the new values).

### Fields (per entry)

| Field | Description |
|-------|-------------|
| `Horiz` variant | One of the sixteen renderable cell kinds (e.g. `Empty`, `Line`, `CrossDownOver`) |
| byte | The single ASCII character naming that variant in diagram text |

### Values changed by this feature

| `Horiz` variant   | Old byte | New byte |
|-------------------|----------|----------|
| `Empty`           | `_`      | `.`      |
| `Line`             | `-`      | `_`      |
| `CrossDownOver`    | `\`      | `x`      |
| `CrossDownUnder`   | `/`      | `y`      |
| `OpenedAbove`      | `.`      | `'`      |
| `TransferUp`       | `i`      | `/`      |
| `TransferDown`     | `k`      | `\`      |

### Values unchanged by this feature

| `Horiz` variant       | Byte |
|-----------------------|------|
| `CrossUpOver`         | `A`  |
| `CrossUpUnder`        | `a`  |
| `OpenedBelow`         | `(`  |
| `ClosedBelow`         | `)`  |
| `ClosedAbove`         | `,`  |
| `TransferUpStart`     | `j`  |
| `TransferUpFinish`    | `r`  |
| `TransferDownStart`   | `2`  |
| `TransferDownFinish`  | `L`  |

### Invariants (unchanged by this feature)

- Every `Horiz` variant maps to exactly one byte (`byte_mapping_round_trips`).
- No two variants share a byte (`byte_mapping_is_distinct`).
- Bytes outside the table (including all whitespace and every retired character not reassigned to
  a new cell) map to `None` from `from_byte` (`unrecognized_bytes_have_no_mapping`).

### Relationships

- `VerboseLine` (a row of the diagram) is a sequence of `Horiz` cells; its text form is the
  concatenation of each cell's byte via `Horiz::as_byte`, and it is parsed back via
  `Horiz::from_byte` per byte (see `src/render.rs`, `VerboseDiagram`'s `FromStr`/`Display`
  implementations). This feature changes none of that structure — only the byte each cell reads or
  writes.
