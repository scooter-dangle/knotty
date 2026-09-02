# Contract: Diagram Text Format Symbol Table

This is the library's public contract for the diagram text format's character-to-cell mapping —
the same contract published in `specs/001-verbose-diagram-text-format/spec.md` FR-002, revised by
this feature. It is implemented by `Horiz::as_byte` / `Horiz::from_byte` in `src/render.rs`, and it
is what `VerboseDiagram`'s text parser (`FromStr`) and writer (`Display`/`to_text`) read and write
one row at a time, and what the example app's in-app reference table renders.

## Mapping (post-change)

| Char | Cell               | Char | Cell                 |
|------|--------------------|------|----------------------|
| `.`  | Empty              | `,`  | ClosedAbove          |
| `_`  | Line               | `j`  | TransferUpStart      |
| `x`  | CrossDownOver      | `/`  | TransferUp           |
| `y`  | CrossDownUnder     | `r`  | TransferUpFinish     |
| `A`  | CrossUpOver        | `2`  | TransferDownStart    |
| `a`  | CrossUpUnder       | `\`  | TransferDown         |
| `(`  | OpenedBelow        | `L`  | TransferDownFinish   |
| `'`  | OpenedAbove        |      |                      |
| `)`  | ClosedBelow        |      |                      |

## Guarantees

- **Total on cells**: every one of the sixteen `Horiz` variants has exactly one character.
- **Injective**: no two variants share a character.
- **Case-sensitive**: `A`/`a` and `L`/`l` (the latter unmapped) are distinct.
- **Closed**: any byte not listed above — including every whitespace byte and the five retired
  bytes not reassigned (`-`, `i`, `k`, plus `\` and `/` only in their *old* meanings) — is rejected
  by `from_byte` (`None`), and reported by the parser as an unrecognized character with its row and
  column (per `specs/001-verbose-diagram-text-format/spec.md` FR-009, unchanged by this feature).
- **Canonical writer output**: `to_text`/`Display` on any `VerboseDiagram` emits only characters
  from this table.

## Consumers of this contract

- `src/render.rs`: `Horiz::as_byte`, `Horiz::from_byte`, and the `VerboseDiagram`/`VerboseLine`
  parse and display logic built on them.
- `examples/knot-so-good/src/main.rs`: `SYMBOL_TABLE` renders this table in the app's manual-mode
  reference by calling `Horiz::as_byte` per variant — it has no independent copy to keep in sync.

## Verification

- `src/render.rs` unit tests `byte_mapping_round_trips`, `byte_mapping_is_distinct`, and
  `unrecognized_bytes_have_no_mapping` verify the Guarantees section above against this exact table
  (see `quickstart.md` for how to run them).
