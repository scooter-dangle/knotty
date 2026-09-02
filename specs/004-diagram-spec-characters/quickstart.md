# Quickstart: Validating the Revised Diagram Text Format Symbol Table

## Prerequisites

- Rust toolchain as pinned in `rust-toolchain.toml` (installed automatically by `cargo` via
  rustup if you have the `rust-toolchain.toml` file present).
- Run all commands from the repository root.

## Run the existing + updated unit tests

```sh
cargo test --lib render::
```

Expected: all tests in `src/render.rs`'s test module pass, including
`byte_mapping_round_trips`, `byte_mapping_is_distinct`, and `unrecognized_bytes_have_no_mapping`
(see `contracts/symbol-table.md` for what each guarantees) and the trefoil/unknot round-trip tests
using the new character literals.

## Confirm WASM compatibility (Constitution Article II)

```sh
cargo check --target wasm32-unknown-unknown
```

Expected: succeeds with no errors — this feature adds no new dependency and touches only `const
fn`s already WASM-compatible.

## Manually verify the trefoil round trip

The worked example from `specs/001-verbose-diagram-text-format/spec.md`, rewritten under the new
mapping, is:

```
.(___).
.'y_y,.
(_AxA_)
'__a__,
```

1. Confirm this text, parsed and rendered, matches the picture produced by the abbreviated
   notation `(0 (2 /1 \0 /1 )2 )0` (see `contracts/symbol-table.md` for the full character table
   used to derive it).
2. Confirm producing compact text from that same rendering reproduces this exact text byte for
   byte (round-trip fidelity, spec 001 FR-008, unaffected by this feature).

## Check the in-app symbol table reference

```sh
cd examples/knot-so-good && trunk serve
```

Open the app, switch to manual diagram mode, and open the symbol table reference. Expected: it
lists exactly the sixteen entries in `contracts/symbol-table.md`'s Mapping table, since it is
generated from `Horiz::as_byte` rather than a separate hard-coded copy — no code change to the app
is required for this to be true.
