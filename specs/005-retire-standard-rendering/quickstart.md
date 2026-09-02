# Quickstart: validating the removal, phase by phase

**Feature**: [spec.md](./spec.md) | **Plan**: [plan.md](./plan.md)

Each phase has its own gate. A phase is not done until its gate passes **and** the baseline gate
below still passes — no phase may leave the project broken for a later one to fix (FR-005, SC-007).

## Prerequisites

```sh
rustup target list --installed        # expect wasm32-unknown-unknown (pinned in rust-toolchain.toml)
cargo test                            # baseline: 92 passing before any change
```

## Baseline gate — run at the end of every phase

```sh
cargo test
cargo check --target wasm32-unknown-unknown          # constitution II, non-negotiable
cargo check --package knot-so-good --target wasm32-unknown-unknown
cargo run --example ascii_print -- examples/<a knot file>
```

## Phase 1 — verification, before anything is deleted

Nothing is removed here; the gate is that coverage arrives while both renderings still exist.

```sh
cargo test                            # still 92 passing, plus the new tests, none failing
git diff --stat                       # expect additions only: no deletions in src/ or examples/
```

Confirm by inspection:

- The differential rotation test exists and passes, covering diagrams **with** transfers and
  asserting that both renderings produced real results rather than failing alike (FR-008a).
- The climb-cost test is stated absolutely — N levels occupy N columns — not as a ratio against the
  split-cell rendering (FR-024), and it asserts it measured at least one diagram (FR-018).
- `RenderMode::Standard` is still the default everywhere (FR-001).

## Phase 2 — rotation off the surviving rendering

```sh
cargo test rotate                     # all rotation tests and both regression tests, expectations unchanged
cargo test                            # full suite
```

Confirm `src/diagram.rs`'s `full_render_lines` names the surviving rendering and that
`src/rotate.rs` is unchanged. Rotation output must be identical to before — the differential test
from Phase 1 is what proves it, and it is still passing here because both renderings still exist.

## Phase 3 — one rendering

```sh
cargo test
grep -rn "RenderMode\|render_mode\|KNOTTY_OPENING_CENTERED" src examples README.md
# expect: no matches (SC-001)
```

Then check the surfaces by hand:

```sh
# The example program: every combination of the two remaining options still works,
# and the retired variable is inert rather than an error.
cargo run --example ascii_print -- <knot file>
KNOTTY_COMPACT=true cargo run --example ascii_print -- <knot file>
KNOTTY_GRID=true    cargo run --example ascii_print -- <knot file>
KNOTTY_COMPACT=true KNOTTY_GRID=true cargo run --example ascii_print -- <knot file>
KNOTTY_OPENING_CENTERED=true cargo run --example ascii_print -- <knot file>   # same output as the first
```

For the app (`examples/knot-so-good`, see its README for `trunk serve`):

1. No rendering-mode button in either notation mode or manual diagram mode.
2. The compact toggle still works in notation mode; the cell-boundary toggle still works in manual mode.
3. In the boundary view, every crossing, opening and closing sits inside one box (FR-015).
4. Paste a pre-feature saved state containing `"render_mode": "standard"` into local storage and
   reload: it loads, and every other setting survives (FR-012, SC-008).

Recorded pictures and documentation:

- `ls src/diagram/snapshots/` — one family of eight, named for what it records, with no
  `_opening_centered` suffix and no split-cell counterparts (FR-016).
- `README.md`'s rendered knot matches what `cargo run --example ascii_print` prints for the notation
  in the block above it (FR-019). The expected picture is in [research.md](./research.md) R5.

## Phase 4 — the half-cells go

```sh
cargo test
```

Confirm:

```sh
# Each freed character is now rejected, by name and position.
printf 'A\n' | cargo run --example ascii_print -- -   # and a, ' , j r 2 L
```

- The parse error names the character with a one-based row and column, exactly as any other
  unrecognised character does (FR-020a).
- Diagram text over `.` `_` `x` `y` `(` `)` `/` `\` still round-trips byte for byte (FR-021).
- In the app, the symbol table shows eight rows, matching the characters the parser accepts (FR-022).
- Any app snapshot whose text uses a freed character reports as invalid and the rest of the app keeps
  working (FR-020b).

## Full-feature acceptance

```sh
cargo test
cargo check --target wasm32-unknown-unknown
cargo check --package knot-so-good --target wasm32-unknown-unknown
```

Then walk the spec's Success Criteria: SC-001 and SC-013 by the `grep` above; SC-002 by the renamed
snapshots being unchanged from their pre-removal contents; SC-003 by the rotation tests; SC-012 by
the Phase 4 checks.
