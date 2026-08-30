# Quickstart: Validating the Verbose Diagram Text Format

How to prove the feature works, in the order the work lands. The library half (scenarios 1–3) is
independently shippable and can be validated with no app changes at all.

## Prerequisites

```bash
rustup target add wasm32-unknown-unknown   # or rely on rust-toolchain.toml
cargo install --locked trunk               # only for the app scenarios
```

The toolchain is pinned to 1.94.0 in `rust-toolchain.toml`; no other setup is needed.

## Scenario 1 — The format reads and renders (P1, FR-005/006/009/010/012)

```bash
cargo test
```

**Expect**: the new `render` tests pass, covering

- the canonical trefoil text rendering identically to `(0 (2 /1 \0 /1 )2 )0` — the picture in
  [contracts/diagram-text-format.md](./contracts/diagram-text-format.md) is the reference;
- ragged rows rendering the same as their padded equivalent;
- empty input yielding an empty diagram, not an error;
- `_(---)_` on line 1 producing the arc across the **top** of the picture, not the bottom.

**Watch for the row-order trap.** A test that only checks "renders without error" will pass with the
picture upside down. Assert against a rendered picture, not just `is_ok()`.

**Error positions** — assert on the message, not just `is_err()`:

```bash
cargo test -- error_position
```

A stray character on the user's **first** line must report line 1. Reporting line 4 of a 4-row
diagram is the reversal bug described in [research.md](./research.md).

## Scenario 2 — Round trips are byte-stable (P2, FR-007/008)

```bash
cargo test -- round_trip
```

**Expect**: guarantees C-1 through C-7 in the contract hold — the mapping round-trips over all 16
variants, output rows are equal length, and ragged input normalizes to canonical text that is then a
fixed point.

## Scenario 3 — Snapshots and wasm (constitution II, III)

```bash
cargo insta review                              # accept new snapshots, then commit them
cargo check --target wasm32-unknown-unknown     # NON-NEGOTIABLE gate
```

**Expect**: snapshots land under `src/snapshots/`, and the wasm check passes — it will, since the new
code is `core`/`alloc` only, but the gate is non-negotiable so run it before calling the library done.

## Scenario 4 — App state survives (P3, FR-024/025/026)

```bash
cd examples/knot-so-good && cargo test
```

**Expect**: the persistence tests pass, including a pre-feature JSON blob (no `mode`, no
`manual_diagram`) still loading as notation mode with empty manual state. Add that case beside the
existing `missing_fields_use_defaults` test rather than inventing a new pattern.

## Scenario 5 — Manual mode in the browser (P3, FR-015..023, 027..031)

```bash
cd examples/knot-so-good && trunk serve --port 3000
```

Then at `localhost:3000`, walk this path:

1. Load a built-in knot, e.g. **trefoil**. Confirm the collapsed compact-text readout shows the
   canonical four lines and does not compete with the existing encoding line (FR-028, FR-029).
2. Switch to manual mode. The text box is **pre-filled** with that compact text; every
   notation-driven control is gone — notation and moves boxes, knot buttons, move pickers, rotate,
   encoding readout (FR-019, FR-030).
3. Confirm the picture is plain ASCII, with no SVG toggle, no compact toggle, no SVG download
   (FR-020).
4. Delete a trailing character so a row is short. The picture still renders — **no need to balance
   line lengths** (FR-011). This is the behavior the whole format exists for.
5. Type a `b` or a space. The picture **stays on screen**, visibly marked stale, with the error
   beside it. Fix the character; the marking clears (FR-017). Confirm the snapshot button is
   unavailable while invalid (FR-023).
6. Clear the box entirely, then type `b`. Now the error shows **alone** — there is no prior render to
   keep (FR-018).
7. Take a snapshot. Switch to notation mode: your notation, moves, and notation snapshots are
   untouched, and the manual snapshot is **not** in this mode's catalog (FR-021, FR-024).
8. Switch back to manual: your text is exactly as you left it — the seed does **not** overwrite it
   (FR-031).
9. Reload the page: mode, text, and manual snapshots all return (FR-025).
10. Confirm the symbol table is reachable from the manual surface without leaving the app (FR-027).

## Scenario 6 — Full CI parity

```bash
cargo check --target wasm32-unknown-unknown
cargo build
cargo test
cd examples/knot-so-good && cargo test && trunk build --release
```

This mirrors `.github/workflows/test.yml` exactly. Run it before pushing — the example app is built
by a separate CI job, so a library change that compiles can still break the app build.
