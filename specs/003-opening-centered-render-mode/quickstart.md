# Quickstart: Validating Opening-Centered Rendering

Runnable checks that prove the feature works end to end. Details of the API live in
[contracts/opening-centered-rendering.md](./contracts/opening-centered-rendering.md); the cell shapes
and grid rules live in [data-model.md](./data-model.md).

## Prerequisites

```sh
rustup show                      # 1.94.0, wasm32-unknown-unknown target (rust-toolchain.toml)
cargo install --locked trunk     # only for the app checks
```

## 1. Nothing existing changed (FR-008, SC-002)

```sh
cargo test
```

**Expect**: green, with **no** snapshot in `src/snapshots/` or `src/diagram/snapshots/` reported as
changed. A pending `.snap.new` for an existing snapshot is a failure of this feature, not a snapshot
to accept. New snapshots for the opening-centered rendering are expected and are reviewed with
`cargo insta review`.

## 2. The library renders both ways (FR-001, US1)

```sh
cargo run --example ascii_print examples/samples/trefoil_1_knot
KNOTTY_OPENING_CENTERED=true cargo run --example ascii_print examples/samples/trefoil_1_knot
```

**Expect**: byte-identical output. The trefoil has no transfers, so the two renderings agree exactly —
this is the transfer-free equality guarantee in its most direct form.

```sh
cargo run --example ascii_print examples/samples/trefoil_1_knot | md5sum
KNOTTY_OPENING_CENTERED=true cargo run --example ascii_print examples/samples/trefoil_1_knot | md5sum
```

**Expect**: the same digest.

## 3. Transfers are where the two differ (FR-007, SC-009)

```sh
cargo run --example ascii_print examples/samples/square_knot
KNOTTY_OPENING_CENTERED=true cargo run --example ascii_print examples/samples/square_knot
```

**Expect**: two pictures of the same knot, the same width, differing in how the climbing strands are
stepped — one whole level per cell in the opening-centered one. This difference is intended; see the
spec's Edge Cases.

## 4. Every feature is whole inside one cell (SC-004, US1)

```sh
KNOTTY_OPENING_CENTERED=true KNOTTY_GRID=true cargo run --example ascii_print examples/samples/trefoil_1_knot
KNOTTY_GRID=true cargo run --example ascii_print examples/samples/trefoil_1_knot
```

**Expect**: in the opening-centered picture every `(`, `)` and crossing sits inside a single box; in
the current one every one of them straddles a boundary line. The spec's Worked Example shows both.

## 5. All eight combinations are reachable (FR-010, SC-007)

```sh
for oc in "" true; do for c in "" true; do for g in "" true; do
  echo "== oc=${oc:-off} compact=${c:-off} grid=${g:-off}"
  KNOTTY_OPENING_CENTERED=$oc KNOTTY_COMPACT=$c KNOTTY_GRID=$g \
    cargo run -q --example ascii_print examples/samples/trefoil_1_knot
done; done; done
```

**Expect**: eight distinct, well-formed pictures; no panic, no empty output.

## 6. Retired characters are synonyms of `_` (FR-005, FR-016, SC-008)

Covered by the library test suite rather than a command line, since it exercises `to_text`. The
behaviour to confirm in review:

- diagram text naming `A a . , j r 2 L` is accepted under either mode;
- under `OpeningCentered` those cells draw blank and `to_text` writes them as `_`;
- `to_text(OpeningCentered)` applied twice equals applying it once;
- `to_text(Standard)` still round-trips all sixteen characters byte for byte (spec 001).

## 7. WASM still builds (constitution Article II)

```sh
cargo check --target wasm32-unknown-unknown
```

**Expect**: clean. Run before marking any library task done.

## 8. The app (US2, FR-012, FR-013)

```sh
cd examples/knot-so-good && trunk serve --port 3000   # then open localhost:3000
```

Walk through:

1. In notation mode, enter a knot and press the rendering toggle. **Expect**: the picture is redrawn
   opening-centered; the notation text is untouched.
2. Press it again. **Expect**: exactly the picture shown before.
3. Switch to manual diagram mode. **Expect**: the toggle is still on the mode you left it in — the two
   app modes share one setting.
4. In manual mode, turn on the bordered view. **Expect**: every feature inside one box.
5. Type an unrecognised character, then toggle the rendering. **Expect**: the stale picture stays on
   screen with its error; fix the character and it redraws in the selected mode.
6. Reload the page. **Expect**: the same rendering mode still selected, with the same diagram.
7. In notation mode, switch to the SVG display. **Expect**: the SVG follows the selected rendering,
   since it is drawn from the same ASCII.

```sh
cargo test --manifest-path examples/knot-so-good/Cargo.toml
```

**Expect**: green, including the persistence round-trip for the new field.
