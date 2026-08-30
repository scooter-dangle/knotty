# Quickstart: Validating the Cell Boundary View

How to prove the feature works. The automated half (scenarios 1–3) covers the rendering contract and
persistence; the manual half (scenarios 4–6) covers the parts of the spec that live in the DOM, which
this app has no harness for.

## Prerequisites

```bash
rustup target add wasm32-unknown-unknown   # or rely on rust-toolchain.toml
cargo install --locked trunk               # only for the app scenarios
```

The toolchain is pinned to 1.94.0 in `rust-toolchain.toml`; no other setup is needed.

## Scenario 1 — The bordered rendering is what the feature promises (SC-001, FR-003)

```bash
cargo test
cargo insta review        # accept the new display::<true>() snapshot, then commit it
```

**Expect**: the new snapshot under `src/snapshots/` shows the trefoil with `+---` boundary lines, and
the geometry test in the app's suite asserts, for a diagram parsed from text:

- one border line per line of text;
- one `+---` group per character of the widest line;
- `4 × rows − 2` lines in total, against `3 × rows − 2` for the plain view.

The reference numbers are in [contracts/bordered-rendering.md](./contracts/bordered-rendering.md).

**Watch for**: a test that only asserts "the output contains `+`". That passes on a rendering with the
grid drawn in the wrong place. Assert the counts.

## Scenario 2 — The setting persists and defaults off (FR-002, FR-012, FR-014)

```bash
cargo test --manifest-path examples/knot-so-good/Cargo.toml
```

**Expect**:

- state JSON with no `manual_borders` key deserializes with the view **off** — this is the
  back-compatibility guarantee, and it is the one that silently regresses if the field ever loses its
  `#[serde(default)]`;
- `manual_borders: true` round-trips through save and load.

## Scenario 3 — The wasm gate (constitution II)

```bash
cargo check --target wasm32-unknown-unknown
cargo check --manifest-path examples/knot-so-good/Cargo.toml --target wasm32-unknown-unknown
```

**Expect**: both pass. They will — nothing added here is `std`-only — but the gate is non-negotiable.

## Scenario 4 — The toggle, in the app (US1: FR-001, FR-004, FR-005)

```bash
cd examples/knot-so-good && trunk serve --port 3000
```

Then, at `localhost:3000`:

1. Switch to manual diagram mode and paste the trefoil text:

   ```text
   _(---)_
   _./-/,_
   (-A\A-)
   .--a--,
   ```

2. **Expect** the seamless picture, and no boundary lines — the view is off by default.
3. Press the boundary-view button. **Expect** the same picture with a 4-row × 7-column grid drawn:
   one box per character you typed. Count them against the text.
4. **Expect** the text box to be untouched — same characters, same cursor content.
5. Edit one character. **Expect** the bordered picture to update on the keystroke, with no button
   press.
6. Press the button again. **Expect** the plain picture back.

## Scenario 5 — Staleness survives the toggle (US1/US2: FR-006, FR-007, FR-008)

Continuing in manual mode:

1. With a valid picture on screen and the boundary view **on**, type an unrecognized character (a
   space will do — feature 001 rejects whitespace).
2. **Expect** the bordered picture to stay on screen, dimmed as stale, with the error beside it.
3. Toggle the view **off** while it is still stale. **Expect** the *same* picture, now plain, still
   dimmed, with the error still shown. This is the check that fails if the app caches a rendered
   string instead of the diagram — see [research.md](./research.md) R2.
4. Fix the character. **Expect** the dimming and the error to clear.
5. Now clear the text box entirely and type a single bad character. **Expect** the error alone — no
   grid, empty or otherwise.

## Scenario 6 — It is remembered, and it belongs to manual mode only (US2/US3: FR-010 – FR-015)

1. With the boundary view on, take two snapshots. **Expect** both previews drawn with boundaries.
2. Toggle the view off. **Expect** both previews to follow, drawn plain.
3. Restore a snapshot. **Expect** the text to come back and the view setting **not** to change.
4. Switch to notation mode. **Expect** no boundary-view control anywhere, and the notation display
   controls (drawn/ASCII, compact, download SVG) exactly as they were.
5. Switch back. **Expect** the setting as you left it.
6. Reload the page. **Expect** manual mode, the same text, the same snapshots, and the same view
   setting.
7. Finally, with dev tools, replace the `knotty_state` value with a copy that has the
   `manual_borders` key deleted, and reload. **Expect** the app to load with the view off and
   everything else intact (FR-014).
