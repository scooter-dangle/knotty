# Quickstart: validating the makeover

**Feature**: [spec.md](./spec.md) | **Plan**: [plan.md](./plan.md) | **Contract**: [contracts/ui.md](./contracts/ui.md)

The makeover touches only `examples/knot-so-good/`. The library is not changed, so the library gate
is a no-op check that it *stayed* unchanged.

## Prerequisites

```sh
rustup target list --installed                 # expect wasm32-unknown-unknown
cargo test --manifest-path examples/knot-so-good/Cargo.toml   # baseline: 26 passing
trunk --version                                # 0.21.4, the version deploy.yml pins
```

`trunk` is not a cargo dependency; install it once with `cargo install --locked trunk` (or fetch the
0.21.4 release binary as the deploy workflow does). Playwright is needed only for the screenshot
checks, not for the build.

## Gate — run before every commit

```sh
cargo test --manifest-path examples/knot-so-good/Cargo.toml                          # 26 + new, none failing
cargo check --manifest-path examples/knot-so-good/Cargo.toml --target wasm32-unknown-unknown
cargo test                                                                            # library: unchanged, still green
git diff --stat -- src/                                                               # expect: nothing
```

## Build and serve

```sh
cd examples/knot-so-good
trunk build --release                          # writes dist/
python3 -m http.server 8123 --bind 127.0.0.1 --directory dist
```

Then open `http://127.0.0.1:8123/`. For live editing, `trunk serve --port 3000` works too.

## Re-take the screenshots

```sh
cd specs/006-gui-makeover/screenshots
BASE_URL=http://127.0.0.1:8123/ OUT_DIR=$PWD/after node capture.js
```

`capture.js` must first be updated to drive the new controls by the names in
[contracts/ui.md](./contracts/ui.md) (the radio labels replace the "switch to …" buttons). It
writes the same fifteen states at both viewports as the baseline, plus `*-measurements.json`.

## What to check, criterion by criterion

| Criterion | How | Pass when |
|---|---|---|
| **SC-001** which state each setting is in | look at any `after/*` toolbar | both names of each pair are visible and one is filled |
| **SC-002** no shift on valid → error → empty | `after/desktop-measurements.json` | `notationValidTextareaTop == notationErrorTextareaTop`; `manualEmptyTextareaTop == manualValidTextareaTop == manualErrorTextareaTop` (baseline: 305/133, 8/211/245) |
| **SC-003** continuous strokes | `node measure-gaps.js` with the stylesheet's font stack and line-height; and eyeball `after/desktop-notation-trefoil-ascii.png` at zoom | 0 gaps on all four joins for DejaVu Sans Mono |
| **SC-004** error visible in picture display | `after/desktop-notation-diagram-error.png` | the message text is on screen |
| **SC-005** phone width | `after/mobile-measurements.json` | `pageWidth == 390` for every state (baseline: 980–1088) |
| **SC-006** catalog height | `after/desktop-measurements.json` | `notation-many-snapshots.pageHeight <= 1283`, `manual-many-snapshots.pageHeight <= 1375` |
| **SC-007** behaviour and saved state unchanged | the gate above; plus load the app with baseline-era `localStorage` (paste a saved `knotty_state`) | 26 original tests pass unchanged; settings restore identically |
| **SC-008** every baseline finding addressed | walk [baseline.md](./baseline.md) *Findings* against `after/` | each finding fixed, or explicitly listed as deferred in the PR |

Manual checks the script cannot make:

- Tab through the toolbar: each radio pair is reachable, arrow keys move within it, and the
  screen-reader name is the group name plus the option (FR-006).
- Type into the manual text box on a phone: the box does not move when the text goes bad
  (FR-012).
- Open the character reference and diagram text disclosures: unchanged content.
- Nine snapshots at 390 px: one column of cards, no page-level horizontal scroll.

## Done means

- The gate passes.
- `after/` exists with all thirty captures and the SC table above is all pass, or the PR says
  which criterion is deferred and why.
- `git diff --stat` shows changes only under `examples/knot-so-good/` and `specs/006-gui-makeover/`.
