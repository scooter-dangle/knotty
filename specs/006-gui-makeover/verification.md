# Verification: the makeover against the baseline

**Feature**: [spec.md](./spec.md) | **Baseline**: [baseline.md](./baseline.md) | **Quickstart**: [quickstart.md](./quickstart.md)

**Captured**: 2026-09-03, from the app at the head of `claude/speckit-gui-makeover-vj0169`, with
the same script, states and viewports as the baseline. The captures are in
[`screenshots/after/`](./screenshots/after/) alongside [`screenshots/baseline/`](./screenshots/baseline/),
file for file, so any pair can be compared directly.

## Gate

| Check | Result |
|---|---|
| `cargo test --manifest-path examples/knot-so-good/Cargo.toml` | 28 passed (25 at baseline + 3 new) |
| `cargo check … --target wasm32-unknown-unknown` (via `trunk build --release`) | clean |
| `cargo test` (library) | 85 passed, unchanged |
| `git diff --stat -- src/` | empty — nothing under `src/` changed |
| `cargo clippy` on the app | 3 warnings, all pre-existing and outside the changed code (`ctx` only used in recursion; two `.into()` on `JsValue`) |

## Success criteria

| Criterion | Measure | Baseline | After | Result |
|---|---|---|---|---|
| **SC-001** each setting's state readable from a screenshot | both names of every pair visible, active one filled | labels named only the *other* state | `notation`/`manual`, `picture`/`characters`, `full`/`compact`, `plain`/`bordered`, active segment filled blue | **pass** |
| **SC-002** no shift on valid → error → empty | notation box top, valid vs error (desktop) | 305 → 133 (172 px) | 458.78 → 458.78 (**0 px**) | **pass** |
| | manual box top, empty / valid / error (desktop) | 8 / 211 / 245 | 352.39 / 352.39 / 352.39 (**0 px**) | **pass** |
| | same, phone | — | 600.78 / 600.78 and 396.39 × 3 (**0 px**) | **pass** |
| **SC-003** continuous strokes | `measure-gaps.js`, shipped stack at `line-height: 0.8` | 2 gaps on every join at `normal` | **0 gaps** on diagonal, `\|` column, line→diagonal, paren join | **pass** |
| | zoomed trefoil (`desktop-notation-trefoil-ascii-zoom.png`) | broken at every row | unbroken three-row diagonals; `(` `)` meet their strands | **pass** |
| **SC-004** notation error visible in picture display | `desktop-notation-diagram-error.png` | nothing shown | "Error: invalid index: invalid digit found in string" in the message line | **pass** |
| **SC-005** phone page width = viewport | `pageWidth`, all 15 states at 390 px | 980–1,088 | **390 in 15/15** | **pass** |
| | tap targets | ~4 px on screen (scaled) | every toolbar control, picker and button 40 px tall | **pass** |
| **SC-006** nine snapshots ≤ half baseline height | manual catalog, desktop | 2,750 | **980** (target ≤ 1,375) | **pass** |
| | notation catalog, desktop | 2,566 | **1,338** (target ≤ 1,283) | **near miss** — see below |
| **SC-007** behaviour and saved state unchanged | original 25 app tests | pass | pass, unchanged | **pass** |
| | baseline-era `knotty_state` (every field, plus the retired `render_mode` key) pasted into localStorage | — | restores manual mode + bordered view; switching to notation shows characters + compact, the saved notation, moves and one snapshot in each catalog; the same nine keys are written back | **pass** |
| **SC-008** every baseline finding addressed or deferred | table below | | | **pass** |

### SC-006, notation catalog

1,338 px against a 1,283 px target: a 48 % reduction where the criterion asked for 50 %. The nine
cards sit in two rows of a five-column grid at desktop width, and about 880 px of the page is the
workspace above the catalog, not the catalog itself. Getting under the line would mean either
previews smaller than 120 px (already down from 150) or a catalog narrower than the cards need,
neither of which serves the story's goal ("a compact catalog of like-sized cards"). The manual
catalog, whose cards have no encoding line, clears its target by a wide margin. Left as is.

## Baseline findings, one by one

| Finding ([baseline.md](./baseline.md)) | Status |
|---|---|
| Two-state controls labelled with the *other* state | **fixed** — segmented radio pairs (US1) |
| Disabled controls painted heavy grey, placeholder unreadable | **fixed** — dimmed in place at 50 % opacity, same size and position |
| Toolbar one unbroken row with no grouping | **fixed** — mode / presets / display / view / actions groups, wrapping as units |
| Default serif at default sizes; 8 px download link | **fixed** — system sans-serif; link at 0.875 rem |
| Notation error collapses the picture; nothing replaces it; error invisible in picture display | **fixed** — reserved region, message pinned to its bottom, shown in both displays |
| Notation empty state floats the inputs up | **fixed** — region floor holds |
| Manual empty state puts the text box on the toolbar row | **fixed** — block-level box under the region |
| Manual error inserts a line and pushes the box 34 px | **fixed** — message line always present |
| Bad moves flip pickers to grey | **fixed** — dimmed, same geometry |
| Storage notice pushes everything down | **kept by design** — a notice above the toolbar is the spec's edge case; it is now styled as a notice and settles once on dismissal |
| `/` `\` broken at row boundaries; `_` gap above; `(` `)` short | **fixed** — `line-height: 0.8`, 0 gaps measured |
| Bordered grid dashed | **fixed** — `+ - \|` drawn as full-cell rules |
| Picture at natural size, page widens on phone | **fixed** — scrolls inside the region; page never wider than the viewport |
| Snapshot entries full-width, 2,566 / 2,750 px pages | **fixed** — card grid, 1,338 / 980 px (see SC-006) |
| Manual previews same size as the main picture, no "current" marker | **partly** — previews are 120 px cards at reduced type; marking the current one is not in the spec and not done |
| No viewport declaration | **fixed** |
| Text boxes at 20 columns | **fixed** — full width to 40 rem |

## Not done, deliberately

- The app README still describes the old "switch to …" buttons and a pre-spec-005 character set.
  Out of scope per the spec's assumptions; it was already stale before this feature.
- No dark theme, no colour beyond active/inactive, available/unavailable and notices.
- `capture.js` selects a segmented option by clicking its label, as a person does, because the
  radio input itself is visually hidden; the accessible names in [contracts/ui.md](./contracts/ui.md)
  are unchanged and are what the labels carry.
