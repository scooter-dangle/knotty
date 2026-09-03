---
description: "Task list for the GUI makeover"
---

# Tasks: GUI Makeover

**Input**: Design documents from `/specs/006-gui-makeover/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md), [data-model.md](./data-model.md), [contracts/ui.md](./contracts/ui.md), [quickstart.md](./quickstart.md), [baseline.md](./baseline.md)

**Tests**: Host-target unit tests are included only where the plan adds a pure function
(`grid_class` in User Story 2), per research R7 and constitution III. Everything else about this
feature is visual, and is checked by re-taking the baseline captures with `screenshots/capture.js`
and by `screenshots/measure-gaps.js`; each story ends with that check.

**Organization**: One phase per user story, in spec priority order. The three P1 stories are the
user's three minimum outcomes and are independent of each other once the foundation is in: each
touches a different part of the view (toolbar / drawing / diagram region) and a different block of
`style.css`. They all edit `examples/knot-so-good/src/main.rs`, so they are sequential for one
person but separable by story for review.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1–US5)
- Include exact file paths in descriptions

## Path Conventions

Everything is under `examples/knot-so-good/`: `index.html` (HTML shell), `style.css` (new),
`src/main.rs` (the one Yew component), `src/tests.rs` (host-target tests). Capture tooling and
evidence live under `specs/006-gui-makeover/screenshots/`. Nothing under `src/` at the repository
root changes.

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Get the stylesheet out of the HTML shell and the page laying out to the viewport, with
nothing else changed — every story builds on this (research R4, R6; FR-016, FR-022).

- [X] T001 Create `examples/knot-so-good/style.css` containing exactly the rules from the inline `<style>` block in `examples/knot-so-good/index.html`; replace that block with `<link data-trunk rel="css" href="style.css">` in the `<head>`
- [X] T002 Add `<meta name="viewport" content="width=device-width, initial-scale=1">` to `examples/knot-so-good/index.html`, and add base rules to `examples/knot-so-good/style.css`: `*, *::before, *::after { box-sizing: border-box }`; `body` with a system sans-serif stack (`system-ui, -apple-system, "Segoe UI", Roboto, sans-serif`), `font-size: 1rem`, `line-height: 1.4`, `margin: 0`, `padding: 1rem`, and `max-width: 64rem; margin-inline: auto`; `textarea, input, code, pre` with the monospace stack `"DejaVu Sans Mono", Menlo, Consolas, "Liberation Mono", monospace`
- [X] T003 Run the quickstart gate and confirm the build: `cargo test --manifest-path examples/knot-so-good/Cargo.toml` (26 passing), `cargo check --manifest-path examples/knot-so-good/Cargo.toml --target wasm32-unknown-unknown`, then `trunk build --release` in `examples/knot-so-good/` and confirm `dist/index.html` links a hashed `style-*.css` and the page renders unchanged apart from the phone viewport

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: The page skeleton — a toolbar of named groups and a stacked column below it — that
US1 puts its toggles into, US3 puts its diagram region into, and US4 lays out
([data-model.md](./data-model.md) *control groups*; [contracts/ui.md](./contracts/ui.md) *Regions*).

**⚠️ CRITICAL**: US1, US3 and US4 all place their elements into these groups and containers.

- [X] T004 In `examples/knot-so-good/src/main.rs`, restructure the `html!` in `view()` so the toolbar is `<nav class="toolbar">` holding `<div class="group group-mode">` (the mode control), `<div class="group group-presets">` (the four `BUILT_IN_KNOTS` buttons), `<div class="group group-display">` (the display and compact controls), `<div class="group group-actions">` (the snapshot button), and everything after the toolbar is wrapped in `<div class="workspace">`; keep every existing button and its label for now
- [X] T005 In `examples/knot-so-good/src/main.rs`, do the same in `manual_view()`: `<nav class="toolbar">` with `group-mode`, `<div class="group group-view">` (the bordered/plain control) and `group-actions` (snapshot), then `<div class="workspace">` around the rest
- [X] T006 [P] In `examples/knot-so-good/style.css`, add `.toolbar { display: flex; flex-wrap: wrap; gap: 0.75rem 1rem; align-items: center; margin-bottom: 1rem }`, `.group { display: flex; flex-wrap: nowrap; gap: 0.25rem; align-items: center }`, and a base `button` style (padding `0.4rem 0.8rem`, `min-height: 2.5rem`, 1px border, small radius, neutral background, `cursor: pointer`, a visible `:focus-visible` outline)
- [X] T007 Gate: `cargo test --manifest-path examples/knot-so-good/Cargo.toml` passes; `trunk build --release`; serve `dist/` and run `BASE_URL=http://127.0.0.1:8123/ OUT_DIR=$PWD/scratch node specs/006-gui-makeover/screenshots/capture.js` to confirm the unchanged control names still drive the app — *done: desktop run passes end to end; the mobile run is blocked at the rotate button by the unclipped wide picture, which T025/T028 fix*

**Checkpoint**: Toolbar groups and a workspace column exist in both modes; the app behaves as before.

---

## Phase 3: User Story 1 - See which state each two-way setting is in (Priority: P1) 🎯 MVP

**Goal**: The four "switch to …" buttons become segmented radio pairs that name both states and mark
the active one, dispatching the same messages (FR-004–FR-007; research R2).

**Independent Test**: Load the app in each of the sixteen setting combinations and read every
setting's state off a screenshot of the toolbar; flip each and confirm the marking follows and the
saved state reloads identically (spec US1 acceptance scenarios; SC-001).

### Implementation for User Story 1

- [X] T008 [US1] Add a `segmented_pair` helper to `examples/knot-so-good/src/main.rs` that takes the group name (used as `name=` on the radios and `aria-label` on the wrapper) and two options, each `(label: &str, checked: bool, onchange: Callback<Event>)`, and returns `<fieldset class="segmented" role="radiogroup" aria-label={name}>` containing, per option, `<input type="radio" id="{name}-{label}" name={name} checked={checked} onchange={callback} />` followed by `<label for="{name}-{label}">{label}</label>`; the callbacks come from `link.callback(move |_| Msg::…)` at the call site so `Msg` need not be `Clone`
- [X] T009 [US1] In `examples/knot-so-good/src/main.rs`, replace `mode_toggle()` with a call to `segmented_pair("mode", …)` whose options are `notation` (checked when `self.mode == Mode::Notation`, dispatching `Msg::SetMode(Mode::Notation)`) and `manual` (checked for `Mode::Manual`, dispatching `Msg::SetMode(Mode::Manual)`); use it in both `view()` and `manual_view()` inside `group-mode`
- [X] T010 [US1] In `view()` in `examples/knot-so-good/src/main.rs`, replace the "switch to … display" button with `segmented_pair("display", …)` — options `picture` (checked for `DisplayMode::Svg`) and `characters` (checked for `DisplayMode::Ascii`), dispatching `Msg::DisplayMode(_)` — and the "switch to compact/full display" button with `segmented_pair("drawing", …)` — options `full` (checked when `!self.compact`) and `compact`, dispatching `Msg::Compact(_)`; both inside `group-display`; delete the `other_mode` / `other_compact` locals
- [X] T011 [US1] In `manual_view()` in `examples/knot-so-good/src/main.rs`, replace the "switch to bordered/plain view" button with `segmented_pair("view", …)` — options `plain` (checked when `!self.manual_borders`) and `bordered`, dispatching `Msg::ManualBorders(_)` — inside `group-view`; delete the `other_borders` local
- [X] T012 [US1] In `view()` in `examples/knot-so-good/src/main.rs`, give the notation textarea `id="knot-notation"` and the moves textarea `id="moves"`, each preceded by a `<label for=…>` reading `knot notation` and `moves`; give the manual textarea in `manual_view()` `id="diagram-text"` with a `<label for="diagram-text">diagram text</label>`
- [X] T013 [P] [US1] In `examples/knot-so-good/style.css`, style the pair: `.segmented { display: inline-flex; border: 1px solid; border-radius: 0.4rem; padding: 0; margin: 0; overflow: hidden }`; `.segmented input { position: absolute; opacity: 0; width: 1px; height: 1px; margin: -1px; pointer-events: none }` (visually hidden, still focusable); `.segmented label { padding: 0.4rem 0.8rem; min-height: 2.5rem; display: inline-flex; align-items: center; cursor: pointer }`; `.segmented input:checked + label { background: <accent>; color: <on-accent>; font-weight: 600 }`; `.segmented input:focus-visible + label { outline: 2px solid <accent>; outline-offset: -2px }`; choose one accent colour and one neutral border colour and reuse them for `button:focus-visible`
- [X] T014 [US1] Update `specs/006-gui-makeover/screenshots/capture.js` to drive the new controls by their contract names: replace every `button('switch to …')` click with `page.getByRole('radio', { name: '<option>' }).check()` — `manual` / `notation` for the mode, `characters` / `picture` for the display, `compact` / `full` for the drawing, `bordered` / `plain` for the view — and leave the preset, snapshot, rotate, restore and delete button lookups as they are
- [X] T015 [US1] Verify: `trunk build --release`, serve, run `capture.js` into `specs/006-gui-makeover/screenshots/scratch/`; confirm from `desktop-notation-trefoil-ascii-compact.png` and `desktop-manual-trefoil-bordered.png` that both names of every pair are visible with one filled (SC-001); Tab through the toolbar in a browser and confirm arrow keys move within a pair and the checked option is announced (FR-006); confirm the 26 app tests still pass

**Checkpoint**: Every two-state setting reads at a glance; behaviour and persistence unchanged.

---

## Phase 4: User Story 2 - Read a character-drawn diagram as continuous strands (Priority: P1)

**Goal**: The drawing's `/ \ ( ) _` strokes meet across rows via the measured font stack and
`line-height: 0.8`; the bordered view's `+ - |` become CSS-drawn rules (FR-008, FR-009; research R1).

**Independent Test**: `measure-gaps.js` with the shipped font stack and line-height reports 0 gaps on
all four joins for DejaVu Sans Mono, and a zoomed capture of the trefoil shows unbroken diagonals
(spec US2 acceptance scenarios; SC-003).

### Tests for User Story 2

- [ ] T016 [P] [US2] Add tests to `examples/knot-so-good/src/tests.rs`: (a) `grid_class` returns `Some("grid grid-cross")` for `b'+'`, `Some("grid grid-h")` for `b'-'`, `Some("grid grid-v")` for `b'|'`, and `None` for each of `b' '`, `b'('`, `b')'`, `b'/'`, `b'\\'`, `b'_'`, `b'0'`; (b) for every `BUILT_IN_KNOTS` entry, both `try_ascii_print::<false>()` and `try_ascii_print_compact::<false>()` contain no byte for which `grid_class` is `Some` (the plain drawing is never touched); (c) the bordered render of `"..___..\n.(._.).\n._y.y_.\n(__x__)\n"` (`display::<true>()`) contains all three grid bytes

### Implementation for User Story 2

- [ ] T017 [US2] In `examples/knot-so-good/src/main.rs`, add `fn grid_class(byte: u8) -> Option<&'static str>` per T016, and change `ascii_diagram_to_html` so bytes with a class are emitted as `<span class={class}>{byte as char}</span>` while every other accepted byte and `\n` are emitted exactly as today; keep the `unreachable!` arm
- [ ] T018 [US2] In `examples/knot-so-good/src/main.rs`, give every `<pre>` that holds a drawing the class `ascii`: the characters-display `<pre>` in `view()`, the `manual-render` `<pre>` in `manual_view()` (so its class list becomes `ascii manual-render` / `ascii manual-render stale`), and the manual snapshot preview `<pre>`
- [ ] T019 [P] [US2] In `examples/knot-so-good/style.css`, add `pre.ascii { font-family: "DejaVu Sans Mono", Menlo, Consolas, "Liberation Mono", monospace; font-size: 0.875rem; line-height: 0.8; padding-block: 0.3em; margin: 0; white-space: pre }` and the grid rules: `.grid { color: transparent; display: inline-block; width: 1ch; height: 0.8em; vertical-align: top; position: relative }`, `.grid::before, .grid::after { content: ""; position: absolute; background: currentColor; color: initial }` with `.grid-h::after { left: 0; right: 0; top: calc(50% - 0.5px); height: 1px }`, `.grid-v::after { top: 0; bottom: 0; left: calc(50% - 0.5px); width: 1px }`, and `.grid-cross::before` / `::after` drawing both; set the rule colour explicitly (e.g. `#888`) rather than inheriting through the transparent glyph
- [ ] T020 [US2] Verify: run `node specs/006-gui-makeover/screenshots/measure-gaps.js` after editing its `stacks` and `lhs` to the shipped values and confirm 0 gaps on all four joins for DejaVu Sans Mono; `trunk build --release`, serve, and capture zoomed crops of the trefoil in the characters display and of the bordered manual trefoil (4× device scale, element screenshots of `pre.ascii`) into `specs/006-gui-makeover/screenshots/scratch/`; confirm no visible break along a three-row diagonal, `(`/`)` meeting their strands, and a ruled (not dashed) grid; confirm the first and last rows are not clipped; `cargo test --manifest-path examples/knot-so-good/Cargo.toml` passes with the T016 tests

**Checkpoint**: Strands are continuous; the grid is ruled; the drawing's characters are unchanged.

---

## Phase 5: User Story 3 - Keep typing when the diagram goes empty or bad (Priority: P1)

**Goal**: An always-present diagram region with a permanent message line, block-level inputs, and
dimmed (not repainted) disabled controls, so nothing below the drawing moves on valid ↔ empty ↔
error, and the notation error shows in the picture display (FR-010–FR-015; research R3).

**Independent Test**: `capture.js` measurements show the text box at the same position in the
valid, empty and error states in both modes, and the notation error text is visible in the picture
display capture (spec US3 acceptance scenarios; SC-002, SC-004).

### Implementation for User Story 3

- [ ] T021 [US3] Add a `diagram_region(canvas: Html, message: Option<&str>) -> Html` helper to `examples/knot-so-good/src/main.rs` returning `<section class="diagram"><div class="canvas">{canvas}</div><p class="message" role="status">{message.unwrap_or("")}</p></section>`
- [ ] T022 [US3] In `examples/knot-so-good/src/main.rs`, change `update_modified()` so `ascii_html_diagram` is built only from the `Ok` side (`.as_deref().map(ascii_diagram_to_html).unwrap_or_default()`), delete `error_to_html`, and in `view()` replace the `<p><pre>…</pre></p>` / `<p><RawHtml …/></p>` display block with `diagram_region(…)` whose canvas is `<pre class="ascii">{ascii_html_diagram}</pre>` or `<div class="picture"><RawHtml inner_html={svg}/></div>` by display mode, and whose message is `self.ascii_modified_diagram.as_ref().err().map(|e| format!("Error: {e}"))` — so the error shows in both displays (FR-014)
- [ ] T023 [US3] In `manual_view()` in `examples/knot-so-good/src/main.rs`, replace the `if let Some(ref diagram) = self.manual_render { <p><pre …/></p> }` block and the `if let Some(ref err) = self.manual_error { <p class="manual-error">…</p> }` block with one `diagram_region(…)` whose canvas is the (possibly `stale`) `<pre class="ascii manual-render">` when there is a render and empty `Html` otherwise, and whose message is `self.manual_error.as_ref().map(|e| format!("Error: {e}"))`; keep the `stale` class logic (FR-013)
- [ ] T024 [US3] In `storage_error_html()` in `examples/knot-so-good/src/main.rs`, emit `<aside class="notice" role="alert">` around the message and the existing `Dismiss` button instead of the bare `<p>`
- [ ] T025 [P] [US3] In `examples/knot-so-good/style.css`: `section.diagram { min-height: 14rem; margin-bottom: 1rem }`; `.canvas { max-width: 100%; overflow-x: auto }`; `.picture svg { display: block; max-width: none }`; `p.message { min-height: 1.5em; margin: 0.5rem 0 0; color: #a02020 }` (the existing `.manual-error` colour, whose rule can now go); `textarea { display: block; width: 100%; max-width: 40rem; margin-bottom: 0.75rem }`; `.notice { border: 1px solid; border-radius: 0.4rem; padding: 0.75rem 1rem; margin-bottom: 1rem; background: <pale warning tint> }`; **delete** the `input.select-move[disabled]` and `button.snapshot[disabled]` grey rules and replace them with `button:disabled, input:disabled { opacity: 0.5; cursor: not-allowed }`; keep `.manual-render.stale { opacity: 0.4 }`
- [ ] T026 [US3] Verify: `trunk build --release`, serve, run `capture.js` into `specs/006-gui-makeover/screenshots/scratch/`, and check `desktop-measurements.json` and `mobile-measurements.json`: `notationValidTextareaTop == notationErrorTextareaTop` and `manualEmptyTextareaTop == manualValidTextareaTop == manualErrorTextareaTop` (SC-002; baseline 305/133 and 8/211/245); `desktop-notation-diagram-error.png` shows the error text under an empty region (SC-004); `desktop-notation-moves-error.png` shows dimmed pickers with legible placeholders at the same positions as in `desktop-notation-trefoil-svg.png` (FR-015); `desktop-manual-empty.png` shows the text box below the empty region, not beside the toolbar (FR-012)

**Checkpoint**: Nothing below the drawing moves on validity changes; errors always have a place.

---

## Phase 6: User Story 4 - Use the app on a phone (Priority: P2)

**Goal**: A single stacked column that fits the viewport at 390 px: wrapping toolbar groups,
full-width inputs, a scrolling canvas, tappable controls (FR-016–FR-019; research R4).

**Independent Test**: The `mobile-*` captures report `pageWidth == 390` in all fifteen states and no
control is under 40 px tall (spec US4 acceptance scenarios; SC-005).

### Implementation for User Story 4

- [ ] T027 [US4] In `view()` in `examples/knot-so-good/src/main.rs`, replace the `<br/>`-separated layout below the diagram region with block containers: `<pre class="encoding">` for the modified-diagram notation line, the existing `details.compact-text`, `<div class="inputs">` holding the two labelled textareas from T012, `<div class="moves">` holding the four `move_select` forms, the rotate button and the Download SVG link, then the catalog; remove every bare `<br/>`
- [ ] T028 [P] [US4] In `examples/knot-so-good/style.css`: `.workspace { display: flex; flex-direction: column; gap: 0.75rem }`; `.moves { display: flex; flex-direction: column; gap: 0.5rem; max-width: 40rem }`; `.moves form { margin: 0 }`; `.moves input { width: 100%; min-height: 2.5rem; padding: 0.4rem 0.6rem }`; `.toolbar .group { flex: 0 0 auto }` so groups wrap as units; `.encoding { overflow-x: auto; margin: 0 }`; `@media (max-width: 480px) { body { padding: 0.75rem } .group-presets { flex-wrap: wrap } }` (presets may wrap inside their group at the narrowest widths, the other groups never do)
- [ ] T029 [US4] Verify: `trunk build --release`, serve, run `capture.js`; in `mobile-measurements.json` every state has `pageWidth == 390` (SC-005; baseline 980–1088), including `notation-large-svg` and `notation-large-ascii`, whose drawing must scroll inside `.canvas` (FR-017); in `mobile-notation-trefoil-svg.png` every toolbar control is readable and at least 40 px tall (check with `getByRole(...).boundingBox()` for one button and one radio label); groups stay intact when the row wraps (FR-019)

**Checkpoint**: The phone layout is the same column, fitted to the width, with nothing scaled down.

---

## Phase 7: User Story 5 - Scan the snapshot catalog (Priority: P3)

**Goal**: Snapshots as uniform cards in an auto-fill grid with fixed-height previews (FR-020,
FR-021, FR-024; research R5).

**Independent Test**: Nine snapshots in each mode render as multi-column cards at desktop width and
the page is at most half the baseline height (spec US5 acceptance scenarios; SC-006).

### Implementation for User Story 5

- [ ] T030 [US5] In `examples/knot-so-good/src/main.rs`, change both catalogs to `<section class="snapshot-catalog" aria-label="snapshots">` of `<article class="snapshot-entry">` cards: notation cards hold `<div class="snapshot-preview">` (the existing `make_svg_scalable` `RawHtml`), `<pre class="encoding">` with `current_diagram_encoding`, and `<div class="snapshot-actions">` with the `restore` and `delete` buttons; manual cards hold `<div class="snapshot-preview">` containing either the `<pre class="ascii">` preview or the existing `<p class="manual-error">unreadable snapshot</p>`, then `snapshot-actions`; messages and the "unreadable snapshot" text unchanged
- [ ] T031 [P] [US5] In `examples/knot-so-good/style.css`: `.snapshot-catalog { display: grid; grid-template-columns: repeat(auto-fill, minmax(11rem, 1fr)); gap: 0.75rem; margin-top: 1rem }`; `.snapshot-entry { border: 1px solid; border-radius: 0.4rem; padding: 0.5rem; display: flex; flex-direction: column; gap: 0.5rem }`; `.snapshot-preview { height: 150px; overflow: auto }`; `.snapshot-preview svg { width: 100%; height: 100%; display: block }` (replacing the fixed 150 × 150 rule); `.snapshot-preview pre.ascii { font-size: 0.6rem }`; `.snapshot-actions { display: flex; gap: 0.25rem }`; `.snapshot-entry .encoding { font-size: 0.75rem }`
- [ ] T032 [US5] Verify: `trunk build --release`, serve, run `capture.js`; `desktop-measurements.json` reports `notation-many-snapshots.pageHeight <= 1283` and `manual-many-snapshots.pageHeight <= 1375` (SC-006); `desktop-notation-many-snapshots.png` shows cards in more than one column with preview, encoding and both buttons legible; `mobile-manual-many-snapshots.png` shows a single column with no page-level horizontal scroll

**Checkpoint**: All five stories done; the catalog is compact and nothing about snapshots changed.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: The remaining general-presentation requirements, cleanup, and the full verification
the spec's SC-008 asks for.

- [ ] T033 [P] In `view()` in `examples/knot-so-good/src/main.rs`, remove the inline `style="font-size: 8px;"` from the Download SVG link and give it `class="secondary"`; add `.secondary { font-size: 0.875rem }` to `examples/knot-so-good/style.css` (FR-023)
- [ ] T034 [P] Run `cargo fmt --manifest-path examples/knot-so-good/Cargo.toml` and `cargo clippy --manifest-path examples/knot-so-good/Cargo.toml --target wasm32-unknown-unknown`; remove anything now unused in `examples/knot-so-good/src/main.rs` (`error_to_html`, the `other_*` locals) and any orphaned rule in `examples/knot-so-good/style.css` (`.manual-error`, `.compact-text pre` if superseded, `.snapshot-preview svg` fixed sizes)
- [ ] T035 Run the full quickstart gate from `specs/006-gui-makeover/quickstart.md` (app tests, app wasm check, root `cargo test`, `git diff --stat -- src/` empty), then `trunk build --release`, serve, and re-take all captures with `BASE_URL=http://127.0.0.1:8123/ OUT_DIR=$PWD/after node capture.js` from `specs/006-gui-makeover/screenshots/`, committing `after/` (30 captures + 2 measurement files) alongside the baseline
- [ ] T036 Write `specs/006-gui-makeover/verification.md`: the quickstart's criterion-by-criterion table (SC-001–SC-008) filled with the measured numbers from `screenshots/after/*-measurements.json` and the `measure-gaps.js` result, a side-by-side list of each *Findings* entry in `baseline.md` marked fixed or deferred with a reason, and confirmation that a baseline-era `knotty_state` pasted into localStorage restores identical settings (SC-007)
- [ ] T037 Delete `specs/006-gui-makeover/screenshots/scratch/` and confirm `git status` shows changes only under `examples/knot-so-good/` and `specs/006-gui-makeover/`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: no dependencies; T001 → T002 → T003 in order (same two files).
- **Foundational (Phase 2)**: needs Phase 1. T004 → T005 (same file); T006 in parallel with them;
  T007 after all three. **Blocks every story.**
- **User Stories (Phases 3–7)**: each needs Phase 2. They are independent of one another in what
  they change, but US1, US2, US3, US4 and US5 all edit `src/main.rs`, so for one implementer they
  run in priority order: US1 → US2 → US3 → US4 → US5. Each story's `style.css` task is marked [P]
  because it can be written while the same story's `main.rs` task is in progress.
- **Polish (Phase 8)**: needs every story that is being shipped. T033 and T034 are parallel; T035 →
  T036 → T037 in order.

### User Story Dependencies

- **US1 (toggles)**: needs the toolbar groups from T004/T005. Nothing else.
- **US2 (spacing)**: needs nothing from the other stories; T018 touches the same `<pre>` elements
  that US3's T022/T023 later wrap in the diagram region, so doing US2 first keeps T022/T023 small.
- **US3 (stability)**: needs the workspace container from T004/T005; builds on US2's `pre.ascii`
  class if US2 is done, otherwise adds it.
- **US4 (phone)**: needs the groups and workspace from Phase 2 and the labelled textareas from
  US1's T012 (T027 moves them into `.inputs`); if US1 is skipped, T027 adds the labels itself.
- **US5 (catalog)**: needs nothing from the other stories.

### Within Each User Story

- `main.rs` tasks in listed order; the `style.css` task alongside; the verify task last.
- US2's test task T016 is written before T017 and must fail until `grid_class` exists.

### Parallel Opportunities

- T006 with T004/T005; T013 with T008–T012; T016 and T019 with T017/T018; T025 with T021–T024;
  T028 with T027; T031 with T030; T033 with T034.
- A second implementer could own `style.css` end to end (T006, T013, T019, T025, T028, T031) while
  the first owns `main.rs`, meeting at each story's verify task.

---

## Parallel Example: User Story 2

```bash
# Written together, different files:
Task: "T016 tests for grid_class and the plain/bordered drawings in examples/knot-so-good/src/tests.rs"
Task: "T019 pre.ascii and .grid rules in examples/knot-so-good/style.css"
# Then, sequentially in main.rs:
Task: "T017 grid_class + ascii_diagram_to_html spans"
Task: "T018 class=\"ascii\" on every drawing <pre>"
# Then:
Task: "T020 measure-gaps.js and zoomed captures"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Phases 1–2: stylesheet file, viewport, toolbar groups.
2. Phase 3: the four toggles and the capture-script update.
3. **Stop and validate**: SC-001 from the captures; keyboard check; app tests green.

That alone removes the app's most persistent irritation and is shippable.

### Incremental Delivery

Each of the three P1 stories is one of the user's minimum outcomes and lands as its own commit
(conventional-commit style, one logical change each): `feat(gui): segmented toggles…`,
`feat(gui): continuous character drawings…`, `feat(gui): stable diagram region…`. US4 and US5
follow as `feat(gui): phone layout…` and `feat(gui): snapshot card grid…`. Phase 8's re-capture
and `verification.md` is the last commit: `doc: verify GUI makeover against baseline`.

### Notes

- Constitution *Branching*: implementation happens on a `-story` branch so `test.yml` runs;
  confirm the branch before T001 — this task list is written on the spec branch.
- Nothing under `src/` changes; if a task seems to need a library change, it is out of scope and
  the spec's *What "cosmetic" means here* applies.
- `capture.js` is the executable form of [contracts/ui.md](./contracts/ui.md): when a control name
  in the contract and the script disagree, the contract wins and the script is fixed.
- The reduced `line-height` makes glyph ink overhang the line box; `padding-block` on `pre.ascii`
  (T019) is what keeps the first and last rows inside the scrolling `.canvas` (T025). If a row is
  clipped, that padding is the knob.
