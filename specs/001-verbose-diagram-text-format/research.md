# Phase 0 Research: Verbose Diagram Text Format

No `NEEDS CLARIFICATION` markers were carried in from the spec — five clarification rounds resolved
the open questions. What follows is the design research the plan depends on: decisions taken against
the existing code, and the two traps that would otherwise produce a subtly wrong implementation.

## Decision: parsing and writing live in `src/render.rs`

**Decision**: Implement `impl FromStr for VerboseDiagram` and `impl fmt::Display for VerboseDiagram`
in `src/render.rs`, alongside a `Horiz` byte mapping pair.

**Rationale**: `VerboseDiagram(pub(crate) Vec<VerboseLine>)` and `VerboseLine(pub(crate) Vec<Horiz>)`
are crate-private in their fields (`src/render.rs:23-26`). No consumer outside the crate can build a
`VerboseDiagram` at all, so this is a hard constraint rather than a style choice — and it happens to
land exactly where the constitution's library-first article wants it. The symbol table also mirrors
`Horiz::display()`, which lives in the same file; keeping them adjacent is what stops them drifting.

**Alternatives considered**: A new `src/verbose_text.rs` module — rejected because it would separate
the symbol table from the display table it parallels, for no gain in a ~350-line file. Making the
fields public so the app could build diagrams itself — rejected as a much larger API commitment than
this feature needs.

## Decision: `FromStr` / `Display`, error type `String`

**Decision**: Use the standard traits, with `type Err = String`.

**Rationale**: `AbbreviatedDiagram` already implements `FromStr<Err = String>` and `Display`
(`src/diagram.rs:727-747`), and the whole crate returns `Result<_, String>`. Matching that keeps the
two formats symmetrical for callers and adds no error-type machinery, per CLAUDE.md's instruction not
to introduce error handling beyond what is needed.

**Alternatives considered**: A dedicated error enum carrying row/column as fields — rejected as
unneeded ceremony; the message already carries the position, and no caller branches on the reason.

## Trap 1: row order reverses, and the error position must not

**Finding**: `VerboseDiagram::display()` iterates `self.0.iter().rev()` (`src/render.rs:295`), so
`self.0[0]` is rendered **last** — at the bottom of the picture. Confirmed by running the library:
the unknot's `VerboseLine[0]` holds `OpenedAbove/ClosedAbove` and renders as the closing `\/`.

Spec FR-005 requires the first line of text to be the **top** of the picture. So:

- `FromStr`: parse lines in reading order, then reverse before storing.
- `Display`: iterate `self.0` in reverse so line 0 of the output is the last stored row.

**The trap**: if the reversal happens before error reporting, the row number in the message will be
counted from the wrong end. A stray character on the user's line 1 would be reported as line 4 of a
4-row diagram. Error positions must be recorded in **input coordinates** — the line as typed, 1-based
— during the forward pass, before any reversal. This is the single most likely defect in the feature
and it is invisible to a test that only checks `is_err()`, so tests must assert on the message.

## Trap 2: padding must be a second pass

**Finding**: FR-010 pads each row to the diagram's longest row, but the longest row is not known until
every line has been read. Padding therefore cannot happen inside the per-line loop.

**Decision**: parse all lines into `Vec<Vec<Horiz>>`, compute `max width`, then pad each row on the
right with `Horiz::Empty`. `Horiz` is `Copy` with `#[default] Empty`, so `row.resize(width,
Horiz::Empty)` does it in one call.

**Consequence for `Display`**: since `from_abbreviated` already produces rectangular grids (`advance`
in `src/raw_lines.rs` pushes to every row) and `FromStr` pads, output is naturally rectangular. The
`Display` impl should still pad to the maximum row width defensively — `VerboseDiagram::default()`
and any future constructor could be ragged, and an unpadded row would silently break FR-008's
byte-for-byte round trip.

## Decision: line splitting rule

**Decision**: strip **one** trailing `\n` if present, then split on `\n`, then strip a trailing `\r`
from each line. Every remaining element — including empty ones — is a row.

**Rationale**: this is exactly FR-013. `"abc\n"` and `"abc"` both give one row; `"abc\n\n"` gives two,
the second empty, which pads to a full row of `Empty`. Empty input gives zero rows, satisfying FR-012.

**Note**: this deliberately does *not* reuse `CommentLines` (`src/moves.rs:5`), which the notation
parser uses. `CommentLines` trims each line and drops empty ones — both fatal here, since this format
is column-significant and blank lines are meaningful. The divergence is recorded in the spec's
Assumptions so a future reader does not "fix" it.

## Decision: two match arms, guarded by a round-trip test

**Decision**: `Horiz::as_byte(&self) -> u8` and `Horiz::from_byte(u8) -> Option<Self>`, both
`const fn` matches, with a unit test asserting `from_byte(as_byte(v)) == Some(v)` for all 16 variants
and that the 16 bytes are distinct.

**Rationale**: Rust has no single-source bidirectional match without a macro or a lookup table. Two
matches plus an exhaustive test is less machinery than either, and the test is what actually prevents
drift. Listing all 16 variants explicitly in the test (no wildcard) also makes a future 17th variant
fail to compile until it is mapped.

**Alternatives considered**: a `const` array of `(u8, Horiz)` pairs searched linearly — rejected, it
trades a compile-time-exhaustive match for a runtime scan and loses the non-exhaustive-match error.

## Verified: manual mode cannot break the app's ASCII-to-HTML path

**Finding**: `ascii_diagram_to_html` (`examples/knot-so-good/src/main.rs:628`) ends in
`unreachable!("bug!")` for unexpected bytes, so feeding it a hand-authored diagram is a plausible
panic risk.

**Checked**: extracting every literal from `Horiz::display()` yields exactly
`{' ', '(', ')', '/', '\\', '_'}`, all of which that match already accepts. A hand-authored diagram
can produce cell *combinations* the notation never generates, but never a new *character* — the
per-cell art is a fixed table. No change needed, and no panic risk.

## Decision: app state splits per mode; snapshots store text only

**Decision**: `Model` holds notation-mode state and manual-mode state side by side, with a `Mode`
discriminant. `PersistedState` gains `mode`, `manual_diagram`, and `manual_snapshots`, each
`#[serde(default)]`. `PersistedMode` gets a `#[serde(other)] Other` variant.

**Rationale**: FR-024/FR-025 require both modes' state to survive switching and reload, and FR-026
requires pre-feature saved state to keep loading. The existing tests
(`examples/knot-so-good/src/tests.rs`) already prove the `serde(default)` + `serde(other)` pattern
works for exactly this — `missing_fields_use_defaults` and `display_mode_unknown_string_
deserializes_to_other`. Following the established pattern means back-compat needs no new mechanism.

**Manual snapshots store only the diagram text**, re-rendering the ASCII preview on display. Notation
snapshots cache their SVG because svgbob is comparatively expensive; ASCII rendering is a string
walk, so caching it would only create an opportunity for the preview to disagree with the text.

## Decision: stale render is `Option<String>` plus a flag

**Decision**: manual mode keeps `last_valid_render: Option<String>` and a current parse
`Result<_, String>`. `None` + error → error alone (FR-018). `Some` + error → that render, marked
stale, with the error beside it (FR-017). `Some` + ok → normal.

**Rationale**: directly models the three states the spec names, with no extra machinery. The stale
marking is a CSS class in `index.html`, consistent with how the app already styles disabled controls.

**Snapshot gating**: FR-023 disables snapshotting while invalid. `Model::snapshot_disabled` already
exists for notation mode and is the natural place to extend.
