# Feature Specification: Revised Diagram Text Format Symbol Table

**Feature Branch**: `claude/diagram-spec-characters-rqm0ye`

**Created**: 2026-09-02

**Status**: Draft

**Input**: User description: "Modify a few of the characters used for manual diagram specification:
`_` => `.`, `-` => `_`, `\` => `x`, `/` => `y`, `.` => `'`, `i` => `/`, `k` => `\`"

## Overview

The diagram text format (introduced for specifying and reading back rendered diagrams directly,
independent of knot notation) assigns one character to each of the sixteen renderable cell kinds.
Several of the current character choices are awkward for people actually typing diagrams by hand:
`_` for an empty cell is visually heavy for what should be the most common, least noticeable cell;
`\` and `/` are used for crossing cells even though they don't visually suggest "over" vs. "under"
distinctly from the up-crossing letters; and `i` and `k` for the transfer cells don't evoke the
mostly-vertical strand movement they represent. This feature replaces seven of the sixteen
character assignments with characters that read more naturally, while leaving the format's rules
(one character per cell, case-sensitive, whitespace rejected, ragged-row padding, etc.) unchanged.

This is a revision of the existing character-to-cell mapping, not a new format — every rule other
than the character choices themselves stays exactly as already specified.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Write and read diagrams with the revised character set (Priority: P1)

A contributor writing the compact text for a diagram, or reading text emitted by the library, uses
the revised character-to-cell mapping: an empty cell is now written `.` instead of `_`, a plain
line is `_` instead of `-`, an over-crossing is `x` instead of `\`, an under-crossing is `y` instead
of `/`, an upward opening arc is `'` instead of `.`, and the two transfer cells are `/` and `\`
instead of `i` and `k`.

**Why this priority**: This is the entire feature — every other change is downstream of the symbol
table itself changing.

**Independent Test**: Write the compact text for a known diagram (e.g. the trefoil) using the
revised characters, render it, and confirm the resulting picture matches the picture produced by
the equivalent knot notation.

**Acceptance Scenarios**:

1. **Given** the compact text for a trefoil rendering written with the revised characters, **When**
   it is parsed and rendered, **Then** the resulting picture is byte-for-byte identical to the
   picture produced from the abbreviated notation `(0 (2 /1 \0 /1 )2 )0`.
2. **Given** any diagram the library can render, **When** its compact text is produced, **Then**
   every character in the output is drawn from the revised symbol table, and none of the seven
   retired characters (`_`, `-`, `\`, `/`, `.`, `i`, `k` in their old meanings) appears with its old
   meaning.
3. **Given** text containing a character from the old mapping that no longer names any cell (e.g.
   a literal `i` or `k`, now unrecognized), **When** it is parsed, **Then** parsing fails with a
   message naming the offending character and its row and column, exactly as any other unrecognized
   character does today.

---

### User Story 2 - Discover the revised symbol table from within the app (Priority: P2)

Someone using manual diagram mode looks up what a character means and sees the current, revised
mapping — not the mapping the format shipped with originally.

**Why this priority**: The format is only usable if its documentation and any in-app reference are
never out of sync with what the parser and writer actually accept.

**Independent Test**: Open the in-app symbol table reference and confirm every entry matches the
revised mapping, with no leftover reference to a retired character's old meaning.

**Acceptance Scenarios**:

1. **Given** the in-app symbol table reference, **When** it is displayed, **Then** it lists exactly
   the sixteen current character-to-cell pairs from the revised mapping.

---

### Edge Cases

- **Old-format text**: text written under the previous mapping that happens to still parse under
  the new one (because it only uses characters whose meaning didn't change) is read as the *new*
  meaning of those characters, since the mapping is a single global table with no versioning —
  there is no way to tell old text from new text, and the format does not attempt to.
- **Retired characters now unused**: `-`, `\`, `/`, `i`, and `k` are no longer emitted by the writer
  and, where they are not reassigned to a different cell, are rejected the same as any other
  unrecognized character.
- **Reused characters**: `.` and `/` are each reassigned to a *different* cell than they named
  before (`.` moves from Empty to OpenedAbove; `/` moves from CrossDownUnder to TransferUp), so text
  containing them still parses without error but now describes a different picture than it would
  have before this change.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST use exactly this character-to-cell mapping, with no aliases and no
  additional accepted characters:

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

- **FR-002**: The nine characters not named in this feature's input (`A`, `a`, `(`, `)`, `,`, `j`,
  `r`, `2`, `L`) MUST keep their existing meanings, unchanged.
- **FR-003**: The system MUST reject `-`, `\`, `/`, `i`, and `k` wherever they are not reassigned to
  a cell by the revised mapping above (that is, `-` and `i` and `k` are unrecognized; `\` and `/`
  are recognized but now name different cells than before).
- **FR-004**: The system MUST NOT accept both an old and a new character for the same cell; each
  cell has exactly one current character, matching the rest of the format's one-to-one symbol-table
  rule.
- **FR-005**: The writer MUST emit only characters from the revised mapping when producing compact
  text for a diagram, so canonical output never contains a retired character.
- **FR-006**: All other rules of the diagram text format — one character per cell, case sensitivity,
  rejection of whitespace, row-padding with the empty-cell character, ragged-row inference, empty
  input yielding an empty diagram, and round-trip fidelity — MUST continue to apply exactly as
  already specified, with only the character assignments changing.
- **FR-007**: Any in-app or documented reference to the symbol table MUST reflect the revised
  mapping, with no remaining reference to a retired character's former meaning.

### Key Entities

- **Symbol table**: the fixed, one-to-one correspondence between cell characters and cell kinds;
  this feature changes seven of its sixteen entries and leaves the rest and all surrounding rules
  unchanged.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Every diagram the library can render still survives a write-then-read-then-write
  round trip byte for byte under the revised mapping, in 100% of cases.
- **SC-002**: A user consulting the in-app symbol table reference sees only current characters —
  0% of listed entries reference a retired character's old meaning.
- **SC-003**: Text that used a now-retired, now-unrecognized character is reported as invalid with
  the same row/column precision the format already guarantees for any unrecognized character.

## Assumptions

- This is a breaking change to the format's symbol table: text authored under the previous mapping
  is not guaranteed to describe the same picture after this change, and no migration or
  compatibility mode is provided — the format has one current mapping at a time, consistent with it
  having no versioning today.
- The seven characters to change and their new assignments are exactly as given in the feature
  input; the nine characters not mentioned are intentionally left alone.
- This feature is a targeted revision of the mapping introduced by the verbose diagram text format
  feature; it does not otherwise change that feature's scope (parsing, writing, or the example app's
  manual mode behavior).
