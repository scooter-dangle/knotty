# Feature Specification: ASCII Print Tool Upgrade

**Feature Branch**: `008-ascii-print-tool-upgrade`

**Created**: 2026-09-04

**Status**: Draft

**Input**: User description: "ascii_print tool needs more capabilities and better defaults. default output should be compacted succinct text-mode diagram. it should spit out the precalculated height style by default when supplied with an encoded diagram (possibly with a set of diagram manipulations). compactedness should be a command flag. the fully spaced diagram style should be another, optional output style. it should be able to take in the succinct text format and then output the fully spaced diagram style. it should also be modified to use the latest stable version of clap. features of clap / clap ecosystem it should use, at a minimum: 1. declarative/derive style 2. Shell auto completions 3. NEVER short opts"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Succinct output with precalculated placement by default (Priority: P1)

A user runs the diagram-printing tool against an encoded diagram (optionally with a set of diagram manipulations applied to it) and, without setting any special options, gets back a compact, easy-to-scan text-mode diagram whose strand rows are already laid out using the improved, precalculated-height placement behavior.

**Why this priority**: This is the core "better defaults" request. Today a user must know about and set hidden environment variables to get either the compact rendering or the improved placement behavior; making both the default for the most common invocation delivers the most value with the least user effort.

**Independent Test**: Run the tool with only an encoded diagram as input and no additional options. Verify the printed diagram is the compacted (succinct) style and that its strand placement reflects the precalculated-height behavior rather than the older default.

**Acceptance Scenarios**:

1. **Given** an encoded diagram as the only input, **When** the tool is run with no additional options, **Then** the tool prints the succinct (space-compacted) text-mode diagram.
2. **Given** an encoded diagram as the only input, **When** the tool is run with no additional options, **Then** the diagram's strands are placed using the precalculated-height behavior, matching what previously required an extra environment variable to enable.
3. **Given** an encoded diagram together with a set of diagram manipulations, **When** the tool is run with no additional options, **Then** the manipulations are applied before rendering, and the succinct/precalculated defaults still apply to the result.

---

### User Story 2 - Selecting the fully-spaced output style (Priority: P2)

A user who needs to inspect a diagram in full detail — every row and column laid out without compaction — asks for the fully-spaced output style explicitly, while the succinct style remains what they get if they don't ask for anything special.

**Why this priority**: Full-detail inspection is a real, recurring need (debugging, documentation, side-by-side comparison), but it is secondary to the default, most-common case covered by Story 1.

**Independent Test**: Run the tool against an encoded diagram, once with no style option and once explicitly requesting the fully-spaced style. Verify the first run is succinct and the second is fully spaced.

**Acceptance Scenarios**:

1. **Given** an encoded diagram, **When** the user explicitly requests the fully-spaced output style, **Then** the tool prints the diagram at full spacing, with no blank-column compaction applied.
2. **Given** an encoded diagram, **When** no output-style option is supplied, **Then** the succinct style is produced (fully-spaced output is never produced unless explicitly requested).

---

### User Story 3 - Expanding a succinct diagram into the fully-spaced style (Priority: P3)

A user who already has a succinct text-mode diagram (produced earlier, perhaps saved in a file or shared by a colleague) feeds that succinct text back into the tool and asks for the fully-spaced style, without needing the original encoded diagram or manipulations that produced it.

**Why this priority**: This closes the loop between the two output styles, letting the compact form serve as a durable, shareable representation that can still be expanded for detailed review later. It depends on Stories 1 and 2 existing first.

**Independent Test**: Take succinct output produced by the tool (Story 1), feed it back in as input, and request the fully-spaced style. Verify the result is a fully-spaced rendering representing the same diagram topology and strand placement as the succinct input.

**Acceptance Scenarios**:

1. **Given** a previously produced succinct text-mode diagram as input, **When** the user requests the fully-spaced output style, **Then** the tool produces a fully-spaced rendering of the same diagram topology and strand placement encoded in that succinct text.
2. **Given** succinct text-mode input that is malformed or internally inconsistent, **When** the user runs the tool against it, **Then** the tool reports a clear error instead of printing an incorrect or misleading diagram.

---

### User Story 4 - Discoverable, script-friendly command line (Priority: P4)

A new or infrequent user of the tool discovers all available options from its built-in help without consulting external notes, and a user who wants to script or interactively use the tool from their shell can generate a completion script for it.

**Why this priority**: This is an ergonomics improvement that makes the other capabilities easier to find and use, but the tool is fully functional without it, so it is the lowest priority.

**Independent Test**: Inspect the tool's help output and confirm every option is long-form only. Generate a shell completion script for a supported shell and confirm it loads without error.

**Acceptance Scenarios**:

1. **Given** the tool's built-in help output, **When** a user inspects the available options, **Then** every option is presented as a long-form flag only, with no single-letter shortcuts offered.
2. **Given** a supported shell, **When** the user asks the tool to generate a completion script for that shell, **Then** the tool emits a valid completion script for it.

---

### Edge Cases

- What happens when the encoded diagram input is malformed or cannot be parsed?
- What happens when a diagram-manipulation refers to a strand/crossing index that doesn't exist, or can't be applied to the diagram's current state?
- What happens when succinct text-mode input is truncated, hand-edited, or otherwise inconsistent such that it can't be unambiguously expanded to the fully-spaced style?
- What happens when a user requests conflicting options at the same time (e.g., asking for both output styles, or for both placement behaviors)?
- What happens when a user requests a completion script for a shell the tool doesn't support?
- How does the tool behave when the fully-spaced rendering of a very large diagram is requested (significantly wider/taller output than the succinct style)?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Tool MUST continue to accept an encoded diagram as its primary diagram input, optionally combined with a set of diagram manipulations to apply to it before rendering.
- **FR-002**: When rendering from encoded-diagram input, tool MUST default to the precalculated-height placement behavior, without requiring the user to set anything beyond running the tool.
- **FR-003**: Tool MUST still let a user explicitly request the prior default placement behavior instead, via a command-line option.
- **FR-004**: Tool MUST default to producing its output in the succinct (compacted) text-mode diagram style.
- **FR-005**: Tool MUST provide a command-line option that controls output compactedness, letting the user explicitly choose the fully-spaced style instead of the succinct default.
- **FR-006**: Tool MUST support the fully-spaced diagram style as a selectable output style, representing the same diagram information as the succinct style but without blank-column compaction.
- **FR-007**: Tool MUST accept a previously produced succinct text-mode diagram as an input format, in addition to the encoded-diagram input.
- **FR-008**: When given succinct text-mode diagram input, tool MUST be able to produce a fully-spaced rendering that faithfully represents the same diagram topology and strand placement encoded in that input.
- **FR-009**: Tool MUST report a clear, descriptive error — not a crash — when input in any supported format (encoded diagram, diagram manipulations, or succinct text) is malformed or cannot be unambiguously interpreted.
- **FR-010**: Tool's command-line interface MUST be defined in a declarative/derive-based style.
- **FR-011**: Tool MUST expose every command-line option as a long-form option only; no single-character short options MUST be offered.
- **FR-012**: Tool MUST be able to generate a shell auto-completion script for a supported shell on request.
- **FR-013**: Tool MUST use the current latest stable release of its command-line argument-parsing library.
- **FR-014**: Tool MUST expose its existing display toggles (grid borders, echoing the encoded-diagram form) as command-line options rather than requiring environment variables.

### Key Entities

- **Encoded Diagram**: The existing compact textual notation for a knot/link diagram's strands and crossings, used as the tool's primary input format.
- **Diagram Manipulations**: An ordered set of transformations (e.g., rotations, strand swaps, crossing changes) that the tool applies to a diagram before rendering it.
- **Succinct Text-Mode Diagram**: The default, space-compacted ASCII rendering of a diagram; also usable as an input format that can be expanded back into the fully-spaced style.
- **Fully-Spaced Diagram**: The uncompacted ASCII rendering of a diagram, preserving its full grid layout without blank-column compaction.
- **Placement Behavior**: The strategy used to assign vertical rows to a diagram's strands — either precalculated-height (new default) or the prior index-aligned behavior (still available on request).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A user running the tool against an encoded diagram with no additional options receives a succinct, precalculated-height diagram rendering, with no environment variables set.
- **SC-002**: A user can obtain the fully-spaced rendering of any encoded diagram by adding exactly one command-line option, with no other change to their input.
- **SC-003**: A user can take succinct output the tool previously produced and, with one command, obtain an equivalent fully-spaced rendering of the same diagram.
- **SC-004**: A user can see every available option and its purpose from the tool's own help output, with 100% of options expressed as long-form flags and none as single-letter shortcuts.
- **SC-005**: A user can generate a working shell completion script for their shell in a single command.
- **SC-006**: Malformed input in any accepted format produces a descriptive error message rather than a crash, verified across representative malformed-input cases for each input format.

## Assumptions

- "Encoded diagram" refers to the tool's existing abbreviated diagram notation; this feature does not introduce a new notation for encoding diagrams from scratch.
- "Diagram manipulations" refers to the tool's existing set of diagram transformations; this feature does not add new manipulation types.
- Expanding succinct text-mode input into the fully-spaced style is expected to faithfully reconstruct the same diagram topology and strand placement already encoded in that succinct text, rather than recomputing placement with a different strategy.
- The tool's existing environment-variable-driven toggles are being replaced by command-line options as part of this upgrade, rather than kept alongside the new options, since the tool is moving off ad hoc environment-variable configuration entirely.
- The set of shells supported for auto-completion follows whatever the chosen command-line-parsing library supports by default (commonly bash, zsh, fish, PowerShell, and elvish).
- Users of this tool are developers and researchers working with knot diagrams from the command line, already comfortable with typical CLI conventions (help output, long-form flags, shell completions).
