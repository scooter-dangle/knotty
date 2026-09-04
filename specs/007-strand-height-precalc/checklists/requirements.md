# Specification Quality Checklist: Height-Precalculated Strand Placement (Placement Mode)

**Purpose**: Validate specification completeness and quality before proceeding to planning
**Created**: 2026-06-18
**Feature**: [spec.md](../spec.md)

## Content Quality

- [x] No implementation details (languages, frameworks, APIs)
- [x] Focused on user value and business needs
- [x] Written for non-technical stakeholders
- [x] All mandatory sections completed

## Requirement Completeness

- [x] No [NEEDS CLARIFICATION] markers remain
- [x] Requirements are testable and unambiguous
- [x] Success criteria are measurable
- [x] Success criteria are technology-agnostic (no implementation details)
- [x] All acceptance scenarios are defined
- [x] Edge cases are identified
- [x] Scope is clearly bounded
- [x] Dependencies and assumptions identified

## Feature Readiness

- [x] All functional requirements have clear acceptance criteria
- [x] User scenarios cover primary flows
- [x] Feature meets measurable outcomes defined in Success Criteria
- [x] No implementation details leak into specification

## Notes

- Items marked incomplete require spec updates before `/speckit-clarify` or `/speckit-plan`
- Domain terms (strand pair, opening/closing feature, diagonal transfer) are described in plain language in the spec's User Scenarios background note so non-technical stakeholders can follow.
- The new mode is specified as opt-in and additive to protect existing rendering output and snapshots (FR-005, SC-004).
- Motivating use case (added 2026-06-18): the rotation move scans the rendered grid; only reversed-direction (up-then-down) transfers re-encode as extra scanned features and compound across rotations (many transfers scan to nothing; crossing-alignment transfers don't add features). Captured as User Story 2 (P2), SC-006, and a repeated-rotation edge case.
- Operating-context decision (2026-06-18): the placement mode is a single mode the user works in (governs all operations; only rotation's output depends on it). The existing placement behavior stays the default; new mode opt-in; migration out of scope. Captured as FR-012/FR-013, US3, and Assumptions. (Recorded on 2026-06-18 as "rendering mode" with the default called "legacy"; renamed 2026-09-03 once the placement axis was established as orthogonal to the grid mapping, and after the split-cell rendering that "legacy" named was retired upstream.)
- Orthogonality decision (2026-09-03): the placement axis — which vertical level each strand occupies — is orthogonal to the grid mapping, which is how an already-placed diagram becomes characters and is what the opening-centered rendering governs. Adding a choice on the placement axis therefore does not reintroduce the rendering choice retired upstream. Drove the "rendering mode" → "placement mode" rename throughout. Captured as FR-014, a Key Entities entry, and the scope note in the User Scenarios background.
- Uniform midpoint rule (2026-09-03): heights are calculated per strand rather than per pair, because an opening occurring between a pair's two strands raises the upper without raising the lower. A cap, cup, or crossing is drawn at the floored midpoint of the two strands it joins, splitting the movement evenly between them; flooring is exact rather than lossy because the glyph renders halfway up its tile. A strand's maximum covers its flat run only, which removes the self-reference and keeps the calculation a single forward pass with no fixpoint. Captured as FR-001, FR-002, FR-011, FR-015, FR-016, and the divergent-pair and odd-midpoint edge cases.
- Transfer accounting correction (2026-09-03): segments are counted per glyph and classified into three categories — open/close displacement, boundary, and crossing-alignment. This corrected SC-002, which had been **false as written**: it claimed crossing-free diagrams exhibiting avoidable up-then-down movement render with strictly fewer total segments and none with more, but `(0 (1 )1 )0` is crossing-free, exhibits exactly that movement, and goes from 4 displacement segments to 4 boundary segments for no change in total. The guarantee now covers displacement only, and no reduction in the total is claimed for any diagram class. Captured as SC-002, Key Entities, and Assumptions.
