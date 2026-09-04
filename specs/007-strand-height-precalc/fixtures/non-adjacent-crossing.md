# Golden Fixture: non-adjacent crossing

Supplied by the feature owner 2026-09-03. **Authoritative.** ✅ Fully verified.

**This is the fixture that pins down crossing alignment** — the highest-uncertainty
part of Component B (research R4), and the one no earlier fixture exercised.

## Input

```text
(0 (0 /1 (2 )2 \1 /2 \1 )1 )0
```

10 features: 3 openings, 4 crossings, 3 closings.

## Expected — Component A

Per-strand maxima, in opening order:

```text
(4, 5), (0, 1), (2, 3)
```

## Expected — Component B

6 rows × 19 columns (10 feature columns + **9** transfer columns), read top-down:

```text
._________._____...
(_...__...y.....\..
..\./..\./.\./\..\.
...y.().x...x..)..)
../.\__/.\_/.\/../.
.(______________/..
```

## Verification

| col | feature | drawn @ row | joins rows | `floor` | check |
|-----|---------|-------------|------------|---------|-------|
| 0 | `(0` | 4 | maxima (4,5) | 4 | ✅ |
| 1 | `(0` | 0 | maxima (0,1) | 0 | ✅ |
| 2 | *converge* | — | 1→2, 4→3 | — | gap 2, split 1/1 |
| 3 | `/1` | 2 | 1, 4 | 2 | ✅ **non-adjacent** |
| 4 | *return* | — | 2→1, 3→4 | — | both back to maxima |
| 5 | `(2` | 2 | maxima (2,3) | 2 | ✅ |
| 6 | `)2` | 2 | 2, 3 | 2 | ✅ |
| 7 | *converge* | — | 1→2, 4→3 | — | gap 2, split 1/1 |
| 8 | `\1` | 2 | 1, 4 | 2 | ✅ **non-adjacent** |
| 9 | *return* | — | 2→1, 3→4 | — | both back to maxima |
| 10 | `/2` | 4 | 3, 5 | 4 | ✅ **non-adjacent**, gap 1, split 1/0 |
| 11 | *converge* | — | 1→2, 4→3 | — | gap 2, split 1/1 |
| 12 | `\1` | 2 | 1, 4 | 2 | ✅ **non-adjacent** |
| 13 | *return* | — | 2→1, 3→4 | — | both back to maxima |
| 14 | *converge* | — | 1→2, 4→3 | — | gap 2, split 1/1 |
| 15 | `)1` | 2 | 1, 4 | 2 | ✅ **non-adjacent** |
| 16–17 | *converge* | — | 0→1→2, 5→4→3 | — | gap 4, split 2/2 |
| 18 | `)0` | 2 | 0, 5 | 2 | ✅ **non-adjacent** |

## What this fixture establishes

**Crossing alignment obeys the same midpoint rule as caps and cups.** Three
separate crossings (`/1`, `\1`, `\1`) each join strands at rows 1 and 4, and each
is drawn at `floor((1+4)/2) = 2` with the convergence split 1/1. FR-011's
extension of the uniform rule is confirmed against real expected output.

**Both strands return afterward, symmetrically.** Every crossing here is
immediately followed by a transfer column that undoes the convergence — column 4
after column 3, column 9 after column 8, column 13 after column 12. This is the
concrete form of FR-011's "because a crossing is not a boundary, both strands
MUST return to their own maxima after it," and it is what distinguishes a
crossing from a cap or cup.

**Alignment dominates the column count.** 9 of the 19 columns are transfers —
nearly half the diagram. A crossing whose partners sit 3 rows apart costs 2
columns (converge + return) on top of its own. This is the cost SC-002 tracks as
the crossing-alignment category, and it is substantial.

**Convergence is not always symmetric.** The `/2` at column 10 joins rows 3 and
5 — an odd gap of 1 — so the lower strand takes the single step and the upper
does not move.
