# Golden Fixture: little dumb link

Supplied by the feature owner 2026-09-03. **Authoritative.** ✅ Fully verified.

## Input

```text
(0 (0 )2 (2 (4 )2 (3 )3 )1 )0
```

10 features: 5 openings, 5 closings, no crossings.

## Expected — Component A

Per-strand maxima, in opening order:

```text
(2, 3), (0, 1), (2, 3), (4, 7), (5, 6)
```

The fourth pair **diverges** — separation `7 − 4 − 1 = 2`.

## Expected — Component B

8 rows × 15 columns (10 feature columns + 5 transfer columns), read top-down:

```text
......_____....
...../.....\...
....(..()...\..
.....\___....\.
._..__...\....)
(_)(__)...)../.
.._______/../..
.(_________/...
```

## Verification

| col | feature | drawn @ row | joins rows | `floor` | check |
|-----|---------|-------------|------------|---------|-------|
| 0 | `(0` | 2 | maxima (2,3) | 2 | ✅ |
| 1 | `(0` | 0 | maxima (0,1) | 0 | ✅ |
| 2 | `)2` | 2 | 2, 3 | 2 | ✅ |
| 3 | `(2` | 2 | maxima (2,3) | 2 | ✅ |
| 4 | `(4` | 5 | maxima (4,7) | 5 | ✅ |
| 5 | *transfer* | — | lower 5→4, upper 6→7 | — | separation 2, split 1/1 |
| 6 | `)2` | 2 | 2, 3 | 2 | ✅ |
| 7 | `(3` | 5 | maxima (5,6) | 5 | ✅ |
| 8 | `)3` | 5 | 5, 6 | 5 | ✅ |
| 9 | *transfer* | — | 1→2, 4→3 | — | gap 2, split 1/1 |
| 10 | `)1` | 2 | 1, 4 | 2 | ✅ **non-adjacent** |
| 11–13 | *transfer* | — | 0→1→2→3, 7→6→5→4 | — | gap 6, split 3/3 |
| 14 | `)0` | 3 | 0, 7 | 3 | ✅ **non-adjacent** |

## What this fixture establishes

**A pair can be opened inside another pair's divergence gap.** Pair 4 has maxima
`(4, 7)`, so after its boundary transfers it occupies rows 4 and 7 with rows 5
and 6 empty between them. Pair 5 (`(3` at column 7, maxima `(5, 6)`) then opens
*into* that gap and closes again, entirely within it. Component B must therefore
treat the rows between a divergent pair's two strands as genuinely free space,
not as reserved by the pair that straddles them.

**The largest even split so far.** `)0` at column 14 joins rows 0 and 7 — a
separation of 6, split 3/3 across three transfer columns. Confirms the FR-015
invariant at a scale where an off-by-one would be obvious.

**Crossing-free but not transfer-free.** With no crossings at all, 5 of the 15
columns are still transfers, every one of them a boundary transfer. This is
SC-002's point made concretely: removing displacement does not mean removing
diagonals.
