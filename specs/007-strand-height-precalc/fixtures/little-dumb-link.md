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

**A divergence gap is held open longer than it is occupied.** Pair 4's maxima
`(4, 7)` come from pair 5 opening between its strands — that intervening opening
is the *only* reason the maxima are non-sequential, so pair 5 occupying rows 5
and 6 is the cause of the gap, not a surprising use of it. What the fixture does
show is the duration: pair 4 sits at rows 4 and 7 from column 5 to column 13,
but pair 5 exists only for columns 7–8. At column 6 the gap stands empty. Under
precalculated placement a strand holds its maximum for its whole flat run, so a
divergent pair holds its gap open for its whole life rather than opening it on
demand — which is the same tradeoff SC-002 measures, seen as vertical space
rather than as transfer count.

**The largest even split so far.** `)0` at column 14 joins rows 0 and 7 — a
separation of 6, split 3/3 across three transfer columns. Confirms the FR-015
invariant at a scale where an off-by-one would be obvious.

**Crossing-free but not transfer-free.** With no crossings at all, 5 of the 15
columns are still transfers, every one of them a boundary transfer. This is
SC-002's point made concretely: removing displacement does not mean removing
diagonals.
