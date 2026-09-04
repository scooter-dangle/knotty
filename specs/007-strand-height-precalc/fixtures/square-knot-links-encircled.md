# Golden Fixture: square knot with links, encircled

Supplied by the feature owner 2026-09-03. **Authoritative.** ✅ Fully verified.

> The 8th feature was originally supplied as `\5`, which conflicted with the
> diagram. The owner confirmed 2026-09-03 that the **encoding** was at fault and
> it should read `\4`; the diagram was correct as drawn. Corrected below. See
> *Why `\4`* for what the discrepancy pinned down.

## Input

```text
(0 (1 (3 (3 (7 (7 )7 \4 (4 /3 /5 )4 \4 )3 )3 )3 (1 )3 )1 )0
```

20 features: 8 openings, 4 crossings, 8 closings. The largest fixture, and the
only one exercising deep nesting and a separation of 14.

## Expected — Component A

Per-strand maxima, in opening order:

```text
(0, 15), (3, 4), (9, 10), (5, 8), (13, 14), (11, 12), (6, 7), (1, 2)
```

Labelling the openings `A`–`H` in that order.

## Expected — Component B

16 rows × 36 columns (20 feature columns + 16 transfer columns), read top-down:

```text
........____________________........
......./.....___________....\.......
....../.....(___________)....\......
...../........................\.....
..../........()................\....
.../......_____________.........\...
../......(_____.____.__).........\..
./..........___x__._x.............\.
(........../....._y..\.............)
.\........(.....(._)..).........../.
..\........\_____y___/.........../..
...\....._________________....../...
....\...(_________________)..../....
.....\...................._.../.....
......\..................(_)./......
.......\____________________/.......
```

## Verification

All 20 features match.

| col | feature | strands | rows | `floor` | check |
|-----|---------|---------|------|---------|-------|
| 0 | `(0` | opens A | maxima (0,15) | 7 | ✅ |
| 1–7 | *transfer* | A_lo 7→0, A_hi 8→15 | — | — | separation 14, split 7/7 |
| 8 | `(1` | opens B | maxima (3,4) | 3 | ✅ |
| 9 | `(3` | opens C | maxima (9,10) | 9 | ✅ |
| 10 | `(3` | opens D | maxima (5,8) | 6 | ✅ |
| 11 | *transfer* | D_lo 6→5, D_hi 7→8 | — | — | separation 2, split 1/1 |
| 12 | `(7` | opens E | maxima (13,14) | 13 | ✅ |
| 13 | `(7` | opens F | maxima (11,12) | 11 | ✅ |
| 14 | `)7` | F_lo, F_hi | 11, 12 | 11 | ✅ |
| 15 | `\4` | D_hi, C_lo | 8, 9 | 8 | ✅ |
| 16 | `(4` | opens G | maxima (6,7) | 6 | ✅ |
| 17 | `/3` | D_lo, G_lo | 5, 6 | 5 | ✅ |
| 18 | `/5` | G_hi, D_hi | 7, 8 | 7 | ✅ |
| 19 | `)4` | G_lo, G_hi | 6, 7 | 6 | ✅ |
| 20 | `\4` | D_hi, C_lo | 8, 9 | 8 | ✅ |
| 22 | `)3` | D_lo, D_hi | 5, 8 | 6 | ✅ gap 2, split 1/1 |
| 23 | `)3` | C_lo, C_hi | 9, 10 | 9 | ✅ |
| 24 | `)3` | E_lo, E_hi | 13, 14 | 13 | ✅ |
| 25 | `(1` | opens H | maxima (1,2) | 1 | ✅ |
| 26 | `)3` | B_lo, B_hi | 3, 4 | 3 | ✅ |
| 27 | `)1` | H_lo, H_hi | 1, 2 | 1 | ✅ |
| 28–34 | *transfer* | A_lo 0→7, A_hi 15→8 | — | — | separation 14, split 7/7 |
| 35 | `)0` | A_lo, A_hi | 0, 15 | 7 | ✅ gap 14, split 7/7 |

## What this fixture establishes

**The outermost pair diverges maximally.** `A` has maxima `(0, 15)` — its two
strands sit at the very bottom and very top of the diagram, separated by 14. Its
cap at row 7 costs 7 transfer columns on each side of the diagram, 14 of the 16
transfer columns in total. This is the FR-015 invariant at the largest scale
available, and it makes the cost of the midpoint rule vivid: a strand pair that
straddles the whole diagram pays for it twice, once opening and once closing.

**Nesting depth of 6 pairs.** At column 14 there are 12 live strands across
16 rows. Component A's ordered-stack walk has to stay correct at this depth, and
every one of the 8 openings lands on its predicted row.

**Two crossings between the same two strands.** `\4` at columns 15 and 20 both
join `D_hi` and `C_lo` — a clasp, which is what makes this a *link* rather than
a knot.

## Why `\4`

The original `\5` would have joined logical levels 5 and 6. At that column the
live levels are:

```text
logical:  0     1     2     3     4     5     6     7     8     9
strand:  A_lo  B_lo  B_hi  D_lo  D_hi  C_lo  C_hi  E_lo  E_hi  A_hi
row:      0     3     4     5     8     9    10    13    14    15
```

so `\5` means `C_lo`/`C_hi` at rows 9,10 → drawn at row 9. The diagram had it at
row 8, and the grid was internally consistent with row 8 (row 9 of that column
is empty — the shadow a glyph at row 8 casts), so the diagram was not the error.
`\4` joins `D_hi`/`C_lo` at rows 8,9 → row 8, matching.

Worth keeping in mind for Component B: **a notation index that "looks wrong"
against the rendered rows is expected, not a bug.** `\4` renders at row 8 and
`)3` renders at row 6 — under precalculated placement the index names a logical
level, never a grid row (see [../contracts/strand-heights.md](../contracts/strand-heights.md)).
That is precisely what made this discrepancy hard to spot by eye and easy to
catch by simulating the stack.
