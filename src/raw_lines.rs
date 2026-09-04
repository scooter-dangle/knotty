use std::cmp::Ordering;

use crate::diagram::AbbreviatedItem;
use crate::render::Horiz;

/// Assigns every strand the row it holds for its whole flat run: one more than
/// the tallest strand ever below it, or zero when nothing ever is. Returns
/// `(lower, upper)` per opening, in opening order.
///
/// The gap between a pair's two strands falls out of this; it is never computed
/// directly. Deriving it directly is what produced two wrong rules — notably
/// that the gap is the count of strands opened between the pair, which agrees
/// with every supplied fixture and is still wrong.
pub(crate) fn strand_heights(items: &[AbbreviatedItem]) -> Vec<(usize, usize)> {
    // Strands are numbered as they open. `below[s]` collects the strands seen
    // immediately under `s`; only adjacent neighbours are needed, since the
    // live strands are totally ordered at any instant, so any "ever below"
    // relation is realized by a chain of adjacencies at that same instant.
    let mut stack: Vec<usize> = Vec::new();
    let mut below: Vec<Vec<usize>> = Vec::new();
    let mut openings: Vec<(usize, usize)> = Vec::new();

    for &AbbreviatedItem { element, index } in items {
        match element {
            b'(' => {
                let (lower, upper) = (below.len(), below.len() + 1);
                below.push(Vec::new());
                below.push(Vec::new());
                openings.push((lower, upper));
                stack.splice(index..index, [lower, upper]);
            }
            b')' => {
                stack.drain(index..index + 2);
            }
            // Crossings join two levels without reordering them.
            _ => continue,
        }

        for pair in stack.windows(2) {
            let (under, over) = (pair[0], pair[1]);
            if !below[over].contains(&under) {
                below[over].push(under);
            }
        }
    }

    let mut heights = vec![None; below.len()];
    openings
        .into_iter()
        .map(|(lower, upper)| {
            (
                height_of(lower, &below, &mut heights),
                height_of(upper, &below, &mut heights),
            )
        })
        .collect()
}

/// Longest path to `strand` through the "ever below" relation. The relation is
/// acyclic because co-live strands never change relative order, so a plain memo
/// suffices.
fn height_of(strand: usize, below: &[Vec<usize>], heights: &mut Vec<Option<usize>>) -> usize {
    if let Some(height) = heights[strand] {
        return height;
    }

    let height = below[strand]
        .clone()
        .into_iter()
        .map(|under| height_of(under, below, heights) + 1)
        .max()
        .unwrap_or(0);

    heights[strand] = Some(height);
    height
}

/// Rows the grid needs: one past the tallest strand. Exceeds the count of
/// simultaneously live strands whenever a pair diverges, because a divergent
/// pair holds its gap open for its whole life.
pub(crate) fn grid_height(heights: &[(usize, usize)]) -> usize {
    heights
        .iter()
        .map(|&(_, upper)| upper + 1)
        .max()
        .unwrap_or(0)
}

/// Transfer glyphs by cause. Counted per glyph: a strand rising two levels
/// counts twice, and one opening displacing five strands counts five times.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TransferCounts {
    /// A passing strand pushed up by an opening beneath it and later pulled
    /// back down. What this feature removes.
    pub(crate) displacement: usize,
    /// A strand moving between a cap or cup and its own height.
    pub(crate) boundary: usize,
    /// Bringing two crossing partners together, and returning them.
    pub(crate) crossing_alignment: usize,
}

/// The grid under construction and which rows currently carry a strand. Every
/// placement drives the same emitter, so the two placement modes cannot drift
/// on how a placed diagram becomes cells.
pub(crate) struct Grid {
    lines: Vec<Vec<Horiz>>,
    live: Vec<bool>,
}

impl Grid {
    fn new(height: usize, columns: usize) -> Self {
        Self {
            lines: vec![Vec::with_capacity(columns); height],
            live: vec![false; height],
        }
    }

    fn height(&self) -> usize {
        self.live.len()
    }

    fn into_lines(self) -> Vec<Vec<Horiz>> {
        self.lines
    }

    fn column(&mut self, glyphs: &[(usize, Horiz)]) {
        for row in 0..self.live.len() {
            // A glyph joins the levels above and below it inside its own
            // cell, so the cell directly above it stays empty; a line there
            // would draw a strand a whole cell too long.
            let shadowed = glyphs.iter().any(|&(at, _)| at + 1 == row);

            self.lines[row].push(match glyphs.iter().find(|&&(at, _)| at == row) {
                Some(&(_, glyph)) => glyph,
                None if shadowed || !self.live[row] => Horiz::Empty,
                None => Horiz::Line,
            });
        }
    }
}

/// Builds the grid from precalculated heights. Each strand sits at its height
/// for its whole flat run; a cap, cup or crossing is drawn at the floored
/// midpoint of the two strands it joins, and the movement to meet it is split
/// between them.
///
/// `rows` holds the rendered row of every live strand, ascending — which is
/// also their logical order, since strands never change relative order. A
/// notation index addresses `rows`, not the grid: under this placement the two
/// diverge.
pub(crate) struct Precalculated {
    grid: Grid,
    rows: Vec<usize>,
    counts: TransferCounts,
}

impl Precalculated {
    pub(crate) fn new(height: usize, columns: usize) -> Self {
        Self {
            grid: Grid::new(height, columns),
            rows: Vec::new(),
            counts: TransferCounts::default(),
        }
    }

    pub(crate) fn into_lines(self) -> Vec<Vec<Horiz>> {
        self.grid.into_lines()
    }

    pub(crate) fn counts(&self) -> TransferCounts {
        self.counts
    }

    fn sync_live(&mut self) {
        for row in 0..self.grid.height() {
            self.grid.live[row] = self.rows.contains(&row);
        }
    }

    /// Walks the named strands to their targets, one level per column. The two
    /// strands of any group always move in opposite directions, so they neither
    /// collide nor overtake.
    fn transfer(&mut self, moves: [(usize, usize); 2], boundary: bool) {
        loop {
            let glyphs: Vec<_> = moves
                .iter()
                .filter_map(|&(at, target)| match self.rows[at].cmp(&target) {
                    Ordering::Less => Some((self.rows[at], Horiz::TransferUp)),
                    Ordering::Greater => Some((self.rows[at] - 1, Horiz::TransferDown)),
                    Ordering::Equal => None,
                })
                .collect();

            if glyphs.is_empty() {
                return;
            }

            if boundary {
                self.counts.boundary += glyphs.len();
            } else {
                self.counts.crossing_alignment += glyphs.len();
            }
            self.grid.column(&glyphs);

            for &(at, target) in &moves {
                match self.rows[at].cmp(&target) {
                    Ordering::Less => self.rows[at] += 1,
                    Ordering::Greater => self.rows[at] -= 1,
                    Ordering::Equal => {}
                }
            }

            self.sync_live();
        }
    }

    /// `opening` carries the two heights the feature's strands settle at, and is
    /// required for `(` and unused otherwise.
    pub(crate) fn append(&mut self, element: u8, idx: usize, opening: Option<(usize, usize)>) {
        match element {
            b'(' => {
                let (lower, upper) = opening.expect("opening requires its two heights");
                let cap = (lower + upper) / 2;

                self.grid.column(&[(cap, Horiz::OpenedBelow)]);
                self.rows.splice(idx..idx, [cap, cap + 1]);
                self.sync_live();

                self.transfer([(idx, lower), (idx + 1, upper)], true);
            }
            b')' => {
                let cup = (self.rows[idx] + self.rows[idx + 1]) / 2;
                self.transfer([(idx, cup), (idx + 1, cup + 1)], true);

                debug_assert_eq!(
                    self.rows[idx] + 1,
                    self.rows[idx + 1],
                    "a cup is never drawn between non-adjacent rows",
                );
                self.grid.column(&[(cup, Horiz::ClosedBelow)]);
                self.rows.drain(idx..idx + 2);
                self.sync_live();
            }
            b'\\' | b'/' => {
                let (lower, upper) = (self.rows[idx], self.rows[idx + 1]);
                let meet = (lower + upper) / 2;

                self.transfer([(idx, meet), (idx + 1, meet + 1)], false);
                debug_assert_eq!(
                    self.rows[idx] + 1,
                    self.rows[idx + 1],
                    "a crossing is never drawn between non-adjacent rows",
                );
                self.grid.column(&[(
                    meet,
                    if element == b'\\' {
                        Horiz::CrossDownOver
                    } else {
                        Horiz::CrossDownUnder
                    },
                )]);
                // A crossing is not a boundary: both strands resume afterwards.
                self.transfer([(idx, lower), (idx + 1, upper)], false);
            }
            _ => unimplemented!(),
        }
    }
}

/// Builds the opening-centered grid under index-aligned placement, in which a
/// feature at abbreviated index `idx` sits at row `idx` alone rather than
/// straddling rows `idx` and `idx + 1`. That leaves the cell above a feature
/// empty, so `Horiz::subsequent` can no longer say whether the next column
/// carries a strand — the live levels are tracked instead.
pub(crate) struct OpeningCentered {
    grid: Grid,
    counts: TransferCounts,
}

impl OpeningCentered {
    pub(crate) fn new(height: usize, columns: usize) -> Self {
        Self {
            grid: Grid::new(height, columns),
            counts: TransferCounts::default(),
        }
    }

    pub(crate) fn into_lines(self) -> Vec<Vec<Horiz>> {
        self.grid.into_lines()
    }

    pub(crate) fn counts(&self) -> TransferCounts {
        self.counts
    }

    fn raise_once(&mut self, from: usize) {
        let glyphs: Vec<_> = (from..self.grid.height())
            .filter(|&row| self.grid.live[row])
            .map(|row| (row, Horiz::TransferUp))
            .collect();

        self.counts.displacement += glyphs.len();
        self.grid.column(&glyphs);

        for row in (from + 1..self.grid.height()).rev() {
            self.grid.live[row] = self.grid.live[row - 1];
        }
        self.grid.live[from] = false;
    }

    fn lower_once(&mut self, from: usize) {
        let glyphs: Vec<_> = (from..self.grid.height())
            .filter(|&row| self.grid.live[row])
            .map(|row| (row - 1, Horiz::TransferDown))
            .collect();

        self.counts.displacement += glyphs.len();
        self.grid.column(&glyphs);

        for row in from - 1..self.grid.height() - 1 {
            self.grid.live[row] = self.grid.live[row + 1];
        }
        *self.grid.live.last_mut().unwrap() = false;
    }

    pub(crate) fn append(&mut self, element: u8, idx: usize) {
        match element {
            b'(' => {
                if self.grid.live[idx..].iter().any(|live| *live) {
                    // One cell per level of climb, and the opening cannot
                    // share either column: its own shadow row is where the
                    // rising strand would land.
                    self.raise_once(idx);
                    self.raise_once(idx + 1);
                }

                self.grid.column(&[(idx, Horiz::OpenedBelow)]);
                self.grid.live[idx] = true;
                self.grid.live[idx + 1] = true;
            }
            b')' => {
                let above = self.grid.live[idx + 2..].iter().any(|live| *live);

                self.grid.column(&[(idx, Horiz::ClosedBelow)]);
                self.grid.live[idx] = false;
                self.grid.live[idx + 1] = false;

                if above {
                    self.lower_once(idx + 2);
                    self.lower_once(idx + 1);
                }
            }
            b'\\' => self.grid.column(&[(idx, Horiz::CrossDownOver)]),
            b'/' => self.grid.column(&[(idx, Horiz::CrossDownUnder)]),
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_opening_centered_append() {
        let mut lines = OpeningCentered::new(4, 8);

        lines.append(b'(', 0);
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b'(', 1);
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b'\\', 1);
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b')', 1);
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b')', 0);
        insta::assert_debug_snapshot!(lines.grid.lines);
    }

    /// The climb an opening has to make when something is already live above it,
    /// and the descent a closing gives back — one whole cell per level, which is
    /// what makes `expand_above`/`contract_above`'s part-way-through halves
    /// unnecessary here.
    #[test]
    fn snapshot_opening_centered_raise_and_lower() {
        let mut lines = OpeningCentered::new(5, 6);

        lines.append(b'(', 0);
        lines.append(b'(', 0);
        insta::assert_debug_snapshot!(lines.grid.lines);

        lines.append(b')', 2);
        insta::assert_debug_snapshot!(lines.grid.lines);
    }

    struct Fixture {
        name: &'static str,
        encoding: &'static str,
        heights: &'static [(usize, usize)],
        grid: &'static str,
    }

    /// Owner-supplied expected values, transcribed from
    /// `specs/007-strand-height-precalc/fixtures/`. Never derived from running
    /// this code.
    const FIXTURES: &[Fixture] = &[
        Fixture {
            name: "rotated-5_1",
            encoding: r"(0 (2 (4 (6 /1 /3 /5 )4 )2 \0 \2 )1 )0",
            heights: &[(0, 1), (2, 3), (4, 5), (6, 7)],
            grid: r"
....______.___....
...(__.___x...\...
...___y....\...\..
..(__._)....\...\.
..___y__.....)...)
.(__.___).../.../.
.___y____._/.../..
(________x____/...
",
        },
        Fixture {
            name: "square-knot",
            encoding: r"(0 (0 \1 (1 /0 /2 )1 \1 )0 )0",
            heights: &[(4, 5), (0, 3), (1, 2)],
            grid: r"
.__________.
(__.____.__)
...x__._x...
../.._y..\..
.(..(._)..).
..\__y___/..
",
        },
        Fixture {
            name: "non-adjacent-crossing",
            encoding: r"(0 (0 /1 (2 )2 \1 /2 \1 )1 )0",
            heights: &[(4, 5), (0, 1), (2, 3)],
            grid: r"
._________._____...
(_...__...y.....\..
..\./..\./.\./\..\.
...y.().x...x..)..)
../.\__/.\_/.\/../.
.(______________/..
",
        },
        Fixture {
            name: "little-dumb-link",
            encoding: r"(0 (0 )2 (2 (4 )2 (3 )3 )1 )0",
            heights: &[(2, 3), (0, 1), (2, 3), (4, 7), (5, 6)],
            grid: r"
......_____....
...../.....\...
....(..()...\..
.....\___....\.
._..__...\....)
(_)(__)...)../.
.._______/../..
.(_________/...
",
        },
        Fixture {
            name: "square-knot-links-encircled",
            encoding: r"(0 (1 (3 (3 (7 (7 )7 \4 (4 /3 /5 )4 \4 )3 )3 )3 (1 )3 )1 )0",
            heights: &[
                (0, 15),
                (3, 4),
                (9, 10),
                (5, 8),
                (13, 14),
                (11, 12),
                (6, 7),
                (1, 2),
            ],
            grid: r"
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
",
        },
    ];

    mod height_fixtures {
        use super::*;
        use crate::diagram::AbbreviatedDiagram;
        use std::str::FromStr;

        fn heights(encoding: &str) -> Vec<(usize, usize)> {
            strand_heights(&AbbreviatedDiagram::from_str(encoding).unwrap().items)
        }

        #[test]
        fn fixtures_match_supplied_heights() {
            for fixture in FIXTURES {
                assert_eq!(&heights(fixture.encoding), fixture.heights, "{}", fixture.name);
            }
        }

        /// The five fixtures cannot distinguish the correct rule from the
        /// plausible wrong one — that a pair's gap is the count of strands
        /// opened between it, which agrees with all 23 of their pairs. These
        /// three can.
        #[test]
        fn discriminating_cases() {
            // Sequential siblings never coexist, so the second reuses the
            // first's rows. The counting rule would demand a gap of 4.
            assert_eq!(
                heights(r"(0 (1 )1 (1 )1 )0"),
                [(0, 3), (1, 2), (1, 2)],
                "sequential siblings",
            );

            // A sibling stacked above a divergent pair cannot reuse the rows
            // that pair holds open.
            assert_eq!(
                heights(r"(0 (1 (2 )2 (3 )3 )1 )0"),
                [(0, 7), (1, 4), (2, 3), (5, 6)],
                "sibling stacked above a divergent pair",
            );

            // Two strands that never coexist and never relate are still pushed
            // apart, because a third lies between them in the order.
            assert_eq!(
                heights(r"(0 (0 )0 (2 )2 )0"),
                [(2, 3), (0, 1), (4, 5)],
                "transitive push",
            );
        }

        #[test]
        fn degenerate_diagrams() {
            assert_eq!(heights(""), []);
            assert_eq!(heights(r"(0 )0"), [(0, 1)]);
            assert_eq!(heights(r"(0 (1 (2 )2 )1 )0"), [(0, 5), (1, 4), (2, 3)]);
        }

        #[test]
        fn deterministic() {
            for fixture in FIXTURES {
                assert_eq!(heights(fixture.encoding), heights(fixture.encoding));
            }
        }
    }

    mod render_fixtures {
        use super::*;
        use crate::diagram::AbbreviatedDiagram;
        use crate::render::{VerboseDiagram, VerboseLine};
        use std::str::FromStr;

        /// Renders against *supplied* heights, never against Component A's
        /// output, so a defect in one half cannot mask a defect in the other.
        fn render(encoding: &str, heights: &[(usize, usize)]) -> (String, TransferCounts) {
            let items = AbbreviatedDiagram::from_str(encoding).unwrap().items;
            let mut builder = Precalculated::new(grid_height(heights), items.len());
            let mut openings = heights.iter().copied();

            for &AbbreviatedItem { element, index } in &items {
                let opening = (element == b'(').then(|| openings.next().unwrap());
                builder.append(element, index, opening);
            }

            let counts = builder.counts();
            let text = VerboseDiagram(
                builder
                    .into_lines()
                    .into_iter()
                    .map(VerboseLine)
                    .collect(),
            )
            .to_text();

            (text, counts)
        }

        #[test]
        fn fixtures_match_supplied_grids() {
            for fixture in FIXTURES {
                let (text, _) = render(fixture.encoding, fixture.heights);
                assert_eq!(
                    text,
                    fixture.grid.trim_start_matches('\n'),
                    "{}",
                    fixture.name,
                );
            }
        }

        /// Every cap, cup and crossing is drawn at the floored midpoint of the
        /// two strands it joins, so the participants are adjacent by the time
        /// the glyph lands. The builder's debug assertions enforce this; the
        /// non-adjacent-crossing fixture is what exercises them.
        #[test]
        fn crossings_are_never_drawn_between_non_adjacent_rows() {
            for fixture in FIXTURES {
                render(fixture.encoding, fixture.heights);
            }
            render(r"(0 (1 )1 (1 )1 )0", &[(0, 3), (1, 2), (1, 2)]);
        }

        /// Precalculated placement emits no displacement transfers at all —
        /// that is the point. Its cost shows up as boundary and
        /// crossing-alignment glyphs instead (SC-002).
        #[test]
        fn transfers_are_classified() {
            for fixture in FIXTURES {
                let (_, counts) = render(fixture.encoding, fixture.heights);
                assert_eq!(counts.displacement, 0, "{}", fixture.name);
            }

            let (_, counts) = render(r"(0 (0 )0 )0", &[(2, 3), (0, 1)]);
            assert_eq!(counts, TransferCounts::default(), "flat pair needs no transfers");
        }

        #[test]
        fn grid_height_is_one_past_the_tallest_strand() {
            for fixture in FIXTURES {
                let (text, _) = render(fixture.encoding, fixture.heights);
                let rows = text.trim_end().split('\n').count();
                assert_eq!(rows, grid_height(fixture.heights), "{}", fixture.name);
            }
        }
    }
}
