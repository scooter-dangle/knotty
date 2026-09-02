use super::*;
use crate::moves::Lean;
use pretty_assertions::assert_eq;

#[test]
fn snapshot_from_abbreviated() {
    let knot = AbbreviatedDiagram::new_from_tuples(vec![(b'(', 0), (b'\\', 0), (b')', 0)]).unwrap();

    let verbose = VerboseDiagram::from_abbreviated(&knot, RenderMode::Standard).unwrap();
    insta::assert_debug_snapshot!(verbose);
}

#[test]
fn test_available_bulges() {
    let diagram = AbbreviatedDiagram::from_str(
        "\
        (0\n\
        )0\n\
        ",
    )
    .unwrap();

    let available_bulges = diagram.available_bulges().collect::<Vec<_>>();

    assert_eq!(
        available_bulges,
        vec![
            (1, (Lean::Backward, 0)),
            (1, (Lean::Forward, 0)),
            (1, (Lean::Backward, 1)),
            (1, (Lean::Forward, 1)),
        ]
    );

    // ------------------------------------------

    let diagram = AbbreviatedDiagram::from_str(
        "\
        (0\n\
        (0\n\
        )0\n\
        )0\n\
        ",
    )
    .unwrap();

    let available_bulges = diagram.available_bulges().collect::<Vec<_>>();

    assert_eq!(
        available_bulges,
        vec![
            (1, (Lean::Backward, 0)),
            (1, (Lean::Forward, 0)),
            (1, (Lean::Backward, 1)),
            (1, (Lean::Forward, 1)),
            // ---
            (2, (Lean::Backward, 0)),
            (2, (Lean::Forward, 0)),
            (2, (Lean::Backward, 1)),
            (2, (Lean::Forward, 1)),
            (2, (Lean::Backward, 2)),
            (2, (Lean::Forward, 2)),
            (2, (Lean::Backward, 3)),
            (2, (Lean::Forward, 3)),
            // ---
            (3, (Lean::Backward, 0)),
            (3, (Lean::Forward, 0)),
            (3, (Lean::Backward, 1)),
            (3, (Lean::Forward, 1)),
        ]
    );
}

fn sample_knots() -> [(&'static str, Vec<(u8, usize)>); 8] {
    [
        // Unknot:
        //
        //  /\
        // <  >
        //  \/
        //
        ("unknot", vec![(b'(', 0), (b')', 0)]),
        // Trefoil:
        //       _____________
        //      /             \
        //     <               >
        //      \__   _____   /
        //         \ /     \ /
        //          \       \
        //   ______/ \_   _/ \________
        //  /          \ /            \
        // <            /              >
        //  \__________/ \____________/
        //
        (
            "trefoil",
            vec![
                (b'(', 0),
                (b'(', 2),
                (b'\\', 1),
                (b'/', 0),
                (b'\\', 1),
                (b')', 2),
                (b')', 0),
            ],
        ),
        ("donut", vec![(b'(', 0), (b'(', 1), (b')', 1), (b')', 0)]),
        // C:
        ("c_thingy", vec![(b'(', 0), (b'(', 1), (b')', 2), (b')', 0)]),
        // weird terrace thing:
        (
            "terrace",
            vec![
                (b'(', 0),
                (b'(', 2),
                (b'(', 4),
                (b'(', 6),
                (b')', 5),
                (b')', 3),
                (b')', 1),
                (b'(', 1),
                (b'(', 3),
                (b'(', 5),
                (b')', 6),
                (b')', 4),
                (b')', 2),
                (b')', 0),
            ],
        ),
        (
            "basket",
            vec![
                (b'(', 0),
                (b'(', 1),
                (b'(', 1),
                (b'\\', 3),
                (b'\\', 2),
                (b'\\', 4),
                (b'\\', 3),
                (b')', 1),
                (b')', 1),
                (b')', 0),
            ],
        ),
        (
            "ugly_trefoil",
            vec![
                (b'(', 0),
                (b'(', 0),
                (b'\\', 1),
                (b'/', 0),
                (b'\\', 1),
                (b')', 0),
                (b')', 0),
            ],
        ),
        (
            "weird_thing_that_broke_once",
            vec![
                (b'(', 0),
                (b'(', 2),
                (b')', 0),
                (b'(', 2),
                (b')', 2),
                (b'(', 0),
                (b')', 1),
                (b')', 0),
            ],
        ),
    ]
}

fn has_transfer(knot: &[(u8, usize)]) -> bool {
    use Horiz::*;

    let diagram = AbbreviatedDiagram::new_from_tuples(knot.to_vec()).unwrap();

    VerboseDiagram::from_abbreviated(&diagram, RenderMode::Standard)
        .unwrap()
        .0
        .iter()
        .flat_map(|line| line.0.iter())
        .any(|horiz| {
            matches!(
                horiz,
                TransferUpStart
                    | TransferUp
                    | TransferUpFinish
                    | TransferDownStart
                    | TransferDown
                    | TransferDownFinish
            )
        })
}

#[test]
fn snapshot_ascii_print() {
    for (_name, knot) in sample_knots() {
        insta::assert_snapshot!(ascii_print_compact::<false>(knot, RenderMode::Standard));
    }
}

#[test]
fn snapshot_ascii_print_opening_centered() {
    for (_name, knot) in sample_knots() {
        insta::assert_snapshot!(ascii_print_compact::<false>(
            knot,
            RenderMode::OpeningCentered
        ));
    }
}

#[test]
fn transfer_free_knots_render_identically_in_both_modes() {
    // "Identically" is about the picture itself; see the bordered case below.
    let mut checked = 0;

    for (name, knot) in sample_knots() {
        if has_transfer(&knot) {
            continue;
        }
        checked += 1;

        assert_eq!(
            ascii_print::<false>(knot.clone(), RenderMode::Standard),
            ascii_print::<false>(knot.clone(), RenderMode::OpeningCentered),
            "{name}",
        );
        // With the cell boundaries drawn the two must *differ*, even here:
        // the picture is the same, but which cell owns which mark is the
        // whole point of the mode.
        assert_ne!(
            ascii_print::<true>(knot.clone(), RenderMode::Standard),
            ascii_print::<true>(knot.clone(), RenderMode::OpeningCentered),
            "{name}",
        );
        assert_eq!(
            ascii_print_compact::<false>(knot.clone(), RenderMode::Standard),
            ascii_print_compact::<false>(knot, RenderMode::OpeningCentered),
            "{name}",
        );
    }

    // Without this the test passes vacuously the day `has_transfer` breaks.
    assert!(checked >= 2, "only {checked} transfer-free sample knots");
}

fn grid(knot: &[(u8, usize)], mode: RenderMode) -> VerboseDiagram {
    let diagram = AbbreviatedDiagram::new_from_tuples(knot.to_vec()).unwrap();

    VerboseDiagram::from_abbreviated(&diagram, mode).unwrap()
}

fn transfer_columns(knot: &[(u8, usize)], mode: RenderMode) -> usize {
    use Horiz::*;

    let grid = grid(knot, mode);
    let width = grid.0.iter().map(|line| line.0.len()).max().unwrap_or(0);

    (0..width)
        .filter(|&column| {
            grid.0.iter().any(|line| {
                matches!(
                    line.0[column],
                    TransferUpStart
                        | TransferUp
                        | TransferUpFinish
                        | TransferDownStart
                        | TransferDown
                        | TransferDownFinish
                )
            })
        })
        .count()
}

#[test]
fn opening_centered_never_emits_a_retired_cell() {
    use Horiz::*;

    for (name, knot) in sample_knots() {
        let grid = grid(&knot, RenderMode::OpeningCentered);

        for horiz in grid.0.iter().flat_map(|line| line.0.iter()) {
            assert!(
                !matches!(
                    horiz,
                    CrossUpOver
                        | CrossUpUnder
                        | OpenedAbove
                        | ClosedAbove
                        | TransferUpStart
                        | TransferUpFinish
                        | TransferDownStart
                        | TransferDownFinish
                ),
                "{name} emitted {horiz:?}",
            );
        }

        // Every feature sits at row `idx`, and the highest is at `height - 2`,
        // so the top row can only ever carry strands.
        let top = grid.0.last().unwrap();
        assert!(
            top.0.iter().all(|horiz| matches!(horiz, Empty | Line)),
            "{name} top row: {:?}",
            top.0,
        );
    }
}

#[test]
fn opening_centered_spends_two_columns_where_standard_spends_three() {
    // Standard raises a stack two levels over three columns, using halves
    // that start and finish a climb part way through a cell. Opening-centered
    // climbs one whole level per cell, so the same climb costs two.
    let mut measured = 0;

    for (name, knot) in sample_knots() {
        let standard = transfer_columns(&knot, RenderMode::Standard);
        let opening_centered = transfer_columns(&knot, RenderMode::OpeningCentered);

        if standard == 0 {
            assert_eq!(opening_centered, 0, "{name}");
            continue;
        }
        measured += 1;

        assert_eq!(opening_centered * 3, standard * 2, "{name}");
    }

    assert!(measured >= 4, "only {measured} sample knots with transfers");
}

/// Every valid diagram of `len` items whose height never exceeds `max_height`,
/// built straight from the notation's own rules: an opening at `idx <= height`
/// raises the height by two, a closing lowers it, a crossing needs two levels
/// beneath it, and the diagram has to come back to nothing.
fn every_diagram(len: usize, max_height: usize) -> Vec<Vec<(u8, usize)>> {
    fn go(
        cur: &mut Vec<(u8, usize)>,
        height: usize,
        left: usize,
        max_height: usize,
        out: &mut Vec<Vec<(u8, usize)>>,
    ) {
        if left == 0 {
            if height == 0 && !cur.is_empty() {
                out.push(cur.clone());
            }
            return;
        }

        for idx in 0..=max_height {
            if idx <= height && height + 2 <= max_height {
                cur.push((b'(', idx));
                go(cur, height + 2, left - 1, max_height, out);
                cur.pop();
            }

            if height >= 2 && idx + 2 <= height {
                cur.push((b')', idx));
                go(cur, height - 2, left - 1, max_height, out);
                cur.pop();

                for element in [b'\\', b'/'] {
                    cur.push((element, idx));
                    go(cur, height, left - 1, max_height, out);
                    cur.pop();
                }
            }
        }
    }

    let mut out = Vec::new();
    go(&mut Vec::new(), 0, len, max_height, &mut out);
    out
}

/// What `try_rotate_90_ccw` does, with the rendering left open: draw the
/// diagram, reverse each line, and read the rows back bottom-up.
fn rotate_in_mode(
    diagram: &AbbreviatedDiagram,
    mode: RenderMode,
) -> Result<Vec<(u8, usize)>, String> {
    let verbose = VerboseDiagram::from_abbreviated(diagram, mode)?;

    let reversed: Vec<String> = verbose
        .0
        .iter()
        .rev()
        .flat_map(|line| line.display::<false>(mode))
        .map(|line| line.trim_end_matches('\n').chars().rev().collect())
        .collect();

    let mut out = Vec::new();
    let mut prev: Option<&str> = None;
    for cur in reversed.iter().rev() {
        out.extend(crate::rotate::scan_row(cur, prev));
        prev = Some(cur);
    }

    AbbreviatedDiagram::new_from_tuples(out.clone()).map(|_| out)
}

/// The evidence that lets `full_render_lines` stop asking for the standard
/// rendering. `scan_row`'s patterns were written against standard tiles, so
/// whether they read an opening-centered picture the same way is a question
/// about every diagram, not about the handful the rotation tests name.
///
/// The corpus is bounded to keep the suite quick. Widening it to every diagram
/// of length 2–8 and height <= 8 — 175,536 of them, 170,928 carrying transfers —
/// also agrees on all of them, but takes half a minute; that run is recorded in
/// `specs/005-retire-standard-rendering/research.md`.
///
/// Delete this once the standard rendering is gone — it has no second operand
/// then, and the rotation tests become the whole guarantee.
#[test]
fn rotation_reads_both_renderings_the_same_way() {
    let mut compared = 0;
    let mut with_transfer = 0;

    for len in 2..=6 {
        for knot in every_diagram(len, 6) {
            let diagram = AbbreviatedDiagram::new_from_tuples(knot.clone()).unwrap();

            let standard = rotate_in_mode(&diagram, RenderMode::Standard);
            let opening_centered = rotate_in_mode(&diagram, RenderMode::OpeningCentered);

            // Agreeing by failing alike would prove nothing.
            assert!(
                standard.is_ok() && opening_centered.is_ok(),
                "{knot:?} rotated to {standard:?} / {opening_centered:?}",
            );
            assert_eq!(standard, opening_centered, "{knot:?}");

            compared += 1;
            if has_transfer(&knot) {
                with_transfer += 1;
            }
        }
    }

    assert!(compared >= 1_000, "only {compared} diagrams compared");
    // Transfers are the one place the two renderings legitimately differ, so a
    // corpus without them would agree for the wrong reason.
    assert!(with_transfer >= 500, "only {with_transfer} carried transfers");
}

/// FR-024 stated on its own terms. The comparison against the standard
/// rendering's three-columns-per-two-levels says the same thing, but only for
/// as long as there is something to compare against.
#[test]
fn a_climb_costs_one_column_per_level() {
    use Horiz::*;

    // Each opening that has to make room, and each closing that has to give it
    // back, moves the levels above it twice: once for its own row, once for the
    // row above. Every one of those moves is a whole cell, so it is a column.
    let expected = [
        ("unknot", 0),
        ("trefoil", 0),
        ("donut", 4),
        ("c_thingy", 2),
        ("terrace", 12),
        ("basket", 8),
        ("ugly_trefoil", 4),
        ("weird_thing_that_broke_once", 6),
    ];

    let mut measured = 0;

    for ((name, knot), (expected_name, columns)) in sample_knots().into_iter().zip(expected) {
        assert_eq!(name, expected_name);

        let grid = grid(&knot, RenderMode::OpeningCentered);

        // A cell that starts or finishes a climb part way through is what makes
        // a level cost less than a whole column. Opening-centered has none.
        for horiz in grid.0.iter().flat_map(|line| line.0.iter()) {
            assert!(
                !matches!(
                    horiz,
                    TransferUpStart | TransferUpFinish | TransferDownStart | TransferDownFinish
                ),
                "{name} emitted {horiz:?}",
            );
        }

        // And the cells it does use cross their own cell corner to corner —
        // one level, one cell, one column.
        for horiz in [TransferUp, TransferDown] {
            let [top, _, bottom] = horiz.display(RenderMode::OpeningCentered);
            assert_ne!(top.trim(), "", "{horiz:?}");
            assert_ne!(bottom.trim(), "", "{horiz:?}");
        }

        assert_eq!(
            transfer_columns(&knot, RenderMode::OpeningCentered),
            columns,
            "{name}",
        );

        if columns > 0 {
            measured += 1;
        }
    }

    assert!(measured >= 4, "only {measured} sample knots with transfers");
}

/// FR-015 without a second rendering to measure against.
#[test]
fn pictures_are_rectangular_and_end_flush() {
    for (name, knot) in sample_knots() {
        let width = grid(&knot, RenderMode::OpeningCentered)
            .0
            .iter()
            .map(|line| line.0.len())
            .max()
            .unwrap_or(0);

        let picture = ascii_print::<false>(knot, RenderMode::OpeningCentered);
        let lines: Vec<&str> = picture.lines().collect();

        for line in &lines {
            assert_eq!(line.len(), width * 3, "{name}: {line:?}");
        }

        let blank = |line: &&&str| line.trim().is_empty();
        assert_eq!(
            lines.iter().rev().take_while(blank).count(),
            0,
            "{name}: trailing blank lines",
        );
        // The unknot needs one, to leave room above its opening.
        assert!(
            lines.iter().take_while(blank).count() <= 1,
            "{name}: leading blank lines",
        );
    }
}

#[test]
fn both_modes_render_at_the_same_size() {
    for (name, knot) in sample_knots() {
        let standard = ascii_print::<false>(knot.clone(), RenderMode::Standard);
        let opening_centered = ascii_print::<false>(knot, RenderMode::OpeningCentered);

        let lines = |text: &str| text.lines().map(str::to_owned).collect::<Vec<_>>();
        let (standard, opening_centered) = (lines(&standard), lines(&opening_centered));

        assert_eq!(standard.len(), opening_centered.len(), "{name}");
        assert_eq!(
            standard.iter().map(String::len).max(),
            opening_centered.iter().map(String::len).max(),
            "{name}",
        );

        // FR-015: no blank line the other rendering does not also have.
        let blank_run = |lines: &[String], rev: bool| -> usize {
            let mut lines = lines.to_vec();
            if rev {
                lines.reverse();
            }
            lines
                .iter()
                .take_while(|line| line.trim().is_empty())
                .count()
        };

        assert!(
            blank_run(&opening_centered, false) <= blank_run(&standard, false),
            "{name}: leading blank lines",
        );
        assert!(
            blank_run(&opening_centered, true) <= blank_run(&standard, true),
            "{name}: trailing blank lines",
        );
    }
}
