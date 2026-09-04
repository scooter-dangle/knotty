use super::*;
use crate::render::Horiz;
use crate::moves::Lean;
use pretty_assertions::assert_eq;

#[test]
fn snapshot_from_abbreviated() {
    let knot = AbbreviatedDiagram::new_from_tuples(vec![(b'(', 0), (b'\\', 0), (b')', 0)]).unwrap();

    let verbose = VerboseDiagram::from_abbreviated(&knot).unwrap();
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

#[test]
fn snapshot_ascii_print() {
    for (_name, knot) in sample_knots() {
        insta::assert_snapshot!(ascii_print_compact::<false>(knot));
    }
}

fn grid(knot: &[(u8, usize)]) -> VerboseDiagram {
    let diagram = AbbreviatedDiagram::new_from_tuples(knot.to_vec()).unwrap();

    VerboseDiagram::from_abbreviated(&diagram).unwrap()
}

fn transfer_columns(knot: &[(u8, usize)]) -> usize {
    use Horiz::*;

    let grid = grid(knot);
    let width = grid.0.iter().map(|line| line.0.len()).max().unwrap_or(0);

    (0..width)
        .filter(|&column| {
            grid.0.iter().any(|line| {
                matches!(line.0[column], TransferUp | TransferDown)
            })
        })
        .count()
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

        let grid = grid(&knot);

        // Every feature sits at row `idx`, and the highest is at `height - 2`,
        // so the top row can only ever carry strands.
        let top = grid.0.last().unwrap();
        assert!(
            top.0.iter().all(|horiz| matches!(horiz, Empty | Line)),
            "{name} top row: {:?}",
            top.0,
        );

        // A cell that starts or finishes a climb part way through would make a
        // level cost less than a whole column. There is no such cell left to
        // use, so the ones that remain cross their own cell corner to corner —
        // one level, one cell, one column.
        for horiz in [TransferUp, TransferDown] {
            let [top, _, bottom] = horiz.display();
            assert_ne!(top.trim(), "", "{horiz:?}");
            assert_ne!(bottom.trim(), "", "{horiz:?}");
        }

        assert_eq!(
            transfer_columns(&knot),
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
        let width = grid(&knot)
            .0
            .iter()
            .map(|line| line.0.len())
            .max()
            .unwrap_or(0);

        let picture = ascii_print::<false>(knot);
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
fn snapshot_precalculated_heights() {
    for encoding in [
        r"(0 (2 (4 (6 /1 /3 /5 )4 )2 \0 \2 )1 )0",
        r"(0 (0 \1 (1 /0 /2 )1 \1 )0 )0",
        r"(0 (0 /1 (2 )2 \1 /2 \1 )1 )0",
        r"(0 (0 )2 (2 (4 )2 (3 )3 )1 )0",
        r"(0 (1 (3 (3 (7 (7 )7 \4 (4 /3 /5 )4 \4 )3 )3 )3 (1 )3 )1 )0",
    ] {
        let knot = AbbreviatedDiagram::from_str(encoding)
            .unwrap()
            .with_mode(PlacementMode::PrecalculatedHeights);

        insta::assert_snapshot!(knot.ascii_print_compact::<false>());
    }
}

#[test]
fn precalculated_placement_removes_displacement_transfers() {
    let knot =
        AbbreviatedDiagram::from_str(r"(0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0").unwrap();

    let (_, aligned) = knot.build_lines();
    let (_, precalculated) = knot
        .clone()
        .with_mode(PlacementMode::PrecalculatedHeights)
        .build_lines();

    assert!(
        aligned.displacement > 0,
        "terrace displaces strands under index-aligned placement",
    );
    assert!(
        precalculated.displacement < aligned.displacement,
        "{} displacement glyphs, down from {}",
        precalculated.displacement,
        aligned.displacement,
    );
    assert_eq!(precalculated.displacement, 0);
}

#[test]
fn unchanging_strands_render_flat() {
    // The outer pair is pushed up and pulled back down under index-aligned
    // placement; under precalculated placement it opens at its height and
    // never moves, so no transfer of any kind is drawn.
    let knot = AbbreviatedDiagram::from_str(r"(0 (0 )0 )0").unwrap();

    let (_, aligned) = knot.build_lines();
    assert!(aligned.displacement > 0);

    let (_, precalculated) = knot
        .with_mode(PlacementMode::PrecalculatedHeights)
        .build_lines();
    assert_eq!(precalculated, TransferCounts::default());
}

#[test]
fn snapshot_precalculated_heights_with_crossings() {
    for encoding in [
        r"(0 (1 (1 \3 \2 \4 \3 )1 )1 )0",
        r"(0 (0 \1 /0 \1 )0 )0",
    ] {
        let knot = AbbreviatedDiagram::from_str(encoding)
            .unwrap()
            .with_mode(PlacementMode::PrecalculatedHeights);

        insta::assert_snapshot!(knot.ascii_print_compact::<false>());
    }
}

#[test]
fn crossings_survive_the_change_of_placement() {
    fn crossings(knot: &AbbreviatedDiagram) -> Vec<Horiz> {
        let (lines, _) = knot.build_lines();
        let mut found: Vec<Horiz> = Vec::new();

        // Column-major, so the crossings come out in drawing order.
        for column in 0..lines.first().map_or(0, Vec::len) {
            for line in &lines {
                if matches!(line[column], Horiz::CrossDownOver | Horiz::CrossDownUnder) {
                    found.push(line[column]);
                }
            }
        }

        found
    }

    for encoding in [
        r"(0 (1 (1 \3 \2 \4 \3 )1 )1 )0",
        r"(0 (0 \1 /0 \1 )0 )0",
        r"(0 (0 /1 (2 )2 \1 /2 \1 )1 )0",
    ] {
        let aligned = AbbreviatedDiagram::from_str(encoding).unwrap();
        let expected = aligned.items.iter().filter(|item| item.is_crossing()).count();
        let precalculated = aligned
            .clone()
            .with_mode(PlacementMode::PrecalculatedHeights);

        // Same crossings, in the same order, over and under preserved.
        assert_eq!(crossings(&aligned).len(), expected, "{encoding}");
        assert_eq!(crossings(&precalculated), crossings(&aligned), "{encoding}");
    }
}

#[test]
fn nested_openings_never_share_a_row() {
    // Exercises the builder's ordering assertion, which is what guarantees two
    // strands never land on the same row.
    for encoding in [
        r"(0 (1 (2 (3 (4 )4 )3 )2 )1 )0",
        r"(0 (1 )1 (1 (2 )2 )1 )0",
        r"(0 (1 (3 (3 (7 (7 )7 \4 (4 /3 /5 )4 \4 )3 )3 )3 (1 )3 )1 )0",
    ] {
        let knot = AbbreviatedDiagram::from_str(encoding)
            .unwrap()
            .with_mode(PlacementMode::PrecalculatedHeights);

        let (lines, _) = knot.build_lines();
        assert!(lines.iter().all(|line| line.len() == lines[0].len()), "{encoding}");
    }
}
