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
