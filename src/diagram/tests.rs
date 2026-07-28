use super::*;
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

#[test]
fn snapshot_ascii_print() {
    // Unknot:
    //
    //  /\
    // <  >
    //  \/
    //
    let unknot = vec![(b'(', 0), (b')', 0)];
    insta::assert_snapshot!(ascii_print_compact::<false>(unknot));

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
    //
    let trefoil = vec![
        (b'(', 0),
        (b'(', 2),
        (b'\\', 1),
        (b'/', 0),
        (b'\\', 1),
        (b')', 2),
        (b')', 0),
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(trefoil));

    // donut:
    let donut = vec![(b'(', 0), (b'(', 1), (b')', 1), (b')', 0)];
    insta::assert_snapshot!(ascii_print_compact::<false>(donut));

    // C:
    let c_thingy = vec![(b'(', 0), (b'(', 1), (b')', 2), (b')', 0)];
    insta::assert_snapshot!(ascii_print_compact::<false>(c_thingy));

    // weird terrace thing:
    let terrace = vec![
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
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(terrace));

    // basket:
    let basket = vec![
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
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(basket));

    // ugly trefoil:
    let ugly_trefoil = vec![
        (b'(', 0),
        (b'(', 0),
        (b'\\', 1),
        (b'/', 0),
        (b'\\', 1),
        (b')', 0),
        (b')', 0),
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(ugly_trefoil));

    // weird_thing_that_broke_once:
    let weird_thing_that_broke_once = vec![
        (b'(', 0),
        (b'(', 2),
        (b')', 0),
        (b'(', 2),
        (b')', 2),
        (b'(', 0),
        (b')', 1),
        (b')', 0),
    ];
    insta::assert_snapshot!(ascii_print_compact::<false>(weird_thing_that_broke_once));
}

fn transfer_count(diagram: &AbbreviatedDiagram) -> usize {
    VerboseDiagram::from_abbreviated(diagram)
        .unwrap()
        .0
        .iter()
        .flat_map(|line| line.0.iter())
        .filter(|horiz| {
            matches!(
                horiz,
                Horiz::TransferUpStart
                    | Horiz::TransferUp
                    | Horiz::TransferUpFinish
                    | Horiz::TransferDownStart
                    | Horiz::TransferDown
                    | Horiz::TransferDownFinish
            )
        })
        .count()
}

fn terrace() -> AbbreviatedDiagram {
    "(0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0"
        .parse::<AbbreviatedDiagram>()
        .unwrap()
}

#[test]
fn snapshot_precalculated_heights_terrace() {
    insta::assert_snapshot!(terrace()
        .with_mode(RenderMode::PrecalculatedHeights)
        .ascii_print_compact::<false>());
}

#[test]
fn precalculated_heights_removes_avoidable_transfers() {
    let legacy = terrace();
    let precalc = legacy.clone().with_mode(RenderMode::PrecalculatedHeights);

    assert!(
        transfer_count(&legacy) > 0,
        "terrace should zig-zag under the legacy renderer",
    );
    assert_eq!(
        transfer_count(&precalc),
        0,
        "every terrace strand should run flat under precalculated heights",
    );
}

#[test]
fn precalculated_heights_never_adds_transfers() {
    // Includes diagrams that fall back to the legacy placement, which must
    // still never render worse than legacy.
    for source in [
        "(0 )0",
        "(0 (1 )1 )0",
        "(0 (1 )2 )0",
        "(0 (2 (4 )4 )2 )0",
        "(0 (2 \\1 /0 \\1 )2 )0",
        "(0 (0 \\1 /0 \\1 )0 )0",
        "(0 (1 (1 \\3 \\2 \\4 \\3 )1 )1 )0",
    ] {
        let legacy = source.parse::<AbbreviatedDiagram>().unwrap();
        let precalc = legacy.clone().with_mode(RenderMode::PrecalculatedHeights);

        assert!(
            transfer_count(&precalc) <= transfer_count(&legacy),
            "{source} rendered more transfers under precalculated heights",
        );
    }
}

#[test]
fn precalculated_heights_handles_degenerate_diagrams() {
    let empty = AbbreviatedDiagram::default().with_mode(RenderMode::PrecalculatedHeights);
    assert_eq!(empty.try_ascii_print_compact::<false>().unwrap(), "");

    let unknot = "(0 )0"
        .parse::<AbbreviatedDiagram>()
        .unwrap()
        .with_mode(RenderMode::PrecalculatedHeights);
    assert_eq!(
        unknot.ascii_print_compact::<false>(),
        "(0 )0"
            .parse::<AbbreviatedDiagram>()
            .unwrap()
            .ascii_print_compact::<false>(),
    );
}
