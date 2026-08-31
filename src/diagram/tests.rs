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

#[test]
fn snapshot_ascii_print() {
    // Unknot:
    //
    //  /\
    // <  >
    //  \/
    //
    let unknot = vec![(b'(', 0), (b')', 0)];
    insta::assert_snapshot!(ascii_print_compact::<false>(unknot, RenderMode::Standard));

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
    insta::assert_snapshot!(ascii_print_compact::<false>(trefoil, RenderMode::Standard));

    // donut:
    let donut = vec![(b'(', 0), (b'(', 1), (b')', 1), (b')', 0)];
    insta::assert_snapshot!(ascii_print_compact::<false>(donut, RenderMode::Standard));

    // C:
    let c_thingy = vec![(b'(', 0), (b'(', 1), (b')', 2), (b')', 0)];
    insta::assert_snapshot!(ascii_print_compact::<false>(c_thingy, RenderMode::Standard));

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
    insta::assert_snapshot!(ascii_print_compact::<false>(terrace, RenderMode::Standard));

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
    insta::assert_snapshot!(ascii_print_compact::<false>(basket, RenderMode::Standard));

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
    insta::assert_snapshot!(ascii_print_compact::<false>(
        ugly_trefoil,
        RenderMode::Standard
    ));

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
    insta::assert_snapshot!(ascii_print_compact::<false>(
        weird_thing_that_broke_once,
        RenderMode::Standard
    ));
}
