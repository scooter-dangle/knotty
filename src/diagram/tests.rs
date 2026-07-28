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

#[test]
fn rotation_preserves_the_active_mode() {
    let mut diagram = terrace().with_mode(RenderMode::PrecalculatedHeights);
    diagram.try_rotate_90_ccw().unwrap();

    assert_eq!(diagram.mode(), RenderMode::PrecalculatedHeights);
}

#[test]
fn rotation_does_not_inflate_scanned_features() {
    let original = terrace();

    let mut legacy = original.clone();
    legacy.try_rotate_90_ccw().unwrap();

    let mut precalc = original.clone().with_mode(RenderMode::PrecalculatedHeights);
    precalc.try_rotate_90_ccw().unwrap();

    assert!(
        precalc.len() <= original.len(),
        "precalculated rotation grew the diagram: {} -> {}",
        original.len(),
        precalc.len(),
    );
    assert!(
        precalc.len() < legacy.len(),
        "precalculated rotation ({}) should scan fewer features than legacy ({})",
        precalc.len(),
        legacy.len(),
    );
}

#[test]
fn repeated_rotation_does_not_accumulate_features() {
    let mut diagram = terrace().with_mode(RenderMode::PrecalculatedHeights);
    let original_len = diagram.len();

    // Rotation is not yet period-4 (see test_try_rotate_90_ccw_period_4_regressions),
    // so this asserts the feature count stays bounded rather than that the
    // notation returns to its starting value.
    for rotation in 0..4 {
        diagram.try_rotate_90_ccw().unwrap();
        assert!(
            diagram.len() <= original_len,
            "rotation {rotation} grew the diagram to {} features (started at {original_len})",
            diagram.len(),
        );
    }
}

#[test]
fn snapshot_precalculated_heights_terrace_rotated() {
    let mut diagram = terrace().with_mode(RenderMode::PrecalculatedHeights);
    diagram.try_rotate_90_ccw().unwrap();

    insta::assert_snapshot!(diagram.ascii_print_compact::<false>());
}

#[test]
fn rotation_agrees_with_legacy_on_diagrams_without_avoidable_movement() {
    // Where the legacy renderer already has nothing to gain, both modes must
    // rotate to exactly the same notation.
    for source in ["(0 (2 )2 )0", "(0 (2 (4 )4 )2 )0"] {
        let original = source.parse::<AbbreviatedDiagram>().unwrap();

        let mut legacy = original.clone();
        legacy.try_rotate_90_ccw().unwrap();

        let mut precalc = original.with_mode(RenderMode::PrecalculatedHeights);
        precalc.try_rotate_90_ccw().unwrap();

        assert_eq!(precalc.to_tuples(), legacy.to_tuples(), "{source}");
    }
}

#[test]
fn test_render_mode_defaults_to_legacy() {
    assert_eq!(AbbreviatedDiagram::default().mode(), RenderMode::Legacy);
    assert_eq!(terrace().mode(), RenderMode::Legacy);
    assert_eq!(
        AbbreviatedDiagram::new_from_tuples(vec![(b'(', 0), (b')', 0)])
            .unwrap()
            .mode(),
        RenderMode::Legacy,
    );
}

#[test]
fn notation_only_moves_are_independent_of_mode() {
    let source = "(0 (2 \\1 /0 \\1 )2 )0";

    for r#move in ["swap@3", "change_crossing@2", "wrap_around@2"] {
        let diagram_move = r#move.parse::<DiagramMove>().unwrap();

        let mut legacy = source.parse::<AbbreviatedDiagram>().unwrap();
        let mut precalc = source
            .parse::<AbbreviatedDiagram>()
            .unwrap()
            .with_mode(RenderMode::PrecalculatedHeights);

        let legacy_result = legacy.try_apply(diagram_move);
        let precalc_result = precalc.try_apply(diagram_move);

        assert_eq!(legacy_result, precalc_result, "{move:?} outcome differed");
        assert_eq!(
            precalc.to_tuples(),
            legacy.to_tuples(),
            "{move:?} produced different notation per mode",
        );
        assert_eq!(precalc.mode(), RenderMode::PrecalculatedHeights);
    }
}

#[test]
fn setting_a_mode_leaves_the_notation_untouched() {
    let original = terrace();
    let switched = original.clone().with_mode(RenderMode::PrecalculatedHeights);

    assert_eq!(switched.to_tuples(), original.to_tuples());
}

#[test]
fn snapshot_precalculated_heights_ugly_trefoil() {
    // A crossing-bearing diagram that the flat placement can represent: the
    // precalculation keeps each crossing's two strands on adjacent rows.
    insta::assert_snapshot!("(0 (0 \\1 /0 \\1 )0 )0"
        .parse::<AbbreviatedDiagram>()
        .unwrap()
        .with_mode(RenderMode::PrecalculatedHeights)
        .ascii_print_compact::<false>());
}

#[test]
fn crossings_always_render_between_adjacent_rows() {
    // Whether a diagram takes the flat path or falls back, a crossing is only
    // ever drawn across two neighbouring rows.
    for source in [
        "(0 (0 \\1 /0 \\1 )0 )0",
        "(0 (2 \\1 /0 \\1 )2 )0",
        "(0 (1 (1 \\3 \\2 \\4 \\3 )1 )1 )0",
    ] {
        let diagram = source
            .parse::<AbbreviatedDiagram>()
            .unwrap()
            .with_mode(RenderMode::PrecalculatedHeights);
        let verbose = VerboseDiagram::from_abbreviated(&diagram).unwrap();

        for (row, line) in verbose.0.iter().enumerate() {
            for (column, horiz) in line.0.iter().enumerate() {
                let partner = match horiz {
                    Horiz::CrossUpUnder => Some(Horiz::CrossDownOver),
                    Horiz::CrossUpOver => Some(Horiz::CrossDownUnder),
                    _ => None,
                };

                if let Some(partner) = partner {
                    assert_eq!(
                        verbose.0.get(row + 1).and_then(|above| above.0.get(column)),
                        Some(&partner),
                        "{source}: crossing at row {row} column {column} is not adjacent",
                    );
                }
            }
        }
    }
}

#[test]
fn diagrams_needing_split_pairs_match_legacy_exactly() {
    // Nested diagrams cannot be drawn flat, so the new mode falls back to the
    // legacy placement rather than rendering something unfaithful.
    for source in ["(0 (1 )1 )0", "(0 (1 )2 )0", "(0 (2 \\1 /0 \\1 )2 )0"] {
        let legacy = source.parse::<AbbreviatedDiagram>().unwrap();
        let precalc = legacy.clone().with_mode(RenderMode::PrecalculatedHeights);

        assert_eq!(
            precalc.ascii_print_compact::<false>(),
            legacy.ascii_print_compact::<false>(),
            "{source}",
        );
    }
}
