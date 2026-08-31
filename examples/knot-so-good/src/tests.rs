// These tests run against the host target (`cargo test`).
// The LocalStorage glue (load_from_storage/save_to_storage) wraps web_sys
// and is not tested here; it is too thin to warrant browser-based tests.
use super::{
    ascii_diagram_to_svg, make_svg_scalable, PersistedDisplayMode, PersistedManualSnapshot,
    PersistedMode, PersistedSnapshot, PersistedState, SYMBOL_TABLE,
};

#[test]
fn round_trip_full() {
    let state = PersistedState {
        diagram: "(0 )0".into(),
        moves: "swap".into(),
        display_mode: PersistedDisplayMode::Ascii,
        compact: false,
        snapshots: Vec::new(),
        ..Default::default()
    };
    let json = serde_json::to_string(&state).unwrap();
    let restored: PersistedState = serde_json::from_str(&json).unwrap();
    assert_eq!(restored.diagram, "(0 )0");
    assert_eq!(restored.moves, "swap");
    assert_eq!(restored.display_mode, PersistedDisplayMode::Ascii);
    assert!(restored.snapshots.is_empty());
}

#[test]
fn round_trip_empty() {
    let json = serde_json::to_string(&PersistedState::default()).unwrap();
    let restored: PersistedState = serde_json::from_str(&json).unwrap();
    assert_eq!(restored.diagram, "");
    assert_eq!(restored.moves, "");
    assert_eq!(restored.display_mode, PersistedDisplayMode::Svg);
}

#[test]
fn missing_fields_use_defaults() {
    // Older version wrote state without display_mode
    let restored: PersistedState =
        serde_json::from_str(r#"{"diagram":"(0 )0","moves":""}"#).unwrap();
    assert_eq!(restored.diagram, "(0 )0");
    assert_eq!(restored.display_mode, PersistedDisplayMode::Svg);
}

#[test]
fn unknown_fields_are_ignored() {
    // Newer version wrote a field this version doesn't know about
    let restored: PersistedState = serde_json::from_str(
        r#"{"diagram":"","moves":"","display_mode":"svg","future_field":42}"#,
    )
    .unwrap();
    assert_eq!(restored.display_mode, PersistedDisplayMode::Svg);
}

#[test]
fn invalid_json_triggers_error_path() {
    assert!(serde_json::from_str::<PersistedState>("not json").is_err());
}

#[test]
fn display_mode_unknown_string_deserializes_to_other() {
    // Unknown display_mode values deserialize to Other for forward compatibility.
    // In create(), Other falls back to DisplayMode::Svg.
    for mode_str in ["", "garbage", "SVG", "ASCII"] {
        let json = format!(r#"{{"diagram":"","moves":"","display_mode":{mode_str:?}}}"#);
        let restored: PersistedState = serde_json::from_str(&json).unwrap();
        assert_eq!(
            restored.display_mode,
            PersistedDisplayMode::Other,
            "expected Other for {mode_str:?}",
        );
    }
}

#[test]
fn round_trip_with_snapshots() {
    let state = PersistedState {
        diagram: "(0 )0".into(),
        moves: "".into(),
        display_mode: PersistedDisplayMode::Svg,
        compact: false,
        snapshots: vec![
            PersistedSnapshot {
                diagram: "(0 (2 /1 \\0 /1 )2 )0".into(),
                moves: "swap@0".into(),
                display_mode: PersistedDisplayMode::Ascii,
                compact: false,
                current_diagram_encoding: "(0 (2 /1 \\0 /1 )2 )0".into(),
                svg: "<svg>trefoil</svg>".into(),
            },
            PersistedSnapshot {
                diagram: "(0 )0".into(),
                moves: "".into(),
                display_mode: PersistedDisplayMode::Svg,
                compact: false,
                current_diagram_encoding: "(0 )0".into(),
                svg: "<svg>unknot</svg>".into(),
            },
        ],
        ..Default::default()
    };
    let json = serde_json::to_string(&state).unwrap();
    let restored: PersistedState = serde_json::from_str(&json).unwrap();
    assert_eq!(restored.snapshots.len(), 2);
    assert_eq!(restored.snapshots[0].diagram, "(0 (2 /1 \\0 /1 )2 )0");
    assert_eq!(restored.snapshots[0].moves, "swap@0");
    assert_eq!(restored.snapshots[0].display_mode, PersistedDisplayMode::Ascii);
    assert_eq!(restored.snapshots[0].svg, "<svg>trefoil</svg>");
    assert_eq!(restored.snapshots[1].diagram, "(0 )0");
}

#[test]
fn make_svg_scalable_adds_viewbox_and_removes_dimensions() {
    let svg = ascii_diagram_to_svg("(0 )0");
    assert!(svg.contains("width="), "svgbob should produce width attr");
    assert!(svg.contains("height="), "svgbob should produce height attr");

    let scalable = make_svg_scalable(&svg);
    assert!(scalable.contains("viewBox="), "should add viewBox");
    let tag_end = scalable.find('>').unwrap();
    let tag = &scalable[..tag_end];
    assert!(!tag.contains("width="), "opening tag should not contain width");
    assert!(!tag.contains("height="), "opening tag should not contain height");
}

#[test]
fn missing_snapshots_field_defaults_to_empty() {
    // Older persisted state without snapshots field
    let restored: PersistedState = serde_json::from_str(
        r#"{"diagram":"(0 )0","moves":"","display_mode":"svg"}"#,
    )
    .unwrap();
    assert!(restored.snapshots.is_empty());
}

#[test]
fn compact_persists_true() {
    let state = PersistedState {
        diagram: "(0 )0".into(),
        moves: "".into(),
        display_mode: PersistedDisplayMode::Svg,
        compact: true,
        snapshots: Vec::new(),
        ..Default::default()
    };
    let json = serde_json::to_string(&state).unwrap();
    let restored: PersistedState = serde_json::from_str(&json).unwrap();
    assert!(restored.compact);
}

#[test]
fn compact_defaults_to_false_when_missing() {
    let restored: PersistedState =
        serde_json::from_str(r#"{"diagram":"","moves":"","display_mode":"svg"}"#).unwrap();
    assert!(!restored.compact);
}

#[test]
fn compact_and_non_compact_differ_for_trefoil() {
    let trefoil = "(0 (2 /1 \\0 /1 )2 )0"
        .parse::<knotty::AbbreviatedDiagram>()
        .unwrap();
    let full = trefoil.try_ascii_print::<false>(knotty::RenderMode::Standard).unwrap();
    let compact = trefoil.try_ascii_print_compact::<false>(knotty::RenderMode::Standard).unwrap();
    assert_ne!(full, compact);
}

#[test]
fn non_compact_mode_uses_full_ascii() {
    let unknot = "(0 )0".parse::<knotty::AbbreviatedDiagram>().unwrap();
    let full = unknot.try_ascii_print::<false>(knotty::RenderMode::Standard).unwrap();
    let also = unknot.ascii_print::<false>(knotty::RenderMode::Standard);
    assert_eq!(full, also);
}

#[test]
fn unknown_fields_in_snapshots_are_ignored() {
    let json = r#"{"diagram":"","moves":"","snapshots":[{
        "diagram":"(0 )0","moves":"","display_mode":"svg",
        "current_diagram_encoding":"(0 )0","svg":"<svg/>",
        "future_snapshot_field":"hello"
    }]}"#;
    let restored: PersistedState = serde_json::from_str(json).unwrap();
    assert_eq!(restored.snapshots.len(), 1);
    assert_eq!(restored.snapshots[0].diagram, "(0 )0");
}

#[test]
fn round_trip_carries_manual_state() {
    let state = PersistedState {
        diagram: "(0 )0".into(),
        moves: String::new(),
        display_mode: PersistedDisplayMode::Ascii,
        compact: false,
        snapshots: Vec::new(),
        mode: PersistedMode::Manual,
        manual_diagram: "()\n.,\n".into(),
        manual_snapshots: vec![PersistedManualSnapshot {
            diagram: "()\n.,\n".into(),
        }],
        manual_borders: false,
    };
    let json = serde_json::to_string(&state).unwrap();
    let restored: PersistedState = serde_json::from_str(&json).unwrap();

    assert_eq!(restored.mode, PersistedMode::Manual);
    assert_eq!(restored.manual_diagram, "()\n.,\n");
    assert_eq!(restored.manual_snapshots.len(), 1);
    assert_eq!(restored.manual_snapshots[0].diagram, "()\n.,\n");
    // The notation side is untouched by the manual side.
    assert_eq!(restored.diagram, "(0 )0");
}

#[test]
fn state_saved_before_manual_mode_still_loads() {
    // Exactly what an older build wrote: no mode, no manual fields.
    let restored: PersistedState =
        serde_json::from_str(r#"{"diagram":"(0 )0","moves":"","display_mode":"ascii"}"#).unwrap();

    assert_eq!(restored.mode, PersistedMode::Notation);
    assert_eq!(restored.manual_diagram, "");
    assert!(restored.manual_snapshots.is_empty());
    assert!(!restored.manual_borders);
    assert_eq!(restored.diagram, "(0 )0");
}

#[test]
fn manual_borders_round_trips_and_defaults_off() {
    let state = PersistedState {
        manual_borders: true,
        ..Default::default()
    };
    let json = serde_json::to_string(&state).unwrap();
    let restored: PersistedState = serde_json::from_str(&json).unwrap();

    assert!(restored.manual_borders);
}

#[test]
fn unknown_mode_string_deserializes_to_other() {
    // Unknown mode values deserialize to Other for forward compatibility.
    // In create(), Other falls back to Mode::Notation.
    for mode_str in ["", "garbage", "MANUAL", "notation "] {
        let json = format!(r#"{{"diagram":"","moves":"","mode":"{mode_str}"}}"#);
        let restored: PersistedState = serde_json::from_str(&json).unwrap();
        assert_eq!(restored.mode, PersistedMode::Other, "mode {mode_str:?}");
    }
}

#[test]
fn manual_snapshots_round_trip_independently_of_notation_snapshots() {
    let json = r#"{"diagram":"","moves":"","manual_snapshots":[{"diagram":"()\n.,\n"}]}"#;
    let restored: PersistedState = serde_json::from_str(json).unwrap();

    assert_eq!(restored.manual_snapshots.len(), 1);
    assert!(restored.snapshots.is_empty());
}

#[test]
fn symbol_table_characters_match_the_library() {
    // The labels live here; the characters must come from knotty.
    let mut bytes: Vec<u8> = SYMBOL_TABLE
        .iter()
        .map(|(horiz, _)| horiz.as_byte())
        .collect();

    assert_eq!(bytes.len(), 16);
    bytes.sort_unstable();
    bytes.dedup();
    assert_eq!(bytes.len(), 16, "symbol table has duplicate characters");
}

#[test]
fn bordered_render_draws_one_box_per_character() {
    // One box per typed character, one row of boxes per line of text.
    for text in ["(\n", "()\n.,\n", "_(---)_\n_./-/,_\n(-A\\A-)\n.--a--,\n"] {
        let diagram = text.parse::<knotty::VerboseDiagram>().unwrap();
        let plain: String = diagram.display::<false>(knotty::RenderMode::Standard).collect();
        let bordered: String = diagram.display::<true>(knotty::RenderMode::Standard).collect();

        let rows = text.lines().count();
        let width = text.lines().map(str::len).max().unwrap();

        assert_eq!(plain.lines().count(), 3 * rows - 2, "{text:?}");
        assert_eq!(bordered.lines().count(), 4 * rows - 2, "{text:?}");

        let borders = bordered.lines().filter(|line| line.starts_with('+'));
        assert_eq!(borders.clone().count(), rows, "{text:?}");

        for border in borders {
            assert_eq!(border.matches("+---").count(), width, "{text:?}");
        }
    }
}

#[test]
fn both_views_are_empty_for_the_same_diagrams() {
    // The app asks "is there a picture?" once, without knowing which view
    // is selected, so the two must agree.
    for text in ["", "\n", "()\n.,\n"] {
        let diagram = text.parse::<knotty::VerboseDiagram>().unwrap();

        assert_eq!(
            diagram.display::<false>(knotty::RenderMode::Standard).next().is_some(),
            diagram.display::<true>(knotty::RenderMode::Standard).next().is_some(),
            "{text:?}",
        );
    }
}

#[test]
fn manual_diagram_text_renders_without_notation() {
    let diagram = "()\n.,\n".parse::<knotty::VerboseDiagram>().unwrap();
    let rendered: String = diagram.display::<false>(knotty::RenderMode::Standard).collect();

    assert_eq!(
        rendered,
        "(0 )0"
            .parse::<knotty::AbbreviatedDiagram>()
            .unwrap()
            .ascii_print::<false>(knotty::RenderMode::Standard),
    );
}
