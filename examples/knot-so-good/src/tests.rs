// These tests run against the host target (`cargo test`).
// The LocalStorage glue (load_from_storage/save_to_storage) wraps web_sys
// and is not tested here; it is too thin to warrant browser-based tests.
use super::{ascii_diagram_to_svg, make_svg_scalable, PersistedDisplayMode, PersistedSnapshot, PersistedState};

#[test]
fn round_trip_full() {
    let state = PersistedState {
        diagram: "(0 )0".into(),
        moves: "swap".into(),
        display_mode: PersistedDisplayMode::Ascii,
        compact: false,
        snapshots: Vec::new(),
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
    let full = trefoil.try_ascii_print::<false>().unwrap();
    let compact = trefoil.try_ascii_print_compact::<false>().unwrap();
    assert_ne!(full, compact);
}

#[test]
fn non_compact_mode_uses_full_ascii() {
    let unknot = "(0 )0".parse::<knotty::AbbreviatedDiagram>().unwrap();
    let full = unknot.try_ascii_print::<false>().unwrap();
    let also = unknot.ascii_print::<false>();
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
