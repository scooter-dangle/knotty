use knotty::DiagramMove;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{EventTarget, HtmlTextAreaElement, Node};
use yew::{prelude::*, virtual_dom::VNode};

#[derive(serde::Serialize, serde::Deserialize, Default, Clone, PartialEq, Debug)]
#[serde(rename_all = "snake_case")]
enum PersistedDisplayMode {
    #[default]
    Svg,
    Ascii,
    #[serde(other)]
    Other,
}

#[derive(serde::Serialize, serde::Deserialize, Default)]
struct PersistedState {
    #[serde(default)]
    diagram: String,
    #[serde(default)]
    moves: String,
    #[serde(default)]
    display_mode: PersistedDisplayMode,
    #[serde(default)]
    snapshots: Vec<PersistedSnapshot>,
}

impl PersistedState {
    fn from_model(model: &Model) -> Self {
        Self {
            diagram: model.raw_base_diagram.clone(),
            moves: model.raw_moves.clone(),
            display_mode: match model.display_mode {
                DisplayMode::Svg => PersistedDisplayMode::Svg,
                DisplayMode::Ascii => PersistedDisplayMode::Ascii,
            },
            snapshots: model.snapshots.clone(),
        }
    }
}

const MAX_SNAPSHOTS: usize = 9;

#[derive(serde::Serialize, serde::Deserialize, Clone)]
struct PersistedSnapshot {
    diagram: String,
    moves: String,
    display_mode: PersistedDisplayMode,
    current_diagram_encoding: String,
    svg: String,
}

const STORAGE_KEY: &str = "knotty_state";

fn get_local_storage() -> Option<web_sys::Storage> {
    web_sys::window().and_then(|window| window.local_storage().ok().flatten())
}

fn load_from_storage() -> Result<Option<PersistedState>, String> {
    let storage = get_local_storage().ok_or_else(|| "localStorage unavailable".to_string())?;
    match storage.get_item(STORAGE_KEY).ok().flatten() {
        None => Ok(None),
        Some(json_str) => serde_json::from_str(&json_str)
            .map(Some)
            .map_err(|err| err.to_string()),
    }
}

fn save_to_storage(state: &PersistedState) {
    let Some(storage) = get_local_storage() else {
        return;
    };
    if let Ok(json) = serde_json::to_string(state) {
        let _ = storage.set_item(STORAGE_KEY, &json);
    }
}

fn clear_storage() {
    let Some(storage) = get_local_storage() else {
        return;
    };
    let _ = storage.remove_item(STORAGE_KEY);
}

enum Msg {
    DisplayMode(DisplayMode),
    Diagram(Option<String>),
    Moves(Option<String>),
    AddMove(String),
    DismissStorageError,
    Snapshot,
    RestoreSnapshot(usize),
    DeleteSnapshot(usize),
}

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
enum DisplayMode {
    Ascii,
    #[default]
    Svg,
}

struct Model {
    display_mode: DisplayMode,
    raw_base_diagram: String,
    parsed_base_diagram: Result<knotty::AbbreviatedDiagram, String>,
    modified_diagram: Result<knotty::AbbreviatedDiagram, String>,
    ascii_modified_diagram: Result<String, String>,
    raw_moves: String,
    parsed_moves: knotty::DiagramMoves,
    parsed_moves_valid: bool,
    ascii_html_diagram: Html,
    svg_diagram: String,
    storage_error: Option<String>,
    snapshots: Vec<PersistedSnapshot>,
}

const UNKNOT: &str = "\
    (0 )0\
";

const TREFOIL: &str = "\
    (0 (2 /1 \\0 /1 )2 )0\
";

const SQUARE_KNOT: &str = "\
    (0 (2 \\1 (3 /2 /4 )3 \\1 )2 )0\
";

const KNOT_5_1: &str = "\
    (0 (2 /1 \\0 \\0 \\0 /1 )2 )0\
";

const BUILT_IN_KNOTS: &[(&str, &str)] = &[
    ("unknot", UNKNOT),
    ("trefoil", TREFOIL),
    ("square knot", SQUARE_KNOT),
    ("knot 5_1", KNOT_5_1),
];

impl Model {
    fn snapshot_disabled(&self) -> bool {
        self.snapshots.len() >= MAX_SNAPSHOTS
            || self.modified_diagram.is_err()
            || !self.parsed_moves_valid
    }

    fn update_modified(&mut self) {
        self.modified_diagram = self.parsed_base_diagram.clone().and_then(|mut knot| {
            knot.try_apply_all(self.parsed_moves.clone())?;
            Ok(knot)
        });

        self.ascii_modified_diagram = self
            .modified_diagram
            .clone()
            .and_then(|knot| knot.try_ascii_print::<false>());

        self.ascii_html_diagram = self
            .ascii_modified_diagram
            .as_deref()
            .map_or_else(|err| error_to_html(err), ascii_diagram_to_html);

        self.svg_diagram =
            ascii_diagram_to_svg(self.ascii_modified_diagram.as_deref().unwrap_or(""));
    }
}

fn onkeypress_add_move(scope: &html::Scope<Model>) -> Callback<KeyboardEvent> {
    scope.batch_callback(|event: KeyboardEvent| {
        if event.key() != "Enter" {
            return None;
        }

        let value = event
            .dyn_into()
            .ok()
            .and_then(|event: Event| event.target())
            .and_then(
                |event_target: EventTarget| -> Option<web_sys::HtmlInputElement> {
                    event_target.dyn_into().ok()
                },
            )
            .map(|target| target.value());

        value.map(Msg::AddMove)
    })
}

fn move_select(
    link: &html::Scope<Model>,
    label: &str,
    moves: &[DiagramMove],
    parsed_moves_valid: bool,
) -> Html {
    {
        html! {
            // We're only using 'dialog' here to get the Enter button
            // to work correctly in Chrome on Android. Without wrapping
            // each input in a form, Chrome's 'Enter' button is a Tab
            // unless you're on the last input item. But then when you
            // wrap the input in a form, Chrome refreshes the page when
            // you press Enter.
            <form action="" method="dialog">
            <input
                class="select-move"
                placeholder={format!("select a {label} move")}
                autocomplete="on"
                list={format!("{label}-moves")}
                onkeypress={onkeypress_add_move(link)}
                value=""
                disabled={!parsed_moves_valid || moves.is_empty()}
            />
            <datalist id={format!("{label}-moves")}>{ moves.iter().map(|moove| html! {
                <option value={moove.to_string()}>{ moove.to_string() }</option>
            }).collect::<Html>() }</datalist>
            </form>
        }
    }
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        let (raw_base_diagram, raw_moves, display_mode, snapshots, storage_error) =
            match load_from_storage() {
                Ok(Some(persisted)) => {
                    let mode = match persisted.display_mode {
                        PersistedDisplayMode::Ascii => DisplayMode::Ascii,
                        PersistedDisplayMode::Svg | PersistedDisplayMode::Other => DisplayMode::Svg,
                    };
                    (persisted.diagram, persisted.moves, mode, persisted.snapshots, None)
                }
                Ok(None) => (String::new(), String::new(), DisplayMode::Svg, Vec::new(), None),
                Err(err) => {
                    clear_storage();
                    web_sys::console::error_1(&format!("knotty: failed to restore state: {err}").into());
                    (String::new(), String::new(), DisplayMode::Svg, Vec::new(), Some(err))
                }
            };

        let parsed_base_diagram = raw_base_diagram.parse();
        let parsed_moves_result = raw_moves.parse::<knotty::DiagramMoves>();
        let parsed_moves_valid = parsed_moves_result.is_ok();
        let parsed_moves = parsed_moves_result.unwrap_or_default();

        let mut model = Self {
            display_mode,
            raw_base_diagram,
            parsed_base_diagram,
            modified_diagram: Ok(Default::default()),
            ascii_modified_diagram: Ok(String::new()),
            ascii_html_diagram: Default::default(),
            svg_diagram: String::new(),
            parsed_moves,
            parsed_moves_valid,
            raw_moves,
            storage_error,
            snapshots,
        };
        model.update_modified();
        model
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        use Msg::*;

        let should_render = match msg {
            DismissStorageError => {
                self.storage_error = None;
                true
            }
            DisplayMode(mode) => {
                if self.display_mode == mode {
                    false
                } else {
                    self.display_mode = mode;
                    true
                }
            }
            Diagram(Some(diagram)) => {
                if self.raw_base_diagram == diagram {
                    return false;
                }

                self.raw_base_diagram = diagram;
                let parsed_base_diagram = self.raw_base_diagram.parse();

                if self.parsed_base_diagram == parsed_base_diagram {
                    return false;
                }

                self.parsed_base_diagram = self.raw_base_diagram.parse();

                self.update_modified();
                true
            }
            Moves(Some(moves)) => {
                if self.raw_moves == moves {
                    return false;
                }

                let moves_previously_valid = self.parsed_moves_valid;
                self.raw_moves = moves.clone();

                match moves.parse::<knotty::DiagramMoves>() {
                    Ok(parsed_moves) => {
                        self.parsed_moves_valid = true;

                        if self.parsed_moves == parsed_moves {
                            // If the parsed version of the moves hasn't
                            // changed, we only need to update view if
                            // we've gone from invalid to valid.
                            return !moves_previously_valid;
                        }

                        self.parsed_moves = parsed_moves;
                    }
                    Err(_) => {
                        let moves_previously_valid = self.parsed_moves_valid;
                        self.parsed_moves_valid = false;
                        // There's only a change if we're going from valid to invalid.
                        return moves_previously_valid;
                    }
                }

                self.update_modified();
                true
            }
            AddMove(moove) => self.update(
                ctx,
                Moves(Some(format!(
                    "{}{}{moove}",
                    self.raw_moves,
                    if self.raw_moves.is_empty() || self.raw_moves.ends_with('\n') {
                        ""
                    } else {
                        "\n"
                    }
                ))),
            ),
            Snapshot => {
                if self.snapshot_disabled() {
                    return false;
                }

                let modified = self.modified_diagram.as_ref().unwrap();
                self.snapshots.push(PersistedSnapshot {
                    diagram: self.raw_base_diagram.clone(),
                    moves: self.raw_moves.clone(),
                    display_mode: match self.display_mode {
                        self::DisplayMode::Svg => PersistedDisplayMode::Svg,
                        self::DisplayMode::Ascii => PersistedDisplayMode::Ascii,
                    },
                    current_diagram_encoding: modified.to_string().replace('\n', " "),
                    svg: self.svg_diagram.clone(),
                });
                true
            }
            RestoreSnapshot(idx) => {
                let Some(snapshot) = self.snapshots.get(idx) else {
                    return false;
                };

                self.raw_base_diagram = snapshot.diagram.clone();
                self.raw_moves = snapshot.moves.clone();
                self.display_mode = match snapshot.display_mode {
                    PersistedDisplayMode::Ascii => self::DisplayMode::Ascii,
                    PersistedDisplayMode::Svg | PersistedDisplayMode::Other => self::DisplayMode::Svg,
                };
                self.parsed_base_diagram = self.raw_base_diagram.parse();
                let parsed_moves_result = self.raw_moves.parse::<knotty::DiagramMoves>();
                self.parsed_moves_valid = parsed_moves_result.is_ok();
                self.parsed_moves = parsed_moves_result.unwrap_or_default();
                self.update_modified();
                true
            }
            DeleteSnapshot(idx) => {
                if idx < self.snapshots.len() {
                    self.snapshots.remove(idx);
                    true
                } else {
                    false
                }
            }
            Moves(None) | Diagram(None) => false,
        };

        if should_render {
            save_to_storage(&PersistedState::from_model(self));
        }
        should_render
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();

        // This is a mess.
        let diagram_oninput = link.callback(|event: InputEvent| {
            let value = event
                .dyn_into()
                .ok()
                .and_then(|event: Event| event.target())
                .and_then(|event_target: EventTarget| -> Option<HtmlTextAreaElement> {
                    event_target.dyn_into().ok()
                })
                .map(|target| target.value());

            Msg::Diagram(value)
        });

        // This is a mess.
        let moves_oninput = link.callback(|event: InputEvent| {
            let value = event
                .dyn_into()
                .ok()
                .and_then(|event: Event| event.target())
                .and_then(|event_target: EventTarget| -> Option<HtmlTextAreaElement> {
                    event_target.dyn_into().ok()
                })
                .map(|target| target.value());

            Msg::Moves(value)
        });

        let svg = &self.svg_diagram;

        let array: JsValue = std::iter::once(JsValue::from_str(svg))
            .collect::<js_sys::Array>()
            .into();

        let url = web_sys::Blob::new_with_str_sequence_and_options(
            &array,
            &{
                let bag = web_sys::BlobPropertyBag::new();
                bag.set_type("image/svg+xml;charset=utf-8");
                bag
            },
        )
        .map_err(|err| web_sys::console::log_1(&err.into()))
        .and_then(|blob| {
            web_sys::Url::create_object_url_with_blob(&blob)
                .map_err(|err| web_sys::console::log_1(&err.into()))
        });

        let other_mode = match self.display_mode {
            DisplayMode::Ascii => DisplayMode::Svg,
            DisplayMode::Svg => DisplayMode::Ascii,
        };

        let Moves {
            changing,
            complecting,
            rearranging,
            simplifying,
        } = self
            .modified_diagram
            .as_ref()
            .map(|diagram| diagram.available_moves().collect::<Moves>())
            .unwrap_or_default();

        html! {
            <>
                if let Some(ref err) = self.storage_error {
                    <p>
                        { format!("Could not restore saved state (corrupt data was cleared): {err}. ") }
                        <button onclick={link.callback(|_| Msg::DismissStorageError)}>
                            { "Dismiss" }
                        </button>
                    </p>
                }
                { BUILT_IN_KNOTS.iter().map(|(name, diagram)| html! {
                    <button onclick={link.callback(move |_| Msg::Diagram(Some(diagram.to_string())))}>{ name }</button>
                }).collect::<Html>() }
                <button onclick={link.callback(move |_| Msg::DisplayMode(other_mode))}>{format!("switch to {other_mode:?} display")}</button>
                <button
                    class="snapshot"
                    disabled={self.snapshot_disabled()}
                    onclick={link.callback(|_| Msg::Snapshot)}
                >{ "snapshot" }</button>
                { match self.display_mode {
                    DisplayMode::Ascii => html! {
                        <p><pre>{ self.ascii_html_diagram.clone() }</pre></p>
                    },
                    DisplayMode::Svg => html! {
                        <p><RawHtml inner_html={svg.clone()}></RawHtml></p>
                    },
                } }
                <pre>{
                    // TODO modify diagram input to allow moves on the same line
                    self.modified_diagram.clone().unwrap_or_default().to_string().replace('\n', " ")
                }</pre>
                <br/>
                <textarea
                    value={self.raw_base_diagram.clone()}
                    oninput={diagram_oninput}>
                </textarea>
                <br/>
                <textarea
                    value={self.raw_moves.clone()}
                    oninput={moves_oninput}>
                </textarea>

                {
                    [
                        ("simplifying", simplifying),
                        ("reärranging", rearranging),
                        ("complecting", complecting),
                        ("changing", changing),
                    ].into_iter().flat_map(|(label, moves)| {
                        [html! { <br/> }, move_select(link, label, &moves, self.parsed_moves_valid)]
                    }).collect::<Html>()
                }

                <br/>
                <a style="font-size: 8px;" href={url.unwrap_or_default()} download="knot.svg">{ "Download SVG" }</a>
                <br/>

                if !self.snapshots.is_empty() {
                    <div class="snapshot-catalog">
                        { self.snapshots.iter().enumerate().map(|(idx, snapshot)| {
                            html! {
                                <div class="snapshot-entry">
                                    <div class="snapshot-preview">
                                        <RawHtml inner_html={make_svg_scalable(&snapshot.svg)} />
                                    </div>
                                    <pre>{ &snapshot.current_diagram_encoding }</pre>
                                    <button onclick={link.callback(move |_| Msg::RestoreSnapshot(idx))}>
                                        { "restore" }
                                    </button>
                                    <button onclick={link.callback(move |_| Msg::DeleteSnapshot(idx))}>
                                        { "delete" }
                                    </button>
                                </div>
                            }
                        }).collect::<Html>() }
                    </div>
                }
            </>
        }
    }
}

#[derive(Default)]
struct Moves {
    changing: Vec<DiagramMove>,
    complecting: Vec<DiagramMove>,
    rearranging: Vec<DiagramMove>,
    simplifying: Vec<DiagramMove>,
}

impl FromIterator<DiagramMove> for Moves {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = DiagramMove>,
    {
        use knotty::Move::*;

        let mut moves = Self::default();

        iter.into_iter().for_each(|moove| {
            (match moove.r#move() {
                ChangeCrossing => &mut moves.changing,

                Bulge { .. } | Reid1a { .. } | Reid1b { .. } | Reid2 { .. } => {
                    &mut moves.complecting
                }

                Swap | WrapAround | Reid3 | Rotate90Ccw => &mut moves.rearranging,

                CollapseBulge | CollapseReid1a | CollapseReid1b | CollapseReid2 => {
                    &mut moves.simplifying
                }
            })
            .push(moove)
        });

        moves
    }
}

fn error_to_html(error: &str) -> Html {
    html! { <p>{ format!("Error: {error}") }</p> }
}

fn ascii_diagram_to_html(diagram: &str) -> Html {
    diagram
        .bytes()
        .map(|byte| match byte {
            byte
            @ (b' ' | b'(' | b')' | b'/' | b'\\' | b'_' | b'-' | b'+' | b'|' | b'0'..=b'9') => {
                html! { {byte as char} }
            }
            b'\n' => html! { <br/> },
            _ => unreachable!("bug!"),
        })
        .collect()
}

fn ascii_diagram_to_svg(diagram: &str) -> String {
    svgbob::to_svg_with_settings(
        diagram,
        &svgbob::Settings {
            stroke_width: 5.0,
            ..Default::default()
        },
    )
}

fn make_svg_scalable(svg: &str) -> String {
    try_make_svg_scalable(svg).unwrap_or_else(|| svg.to_string())
}

fn try_make_svg_scalable(svg: &str) -> Option<String> {
    let tag_end = svg.find('>')?;
    let svg_tag = &svg[..tag_end];

    let width = svg_attr_value(svg_tag, "width")?;
    let height = svg_attr_value(svg_tag, "height")?;
    let width_num = width.trim_end_matches("px");
    let height_num = height.trim_end_matches("px");

    let new_tag = format!(
        "{} viewBox=\"0 0 {width_num} {height_num}\"",
        svg_tag
            .replacen(&format!(" width=\"{width}\""), "", 1)
            .replacen(&format!(" height=\"{height}\""), "", 1),
    );
    Some(format!("{new_tag}{}", &svg[tag_end..]))
}

fn svg_attr_value<'a>(tag: &'a str, attr: &str) -> Option<&'a str> {
    let needle = format!("{attr}=\"");
    let start = tag.find(needle.as_str())? + needle.len();
    let end = tag[start..].find('"')? + start;
    Some(&tag[start..end])
}

#[derive(Debug, Default, Clone, Eq, PartialEq, Properties)]
struct RawHtmlProps {
    pub inner_html: String,
}

#[derive(Default)]
struct RawHtml {
    props: RawHtmlProps,
}

impl Component for RawHtml {
    type Message = Msg;
    type Properties = RawHtmlProps;

    fn create(ctx: &Context<Self>) -> Self {
        RawHtml {
            props: ctx.props().clone(),
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, _: Self::Message) -> bool {
        true
    }

    fn changed(&mut self, ctx: &Context<Self>) -> bool {
        if self.props != *ctx.props() {
            self.props = (&*ctx.props()).clone();
            true
        } else {
            false
        }
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        let span = web_sys::window()
            .unwrap()
            .document()
            .unwrap()
            .create_element("span")
            .unwrap();
        span.set_inner_html(&self.props.inner_html[..]);

        let node = Node::from(span);
        let vnode = VNode::VRef(node);
        vnode
    }
}

fn main() {
    yew::start_app::<Model>();
}

#[cfg(test)]
mod tests {
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
            snapshots: vec![
                PersistedSnapshot {
                    diagram: "(0 (2 /1 \\0 /1 )2 )0".into(),
                    moves: "swap@0".into(),
                    display_mode: PersistedDisplayMode::Ascii,
                    current_diagram_encoding: "(0 (2 /1 \\0 /1 )2 )0".into(),
                    svg: "<svg>trefoil</svg>".into(),
                },
                PersistedSnapshot {
                    diagram: "(0 )0".into(),
                    moves: "".into(),
                    display_mode: PersistedDisplayMode::Svg,
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
}
