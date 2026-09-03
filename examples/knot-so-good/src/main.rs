use knotty::DiagramMove;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{EventTarget, HtmlTextAreaElement};
use yew::prelude::*;

#[derive(serde::Serialize, serde::Deserialize, Default, Clone, PartialEq, Debug)]
#[serde(rename_all = "snake_case")]
enum PersistedDisplayMode {
    #[default]
    Svg,
    Ascii,
    #[serde(other)]
    Other,
}

#[derive(serde::Serialize, serde::Deserialize, Default, Clone, PartialEq, Debug)]
#[serde(rename_all = "snake_case")]
enum PersistedMode {
    #[default]
    Notation,
    Manual,
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
    compact: bool,
    #[serde(default)]
    snapshots: Vec<PersistedSnapshot>,
    #[serde(default)]
    mode: PersistedMode,
    #[serde(default)]
    manual_diagram: String,
    #[serde(default)]
    manual_snapshots: Vec<PersistedManualSnapshot>,
    #[serde(default)]
    manual_borders: bool,
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
            compact: model.compact,
            snapshots: model.snapshots.clone(),
            mode: match model.mode {
                Mode::Notation => PersistedMode::Notation,
                Mode::Manual => PersistedMode::Manual,
            },
            manual_diagram: model.manual_diagram.clone(),
            manual_snapshots: model.manual_snapshots.clone(),
            manual_borders: model.manual_borders,
        }
    }
}

const MAX_SNAPSHOTS: usize = 9;

#[derive(serde::Serialize, serde::Deserialize, Clone)]
struct PersistedSnapshot {
    diagram: String,
    moves: String,
    display_mode: PersistedDisplayMode,
    #[serde(default)]
    compact: bool,
    current_diagram_encoding: String,
    svg: String,
}

#[derive(serde::Serialize, serde::Deserialize, Clone)]
struct PersistedManualSnapshot {
    diagram: String,
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
    SetMode(Mode),
    ManualDiagram(Option<String>),
    ManualBorders(bool),
    ManualSnapshot,
    RestoreManualSnapshot(usize),
    DeleteManualSnapshot(usize),
    DisplayMode(DisplayMode),
    Compact(bool),
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

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
enum Mode {
    #[default]
    Notation,
    Manual,
}

struct Model {
    mode: Mode,
    display_mode: DisplayMode,
    compact: bool,
    manual_borders: bool,
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
    manual_diagram: String,
    manual_error: Option<String>,
    manual_render: Option<knotty::VerboseDiagram>,
    manual_snapshots: Vec<PersistedManualSnapshot>,
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

// Characters come from the library, so only the labels live here.
const SYMBOL_TABLE: [(knotty::Horiz, &str); 8] = {
    use knotty::Horiz::*;

    [
        (Empty, "empty"),
        (Line, "line"),
        (CrossDownOver, "cross down, over"),
        (CrossDownUnder, "cross down, under"),
        (OpenedBelow, "opened"),
        (ClosedBelow, "closed"),
        (TransferUp, "transfer up"),
        (TransferDown, "transfer down"),
    ]
};

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

    fn manual_snapshot_disabled(&self) -> bool {
        self.manual_snapshots.len() >= MAX_SNAPSHOTS || self.manual_error.is_some()
    }

    fn compact_text(&self) -> Option<String> {
        let knot = self.modified_diagram.as_ref().ok()?;
        Some(
            knotty::VerboseDiagram::from_abbreviated(knot)
                .ok()?
                .to_text(),
        )
    }

    fn storage_error_html(&self, link: &html::Scope<Self>) -> Html {
        let Some(err) = self.storage_error.as_ref() else {
            return Html::default();
        };

        html! {
            <aside class="notice" role="alert">
                <p>{ format!("Could not restore saved state (corrupt data was cleared): {err}.") }</p>
                <button onclick={link.callback(|_| Msg::DismissStorageError)}>
                    { "Dismiss" }
                </button>
            </aside>
        }
    }

    fn mode_toggle(&self, link: &html::Scope<Self>) -> Html {
        segmented_pair(
            "mode",
            [
                (
                    "notation",
                    self.mode == Mode::Notation,
                    link.callback(|_| Msg::SetMode(Mode::Notation)),
                ),
                (
                    "manual",
                    self.mode == Mode::Manual,
                    link.callback(|_| Msg::SetMode(Mode::Manual)),
                ),
            ],
        )
    }

    fn manual_view(&self, link: &html::Scope<Self>) -> Html {
        // This is a mess, in the same way the notation inputs are.
        let manual_oninput = link.callback(|event: InputEvent| {
            let value = event
                .dyn_into()
                .ok()
                .and_then(|event: Event| event.target())
                .and_then(|event_target: EventTarget| -> Option<HtmlTextAreaElement> {
                    event_target.dyn_into().ok()
                })
                .map(|target| target.value());

            Msg::ManualDiagram(value)
        });

        let render_class = if self.manual_error.is_some() {
            "ascii manual-render stale"
        } else {
            "ascii manual-render"
        };

        html! {
            <>
                { self.storage_error_html(link) }
                <nav class="toolbar">
                    <div class="group group-mode">
                        { self.mode_toggle(link) }
                    </div>
                    <div class="group group-view">
                        { segmented_pair("view", [
                            ("plain", !self.manual_borders, link.callback(|_| Msg::ManualBorders(false))),
                            ("bordered", self.manual_borders, link.callback(|_| Msg::ManualBorders(true))),
                        ]) }
                    </div>
                    <div class="group group-actions">
                        <button
                            class="snapshot"
                            disabled={self.manual_snapshot_disabled()}
                            onclick={link.callback(|_| Msg::ManualSnapshot)}
                        >{ "snapshot" }</button>
                    </div>
                </nav>
                <div class="workspace">
                { diagram_region(
                    self.manual_render.as_ref().map(|diagram| html! {
                        <pre class={render_class}>{ ascii_diagram_to_html(&render_manual(diagram, self.manual_borders)) }</pre>
                    }).unwrap_or_default(),
                    self.manual_error.as_ref().map(|err| format!("Error: {err}")),
                ) }
                <label for="diagram-text">{ "diagram text" }</label>
                <textarea
                    id="diagram-text"
                    class="manual-input"
                    rows="10"
                    cols="40"
                    value={self.manual_diagram.clone()}
                    oninput={manual_oninput}
                />
                <details class="symbol-table">
                    <summary>{ "character reference" }</summary>
                    <table>
                        { SYMBOL_TABLE.iter().map(|(horiz, name)| html! {
                            <tr>
                                <td><code>{ horiz.as_byte() as char }</code></td>
                                <td>{ *name }</td>
                            </tr>
                        }).collect::<Html>() }
                    </table>
                </details>
                if !self.manual_snapshots.is_empty() {
                    <div class="snapshot-catalog">
                        { self.manual_snapshots.iter().enumerate().map(|(idx, snapshot)| {
                            // A snapshot saved before a character stopped
                            // naming a cell still lists and still restores;
                            // say so rather than showing an empty picture.
                            let preview = snapshot
                                .diagram
                                .parse::<knotty::VerboseDiagram>()
                                .map(|diagram| render_manual(&diagram, self.manual_borders));

                            html! {
                                <div class="snapshot-entry">
                                    if let Ok(ref preview) = preview {
                                        <pre class="ascii manual-render">{ ascii_diagram_to_html(preview) }</pre>
                                    } else {
                                        <p class="manual-error">{ "unreadable snapshot" }</p>
                                    }
                                    <button onclick={link.callback(move |_| Msg::RestoreManualSnapshot(idx))}>
                                        { "restore" }
                                    </button>
                                    <button onclick={link.callback(move |_| Msg::DeleteManualSnapshot(idx))}>
                                        { "delete" }
                                    </button>
                                </div>
                            }
                        }).collect::<Html>() }
                    </div>
                }
                </div>
            </>
        }
    }

    fn update_manual(&mut self) {
        match self.manual_diagram.parse::<knotty::VerboseDiagram>() {
            Ok(diagram) => {
                self.manual_error = None;

                // An empty diagram draws nothing, so there is no picture
                // to keep once the text goes bad.
                let has_picture = diagram.display::<false>().next().is_some();
                self.manual_render = has_picture.then_some(diagram);
            }
            // Keep the last valid render so a mistyped character does
            // not blank the picture mid-edit.
            Err(err) => self.manual_error = Some(err),
        }
    }

    fn update_modified(&mut self) {
        self.modified_diagram = self.parsed_base_diagram.clone().and_then(|mut knot| {
            knot.try_apply_all(self.parsed_moves.clone())?;
            Ok(knot)
        });

        self.ascii_modified_diagram = self.modified_diagram.clone().and_then(|knot| {
            if self.compact {
                knot.try_ascii_print_compact::<false>()
            } else {
                knot.try_ascii_print::<false>()
            }
        });

        self.ascii_html_diagram = self
            .ascii_modified_diagram
            .as_deref()
            .map(ascii_diagram_to_html)
            .unwrap_or_default();

        self.svg_diagram =
            ascii_diagram_to_svg(self.ascii_modified_diagram.as_deref().unwrap_or(""));
    }
}

/// Where the diagram is drawn. It exists in every state -- empty, drawn, or
/// erroneous -- at a minimum height, with a message line that is always
/// there, so the inputs below never move when the text goes bad.
fn diagram_region(canvas: Html, message: Option<String>) -> Html {
    html! {
        <section class="diagram">
            <div class="canvas">{ canvas }</div>
            <p class="message" role="status">{ message.unwrap_or_default() }</p>
        </section>
    }
}

/// One control for a two-state setting: both states named, the active one
/// checked. Native radios give keyboard and screen-reader handling for free.
fn segmented_pair(name: &str, options: [(&str, bool, Callback<Event>); 2]) -> Html {
    html! {
        <fieldset class="segmented" role="radiogroup" aria-label={name.to_string()}>
            { options.into_iter().map(|(label, checked, onchange)| {
                let id = format!("{name}-{label}");
                html! {
                    <>
                        <input
                            type="radio"
                            id={id.clone()}
                            name={name.to_string()}
                            checked={checked}
                            {onchange}
                        />
                        <label for={id}>{ label }</label>
                    </>
                }
            }).collect::<Html>() }
        </fieldset>
    }
}

fn render_manual(diagram: &knotty::VerboseDiagram, borders: bool) -> String {
    if borders {
        diagram.display::<true>().collect()
    } else {
        diagram.display::<false>().collect()
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
        let (persisted, storage_error) = match load_from_storage() {
            Ok(Some(persisted)) => (persisted, None),
            Ok(None) => (PersistedState::default(), None),
            Err(err) => {
                clear_storage();
                web_sys::console::error_1(
                    &format!("knotty: failed to restore state: {err}").into(),
                );
                (PersistedState::default(), Some(err))
            }
        };

        let PersistedState {
            diagram: raw_base_diagram,
            moves: raw_moves,
            display_mode,
            compact,
            snapshots,
            mode,
            manual_diagram,
            manual_snapshots,
            manual_borders,
        } = persisted;

        let display_mode = match display_mode {
            PersistedDisplayMode::Ascii => DisplayMode::Ascii,
            PersistedDisplayMode::Svg | PersistedDisplayMode::Other => DisplayMode::Svg,
        };

        let mode = match mode {
            PersistedMode::Manual => Mode::Manual,
            PersistedMode::Notation | PersistedMode::Other => Mode::Notation,
        };

        let parsed_base_diagram = raw_base_diagram.parse();
        let parsed_moves_result = raw_moves.parse::<knotty::DiagramMoves>();
        let parsed_moves_valid = parsed_moves_result.is_ok();
        let parsed_moves = parsed_moves_result.unwrap_or_default();

        let mut model = Self {
            mode,
            display_mode,
            compact,
            manual_borders,
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
            manual_diagram,
            manual_error: None,
            manual_render: None,
            manual_snapshots,
        };
        model.update_modified();
        model.update_manual();
        model
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        use Msg::*;

        let should_render = match msg {
            SetMode(mode) => {
                if self.mode == mode {
                    return false;
                }

                // Seed only into an empty box, so entered text is never
                // overwritten on a later switch.
                if mode == Mode::Manual && self.manual_diagram.is_empty() {
                    if let Some(text) = self.compact_text() {
                        self.manual_diagram = text;
                        self.update_manual();
                    }
                }

                self.mode = mode;
                true
            }
            ManualDiagram(Some(diagram)) => {
                if self.manual_diagram == diagram {
                    return false;
                }

                self.manual_diagram = diagram;
                self.update_manual();
                true
            }
            ManualBorders(borders) => {
                if self.manual_borders == borders {
                    false
                } else {
                    self.manual_borders = borders;
                    true
                }
            }
            ManualSnapshot => {
                if self.manual_snapshot_disabled() {
                    return false;
                }

                self.manual_snapshots.push(PersistedManualSnapshot {
                    diagram: self.manual_diagram.clone(),
                });
                true
            }
            RestoreManualSnapshot(idx) => {
                let Some(snapshot) = self.manual_snapshots.get(idx) else {
                    return false;
                };

                self.manual_diagram = snapshot.diagram.clone();
                self.update_manual();
                true
            }
            DeleteManualSnapshot(idx) => {
                if idx < self.manual_snapshots.len() {
                    self.manual_snapshots.remove(idx);
                    true
                } else {
                    false
                }
            }
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
            Compact(compact) => {
                if self.compact == compact {
                    false
                } else {
                    self.compact = compact;
                    self.update_modified();
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
            AddMove(moove) => <Self as Component>::update(
                self,
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
                    compact: self.compact,
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
                    PersistedDisplayMode::Svg | PersistedDisplayMode::Other => {
                        self::DisplayMode::Svg
                    }
                };
                self.compact = snapshot.compact;
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
            Moves(None) | Diagram(None) | ManualDiagram(None) => false,
        };

        if should_render {
            save_to_storage(&PersistedState::from_model(self));
        }
        should_render
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();

        if self.mode == Mode::Manual {
            return self.manual_view(link);
        }

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

        let url = web_sys::Blob::new_with_str_sequence_and_options(&array, &{
            let bag = web_sys::BlobPropertyBag::new();
            bag.set_type("image/svg+xml;charset=utf-8");
            bag
        })
        .map_err(|err| web_sys::console::log_1(&err.into()))
        .and_then(|blob| {
            web_sys::Url::create_object_url_with_blob(&blob)
                .map_err(|err| web_sys::console::log_1(&err.into()))
        });

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
                { self.storage_error_html(link) }
                <nav class="toolbar">
                    <div class="group group-mode">
                        { self.mode_toggle(link) }
                    </div>
                    <div class="group group-presets">
                        { BUILT_IN_KNOTS.iter().map(|(name, diagram)| html! {
                            <button onclick={link.callback(move |_| Msg::Diagram(Some(diagram.to_string())))}>{ name }</button>
                        }).collect::<Html>() }
                    </div>
                    <div class="group group-display">
                        { segmented_pair("display", [
                            ("picture", self.display_mode == DisplayMode::Svg, link.callback(|_| Msg::DisplayMode(DisplayMode::Svg))),
                            ("characters", self.display_mode == DisplayMode::Ascii, link.callback(|_| Msg::DisplayMode(DisplayMode::Ascii))),
                        ]) }
                        { segmented_pair("drawing", [
                            ("full", !self.compact, link.callback(|_| Msg::Compact(false))),
                            ("compact", self.compact, link.callback(|_| Msg::Compact(true))),
                        ]) }
                    </div>
                    <div class="group group-actions">
                        <button
                            class="snapshot"
                            disabled={self.snapshot_disabled()}
                            onclick={link.callback(|_| Msg::Snapshot)}
                        >{ "snapshot" }</button>
                    </div>
                </nav>
                <div class="workspace">
                { diagram_region(
                    match (&self.ascii_modified_diagram, self.display_mode) {
                        (Err(_), _) => Html::default(),
                        (Ok(_), DisplayMode::Ascii) => html! {
                            <pre class="ascii">{ self.ascii_html_diagram.clone() }</pre>
                        },
                        (Ok(_), DisplayMode::Svg) => html! {
                            <div class="picture"><RawHtml inner_html={svg.clone()} /></div>
                        },
                    },
                    self.ascii_modified_diagram.as_ref().err().map(|err| format!("Error: {err}")),
                ) }
                <pre class="encoding">{
                    // TODO modify diagram input to allow moves on the same line
                    self.modified_diagram.clone().unwrap_or_default().to_string().replace('\n', " ")
                }</pre>
                <details class="compact-text">
                    <summary>{ "diagram text" }</summary>
                    <pre>{ self.compact_text().unwrap_or_default() }</pre>
                </details>
                <div class="inputs">
                    <label for="knot-notation">{ "knot notation" }</label>
                    <textarea
                        id="knot-notation"
                        value={self.raw_base_diagram.clone()}
                        oninput={diagram_oninput}
                    />
                    <label for="moves">{ "moves" }</label>
                    <textarea
                        id="moves"
                        value={self.raw_moves.clone()}
                        oninput={moves_oninput}
                    />
                </div>

                <div class="moves">
                    {
                        [
                            ("simplifying", simplifying),
                            ("reärranging", rearranging),
                            ("complecting", complecting),
                            ("changing", changing),
                        ].into_iter().map(|(label, moves)| {
                            move_select(link, label, &moves, self.parsed_moves_valid)
                        }).collect::<Html>()
                    }
                    <button
                        disabled={!self.parsed_moves_valid}
                        onclick={link.callback(|_| Msg::AddMove("rotate_90_counter_clockwise@0".to_string()))}
                    >{ "rotate 90° CCW" }</button>
                    <a style="font-size: 8px;" href={url.unwrap_or_default()} download="knot.svg">{ "Download SVG" }</a>
                </div>

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
                </div>
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

                Swap | WrapAround | Reid3 | Rotate90CounterClockwise => &mut moves.rearranging,

                CollapseBulge | CollapseReid1a | CollapseReid1b | CollapseReid2 => {
                    &mut moves.simplifying
                }
            })
            .push(moove)
        });

        moves
    }
}

/// The bordered view rules its cell grid with `+`, `-` and `|`, and no
/// monospace font draws those edge to edge, so the stylesheet draws them as
/// full-cell rules behind the (transparent) glyph. The diagram's own
/// characters are left alone.
fn grid_class(byte: u8) -> Option<&'static str> {
    match byte {
        b'+' => Some("grid grid-cross"),
        b'-' => Some("grid grid-h"),
        b'|' => Some("grid grid-v"),
        _ => None,
    }
}

fn ascii_diagram_to_html(diagram: &str) -> Html {
    diagram
        .bytes()
        .map(|byte| match byte {
            byte
            @ (b' ' | b'(' | b')' | b'/' | b'\\' | b'_' | b'-' | b'+' | b'|' | b'0'..=b'9') => {
                match grid_class(byte) {
                    Some(class) => html! { <span class={class}>{ byte as char }</span> },
                    None => html! { {byte as char} },
                }
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

struct RawHtml;

impl Component for RawHtml {
    type Message = ();
    type Properties = RawHtmlProps;

    fn create(_ctx: &Context<Self>) -> Self {
        RawHtml
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        Html::from_html_unchecked(ctx.props().inner_html.clone().into())
    }
}

fn main() {
    yew::Renderer::<Model>::new().render();
}

#[cfg(test)]
mod tests;
