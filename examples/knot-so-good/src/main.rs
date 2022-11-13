use knotty::DiagramMove;
use wasm_bindgen::JsValue;
use web_sys::Node;
use yew::{prelude::*, virtual_dom::VNode};

enum Msg {
    DisplayMode(DisplayMode),
    Diagram(Option<String>),
    Moves(Option<String>),
    AddMove(DiagramMove),
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
}

const UNKNOT: &str = "\
    (0\n\
    )0\n\
";

const TREFOIL: &str = "\
    (0\n\
    (2\n\
    /1\n\
    \\0\n\
    /1\n\
    )2\n\
    )0\n\
";

const SQUARE_KNOT: &str = "\
    (0\n\
    (2\n\
    \\1\n\
    (3\n\
    /2\n\
    /4\n\
    )3\n\
    \\1\n\
    )2\n\
    )0\n\
";

const BUILT_IN_KNOTS: &[(&str, &str)] = &[
    ("unknot", UNKNOT),
    ("trefoil", TREFOIL),
    ("square knot", SQUARE_KNOT),
];

impl Model {
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
    }
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            display_mode: Default::default(),
            raw_base_diagram: String::new(),
            parsed_base_diagram: Ok(Default::default()),
            modified_diagram: Ok(Default::default()),
            ascii_modified_diagram: Ok(String::new()),
            ascii_html_diagram: Default::default(),
            parsed_moves: Default::default(),
            parsed_moves_valid: true,
            raw_moves: "".to_string(),
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        use Msg::*;

        match msg {
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
            Moves(None) | Diagram(None) => false,
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        use wasm_bindgen::JsCast;
        use web_sys::{EventTarget, HtmlTextAreaElement};

        let link = ctx.link();

        // This is a mess.
        let diagram_oninput = link.callback(|e: InputEvent| {
            // web_sys::console::log_1(&e);

            let value = e
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
        let moves_oninput = link.callback(|e: InputEvent| {
            // web_sys::console::log_1(&e);

            let value = e
                .dyn_into()
                .ok()
                .and_then(|event: Event| event.target())
                .and_then(|event_target: EventTarget| -> Option<HtmlTextAreaElement> {
                    event_target.dyn_into().ok()
                })
                .map(|target| target.value());

            Msg::Moves(value)
        });

        let svg = ascii_diagram_to_svg(&self.ascii_modified_diagram.as_deref().unwrap_or(""));

        let array: JsValue = std::iter::once(JsValue::from_str(&svg.clone()))
            .collect::<js_sys::Array>()
            .into();

        let url = web_sys::Blob::new_with_str_sequence_and_options(
            &array,
            &*web_sys::BlobPropertyBag::new().type_("image/svg+xml;charset=utf-8"),
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

        let available_moves = self
            .modified_diagram
            .as_ref()
            .map(|diagram| diagram.available_moves().collect::<Vec<_>>())
            .unwrap_or_default();

        html! {
            <div>
                { BUILT_IN_KNOTS.iter().map(|(name, diagram)| html! {
                    <button onclick={link.callback(move |_| Msg::Diagram(Some(diagram.to_string())))}>{ name }</button>
                }).collect::<Html>() }
                <button onclick={link.callback(move |_| Msg::DisplayMode(other_mode))}>{format!("switch to {other_mode:?} display")}</button>
                { match self.display_mode {
                    DisplayMode::Ascii => html! {
                        <p><pre>{ self.ascii_html_diagram.clone() }</pre></p>
                    },
                    DisplayMode::Svg => html! {
                        <p><RawHtml inner_html={svg}></RawHtml></p>
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
                <textarea
                    value={self.raw_moves.clone()}
                    oninput={moves_oninput}>
                </textarea>
                {
                    if self.parsed_moves_valid {
                        html! {
                            <p>
                            <br/>
                            {
                                available_moves.into_iter().map(|moov| {
                                    html! {
                                        <button onclick={link.callback(move |_| Msg::AddMove(moov))}>{ moov }</button>
                                    }
                                }).collect::<Html>()
                            }</p>
                        }
                    } else {
                        html! {}
                    }
                }
                <br/>
                <a style="font-size: 8px;" href={url.unwrap_or_default()} download="knot.svg">{ "Download SVG" }</a>
                <br/>
            </div>
        }
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

    fn create(_ctx: &Context<Self>) -> Self {
        RawHtml {
            props: RawHtmlProps {
                inner_html: String::new(),
            },
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
        let div = web_sys::window()
            .unwrap()
            .document()
            .unwrap()
            .create_element("div")
            .unwrap();
        div.set_inner_html(&self.props.inner_html[..]);

        let node = Node::from(div);
        let vnode = VNode::VRef(node);
        vnode
    }
}

fn main() {
    yew::start_app::<Model>();
}
