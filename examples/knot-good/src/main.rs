use web_sys::Node;
use yew::{prelude::*, virtual_dom::VNode};

enum Msg {
    Diagram(Option<String>),
    Moves(Option<String>),
}

struct Model {
    encoded_diagram: String,
    moves: String,
    parsed_moves: knotty::DiagramMoves,
    diagram: Html,
}

const DIAGRAM: &str = "\
    (0\n\
    (2\n\
    /1\n\
    \\0\n\
    /1\n\
    )2\n\
    )0\n\
";

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            encoded_diagram: DIAGRAM.to_string(),
            diagram: render_knot_to_html(DIAGRAM, Default::default()),
            parsed_moves: Default::default(),
            moves: "".to_string(),
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        use Msg::*;

        match msg {
            Diagram(Some(diagram)) => {
                self.diagram = render_knot_to_html(&diagram, self.parsed_moves.clone());
                self.encoded_diagram = diagram;
                true
            }
            Moves(Some(moves)) => {
                match moves.parse::<knotty::DiagramMoves>() {
                    Ok(parsed_moves) => {
                        self.moves = moves;
                        self.parsed_moves = parsed_moves;
                    }
                    Err(_) => return false,
                }
                self.diagram =
                    render_knot_to_html(&self.encoded_diagram, self.parsed_moves.clone());
                true
            }
            Moves(None) | Diagram(None) => false,
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        use wasm_bindgen::{JsCast, UnwrapThrowExt};
        use web_sys::{EventTarget, HtmlInputElement, HtmlTextAreaElement};

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

        html! {
            <div>
                <RawHtml inner_html={render_knot_to_svg(&self.encoded_diagram, self.parsed_moves.clone()).unwrap_or_default()}></RawHtml>
                <p><pre>{ self.diagram.clone() }</pre></p>
                <textarea
                    value={self.encoded_diagram.clone()}
                    oninput={diagram_oninput}>
                </textarea>
                <textarea
                    value={self.moves.clone()}
                    oninput={moves_oninput}>
                </textarea>
            </div>
        }
    }
}

fn render_knot(diagram: &str, moves: knotty::DiagramMoves) -> Result<String, String> {
    let mut knot = diagram.parse::<knotty::AbbreviatedDiagram>()?;
    knot.try_apply_all(moves)?;

    Ok(knot.ascii_print::<false>())
}

fn render_knot_to_svg(diagram: &str, moves: knotty::DiagramMoves) -> Result<String, String> {
    Ok(svgbob::to_svg_with_settings(
        &render_knot(diagram, moves)?,
        &svgbob::Settings {
            stroke_width: 5.0,
            ..Default::default()
        },
    ))
}

fn render_knot_to_html(diagram: &str, moves: knotty::DiagramMoves) -> Html {
    // TODO return err
    let diagram = match render_knot(diagram, moves) {
        Ok(diagram) => diagram,
        Err(err) => return html! { <p>{ format!("Error: {}", err) }</p> },
    };

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
                inner_html: "hola&nbsp;adios".into(),
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
