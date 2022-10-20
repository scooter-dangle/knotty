// struct Diagram(

#[derive(Debug, Clone, Copy)]
pub enum Horiz {
    Empty,
    Line,
    CrossDownOver,
    CrossDownUnder,
    CrossUpOver,
    CrossUpUnder,
    OpenedBelow,
    OpenedAbove,
    ClosedBelow,
    ClosedAbove,
    TransferUpStart,
    TransferUpFinish,
    TransferDownStart,
    TransferDownFinish,
}

pub struct VerboseLine(Vec<Horiz>);

pub struct VerboseDiagram(Vec<VerboseLine>);

impl Horiz {
    #[rustfmt::skip]
    pub const fn display(&self) -> [&'static str; 3] {
        use Horiz::*;

        match self {
            Empty => [
                r#"    "#,
                r#"    "#,
                r#"    "#,
            ],
            Line => [
                r#"____"#,
                r#"    "#,
                r#"    "#,
            ],
            CrossDownOver => [
                r#"_   "#,
                r#" \ /"#,
                r#"  \ "#,
            ],
            CrossDownUnder => [
                r#"_   "#,
                r#" \ /"#,
                r#"  / "#,
            ],
            CrossUpOver | CrossUpUnder => [
                r#"_/ \"#,
                r#"    "#,
                r#"    "#,
            ],
            OpenedBelow => [
                r#"   _"#,
                r#"  / "#,
                r#" <  "#,
            ],
            OpenedAbove => [
                r#"  \_"#,
                r#"    "#,
                r#"    "#,
            ],
            ClosedBelow => [
                r#"    "#,
                r#"\   "#,
                r#" >  "#,
            ],
            ClosedAbove => [
                r#"/   "#,
                r#"    "#,
                r#"    "#,
            ],
            TransferUpStart => [
                r#"___/"#,
                r#"    "#,
                r#"    "#,
            ],
            TransferUpFinish => [
                r#"  __"#,
                r#" /  "#,
                r#"/   "#,
            ],
            TransferDownStart => [
                r#"__  "#,
                r#"  \ "#,
                r#"   \"#,
            ],
            TransferDownFinish => [
                r#"\___"#,
                r#"    "#,
                r#"    "#,
            ],
        }
    }
}

const HORIZ_LEN: usize = Horiz::Empty.display()[0].len();

impl VerboseLine {
    pub fn display(&self) -> [String; 3] {
        let mut l0 = " ".repeat(self.0.len() * HORIZ_LEN) + "\n";
        let mut l1 = l0.clone();
        let mut l2 = l0.clone();

        for (idx, horiz) in self.0.iter().enumerate() {
            let [h0, h1, h2] = horiz.display();
            let range = (idx * HORIZ_LEN)..((idx + 1) * HORIZ_LEN);

            l0.replace_range(range.clone(), h0);
            l1.replace_range(range.clone(), h1);
            l2.replace_range(range, h2);
        }

        [l0, l1, l2]
    }
}

impl Horiz {
    #[rustfmt::skip]
    pub fn subsequent(&self) -> Self {
        use Horiz::*;

        match self {
            | Empty
            | ClosedBelow
            | ClosedAbove
            | TransferUpStart
            | TransferDownStart
            => Empty,

            | Line
            | CrossDownOver
            | CrossDownUnder
            | CrossUpOver
            | CrossUpUnder
            | OpenedBelow
            | OpenedAbove
            | TransferUpFinish
            | TransferDownFinish
            => Line,
        }
    }

    ///// # Panics
    /////
    ///// If element is not a raw diagram element.
    //fn num_segments(element: u8) -> u8 {
    //    match element {
    //        _ => panic!("Not a raw diagram element: {element:?}"),
    //    }
    //}
}

fn raw_lines_continue(lines: &mut Vec<Vec<Horiz>>) {
    lines
        .iter_mut()
        .for_each(|line| line.push(line.last().unwrap_or(&Horiz::Empty).subsequent()));
}

fn raw_lines_append(lines: &mut Vec<Vec<Horiz>>, idx: usize, element: u8) {
    raw_lines_continue(lines);

    match element {
        b'A' => {}
    }
}

impl VerboseDiagram {
    pub fn display<'a>(&'a self) -> impl 'a + Iterator<Item = String> {
        self.0.iter().flat_map(|line| line.display())
    }

    pub fn from_abbreviated(knot: &AbbreviatedDiagram) -> Result<Self, String> {
        let height = knot.height();

        let mut lines: Vec<Vec<u8>> = vec![Vec::with_capacity(knot.len()); height];

        todo!()
    }
}

pub struct AbbreviatedDiagram(Vec<(usize, u8)>);

impl AbbreviatedDiagram {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    fn height(&self) -> usize {
        self.0
            .iter()
            .fold((0isize, 0usize), |(mut num_open, max_open), (_, horiz)| {
                num_open += match horiz {
                    b'A' => 1,
                    b'V' => -1,
                    _ => 0,
                };

                (num_open, max_open.max(num_open as usize))
            })
            .1
            * 2
    }
}

pub fn ascii_print(knot: &[(usize, u8)]) -> String {
    todo!()
}
