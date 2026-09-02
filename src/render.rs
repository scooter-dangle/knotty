use std::{fmt, str::FromStr};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Horiz {
    #[default]
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
    TransferUp,
    TransferUpFinish,
    TransferDownStart,
    TransferDown,
    TransferDownFinish,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct VerboseLine(pub(crate) Vec<Horiz>);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct VerboseDiagram(pub(crate) Vec<VerboseLine>);

pub(crate) const fn display_lines(grid_borders: bool) -> usize {
    3 + if grid_borders { 1 } else { 0 }
}

pub(crate) const DISPLAY_WITH_BORDERS_LINES: usize = display_lines(true);
pub(crate) const DISPLAY_LINES: usize = display_lines(false);

impl Horiz {
    #[rustfmt::skip]
    pub const fn display(&self) -> [&'static str; DISPLAY_LINES] {
        use Horiz::*;

        match self {
            // The eight halves that only ever held the other end of a
            // feature are synonyms of `Empty` here: every feature is whole
            // inside one cell, so there is nothing left for them to draw.
            | Empty
            | CrossUpOver
            | CrossUpUnder
            | OpenedAbove
            | ClosedAbove
            | TransferUpStart
            | TransferUpFinish
            | TransferDownStart
            | TransferDownFinish
            => [
                r#"   "#,
                r#"   "#,
                r#"   "#,
            ],
            Line => [
                r#"   "#,
                r#"   "#,
                r#"___"#,
            ],
            CrossDownOver => [
                r#"\ /"#,
                r#" \ "#,
                r#"/ \"#,
            ],
            CrossDownUnder => [
                r#"\ /"#,
                r#" / "#,
                r#"/ \"#,
            ],
            OpenedBelow => [
                r#"  /"#,
                r#" ( "#,
                r#"  \"#,
            ],
            ClosedBelow => [
                r#"\  "#,
                r#" ) "#,
                r#"/  "#,
            ],
            TransferUp => [
                r#"  /"#,
                r#" / "#,
                r#"/  "#,
            ],
            TransferDown => [
                r#"\  "#,
                r#" \ "#,
                r#"  \"#,
            ],
        }
    }

    #[rustfmt::skip]
    pub const fn display_with_borders(&self) -> [&'static str; DISPLAY_WITH_BORDERS_LINES] {
        use Horiz::*;

        match self {
            | Empty
            | CrossUpOver
            | CrossUpUnder
            | OpenedAbove
            | ClosedAbove
            | TransferUpStart
            | TransferUpFinish
            | TransferDownStart
            | TransferDownFinish
            => [
                r#"+---"#,
                r#"|   "#,
                r#"|   "#,
                r#"|   "#,
            ],
            Line => [
                r#"+---"#,
                r#"|   "#,
                r#"|   "#,
                r#"|___"#,
            ],
            CrossDownOver => [
                r#"+---"#,
                r#"|\ /"#,
                r#"| \ "#,
                r#"|/ \"#,
            ],
            CrossDownUnder => [
                r#"+---"#,
                r#"|\ /"#,
                r#"| / "#,
                r#"|/ \"#,
            ],
            OpenedBelow => [
                r#"+---"#,
                r#"|  /"#,
                r#"| ( "#,
                r#"|  \"#,
            ],
            ClosedBelow => [
                r#"+---"#,
                r#"|\  "#,
                r#"| ) "#,
                r#"|/  "#,
            ],
            TransferUp => [
                r#"+---"#,
                r#"|  /"#,
                r#"| / "#,
                r#"|/  "#,
            ],
            TransferDown => [
                r#"+---"#,
                r#"|\  "#,
                r#"| \ "#,
                r#"|  \"#,
            ],
        }
    }

    #[rustfmt::skip]
    /// The cells the split-cell rendering drew and this one does not. They stay
    /// readable, but they draw nothing, so canonical text writes them as the
    /// empty cell. Goes away when the cells themselves do.
    #[rustfmt::skip]
    const fn drawn(self) -> Self {
        use Horiz::*;

        match self {
            | CrossUpOver
            | CrossUpUnder
            | OpenedAbove
            | ClosedAbove
            | TransferUpStart
            | TransferUpFinish
            | TransferDownStart
            | TransferDownFinish
            => Empty,

            horiz => horiz,
        }
    }

    pub const fn is_empty(&self) -> bool {
        matches!(self, Horiz::Empty)
    }

    #[rustfmt::skip]
    pub const fn as_byte(&self) -> u8 {
        use Horiz::*;

        match self {
            Empty              => b'.',
            Line               => b'_',
            CrossDownOver      => b'x',
            CrossDownUnder     => b'y',
            CrossUpOver        => b'A',
            CrossUpUnder       => b'a',
            OpenedBelow        => b'(',
            OpenedAbove        => b'\'',
            ClosedBelow        => b')',
            ClosedAbove        => b',',
            TransferUpStart    => b'j',
            TransferUp         => b'/',
            TransferUpFinish   => b'r',
            TransferDownStart  => b'2',
            TransferDown       => b'\\',
            TransferDownFinish => b'L',
        }
    }

    #[rustfmt::skip]
    pub const fn from_byte(byte: u8) -> Option<Self> {
        use Horiz::*;

        Some(match byte {
            b'.'  => Empty,
            b'_'  => Line,
            b'x'  => CrossDownOver,
            b'y'  => CrossDownUnder,
            b'A'  => CrossUpOver,
            b'a'  => CrossUpUnder,
            b'('  => OpenedBelow,
            b'\'' => OpenedAbove,
            b')'  => ClosedBelow,
            b','  => ClosedAbove,
            b'j'  => TransferUpStart,
            b'/'  => TransferUp,
            b'r'  => TransferUpFinish,
            b'2'  => TransferDownStart,
            b'\\' => TransferDown,
            b'L'  => TransferDownFinish,
            _ => return None,
        })
    }
}

impl VerboseLine {
    pub fn display<const GRID_BORDERS: bool>(&self) -> impl 'static + Iterator<Item = String> {
        let horiz_len: usize = if GRID_BORDERS {
            Horiz::Empty.display_with_borders()[0].len()
        } else {
            Horiz::Empty.display()[0].len()
        };

        let mut l0 = " ".repeat(self.0.len() * horiz_len) + "\n";
        let mut l1 = l0.clone();
        let mut l2 = l0.clone();
        let mut l3 = if GRID_BORDERS {
            l0.clone()
        } else {
            String::new()
        };

        for (idx, horiz) in self.0.iter().enumerate() {
            let [h0, h1, h2, h3] = if GRID_BORDERS {
                horiz.display_with_borders()
            } else {
                let [h0, h1, h2] = horiz.display();
                [h0, h1, h2, ""]
            };
            let range = (idx * horiz_len)..((idx + 1) * horiz_len);

            l0.replace_range(range.clone(), h0);
            l1.replace_range(range.clone(), h1);
            l2.replace_range(range.clone(), h2);
            if GRID_BORDERS {
                l3.replace_range(range, h3);
            }
        }

        [l0, l1, l2]
            .into_iter()
            .chain(std::iter::once(l3).filter(|_| GRID_BORDERS))
    }
}

impl VerboseDiagram {
    pub fn display<'a, const GRID_BORDERS: bool>(&'a self) -> impl 'a + Iterator<Item = String> {
        let inner = if self.0.is_empty() {
            &[][..]
        } else {
            self.0.as_slice()
        };

        let total = display_lines(GRID_BORDERS);
        let border_lines = usize::from(GRID_BORDERS);

        inner.iter().rev().enumerate().flat_map(move |(idx, line)| {
            line.display::<GRID_BORDERS>()
                .enumerate()
                // A strand hangs from the bottom of its cell, so the empty
                // pair is at the top of the picture. Keep the last content
                // line, and the rule above it if there is one.
                .filter(move |&(line_idx, _)| {
                    idx != 0 || line_idx + 1 == total || line_idx < border_lines
                })
                .map(|(_, line)| line)
        })
    }
}

impl VerboseDiagram {
    pub fn to_text(&self) -> String {
        // Defensive: a VerboseDiagram built any way other than by
        // parsing or `from_abbreviated` could be ragged, and an
        // unpadded row would break the byte-for-byte round trip.
        let width = self.0.iter().map(|line| line.0.len()).max().unwrap_or(0);

        // Row 0 is the bottom of the picture; the text reads top-down.
        self.0
            .iter()
            .rev()
            .flat_map(|line| {
                line.0
                    .iter()
                    .map(|horiz| horiz.drawn())
                    .chain(std::iter::repeat(Horiz::Empty).take(width - line.0.len()))
                    .map(|horiz| horiz.as_byte() as char)
                    .chain(std::iter::once('\n'))
            })
            .collect()
    }
}

impl FromStr for VerboseDiagram {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        if string.is_empty() {
            return Ok(Self::default());
        }

        let body = string.strip_suffix('\n').unwrap_or(string);

        let mut lines = body
            .split('\n')
            .enumerate()
            .map(|(line_idx, line)| {
                line.strip_suffix('\r')
                    .unwrap_or(line)
                    .bytes()
                    .enumerate()
                    .map(|(column_idx, byte)| {
                        Horiz::from_byte(byte).ok_or_else(|| {
                            format!(
                                "unrecognized character {:?} at line {}, column {}",
                                byte as char,
                                line_idx + 1,
                                column_idx + 1,
                            )
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;

        // The widest row isn't known until every line has been read, so
        // padding has to be a second pass.
        let width = lines.iter().map(Vec::len).max().unwrap_or(0);
        for line in &mut lines {
            line.resize(width, Horiz::Empty);
        }

        // Row 0 renders at the bottom, but the text reads top-down.
        lines.reverse();

        Ok(Self(lines.into_iter().map(VerboseLine).collect()))
    }
}

impl fmt::Display for VerboseDiagram {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.to_text())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::AbbreviatedDiagram;
    use pretty_assertions::assert_eq;

    const ALL_HORIZ: [Horiz; 16] = {
        use Horiz::*;

        [
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
            TransferUp,
            TransferUpFinish,
            TransferDownStart,
            TransferDown,
            TransferDownFinish,
        ]
    };

    const RETIRED: [Horiz; 8] = {
        use Horiz::*;

        [
            CrossUpOver,
            CrossUpUnder,
            OpenedAbove,
            ClosedAbove,
            TransferUpStart,
            TransferUpFinish,
            TransferDownStart,
            TransferDownFinish,
        ]
    };

    #[test]
    #[rustfmt::skip]
    fn opening_centered_cells_match_the_table() {
        use Horiz::*;

        let table: [(Horiz, [&str; DISPLAY_LINES]); 16] = [
            (Empty,              ["   ", "   ", "   "]),
            (Line,               ["   ", "   ", "___"]),
            (CrossDownOver,      [r"\ /", r" \ ", r"/ \"]),
            (CrossDownUnder,     [r"\ /", r" / ", r"/ \"]),
            (CrossUpOver,        ["   ", "   ", "   "]),
            (CrossUpUnder,       ["   ", "   ", "   "]),
            (OpenedBelow,        ["  /", " ( ", r"  \"]),
            (OpenedAbove,        ["   ", "   ", "   "]),
            (ClosedBelow,        [r"\  ", " ) ", "/  "]),
            (ClosedAbove,        ["   ", "   ", "   "]),
            (TransferUpStart,    ["   ", "   ", "   "]),
            (TransferUp,         ["  /", " / ", "/  "]),
            (TransferUpFinish,   ["   ", "   ", "   "]),
            (TransferDownStart,  ["   ", "   ", "   "]),
            (TransferDown,       [r"\  ", r" \ ", r"  \"]),
            (TransferDownFinish, ["   ", "   ", "   "]),
        ];

        for (horiz, cell) in table {
            assert_eq!(horiz.display(), cell, "{horiz:?}");
        }
    }

    #[test]
    fn retired_cells_are_blank() {
        for horiz in RETIRED {
            assert_eq!(
                horiz.display(),
                ["   "; DISPLAY_LINES],
                "{horiz:?}",
            );
        }
    }

    #[test]
    fn bordered_cells_are_the_plain_cells_behind_a_rule() {
        // The border is drawn by the renderer, not by the cell.
        for horiz in ALL_HORIZ {
            let plain = horiz.display();
            let bordered = horiz.display_with_borders();

            assert_eq!(bordered[0], "+---", "{horiz:?}");
            for (idx, line) in plain.iter().enumerate() {
                assert_eq!(bordered[idx + 1], format!("|{line}"), "{horiz:?} line {idx}");
            }
        }
    }

    #[test]
    fn retired_characters_are_read_but_never_written() {
        let diagram = parse("Aa',\njr2L\n");

        assert_eq!(diagram.to_text(), "....\n....\n");
    }

    #[test]
    fn text_settles_in_one_pass() {
        for source in [UNKNOT, TREFOIL, "Aa',\njr2L\n", ".(_/_).\n(__\\__)\n"] {
            let once = parse(source).to_text();
            let twice = parse(&once).to_text();

            assert_eq!(once, twice, "{source:?}");
        }
    }

    #[test]
    fn byte_mapping_round_trips() {
        for horiz in ALL_HORIZ {
            assert_eq!(Horiz::from_byte(horiz.as_byte()), Some(horiz));
        }
    }

    #[test]
    fn byte_mapping_is_distinct() {
        let mut bytes: Vec<u8> = ALL_HORIZ.iter().map(Horiz::as_byte).collect();
        bytes.sort_unstable();
        let distinct = bytes.len();
        bytes.dedup();
        assert_eq!(bytes.len(), distinct);
    }

    #[test]
    fn unrecognized_bytes_have_no_mapping() {
        for byte in [b' ', b'\t', b'l', b'B', b'\r', b'0', b'|', b'-', b'i', b'k'] {
            assert_eq!(Horiz::from_byte(byte), None, "byte {:?}", byte as char);
        }
    }

    const UNKNOT: &str = "\
        ..\n\
        ()\n\
    ";

    const TREFOIL: &str = "\
        ..___..\n\
        .(._.).\n\
        ._y.y_.\n\
        (__x__)\n\
    ";

    fn knot(source: &str) -> AbbreviatedDiagram {
        source.parse().unwrap()
    }

    fn parse(source: &str) -> VerboseDiagram {
        source.parse().unwrap()
    }

    fn render(diagram: &VerboseDiagram) -> String {
        diagram.display::<false>().collect()
    }

    #[test]
    fn parsed_trefoil_renders_as_the_notation_does() {
        assert_eq!(
            render(&parse(TREFOIL)),
            knot("(0 (2 /1 \\0 /1 )2 )0").ascii_print::<false>(),
        );
    }

    #[test]
    fn parsed_unknot_renders_as_the_notation_does() {
        assert_eq!(
            render(&parse(UNKNOT)),
            knot("(0 )0").ascii_print::<false>()
        );
    }

    #[test]
    fn first_line_is_the_top_row() {
        // Structural equality pins the row order exactly: a reversed
        // implementation renders an upside-down picture that a weaker
        // check would accept.
        assert_eq!(
            parse(TREFOIL),
            VerboseDiagram::from_abbreviated(&knot("(0 (2 /1 \\0 /1 )2 )0"))
                .unwrap(),
        );

        let reversed = TREFOIL
            .trim_end()
            .lines()
            .rev()
            .collect::<Vec<_>>()
            .join("\n");
        assert_ne!(render(&parse(&reversed)), render(&parse(TREFOIL)));
    }

    #[test]
    fn ragged_rows_are_padded_on_the_right() {
        let ragged = "\
            ..___\n\
            .(._.)\n\
            ._y.y_\n\
            (__x__)\n\
        ";

        assert_eq!(parse(ragged), parse(TREFOIL));
    }

    #[test]
    fn empty_input_is_an_empty_diagram() {
        assert_eq!(parse(""), VerboseDiagram::default());
        assert_eq!(render(&parse("")), "");
    }

    #[test]
    fn trailing_newline_is_optional() {
        assert_eq!(parse("()\n',\n"), parse("()\n',"));
    }

    #[test]
    fn carriage_returns_terminate_lines() {
        assert_eq!(parse("()\r\n',\r\n"), parse("()\n',"));
    }

    #[test]
    fn blank_line_past_the_terminator_is_an_empty_row() {
        let with_blank = parse("()\n',\n\n");

        assert_eq!(with_blank.0.len(), 3);
        assert_eq!(with_blank.0[0], VerboseLine(vec![Horiz::Empty; 2]));
        assert_ne!(with_blank, parse("()\n',\n"));
    }

    #[test]
    fn interior_blank_line_is_an_empty_row() {
        assert_eq!(parse("()\n\n',").0[1], VerboseLine(vec![Horiz::Empty; 2]));
    }

    #[test]
    fn error_position_uses_input_line_numbers() {
        // Rows are stored bottom-up, so an implementation that reverses
        // before reporting names line 4 for a mistake on line 1.
        let error = "b(___).\n.'y_y,.\n(_AxA_)\n'__a__,"
            .parse::<VerboseDiagram>()
            .unwrap_err();

        assert!(error.contains("line 1"), "{error}");
        assert!(error.contains("column 1"), "{error}");
        assert!(error.contains("'b'"), "{error}");
    }

    #[test]
    fn error_position_is_one_based() {
        let error = "()\n(b)".parse::<VerboseDiagram>().unwrap_err();

        assert!(error.contains("line 2"), "{error}");
        assert!(error.contains("column 2"), "{error}");
    }

    #[test]
    fn only_the_first_bad_character_is_reported() {
        let error = "(bc)".parse::<VerboseDiagram>().unwrap_err();

        assert!(error.contains("'b'"), "{error}");
        assert!(!error.contains("'c'"), "{error}");
    }

    #[test]
    fn whitespace_is_rejected_like_any_other_unknown_character() {
        for source in ["( )", "(\t)"] {
            let error = source.parse::<VerboseDiagram>().unwrap_err();
            assert!(error.contains("column 2"), "{source:?}: {error}");
        }
    }

    #[test]
    fn snapshot_parsed_diagram_render_with_borders() {
        insta::assert_snapshot!(parse(TREFOIL)
            .display::<true>()
            .collect::<String>());
    }

    #[test]
    fn snapshot_parsed_diagram_render() {
        let hand_written = "\
            ...__...\n\
            ../..\\..\n\
            ./.().\\.\n\
            (______)\n\
        ";

        insta::assert_snapshot!(render(&parse(hand_written)));
    }

    const SAMPLE_KNOTS: [&str; 4] = [
        "(0 )0",
        "(0 (2 /1 \\0 /1 )2 )0",
        "(0 (2 \\1 (3 /2 /4 )3 \\1 )2 )0",
        "(0 (2 /1 \\0 \\0 \\0 /1 )2 )0",
    ];

    fn verbose(source: &str) -> VerboseDiagram {
        VerboseDiagram::from_abbreviated(&knot(source)).unwrap()
    }

    #[test]
    fn serializes_to_the_canonical_text() {
        assert_eq!(verbose("(0 )0").to_string(), UNKNOT);
        assert_eq!(verbose("(0 (2 /1 \\0 /1 )2 )0").to_string(), TREFOIL);
    }

    #[test]
    fn serialized_rows_all_have_equal_length() {
        for source in SAMPLE_KNOTS {
            let text = verbose(source).to_string();
            let widths: Vec<usize> = text.lines().map(str::len).collect();

            assert!(
                widths.windows(2).all(|pair| pair[0] == pair[1]),
                "{source}: ragged widths {widths:?}",
            );
        }
    }

    #[test]
    fn round_trips_through_text() {
        for source in SAMPLE_KNOTS {
            let diagram = verbose(source);
            let text = diagram.to_string();

            assert_eq!(parse(&text), diagram, "{source}");
            assert_eq!(parse(&text).to_string(), text, "{source}");
        }
    }

    #[test]
    fn ragged_text_normalizes_to_a_fixed_point() {
        let ragged = "\
            ..___\n\
            .(._.)\n\
            ._y.y_\n\
            (__x__)\n\
        ";

        let canonical = parse(ragged).to_string();

        assert_eq!(canonical, TREFOIL);
        assert_eq!(parse(&canonical).to_string(), canonical);
    }

    #[test]
    fn empty_diagram_serializes_to_empty_text() {
        assert_eq!(VerboseDiagram::default().to_string(), "");
        assert_eq!(parse(""), VerboseDiagram::default());
    }

    #[test]
    fn blank_rows_survive_a_round_trip() {
        let with_blank = "()\n..\n()\n";

        assert_eq!(parse(with_blank).to_string(), with_blank);
    }
}
