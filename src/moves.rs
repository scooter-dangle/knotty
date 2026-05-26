use core::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct CommentLines {
    pub(crate) delimiter: &'static str,
    pub(crate) comment_start: &'static str,
    pub(crate) inner_delimiter: Option<&'static str>,
}

impl CommentLines {
    // Get rid of the comment lines and trailing comments
    pub(crate) fn split_n_strip<'a>(self, string: &'a str) -> impl 'a + Iterator<Item = &'a str> {
        string
            .split(self.delimiter)
            .map(str::trim)
            .filter_map(move |line| line.split(self.comment_start).next().map(str::trim))
            .filter(|line| !line.is_empty())
    }

    pub(crate) fn parse_iter<'a, OK, ERR>(
        self,
        string: &'a str,
    ) -> impl Iterator<Item = Result<OK, ERR>> + 'a
    where
        OK: 'a + FromStr<Err = ERR>,
    {
        let inner_delimiter = self.inner_delimiter.unwrap_or(self.delimiter);

        self.split_n_strip(string)
            .flat_map(move |line| {
                // If we default to the main delimiter when no inner
                // delimiter is provided, it's essentially a no-op since
                // we've already split on the main delimiter.
                line.split(inner_delimiter).map(str::trim)
            })
            .filter(|line| !line.is_empty())
            .map(str::parse)
    }

    pub(crate) fn parse<'a, OK, ERR, T>(
        self,
        constructor: impl Fn(Vec<OK>) -> T,
        string: &str,
    ) -> Result<T, ERR>
    where
        OK: 'a + FromStr<Err = ERR>,
    {
        self.parse_iter(string)
            .collect::<Result<Vec<OK>, ERR>>()
            .map(constructor)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lean {
    Forward,
    Backward,
}

impl FromStr for Lean {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        use Lean::*;

        match string {
            "forward" => Ok(Forward),
            "backward" => Ok(Backward),
            _ => Err(format!("Invalid lean: {string:?}")),
        }
    }
}

impl fmt::Display for Lean {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Lean::*;

        write!(
            formatter,
            "{}",
            match self {
                Forward => "forward",
                Backward => "backward",
            },
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OverUnder {
    Over,
    Under,
}

impl FromStr for OverUnder {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        use OverUnder::*;

        Ok(match string {
            "over" => Over,
            "under" => Under,
            _ => return Err(format!("Invalid over/under: {string:?}")),
        })
    }
}

impl fmt::Display for OverUnder {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        use OverUnder::*;

        write!(
            formatter,
            "{}",
            match self {
                Over => "over",
                Under => "under",
            },
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpDown {
    Up,
    Down,
}

impl FromStr for UpDown {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        use UpDown::*;

        Ok(match string {
            "up" => Up,
            "down" => Down,
            _ => return Err(format!("Invalid up/down: {string:?}")),
        })
    }
}

impl fmt::Display for UpDown {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UpDown::*;

        write!(
            formatter,
            "{}",
            match self {
                Up => "up",
                Down => "down",
            },
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Move {
    Swap,
    WrapAround,
    Bulge {
        lean: Lean,
        vertical_index: usize,
    },
    CollapseBulge,

    Reid1a {
        over_under: OverUnder,
    },
    CollapseReid1a,
    Reid1b {
        up_down: UpDown,
        over_under: OverUnder,
        vertical_index: usize,
    },
    CollapseReid1b,
    Reid2 {
        over_under: OverUnder,
        vertical_index: usize,
    },
    CollapseReid2,
    Reid3,

    ChangeCrossing,

    Rotate90CounterClockwise,
}

impl fmt::Display for Move {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Move::*;

        match self {
            Swap => write!(formatter, "swap"),
            WrapAround => write!(formatter, "wrap_around"),
            ChangeCrossing => write!(formatter, "change_crossing"),
            Rotate90CounterClockwise => write!(formatter, "rotate_90_counter_clockwise"),
            CollapseBulge => write!(formatter, "collapse_bulge"),
            Reid1a { over_under } => write!(formatter, "reid_1a({over_under})"),
            CollapseReid1a => write!(formatter, "collapse_reid_1a"),
            Reid1b {
                up_down,
                over_under,
                vertical_index,
            } => write!(
                formatter,
                "reid_1b({up_down}, {over_under}, {vertical_index})"
            ),
            CollapseReid1b => write!(formatter, "collapse_reid_1b"),
            Reid2 {
                over_under,
                vertical_index,
            } => write!(formatter, "reid_2({over_under}, {vertical_index})"),
            CollapseReid2 => write!(formatter, "collapse_reid_2"),
            Reid3 => write!(formatter, "reid_3"),
            Bulge {
                lean,
                vertical_index,
            } => write!(formatter, "bulge({lean}, {vertical_index})"),
        }
    }
}

fn parse_bulge_args(mut string: &str) -> Result<(Lean, usize), String> {
    if string.ends_with(')') {
        string = &string[..string.len() - 1];
    } else {
        return Err("missing closing parenthesis".into());
    }

    let args = string.split(',').map(str::trim).collect::<Vec<_>>();
    if args.len() != 2 {
        return Err(format!("expected two arguments, got {}", args.len()));
    }

    Ok((
        args[0].parse()?,
        args[1]
            .parse()
            .map_err(|err| format!("invalid vertical index {}: {err}", args[1]))?,
    ))
}

impl FromStr for Move {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        use Move::*;

        let mut open_par_splits = string.split('(');

        let moov = match open_par_splits
            .next()
            .filter(|split| !split.is_empty())
            .ok_or_else(|| "Move can't be empty string")?
        {
            "swap" => Swap,
            "wrap_around" => WrapAround,
            "change_crossing" => ChangeCrossing,
            "rotate_90_counter_clockwise" => Rotate90CounterClockwise,
            "collapse_bulge" => CollapseBulge,
            "collapse_reid_1a" => CollapseReid1a,
            "collapse_reid_1b" => CollapseReid1b,
            "collapse_reid_2" => CollapseReid2,
            "reid_3" => Reid3,
            "reid_1a" => Reid1a {
                over_under: open_par_splits
                    .next()
                    .map(|split| split.trim_end_matches(')'))
                    .ok_or_else(|| "reid_1a requires an argument")?
                    .parse()?,
            },
            "reid_1b" => {
                let (up_down, over_under, vertical_index) = parse_reid_1b_args(
                    open_par_splits
                        .next()
                        .ok_or_else(|| "reid_1b requires arguments")?,
                )?;

                Reid1b {
                    up_down,
                    over_under,
                    vertical_index,
                }
            }
            "reid_2" => {
                let (over_under, vertical_index) = parse_reid_2_args(
                    open_par_splits
                        .next()
                        .ok_or_else(|| "reid_2 requires arguments")?,
                )?;

                Reid2 {
                    over_under,
                    vertical_index,
                }
            }
            "bulge" => {
                let (lean, vertical_index) = parse_bulge_args(
                    open_par_splits
                        .next()
                        .ok_or_else(|| "missing bulge arguments")?,
                )?;

                Bulge {
                    lean,
                    vertical_index,
                }
            }
            other => return Err(format!("Invalid move kind: {other:?}")),
        };

        if open_par_splits.next().is_some() {
            return Err("unexpected opening parenthesis".into());
        }

        Ok(moov)
    }
}

fn parse_reid_1b_args(mut string: &str) -> Result<(UpDown, OverUnder, usize), String> {
    if string.ends_with(')') {
        string = &string[..string.len() - 1];
    } else {
        return Err("missing closing parenthesis".into());
    }

    let args = string.split(',').map(str::trim).collect::<Vec<_>>();
    if args.len() != 3 {
        return Err(format!("expected three arguments, got {}", args.len()));
    }

    Ok((
        args[0].parse()?,
        args[1].parse()?,
        args[2]
            .parse()
            .map_err(|err| format!("invalid vertical index {}: {err}", args[2]))?,
    ))
}

fn parse_reid_2_args(mut string: &str) -> Result<(OverUnder, usize), String> {
    if string.ends_with(')') {
        string = &string[..string.len() - 1];
    } else {
        return Err("missing closing parenthesis".into());
    }

    let args = string.split(',').map(str::trim).collect::<Vec<_>>();
    if args.len() != 2 {
        return Err(format!("expected two arguments, got {}", args.len()));
    }

    Ok((
        args[0].parse()?,
        args[1]
            .parse()
            .map_err(|err| format!("invalid vertical index {}: {err}", args[1]))?,
    ))
}

impl fmt::Display for DiagramMove {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}@{}", self.r#move, self.idx)
    }
}

impl FromStr for DiagramMove {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (moove, idx) = string.split_once('@').ok_or_else(|| "No @ symbol")?;

        let idx = idx
            .parse::<usize>()
            .map_err(|err| format!("Invalid index {idx:?}: {err}"))?;

        let moov = moove.parse()?;

        Ok(Self { idx, r#move: moov })
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct DiagramMove {
    pub(crate) idx: usize,
    pub(crate) r#move: Move,
}

impl DiagramMove {
    pub fn r#move(&self) -> Move {
        self.r#move
    }

    pub fn idx(&self) -> usize {
        self.idx
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct DiagramMoves(Vec<DiagramMove>);

impl From<Vec<DiagramMove>> for DiagramMoves {
    fn from(moves: Vec<DiagramMove>) -> Self {
        Self(moves)
    }
}

impl IntoIterator for DiagramMoves {
    type Item = DiagramMove;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromStr for DiagramMoves {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        CommentLines {
            delimiter: "\n",
            comment_start: "#",
            inner_delimiter: None,
        }
        .parse(Self, string)
    }
}

impl fmt::Display for DiagramMoves {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0
            .iter()
            .map(|moov| writeln!(formatter, "{moov}\n"))
            .collect()
    }
}
