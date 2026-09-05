use std::{
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use clap::{CommandFactory, Parser, ValueEnum};
use clap_complete::{generate, Shell};

use knotty::{self, AbbreviatedDiagram, PlacementMode, VerboseDiagram};

/// Marks a trailer line embedding the exact rendered grid in succinct
/// output, so `--input-format succinct` can recover it exactly. See
/// specs/008-ascii-print-tool-upgrade/research.md R10.
const GRID_MARKER: &str = "# ascii_print-grid: ";

#[derive(Debug, Clone, Copy, ValueEnum)]
enum InputFormat {
    Encoded,
    Succinct,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum Style {
    Succinct,
    FullSpaced,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum Placement {
    PrecalculatedHeights,
    IndexAligned,
}

impl From<Placement> for PlacementMode {
    fn from(placement: Placement) -> Self {
        match placement {
            Placement::PrecalculatedHeights => PlacementMode::PrecalculatedHeights,
            Placement::IndexAligned => PlacementMode::IndexAligned,
        }
    }
}

/// Prints compact, easy-to-scan diagrams from an encoded diagram (abbreviated
/// notation) or from a previously-printed succinct diagram.
#[derive(Debug, Parser)]
#[command(name = "ascii_print")]
struct Cli {
    /// Path to the diagram input, or "-"/omitted for stdin
    diagram: Option<PathBuf>,

    /// Path to a diagram-manipulations file to apply before rendering
    /// (only valid with --input-format encoded)
    moves: Option<PathBuf>,

    /// How to interpret the diagram input
    #[arg(long, value_enum, default_value_t = InputFormat::Encoded)]
    input_format: InputFormat,

    /// Output rendering style
    #[arg(long, value_enum, default_value_t = Style::Succinct)]
    style: Style,

    /// Strand placement behavior (only valid with --input-format encoded)
    #[arg(long, value_enum)]
    placement: Option<Placement>,

    /// Draw grid borders around the diagram
    #[arg(long)]
    grid_borders: bool,

    /// Also print the resulting notation after rendering (only valid with
    /// --input-format encoded)
    #[arg(long)]
    echo_diagram: bool,

    /// Print a shell completion script and exit
    #[arg(
        long,
        value_enum,
        conflicts_with_all = [
            "diagram", "moves", "input_format", "style", "placement",
            "grid_borders", "echo_diagram",
        ],
    )]
    completions: Option<Shell>,
}

fn read_input(file: Option<&Path>) -> Result<String, String> {
    let lines = match file {
        None => std::io::stdin()
            .lock()
            .lines()
            .map(|line| format!("{}\n", line.unwrap()))
            .collect::<String>(),
        Some(path) if path == Path::new("-") => std::io::stdin()
            .lock()
            .lines()
            .map(|line| format!("{}\n", line.unwrap()))
            .collect::<String>(),
        Some(path) => BufReader::new(std::fs::File::open(path).unwrap())
            .lines()
            .map(|line| format!("{}\n", line.unwrap()))
            .collect::<String>(),
    };

    Ok(lines)
}

/// Replicates `AbbreviatedDiagram::ascii_print_compact`'s blank-column
/// stripping (src/diagram.rs) over rendered lines directly, for use when no
/// `AbbreviatedDiagram` is available (succinct input has already been
/// rendered — see research.md R10).
fn strip_blank_columns(inner: &[String]) -> String {
    if inner.is_empty() {
        return String::new();
    }

    let string_len = inner[0].len();
    let mut out = vec![String::with_capacity(string_len); inner.len()];

    for idx in 0..string_len {
        if inner
            .iter()
            .all(|line| matches!(&line[idx..idx + 1], " " | "_"))
        {
            continue;
        }

        for (out, inner) in out.iter_mut().zip(inner.iter()) {
            out.push_str(&inner[idx..idx + 1]);
        }
    }

    out.into_iter().collect()
}

/// Appends the succinct output's hidden grid trailer (research R10): every
/// line of `verbose.to_text()`, marked so it can be told apart from the
/// visible diagram art on the way back in.
fn append_grid_trailer(out: &mut String, verbose: &VerboseDiagram) {
    for line in verbose.to_text().lines() {
        out.push_str(GRID_MARKER);
        out.push_str(line);
        out.push('\n');
    }
}

/// Recovers the exact grid embedded by `append_grid_trailer`, ignoring the
/// visible diagram art and any unrelated comment lines.
fn extract_grid_trailer(text: &str) -> Result<VerboseDiagram, String> {
    let grid_lines: Vec<&str> = text
        .lines()
        .filter_map(|line| line.strip_prefix(GRID_MARKER))
        .collect();

    if grid_lines.is_empty() {
        return Err(
            "no embedded diagram data found; succinct input must have been \
             produced by this tool's --style succinct output"
                .to_string(),
        );
    }

    grid_lines.join("\n").parse::<VerboseDiagram>()
}

fn render_encoded(cli: &Cli, knot: &AbbreviatedDiagram) -> String {
    #[rustfmt::skip]
    let render: fn(&AbbreviatedDiagram) -> String = match (cli.style, cli.grid_borders) {
        (Style::Succinct,   true)  => AbbreviatedDiagram::ascii_print_compact::<true>,
        (Style::Succinct,   false) => AbbreviatedDiagram::ascii_print_compact::<false>,
        (Style::FullSpaced, true)  => AbbreviatedDiagram::ascii_print::<true>,
        (Style::FullSpaced, false) => AbbreviatedDiagram::ascii_print::<false>,
    };

    render(knot)
}

fn run_encoded(cli: &Cli) -> Result<(), String> {
    let mut knot = read_input(cli.diagram.as_deref())?.parse::<AbbreviatedDiagram>()?;

    let placement = cli.placement.unwrap_or(Placement::PrecalculatedHeights);
    knot.set_mode(placement.into());

    if let Some(moves_path) = &cli.moves {
        let moves = read_input(Some(moves_path))?.parse::<knotty::DiagramMoves>()?;
        knot.try_apply_all(moves)?;
    }

    let mut display = render_encoded(cli, &knot);

    if matches!(cli.style, Style::Succinct) {
        let verbose = VerboseDiagram::from_abbreviated(&knot)?;
        append_grid_trailer(&mut display, &verbose);
    }

    print!("{display}");

    if cli.echo_diagram {
        print!("{knot}");
    }

    Ok(())
}

fn run_succinct(cli: &Cli) -> Result<(), String> {
    if cli.moves.is_some() {
        return Err(
            "diagram manipulations require an encoded diagram; succinct \
             input has already been rendered"
                .to_string(),
        );
    }
    if cli.placement.is_some() {
        return Err(
            "--placement has no effect on succinct input, which is already \
             a placed grid"
                .to_string(),
        );
    }
    if cli.echo_diagram {
        return Err(
            "--echo-diagram has no effect on succinct input, which carries \
             no notation to echo"
                .to_string(),
        );
    }

    let text = read_input(cli.diagram.as_deref())?;
    let verbose = extract_grid_trailer(&text)?;

    let inner: Vec<String> = match cli.grid_borders {
        true => verbose.display::<true>().collect(),
        false => verbose.display::<false>().collect(),
    };

    let mut display = match cli.style {
        Style::FullSpaced => inner.concat(),
        Style::Succinct => strip_blank_columns(&inner),
    };

    if matches!(cli.style, Style::Succinct) {
        append_grid_trailer(&mut display, &verbose);
    }

    print!("{display}");

    Ok(())
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();

    if let Some(shell) = cli.completions {
        let mut command = Cli::command();
        let name = command.get_name().to_string();
        generate(shell, &mut command, name, &mut std::io::stdout());
        return Ok(());
    }

    match cli.input_format {
        InputFormat::Encoded => run_encoded(&cli),
        InputFormat::Succinct => run_succinct(&cli),
    }
}
