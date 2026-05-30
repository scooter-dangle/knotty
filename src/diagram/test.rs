use super::*;
use crate::moves::Lean;
use Lean::*;
use pretty_assertions::assert_eq;

macro_rules! assert_eq_after {
    ($operation:expr, $idx:expr, [$($diagram:expr),* $(,)?], [$($expected:expr),* $(,)?] $(,)?) => {
        let idx = $idx;

        let diagram: Vec<(u8, usize)> = vec![$($diagram,)*];
        let expected: Vec<(u8, usize)> = vec![$($expected,)*];
        let actual: Vec<(u8, usize)> = {
            let mut diagram = AbbreviatedDiagram::new_from_tuples(diagram.clone()).unwrap();
            ($operation)(&mut diagram, idx).unwrap();
            diagram.to_tuples()
        };

        assert_eq!(
            actual
                .clone()
                .into_iter()
                .map(|(element, index)| {
                    let element = element as char;
                    format!("{element}{index}\n")
                })
                .collect::<String>(),
            expected
                .clone()
                .into_iter()
                .map(|(element, index)| {
                    let element = element as char;
                    format!("{element}{index}\n")
                })
                .collect::<String>(),
            "{}@{idx}\
            \noriginal:\n{}\
            \nexpected:\n{}\
            \nactual:\n{}",
            stringify!($operation),
            ascii_print::<false>(diagram),
            ascii_print::<false>(expected),
            ascii_print::<false>(actual),
        );
    };
}

fn apply(
    manip: fn(&mut AbbreviatedDiagram, usize) -> Result<(), String>,
    idx: usize,
    diagram: Vec<(u8, usize)>,
) -> Result<Vec<(u8, usize)>, String> {
    let mut diagram = AbbreviatedDiagram::new_from_tuples(diagram)?;
    manip(&mut diagram, idx)?;
    Ok(diagram.to_tuples())
}

macro_rules! assert_eq_after_apply {
    ($operation:ident, $idx:expr, [$($diagram:expr),* $(,)?], [$($expected:expr),* $(,)?] $(,)?) => {
        let idx = $idx;

        let diagram = vec![$($diagram,)*];
        let expected = vec![$($expected,)*];
        let actual = apply(AbbreviatedDiagram::$operation, idx, diagram.clone()).unwrap();

        assert_eq!(
            actual
                .clone()
                .into_iter()
                .map(|(element, index)| {
                    let element = element as char;
                    format!("{element}{index}\n")
                })
                .collect::<String>(),
            expected
                .clone()
                .into_iter()
                .map(|(element, index)| {
                    let element = element as char;
                    format!("{element}{index}\n")
                })
                .collect::<String>(),
            "{}@{idx}\
            \noriginal:\n{}\
            \nexpected:\n{}\
            \nactual:\n{}",
            stringify!($operation),
            ascii_print::<false>(diagram),
            ascii_print::<false>(expected),
            ascii_print::<false>(actual),
        );
    };
}

#[test]
fn test_try_bulge() {
    assert_eq_after!(
        |diagram: &mut AbbreviatedDiagram, idx| diagram.try_bulge(Backward, 0, idx),
        1,
        [(b'(', 0), (b')', 0)],
        [(b'(', 0), (b'(', 1), (b')', 0), (b')', 0)],
    );

    assert_eq_after_apply!(
        try_collapse_bulge,
        1,
        [(b'(', 0), (b'(', 1), (b')', 0), (b')', 0)],
        [(b'(', 0), (b')', 0)],
    );

    assert_eq_after!(
        |diagram: &mut AbbreviatedDiagram, idx| diagram.try_bulge(Forward, 0, idx),
        1,
        [(b'(', 0), (b')', 0)],
        [(b'(', 0), (b'(', 0), (b')', 1), (b')', 0)],
    );

    assert_eq_after_apply!(
        try_collapse_bulge,
        1,
        [(b'(', 0), (b'(', 0), (b')', 1), (b')', 0)],
        [(b'(', 0), (b')', 0)],
    );
}

#[test]
fn test_try_swap() {
    let mut diagram = AbbreviatedDiagram::new_from_tuples(vec![(b'(', 0), (b')', 0)]).unwrap();
    assert!(diagram.try_swap(0).is_err());
    assert!(diagram.try_swap(0).is_err());

    assert_eq_after_apply!(
        try_swap,
        0,
        [(b'(', 0), (b'(', 2), (b')', 2), (b')', 0)],
        [(b'(', 0), (b'(', 0), (b')', 2), (b')', 0)],
    );

    assert_eq_after_apply!(
        try_swap,
        1,
        [(b'(', 0), (b'\\', 0), (b'(', 2), (b')', 2), (b')', 0)],
        [(b'(', 0), (b'(', 2), (b'\\', 0), (b')', 2), (b')', 0)],
    );

    assert_eq_after_apply!(
        try_swap,
        2,
        [
            (b'(', 0),
            (b'(', 2),
            (b'\\', 2),
            (b'\\', 0),
            (b')', 2),
            (b')', 0),
        ],
        [
            (b'(', 0),
            (b'(', 2),
            (b'\\', 0),
            (b'\\', 2),
            (b')', 2),
            (b')', 0),
        ],
    );

    assert_eq_after_apply!(
        try_swap,
        0,
        [(b'(', 0), (b'(', 0), (b')', 0), (b')', 0)],
        [(b'(', 0), (b'(', 2), (b')', 0), (b')', 0)],
    );

    assert_eq_after_apply!(
        try_swap,
        0,
        [(b'(', 0), (b'(', 2), (b')', 0), (b')', 0)],
        [(b'(', 0), (b'(', 0), (b')', 0), (b')', 0)],
    );

    assert_eq_after_apply!(
        try_swap,
        2,
        [(b'(', 0), (b'(', 0), (b')', 0), (b')', 0)],
        [(b'(', 0), (b'(', 0), (b')', 2), (b')', 0)],
    );

    assert_eq_after_apply!(
        try_swap,
        2,
        [(b'(', 0), (b'(', 0), (b')', 2), (b')', 0)],
        [(b'(', 0), (b'(', 0), (b')', 0), (b')', 0)],
    );
}

fn rotate_elements(input: Vec<(u8, usize)>) -> Vec<u8> {
    let mut diag = AbbreviatedDiagram::new_from_tuples(input).unwrap();
    diag.try_rotate_90_ccw().unwrap();
    diag.to_tuples().into_iter().map(|(e, _)| e).collect()
}

fn fmt_elements(elems: &[u8]) -> String {
    elems.iter().map(|&e| e as char).collect::<String>()
}

macro_rules! assert_rotate_features {
    ([$($input:expr),* $(,)?], [$($expected:expr),* $(,)?] $(,)?) => {
        let input: Vec<(u8, usize)> = vec![$($input,)*];
        let expected: Vec<u8> = vec![$($expected,)*];
        let actual = rotate_elements(input.clone());
        let expected_diagram = format!("expected: {}", fmt_elements(&expected));
        let actual_diagram = format!("actual: {}", fmt_elements(&actual));
        assert_eq!(
            fmt_elements(&actual),
            fmt_elements(&expected),
            "\noriginal:\n{}\n{}\n{}",
            ascii_print::<false>(input),
            expected_diagram,
            actual_diagram,
        );
    };
}

#[test]
fn test_try_rotate_90_ccw_features() {
    // unknot — rotation-invariant
    assert_rotate_features!([(b'(', 0), (b')', 0)], [b'(', b')'],);

    // donut — rotation-invariant
    assert_rotate_features!(
        [(b'(', 0), (b'(', 1), (b')', 1), (b')', 0)],
        [b'(', b'(', b')', b')'],
    );

    // (0 /0 /0 )0  ->  (0 (2 (4 \1 \3 )4 )2 )0
    assert_rotate_features!(
        [(b'(', 0), (b'/', 0), (b'/', 0), (b')', 0)],
        [b'(', b'(', b'(', b'\\', b'\\', b')', b')', b')'],
    );

    // (0 (2 /1 \0 /1 )2 )0  ->  (0 (2 /1 \0 \2 )1 )0
    assert_rotate_features!(
        [
            (b'(', 0),
            (b'(', 2),
            (b'/', 1),
            (b'\\', 0),
            (b'/', 1),
            (b')', 2),
            (b')', 0),
        ],
        [b'(', b'(', b'/', b'\\', b'\\', b')', b')'],
    );

    // (0 (2 )1 )0  ->  (0 )0  (feature reduction)
    assert_rotate_features!([(b'(', 0), (b'(', 2), (b')', 1), (b')', 0)], [b'(', b')'],);

    // (0 (2 \1 (3 /2 /4 )3 \1 )2 )0  ->  (0 (1 /0 /2 \1 \1 )2 )0
    assert_rotate_features!(
        [
            (b'(', 0),
            (b'(', 2),
            (b'\\', 1),
            (b'(', 3),
            (b'/', 2),
            (b'/', 4),
            (b')', 3),
            (b'\\', 1),
            (b')', 2),
            (b')', 0),
        ],
        [b'(', b'(', b'/', b'/', b'\\', b'\\', b')', b')'],
    );
}

#[test]
fn test_try_rotate_90_ccw_depths() {
    macro_rules! assert_rotate_depths {
        ([$($input:expr),* $(,)?], [$($expected:expr),* $(,)?] $(,)?) => {
            let input: Vec<(u8, usize)> = vec![$($input,)*];
            let expected: Vec<(u8, usize)> = vec![$($expected,)*];
            let mut diag = AbbreviatedDiagram::new_from_tuples(input.clone()).unwrap();
            diag.try_rotate_90_ccw().unwrap();
            let actual = diag.to_tuples();
            let expected_diagram = match try_ascii_print::<false>(expected.clone()) {
                Ok(s) => format!("expected:\n{s}"),
                Err(e) => format!("expected: error rendering diagram: {e}"),
            };
            let actual_diagram = match try_ascii_print::<false>(actual.clone()) {
                Ok(s) => format!("actual:\n{s}"),
                Err(e) => format!("actual: error rendering diagram: {e}"),
            };
            assert_eq!(
                actual,
                expected,
                "\noriginal:\n{}\n{}\n{}",
                ascii_print::<false>(input),
                expected_diagram,
                actual_diagram,
            );
        };
    }

    // (0 )0  ->  (0 )0
    assert_rotate_depths!([(b'(', 0), (b')', 0)], [(b'(', 0), (b')', 0)],);

    // (0 (1 )1 )0  ->  (0 (1 )1 )0  — rotation-invariant including depths
    assert_rotate_depths!(
        [(b'(', 0), (b'(', 1), (b')', 1), (b')', 0)],
        [(b'(', 0), (b'(', 1), (b')', 1), (b')', 0)],
    );

    // (0 /0 /0 )0  ->  (0 (2 (4 \1 \3 )4 )2 )0
    assert_rotate_depths!(
        [(b'(', 0), (b'/', 0), (b'/', 0), (b')', 0)],
        [
            (b'(', 0),
            (b'(', 2),
            (b'(', 4),
            (b'\\', 1),
            (b'\\', 3),
            (b')', 4),
            (b')', 2),
            (b')', 0)
        ],
    );

    // (0 (2 /1 \0 /1 )2 )0  ->  (0 (2 /1 \0 \2 )1 )0
    assert_rotate_depths!(
        [
            (b'(', 0),
            (b'(', 2),
            (b'/', 1),
            (b'\\', 0),
            (b'/', 1),
            (b')', 2),
            (b')', 0)
        ],
        [
            (b'(', 0),
            (b'(', 2),
            (b'/', 1),
            (b'\\', 0),
            (b'\\', 2),
            (b')', 1),
            (b')', 0)
        ],
    );

    // (0 (2 )1 )0  ->  (0 )0  — feature reduction
    assert_rotate_depths!(
        [(b'(', 0), (b'(', 2), (b')', 1), (b')', 0)],
        [(b'(', 0), (b')', 0)],
    );

    // (0 (2 \1 (3 /2 /4 )3 \1 )2 )0  ->  (0 (1 /0 /2 \1 \1 )2 )0
    assert_rotate_depths!(
        [
            (b'(', 0),
            (b'(', 2),
            (b'\\', 1),
            (b'(', 3),
            (b'/', 2),
            (b'/', 4),
            (b')', 3),
            (b'\\', 1),
            (b')', 2),
            (b')', 0)
        ],
        [
            (b'(', 0),
            (b'(', 1),
            (b'/', 0),
            (b'/', 2),
            (b'\\', 1),
            (b'\\', 1),
            (b')', 2),
            (b')', 0)
        ],
    );
}

fn rotate_n(input: Vec<(u8, usize)>, n: usize) -> Vec<(u8, usize)> {
    let mut diag = AbbreviatedDiagram::new_from_tuples(input).unwrap();
    for _ in 0..n {
        diag.try_rotate_90_ccw().unwrap();
    }
    diag.to_tuples()
}

#[test]
fn test_try_rotate_90_ccw_period_4() {
    // R^5(D) = R(D): one initial rotation triggers any simplifications, then
    // four more rotations must return to the same form.
    for input in [
        // unknot
        vec![(b'(', 0), (b')', 0)],
        // donut
        vec![(b'(', 0), (b'(', 1), (b')', 1), (b')', 0)],
        // (0 /0 /0 )0
        vec![(b'(', 0), (b'/', 0), (b'/', 0), (b')', 0)],
        // (0 (2 /1 \0 /1 )2 )0
        vec![
            (b'(', 0),
            (b'(', 2),
            (b'/', 1),
            (b'\\', 0),
            (b'/', 1),
            (b')', 2),
            (b')', 0),
        ],
        // (0 (2 \1 (3 /2 /4 )3 \1 )2 )0  — square knot
        vec![
            (b'(', 0),
            (b'(', 2),
            (b'\\', 1),
            (b'(', 3),
            (b'/', 2),
            (b'/', 4),
            (b')', 3),
            (b'\\', 1),
            (b')', 2),
            (b')', 0),
        ],
        // (0 (2 /0 /1 /1 )2 )0  — rando link
        vec![
            (b'(', 0),
            (b'(', 2),
            (b'/', 0),
            (b'/', 1),
            (b'/', 1),
            (b')', 2),
            (b')', 0),
        ],
        // (0 (2 /1 \0 \0 \0 /1 )2 )0  — 5_1 knot
        vec![
            (b'(', 0),
            (b'(', 2),
            (b'/', 1),
            (b'\\', 0),
            (b'\\', 0),
            (b'\\', 0),
            (b'/', 1),
            (b')', 2),
            (b')', 0),
        ],
        // (0 (2 /1 \2 \0 \0 (1 \2 /3 /1 \0 /1 )2 /1 )2 )0  — rando annoying knot
        vec![
            (b'(', 0),
            (b'(', 2),
            (b'/', 1),
            (b'\\', 2),
            (b'\\', 0),
            (b'\\', 0),
            (b'(', 1),
            (b'\\', 2),
            (b'/', 3),
            (b'/', 1),
            (b'\\', 0),
            (b'/', 1),
            (b')', 2),
            (b'/', 1),
            (b')', 2),
            (b')', 0),
        ],
    ] {
        let r1 = rotate_n(input.clone(), 1);
        let r5 = rotate_n(input.clone(), 5);
        let expected_diagram = match try_ascii_print::<false>(r1.clone()) {
            Ok(s) => format!("expected:\n{s}"),
            Err(e) => format!("expected: error rendering diagram: {e}"),
        };
        let actual_diagram = match try_ascii_print::<false>(r5.clone()) {
            Ok(s) => format!("actual:\n{s}"),
            Err(e) => format!("actual: error rendering diagram: {e}"),
        };
        assert_eq!(
            r5,
            r1,
            "\noriginal:\n{}\n{}\n{}",
            ascii_print::<false>(input),
            expected_diagram,
            actual_diagram,
        );
    }
}

// Regression: rotating this diagram yields a result that new_from_tuples
// accepts but renders out of bounds in raw_lines::append. A successful
// rotation should always produce a renderable diagram; until try_rotate_90_ccw
// is fixed, rendering the rotated diagram panics with an out-of-bounds index.
// Once fixed, drop #[should_panic] and assert try_ascii_print returns Ok.
#[test]
#[should_panic(expected = "index out of bounds")]
fn rotate_then_render_out_of_bounds_regression() {
    let mut diagram = "(0 (2 (1 (5 \\4 (8 (7 \\4 )3 (8 /9 )8 )2 (7 /7 )8 )6 )2 )1 \\0 )0"
        .parse::<AbbreviatedDiagram>()
        .unwrap();
    diagram.try_rotate_90_ccw().unwrap();
    let _ = diagram.try_ascii_print::<false>();
}

#[test]
fn test_try_wrap_around() {
    assert_eq_after_apply!(
        try_wrap_around,
        1,
        [(b'(', 0), (b'(', 0), (b'\\', 1), (b')', 2), (b')', 0)],
        [(b'(', 0), (b'(', 1), (b'/', 0), (b')', 2), (b')', 0)],
    );
}
