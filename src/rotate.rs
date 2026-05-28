use std::sync::LazyLock;

use regex::Regex;

static ROTATE_OPEN_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"/_*\\").unwrap());
static ROTATE_CLOSE_UNDERSCORE_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r" _+ ").unwrap());
static ROTATE_CENTERED_SLASH_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r" / ").unwrap());
static ROTATE_CENTERED_BACKSLASH_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r" \\ ").unwrap());
static ROTATE_PREV_CLOSE_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\\/").unwrap());
static ROTATE_PREV_X_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\\ /").unwrap());

pub(crate) fn scan_row(cur: &str, prev: Option<&str>) -> Vec<(u8, usize)> {
    let prev_padded = prev.map(|p| {
        let mut s = p.to_string();
        if s.len() < cur.len() {
            s.extend(std::iter::repeat(' ').take(cur.len() - s.len()));
        }
        s
    });

    let mut closes: Vec<(u8, usize)> = Vec::new();
    let mut others: Vec<(u8, usize)> = Vec::new();
    let mut col = 0;
    let mut other_depth: usize = 0;
    let mut close_depth: usize = 0;

    while col < cur.len() {
        let cur_tail = &cur[col..];

        if let Some(mat) = ROTATE_OPEN_RE.find(cur_tail) {
            if mat.start() == 0 {
                let match_len = mat.end();
                // match_len % 3 == 0: TUS→TDF (always spurious; suppress)
                // match_len % 3 != 0: legitimate arc base
                if match_len % 3 != 0 {
                    others.push((b'(', other_depth));
                    other_depth += 2;
                }
                col += match_len;
                continue;
            }
        }

        if let Some(mat) = ROTATE_CLOSE_UNDERSCORE_RE.find(cur_tail) {
            if mat.start() == 0 {
                let match_len = mat.end();
                // A single-underscore match (" _ ", len=3) where the '_' is at
                // scan (col+1)%3==0 can be a spurious TransferUpFinish artifact.
                // Legitimate arc closes always land at even close_depth; odd means
                // a non-arc char (e.g. ClosedAbove's '/' in l0) shifted the counter.
                let spurious = match_len == 3 && (col + 1) % 3 == 0 && close_depth % 2 == 1;
                if !spurious {
                    closes.push((b')', close_depth));
                    close_depth += 2;
                    other_depth += 2;
                }
                col += match_len;
                continue;
            }
        }

        if cur_tail.starts_with("  ") {
            if let Some(prev_str) = prev_padded.as_deref() {
                let prev_tail = &prev_str[col..];
                if let Some(prev_mat) = ROTATE_PREV_CLOSE_RE.find(prev_tail) {
                    if prev_mat.start() == 0 {
                        closes.push((b')', close_depth));
                        close_depth += 2;
                        other_depth += 2;
                        col += 2;
                        continue;
                    }
                }
            }
        }

        if let Some(mat) = ROTATE_CENTERED_SLASH_RE.find(cur_tail) {
            if mat.start() == 0 {
                if let Some(prev_str) = prev_padded.as_deref() {
                    let prev_tail = &prev_str[col..];
                    if let Some(prev_mat) = ROTATE_PREV_X_RE.find(prev_tail) {
                        if prev_mat.start() == 0 {
                            others.push((b'\\', other_depth));
                            other_depth += 2;
                            col += mat.end();
                            continue;
                        }
                    }
                }
            }
        }

        if let Some(mat) = ROTATE_CENTERED_BACKSLASH_RE.find(cur_tail) {
            if mat.start() == 0 {
                if let Some(prev_str) = prev_padded.as_deref() {
                    let prev_tail = &prev_str[col..];
                    if let Some(prev_mat) = ROTATE_PREV_X_RE.find(prev_tail) {
                        if prev_mat.start() == 0 {
                            others.push((b'/', other_depth));
                            other_depth += 2;
                            col += mat.end();
                            continue;
                        }
                    }
                }
            }
        }

        if matches!(cur.as_bytes()[col], b'(' | b')' | b'/' | b'\\') {
            other_depth += 1;
            close_depth += 1;
        }
        col += 1;
    }

    let mut out = Vec::with_capacity(closes.len() + others.len());
    for close in closes.into_iter().rev() {
        out.push(close);
    }
    out.extend(others);
    out
}

#[cfg(test)]
mod test_scan_row {
    use super::scan_row;

    fn reverse_line(line: &str) -> String {
        line.chars().rev().collect()
    }

    macro_rules! test {
        ($name:ident($line1:literal $line2:literal, $output:expr $(,)?)) => {
            #[test]
            fn $name() {
                let cur: String = reverse_line($line1);
                let prev: String = reverse_line($line2);
                assert_eq!(scan_row(&cur, Some(prev.as_str())), $output);
            }
        };
    }

    // All rows from R^4 of rando_link = (0 (2 (4 /3 /0 /3 )4 )2 )0.
    // Each test shows a diagram snippet with cur on top, prev on bottom,
    // in natural left-to-right reading order (the macro reverses them for scan_row).

    test!(row_02(
        r"  \_________/ \_________/  "
        r"",
        vec![(b'(', 0), (b'(', 2)],
    ));

    test!(row_03(
        r" (           /           ) "
        r"  \_________/ \_________/  ",
        vec![(b'\\', 1)],
    ));

    test!(row_04(
        r"  /         \ /         \  "
        r" (           /           ) ",
        vec![],
    ));

    test!(row_05(
        r"   _________   _________   "
        r"  /         \ /         \  ",
        vec![(b')', 2), (b')', 0)],
    ));

    test!(row_08(
        r"     \_______________/     "
        r"",
        vec![(b'(', 0)],
    ));

    test!(row_09(
        r"    (                 )    "
        r"     \_______________/     ",
        vec![],
    ));

    test!(row_10(
        r"     /               \     "
        r"    (                 )    ",
        vec![],
    ));

    test!(row_11(
        r"      ___/ \___/ \___      "
        r"     /               \     ",
        vec![(b'(', 1)],
    ));

    test!(row_12(
        r"          /     /          "
        r"      ___/ \___/ \___      ",
        vec![(b'\\', 0), (b'\\', 2)],
    ));

    test!(row_13(
        r"         \ /   \ /         "
        r"          /     /          ",
        vec![],
    ));

    test!(row_14(
        r"        \   ___   /        "
        r"         \ /   \ /         ",
        vec![(b')', 1)],
    ));

    test!(row_15(
        r"       (           )       "
        r"        \   ___   /        ",
        vec![],
    ));

    test!(row_16(
        r"        /         \        "
        r"       (           )       ",
        vec![],
    ));

    test!(row_17(
        r"         _________         "
        r"        /         \        ",
        vec![(b')', 0)],
    ));

    // All rows from rando_link rotated diagram (rows 0–9).

    test!(rando_link_rotated_row_00(
        r""
        r"        ____   _            ____    ",
        vec![],
    ));

    test!(rando_link_rotated_row_01(
        r"        ____   _            ____    "
        r"       /    \ / \          /    \   ",
        vec![(b')', 4), (b')', 2), (b')', 0)],
    ));

    test!(rando_link_rotated_row_02(
        r"       /    \ / \          /    \   "
        r"      /      \   \        /      )  ",
        vec![],
    ));

    test!(rando_link_rotated_row_03(
        r"      /      \   \        /      )  "
        r"     /   ___/ \   \      /  _   /   ",
        vec![(b'/', 3)],
    ));

    test!(rando_link_rotated_row_04(
        r"     /   ___/ \   \      /  _   /   "
        r"    /   /      \   \    /  / \ /    ",
        vec![],
    ));

    test!(rando_link_rotated_row_05(
        r"    /   /      \   \    /  / \ /    "
        r"   /   (        )   \  /  /   \     ",
        vec![],
    ));

    test!(rando_link_rotated_row_06(
        r"   /   (        )   \  /  /   \     "
        r"  /     \   ___/     \/  /   / \__  ",
        vec![],
    ));

    test!(rando_link_rotated_row_07(
        r"  /     \   ___/     \/  /   / \__  "
        r" /       \ /            /   /     \ ",
        vec![],
    ));

    test!(rando_link_rotated_row_08(
        r" /       \ /            /   /     \ "
        r"(         \            /   (       )",
        vec![],
    ));

    test!(rando_link_rotated_row_09(
        r"(         \            /   (       )"
        r" \_______/ \__________/     \_____/ ",
        vec![],
    ));
}
