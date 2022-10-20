fn main() {
    // TODO: take input from stdin or CLI

    // Unknot:
    //   _
    //  / \
    // <   >
    //  \_/
    //
    let unknot = vec![(0, b'A'), (0, b'V')];
    println!("{}", knotty::ascii_print(unknot));

    // Trefoil:
    //     ,______________
    //    /               \
    //   <                 >
    //    \_   ,_____   ,_/
    //      \ /      \ /
    //       \        \
    //   ,__/ \_   ,_/ \____
    //  /       \ /         \
    // <         /           >
    //  \_______/ \_________/
    //
    let trefoil = vec![
        (0, b'A'),
        (2, b'A'),
        (1, b'/'),
        (0, b'\\'),
        (1, b'/'),
        (2, b'V'),
        (0, b'V'),
    ];
    println!("{}", knotty::ascii_print(trefoil));

    // donut:
    let donut = vec![(0, b'A'), (1, b'A'), (1, b'V'), (0, b'V')];
    println!("{}", knotty::ascii_print(donut));

    // C:
    let donut = vec![(0, b'A'), (1, b'A'), (2, b'V'), (0, b'V')];
    println!("{}", knotty::ascii_print(donut));

    // weird terrace thing:
    let terrace = vec![
        (0, b'A'),
        (2, b'A'),
        (4, b'A'),
        (6, b'A'),
        (5, b'V'),
        (3, b'V'),
        // (2, b'/'),
        (1, b'V'),
        // (0, b'/'),
        (1, b'A'),
        // (2, b'/'),
        (3, b'A'),
        (5, b'A'),
        (6, b'V'),
        (4, b'V'),
        (2, b'V'),
        (0, b'V'),
    ];
    println!("{}", knotty::ascii_print(terrace));

    // basket:
    let basket = vec![
        (0, b'A'),
        (1, b'A'),
        (1, b'A'),
        (3, b'/'),
        (2, b'/'),
        (4, b'/'),
        (3, b'/'),
        (1, b'V'),
        (1, b'V'),
        (0, b'V'),
    ];
    println!("{}", knotty::ascii_print(basket));

    // ugly trefoil:
    let ugly_trefoil = vec![
        (0, b'A'),
        (0, b'A'),
        (1, b'/'),
        (0, b'\\'),
        (1, b'/'),
        (0, b'V'),
        (0, b'V'),
    ];
    println!("{}", knotty::ascii_print(ugly_trefoil));
}
