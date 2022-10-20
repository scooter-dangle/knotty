fn main() {
    // TODO: take input from stdin or CLI

    // Unknot:
    //   _
    //  / \
    // <   >
    //  \_/
    //
    let unknot = [(0, b'A'), (0, b'V')];

    println!("{}", knotty::ascii_print(&unknot));

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
    let trefoil = [
        (0, b'A'),
        (2, b'A'),
        (1, b'/'),
        (0, b'\\'),
        (1, b'/'),
        (2, b'V'),
        (0, b'V'),
    ];

    // Square knot:
    //
    //        ___________
    //       /         | \
    //     _/      _   |  \_
    //    /       / \  |
    //   <       <   > |
    //    \_      \_/  |   _
    //      \          |  /
    //       \         | /
    //        \_________/

    println!("{}", knotty::ascii_print(&trefoil));
}
