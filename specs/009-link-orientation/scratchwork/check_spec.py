import re
from pathlib import Path
from orient import parse, orient, fmt, Malformed, sign
spec = (Path(__file__).resolve().parent.parent / 'spec.md').read_text()
blocks = re.findall(r'^(W\d+)\s+(.+?)\s{2,}.*\n\s+\(1\) (.+)\n\s+\(2\) (.+)\n\s+\(3\) (.+)$', spec, re.M)
assert len(blocks) == 13, len(blocks)
ok = True
for wid, diag, o1, o2, o3 in blocks:
    refs, comps, dirs = orient(parse(diag))
    exp = fmt(refs, comps, dirs).replace('vec!', '').splitlines()
    got = ['1. ' + o1, '2. ' + o2, '3. ' + o3]
    good = exp == got
    ok &= good
    print(('OK ' if good else 'BAD'), wid, diag)
    if not good:
        print('  spec :', got); print('  model:', exp)
ms = re.findall(r'^(M\d+)\s+(.+?)\s{2,}(position (\d+)|end of diagram)', spec, re.M)
assert len(ms) == 7, ms
for mid, diag, _, pos in ms:
    try:
        orient(parse(diag)); print('BAD', mid, 'accepted'); ok = False
    except Malformed as m:
        msg = str(m)
        good = (f'feature {pos} ' in msg) if pos else ('left open' in msg)
        ok &= good
        print(('OK ' if good else 'BAD'), mid, diag, '->', msg)
# SC-004 known values
def signs(d):
    it = parse(d); return [s for s in sign(it, orient(it)[2]) if s is not None]
print('W4', signs('(0 (1 /0 /2 )1 )0'), 'W11', signs('(0 (2 /1 \\1 )2 )0'),
      'W4-rev', signs('(0 (1 /0 \\2 )1 )0'), 'W5', signs('(0 (2 /1 \\0 /1 )2 )0'),
      'W2', signs('(0 /0 )0'), 'W9', signs('(0 \\0 )0'))
print('SPEC EXAMPLES VERIFIED' if ok else 'SPEC HAS ERRORS')
