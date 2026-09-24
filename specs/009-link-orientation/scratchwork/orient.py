"""Throwaway prototype: link orientation for knotty abbreviated notation.

Ports: opening -> RU, RL ; closing -> LU, LL ; crossing -> LU, LL, RU, RL.
A segment joins a right port of an earlier feature to a left port of a later one.

With --debug, prints the stack after every change during the sweep, and the
partial outputs (2) and (3) after every assignment during the walk.
"""
import argparse
import sys

class Malformed(Exception):
    pass

PORT_WORDS = {'RU': 'upper-right', 'RL': 'lower-right', 'LU': 'upper-left', 'LL': 'lower-left'}
# A crossing's two strands: upper-left <-> lower-right (descending), lower-left <-> upper-right (ascending)
THROUGH = {'LU': 'RL', 'RL': 'LU', 'LL': 'RU', 'RU': 'LL'}

def port(p):
    return f"{p[0]}.{p[1]}"

class Trace:
    """Debug output: the sweep's stack, and the walk's partial outputs (2) and (3)."""

    def __init__(self, items):
        self.items = items
        self.tokens = [f"{e}{k}" for e, k in items]
        digits = len(str(max(sum(e == '(' for e, _ in items) - 1, 0)))
        self.widths = [max(len(t), 5, 3 + 2 * digits) for t in self.tokens]
        print("legend: (2) component number, or (upper, lower) for a crossing; "
              "(3) (upper, lower) with + = Pos, - = Neg; ? = not assigned yet")
        print("        stack entries and ports are <feature>.<port>, e.g. 1.RU = "
              "feature #1's upper-right port")

    def row(self, label, cells):
        return (label + ' '.join(c.ljust(w) for c, w in zip(cells, self.widths))).rstrip()

    def caret(self, i):
        print(self.row('    ', [f"^#{i}" if j == i else '' for j in range(len(self.items))]))

    def section(self, title):
        print(f"--- {title}")

    def sweep(self, i, what, stack):
        print(f"sweep #{i} {self.tokens[i]}: {what}")
        print(self.row('    ', self.tokens))
        self.caret(i)
        print(f"    stack, bottom to top: [{', '.join(port(p) for p in stack)}]")
        print()

    def walk(self, i, what, comp, dirs):
        known = lambda x: '?' if x is None else str(x)
        sym = lambda x: '?' if x is None else '+' if x == 'Pos' else '-'
        p2, p3 = [], []
        for (e, _), c, d in zip(self.items, comp, dirs):
            if e in '()':
                p2.append(known(c[0] if c[0] is not None else c[1]))
            else:
                p2.append(f"({known(c[0])},{known(c[1])})")
            p3.append(f"({sym(d[0])},{sym(d[1])})")
        print(f"walk: {what}")
        print(self.row('    ', self.tokens))
        print(self.row('(2) ', p2))
        print(self.row('(3) ', p3))
        self.caret(i)
        print()

def parse(s):
    out = []
    for tok in s.split():
        e, k = tok[0], int(tok[1:])
        assert e in '()/\\'
        out.append((e, k))
    return out

def segments(items, trace=None):
    # stack[pos] = (feature_idx, port) of the right port that started the segment at that position
    stack = []
    right_end = {}  # (feat, rport) -> (feat2, lport)
    left_end = {}   # (feat, lport) -> (feat0, rport)
    if trace:
        trace.section("sweep: link each segment's start port to its end port, tracking strands by position")
    for i, (e, k) in enumerate(items):
        h = len(stack)
        if e == '(':
            if not (0 <= k <= h):
                raise Malformed(f"feature {i} ({e}{k}): opening index out of range for {h} strands")
            stack[k:k] = [(i, 'RL'), (i, 'RU')]
            if trace:
                trace.sweep(i, f"insert {i}.RL at position {k} and {i}.RU at {k + 1}", stack)
        else:
            if not (0 <= k and k + 1 <= h - 1):
                raise Malformed(f"feature {i} ({e}{k}): needs strands {k},{k+1} but only {h}")
            lo, up = stack[k], stack[k + 1]
            right_end[lo] = (i, 'LL'); left_end[(i, 'LL')] = lo
            right_end[up] = (i, 'LU'); left_end[(i, 'LU')] = up
            if e == ')':
                del stack[k:k + 2]
                if trace:
                    trace.sweep(i, f"segments {port(lo)} -> {i}.LL (position {k}) and "
                                   f"{port(up)} -> {i}.LU (position {k + 1}) end here; "
                                   f"remove positions {k},{k + 1}", stack)
            else:
                # descending strand: LU -> RL ; ascending: LL -> RU
                stack[k] = (i, 'RL')
                stack[k + 1] = (i, 'RU')
                if trace:
                    trace.sweep(i, f"segments {port(lo)} -> {i}.LL (position {k}) and "
                                   f"{port(up)} -> {i}.LU (position {k + 1}) end here; "
                                   f"{i}.RL starts at {k} and {i}.RU at {k + 1}", stack)
    if stack:
        raise Malformed(f"{len(stack)} strand(s) left open at end of diagram")
    return right_end, left_end

def orient(items, debug=False):
    trace = Trace(items) if debug else None
    right_end, left_end = segments(items, trace)
    comp = [None] * len(items)   # opening/closing: int ; crossing: [upper-left, lower-left]
    dirs = [None] * len(items)   # [upper, lower] of 'Pos'/'Neg'
    for i, (e, _) in enumerate(items):
        comp[i] = [None, None]; dirs[i] = [None, None]
    refs = []
    # slot index: 0 = upper, 1 = lower. For crossings: 0 = upper-left (descending), 1 = lower-left (ascending)
    def slot(e, port):
        if e == '(':
            return {'RU': 0, 'RL': 1}[port]
        if e == ')':
            return {'LU': 0, 'LL': 1}[port]
        return {'LU': 0, 'RL': 0, 'LL': 1, 'RU': 1}[port]
    def mark(i, port, c, d, action):
        e = items[i][0]
        s = slot(e, port)
        if comp[i][s] is not None:
            raise AssertionError(f"feature {i} slot {s} visited twice")
        comp[i][s] = c; dirs[i][s] = d
        if trace:
            feature = f"#{i} {e}{items[i][1]}"
            which = ('upper', 'lower')[s]
            heading = 'right' if d == 'Pos' else 'left'
            result = f"{which} = component {c}, {'+' if d == 'Pos' else '-'}"
            what = {
                'leave': f"component {c} starts at reference opening {feature}; "
                         f"leave along its upper strand heading right: {result}",
                'arrive': f"c{c} heading {heading} arrives at {feature} on its {which} strand: {result}",
                'turn': f"c{c} turns back at {feature} onto its {which} strand, "
                        f"now heading {heading}: {result}",
                'pass': f"c{c} heading {heading} passes through {feature} from "
                        f"{PORT_WORDS[port]} to {PORT_WORDS[THROUGH[port]]}: {result}",
            }[action]
            trace.walk(i, what, comp, dirs)
    if trace:
        trace.section("walk: follow each loop from its reference opening, filling outputs (2) and (3)")
    for i, (e, _) in enumerate(items):
        if e != '(' or comp[i][0] is not None:
            continue
        c = len(refs); refs.append(i)
        # Leave reference opening via RU, heading right.
        mark(i, 'RU', c, 'Pos', 'leave')
        cur = (i, 'RU'); heading = 'R'
        while True:
            if heading == 'R':
                f, lport = right_end[cur]
                fe = items[f][0]
                if fe == ')':
                    mark(f, lport, c, 'Pos', 'arrive')
                    other = 'LL' if lport == 'LU' else 'LU'
                    mark(f, other, c, 'Neg', 'turn')
                    cur = (f, other); heading = 'L'
                elif fe in '/\\':
                    mark(f, lport, c, 'Pos', 'pass')
                    cur = (f, THROUGH[lport])
                else:
                    raise AssertionError('segment cannot end at opening from the left')
            else:
                f, rport = left_end[cur]
                fe = items[f][0]
                if fe == '(':
                    mark(f, rport, c, 'Neg', 'arrive')
                    other = 'RL' if rport == 'RU' else 'RU'
                    if (f, other) == (i, 'RU'):
                        if trace:
                            print(f"walk: c{c} is back where it started: loop closed\n")
                        break
                    mark(f, other, c, 'Pos', 'turn')
                    cur = (f, other); heading = 'R'
                elif fe in '/\\':
                    mark(f, rport, c, 'Neg', 'pass')
                    cur = (f, THROUGH[rport])
                else:
                    raise AssertionError('segment cannot start at closing')
    comps = []
    for i, (e, _) in enumerate(items):
        assert None not in comp[i] and None not in dirs[i]
        if e in '()':
            assert comp[i][0] == comp[i][1]
            assert set(dirs[i]) == {'Pos', 'Neg'}
            comps.append(comp[i][0])
        else:
            comps.append(tuple(comp[i]))
    return refs, comps, [tuple(d) for d in dirs]

def sign(items, dirs):
    # informative only: + for (\ and product +1) etc. Global handedness flip does not matter for checks.
    out = []
    for (e, _), d in zip(items, dirs):
        if e in '/\\':
            p = (1 if d[0] == 'Pos' else -1) * (1 if d[1] == 'Pos' else -1)
            out.append(p if e == '\\' else -p)
        else:
            out.append(None)
    return out

def fmt(refs, comps, dirs):
    c = ', '.join(str(x) if isinstance(x, int) else f"({x[0]}, {x[1]})" for x in comps)
    d = ', '.join(f"({a}, {b})" for a, b in dirs)
    return f"1. vec!{refs}\n2. vec![{c}]\n3. vec![{d}]"

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Orient knotty diagrams read from stdin, one per line.")
    parser.add_argument('--debug', action='store_true',
                        help="trace every stack change in the sweep and every assignment in the walk")
    args = parser.parse_args()
    for line in sys.stdin:
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        items = parse(line)
        print('===', line)
        try:
            r = orient(items, debug=args.debug)
            print(fmt(*r))
            print('   crossing signs:', [s for s in sign(items, r[2]) if s is not None])
        except Malformed as m:
            print('   MALFORMED:', m)
