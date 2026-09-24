"""Throwaway prototype: link orientation for knotty abbreviated notation.

Ports: opening -> RU, RL ; closing -> LU, LL ; crossing -> LU, LL, RU, RL.
A segment joins a right port of an earlier feature to a left port of a later one.
"""
import sys

class Malformed(Exception):
    pass

def parse(s):
    out = []
    for tok in s.split():
        e, k = tok[0], int(tok[1:])
        assert e in '()/\\'
        out.append((e, k))
    return out

def segments(items):
    # stack[pos] = (feature_idx, port) of the right port that started the segment at that position
    stack = []
    right_end = {}  # (feat, rport) -> (feat2, lport)
    left_end = {}   # (feat, lport) -> (feat0, rport)
    for i, (e, k) in enumerate(items):
        h = len(stack)
        if e == '(':
            if not (0 <= k <= h):
                raise Malformed(f"feature {i} ({e}{k}): opening index out of range for {h} strands")
            stack[k:k] = [(i, 'RL'), (i, 'RU')]
        else:
            if not (0 <= k and k + 1 <= h - 1):
                raise Malformed(f"feature {i} ({e}{k}): needs strands {k},{k+1} but only {h}")
            lo, up = stack[k], stack[k + 1]
            right_end[lo] = (i, 'LL'); left_end[(i, 'LL')] = lo
            right_end[up] = (i, 'LU'); left_end[(i, 'LU')] = up
            if e == ')':
                del stack[k:k + 2]
            else:
                # descending strand: LU -> RL ; ascending: LL -> RU
                stack[k] = (i, 'RL')
                stack[k + 1] = (i, 'RU')
    if stack:
        raise Malformed(f"{len(stack)} strand(s) left open at end of diagram")
    return right_end, left_end

def orient(items):
    right_end, left_end = segments(items)
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
    def mark(i, port, c, d):
        e = items[i][0]
        s = slot(e, port)
        if comp[i][s] is not None:
            raise AssertionError(f"feature {i} slot {s} visited twice")
        comp[i][s] = c; dirs[i][s] = d
    for i, (e, _) in enumerate(items):
        if e != '(' or comp[i][0] is not None:
            continue
        c = len(refs); refs.append(i)
        # Leave reference opening via RU, heading right.
        mark(i, 'RU', c, 'Pos')
        cur = (i, 'RU'); heading = 'R'
        while True:
            if heading == 'R':
                f, lport = right_end[cur]
                fe = items[f][0]
                if fe == ')':
                    mark(f, lport, c, 'Pos')
                    other = 'LL' if lport == 'LU' else 'LU'
                    mark(f, other, c, 'Neg')
                    cur = (f, other); heading = 'L'
                elif fe in '/\\':
                    mark(f, lport, c, 'Pos')
                    cur = (f, {'LU': 'RL', 'LL': 'RU'}[lport])
                else:
                    raise AssertionError('segment cannot end at opening from the left')
            else:
                f, rport = left_end[cur]
                fe = items[f][0]
                if fe == '(':
                    mark(f, rport, c, 'Neg')
                    other = 'RL' if rport == 'RU' else 'RU'
                    if (f, other) == (i, 'RU'):
                        break
                    mark(f, other, c, 'Pos')
                    cur = (f, other); heading = 'R'
                elif fe in '/\\':
                    mark(f, rport, c, 'Neg')
                    cur = (f, {'RL': 'LU', 'RU': 'LL'}[rport])
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
    for line in sys.stdin:
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        items = parse(line)
        print('===', line)
        try:
            r = orient(items)
            print(fmt(*r))
            print('   crossing signs:', [s for s in sign(items, r[2]) if s is not None])
        except Malformed as m:
            print('   MALFORMED:', m)
