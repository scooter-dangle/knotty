from orient import orient, sign
import itertools, time

def gen(n):
    """All well-formed feature sequences of exactly n features."""
    def rec(prefix, h, left):
        if left == 0:
            if h == 0:
                yield list(prefix)
            return
        if h > 2 * left:  # cannot close remaining strands
            return
        for k in range(h + 1):
            prefix.append(('(', k)); yield from rec(prefix, h + 2, left - 1); prefix.pop()
        for k in range(h - 1):
            for e in ')/\\':
                prefix.append((e, k)); yield from rec(prefix, h - 2 if e == ')' else h, left - 1); prefix.pop()
    yield from rec([], 0, n)

def components_uf(items):
    # independent component count via union-find over segment endpoints
    parent = {}
    def find(x):
        parent.setdefault(x, x)
        while parent[x] != x:
            parent[x] = parent[parent[x]]; x = parent[x]
        return x
    def union(a, b): parent[find(a)] = find(b)
    stack = []; seg = 0
    for i, (e, k) in enumerate(items):
        if e == '(':
            a, b = seg, seg + 1; seg += 2
            union(a, b); stack[k:k] = [a, b]
        elif e == ')':
            union(stack[k], stack[k + 1]); del stack[k:k + 2]
        else:
            lo, up = stack[k], stack[k + 1]
            a, b = seg, seg + 1; seg += 2
            union(up, a); union(lo, b)  # upper-left -> new lower ; lower-left -> new upper
            stack[k], stack[k + 1] = a, b
    return len({find(x) for x in range(seg)})

def invariants(items):
    refs, comps, dirs = orient(items)
    s = sign(items, dirs)
    self_w = 0; lk = {}
    for (e, _), c, sg in zip(items, comps, s):
        if sg is None: continue
        if c[0] == c[1]: self_w += sg
        else:
            key = tuple(sorted(c)); lk[key] = lk.get(key, 0) + sg
    assert all(v % 2 == 0 for v in lk.values()), lk
    return len(refs), self_w, sum(abs(v) // 2 for v in lk.values())

def r2_insertions(items):
    h = 0; hs = [0]
    for e, k in items:
        h += 2 if e == '(' else -2 if e == ')' else 0; hs.append(h)
    for idx in range(len(items) + 1):
        for v in range(hs[idx] - 1):
            for a, b in (('/', '\\'), ('\\', '/')):
                yield items[:idx] + [(a, v), (b, v)] + items[idx:]

total = 0; t = time.time()
for n in range(0, 9):
    cnt = 0
    for items in gen(n):
        cnt += 1
        refs, comps, dirs = orient(items)  # internal asserts: each slot once, open/close Pos+Neg
        assert len(refs) == components_uf(items)
        for r in refs:
            assert dirs[r] == ('Pos', 'Neg')
        # crossing glyph independence
        flipped = [(('\\' if e == '/' else '/' if e == '\\' else e), k) for e, k in items]
        assert orient(flipped) == (refs, comps, dirs)
        if n <= 6:
            inv = invariants(items)
            for m in r2_insertions(items):
                assert invariants(m) == inv, (items, m)
    total += cnt
    print(f"n={n}: {cnt} well-formed diagrams (cumulative {total})")
print(f"all checks passed in {time.time()-t:.1f}s")
