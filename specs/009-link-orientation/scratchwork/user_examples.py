from orient import parse, orient
P, N = 'Pos', 'Neg'
cases = [
  ('(0 )0', [0], [0, 0], [(P, N), (P, N)]),
  ('(0 /0 )0', [0], [0, (0, 0), 0], [(P, N), (P, N), (N, P)]),
  ('(0 (0 )0 )0', [0, 1], [0, 1, 1, 0], [(P, N)] * 4),
  ('(0 (1 /0 /2 )1 )0', [0, 1], [0, 1, (1, 0), (0, 1), 0, 1], [(P, N), (P, N), (N, N), (P, P), (P, N), (P, N)]),
  ('(0 (2 /1 \\0 /1 )2 )0', [0], [0, 0, (0, 0), (0, 0), (0, 0), 0, 0], [(P, N), (P, N), (N, P), (N, N), (P, N), (P, N), (P, N)]),
  ('(0 (2 /2 )1 )0', [0], [0, 0, (0, 0), 0, 0], [(P, N), (N, P), (N, P), (N, P), (P, N)]),
]
ok = True
for s, r1, r2, r3 in cases:
    got = orient(parse(s))
    exp = (r1, r2, r3)
    status = 'OK ' if got == exp else 'MISMATCH'
    ok &= got == exp
    print(status, s)
    if got != exp:
        print('  expected', exp); print('  got     ', got)
print('ALL USER EXAMPLES MATCH' if ok else 'SOME MISMATCH')
