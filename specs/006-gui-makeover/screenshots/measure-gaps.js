const { chromium } = require('playwright');
const zlib = require('zlib');
// Minimal PNG decoder (8-bit RGBA/RGB, non-interlaced) -> {w,h,alpha(x,y)}
function decodePng(buf) {
  let p = 8, w, h, ct, idat = [];
  while (p < buf.length) { const len = buf.readUInt32BE(p); const type = buf.toString('ascii', p+4, p+8); const data = buf.subarray(p+8, p+8+len);
    if (type === 'IHDR') { w = data.readUInt32BE(0); h = data.readUInt32BE(4); ct = data[9]; } else if (type === 'IDAT') idat.push(data); p += 12 + len; }
  const bpp = ct === 6 ? 4 : 3; const raw = zlib.inflateSync(Buffer.concat(idat)); const stride = w*bpp; const out = Buffer.alloc(w*h*bpp);
  let prev = Buffer.alloc(stride);
  for (let y = 0; y < h; y++) { const f = raw[y*(stride+1)]; const line = raw.subarray(y*(stride+1)+1, (y+1)*(stride+1)); const cur = Buffer.alloc(stride);
    for (let i = 0; i < stride; i++) { const a = i >= bpp ? cur[i-bpp] : 0, b = prev[i], c = i >= bpp ? prev[i-bpp] : 0; let x = line[i];
      if (f === 1) x += a; else if (f === 2) x += b; else if (f === 3) x += (a+b)>>1; else if (f === 4) { const pp = a+b-c, pa=Math.abs(pp-a), pb=Math.abs(pp-b), pc=Math.abs(pp-c); x += (pa<=pb&&pa<=pc)?a:(pb<=pc?b:c); }
      cur[i] = x & 255; }
    cur.copy(out, y*stride); prev = cur; }
  return { w, h, dark: (x, y) => { const i = (y*w+x)*bpp; return (out[i]+out[i+1]+out[i+2])/3 < 160; } };
}
function rowGaps(img, x0, x1, y0, y1) { // count gaps (dark-free row runs) between first and last dark rows within a column band
  const rows = []; for (let y = y0; y < y1; y++) { let d = false; for (let x = x0; x < x1; x++) if (img.dark(x, y)) { d = true; break; } rows.push(d); }
  const first = rows.indexOf(true), last = rows.lastIndexOf(true); let gaps = 0, px = 0, inGap = false;
  for (let y = first; y <= last; y++) { if (!rows[y]) { px++; if (!inGap) { gaps++; inGap = true; } } else inGap = false; }
  return { gaps, px };
}
(async () => {
  const b = await chromium.launch(); const S = 4;
  const p = await b.newPage({ viewport: { width: 400, height: 300 }, deviceScaleFactor: S });
  const stacks = { dejavu: "'DejaVu Sans Mono', monospace", liberation: "'Liberation Mono', monospace", freemono: "FreeMono, monospace", unifont: "Unifont, monospace", courier10: "'Courier 10 Pitch', monospace" };
  const lhs = ['normal', '1', '0.9', '0.85', '0.8', '0.75'];
  // Strips, each 3 rows x 3 cols, laid side by side with a blank column between:
  //  A: '\' diagonal across rows            B: '|' column
  //  C: '___' then '\' at col 0 next row    D: '/' , '(' , '\' as in the opening tile
  const rows = [
    ['\\  ', ' | ', '___', '  /'],
    [' \\ ', ' | ', '\\  ', ' ( '],
    ['  \\', ' | ', ' \\ ', '  \\'],
  ].map((r) => r.join(' ')).join('\n');
  console.log('stack       lh      diag       pipe       line->diag   paren-join   lineH');
  for (const [name, stack] of Object.entries(stacks)) for (const lh of lhs) {
    await p.setContent(`<pre id="t" style="margin:0;padding:0;font-family:${stack};font-size:16px;line-height:${lh};display:inline-block">${rows}</pre>`);
    const el = await p.$('#t'); const png = decodePng(await el.screenshot());
    const { cw, lhpx, fam } = await p.evaluate(() => { const t = document.getElementById('t'); const s = document.createElement('span'); s.textContent = 'x'; t.appendChild(s); const cw = s.getBoundingClientRect().width; s.remove(); return { cw, lhpx: t.getBoundingClientRect().height / 3, fam: getComputedStyle(t).fontFamily }; });
    const W = cw * S, H = lhpx * S; const band = (i) => [i * 4 * W, i * 4 * W + 3 * W];
    const g = (i) => { const [x0, x1] = band(i); const r = rowGaps(png, Math.round(x0), Math.round(x1), 0, Math.round(3 * H)); return `${r.gaps},${(r.px / S).toFixed(1)}px`; };
    console.log(name.padEnd(11), lh.padEnd(7), g(0).padEnd(10), g(1).padEnd(10), g(2).padEnd(12), g(3).padEnd(12), lhpx.toFixed(1), fam);
  }
  await b.close();
})();
