// Drives the built knot-so-good app through a set of states and screenshots
// each one at a desktop and a mobile viewport.
const { chromium } = require('playwright');
const fs = require('fs');
const path = require('path');

const BASE = process.env.BASE_URL || 'http://127.0.0.1:8123/';
const OUT = process.env.OUT_DIR;
fs.mkdirSync(OUT, { recursive: true });

const VIEWPORTS = {
  desktop: { viewport: { width: 1280, height: 800 } },
  mobile: { viewport: { width: 390, height: 844 }, deviceScaleFactor: 2, isMobile: true, hasTouch: true },
};

const TREFOIL_TEXT = ['..___..', '.(._.).', '._y.y_.', '(__x__)'].join('\n');

async function run(name, opts) {
  const browser = await chromium.launch();
  const context = await browser.newContext(opts);
  const page = await context.newPage();
  const measurements = {};
  const shot = async (state) => {
    await page.waitForTimeout(150);
    await page.screenshot({ path: path.join(OUT, `${name}-${state}.png`), fullPage: true });
    const h = await page.evaluate(() => document.documentElement.scrollHeight);
    const w = await page.evaluate(() => document.documentElement.scrollWidth);
    measurements[state] = { pageHeight: h, pageWidth: w };
    console.log(`${name}/${state}: ${w}x${h}`);
  };
  const button = (label) => page.getByRole('button', { name: label, exact: true });
  // Segmented pairs: the radio itself is visually hidden, so select an
  // option the way a person does, by clicking its label.
  const radio = (label) => ({
    check: () => page.locator('.segmented label').filter({ hasText: new RegExp(`^${label}$`) }).click(),
  });
  const textareas = () => page.locator('textarea');
  // Page coordinates, so an earlier scroll (typing into a picker scrolls it
  // into view) does not masquerade as a layout shift.
  const topOf = (locator) => locator.evaluate((el) => el.getBoundingClientRect().top + window.scrollY);

  await page.goto(BASE);
  await page.evaluate(() => localStorage.clear());
  await page.reload();
  await page.waitForSelector('button');

  // 1. Fresh empty notation mode.
  await shot('notation-empty');

  // 2. Trefoil, SVG display.
  await button('trefoil').click();
  await shot('notation-trefoil-svg');
  measurements.notationValidTextareaTop = await topOf(textareas().first());

  // 3. Trefoil, ASCII display (full, then compact).
  await radio('characters').check();
  await shot('notation-trefoil-ascii');
  await radio('compact').check();
  await shot('notation-trefoil-ascii-compact');
  await radio('full').check();

  // 4. Large knot: 5_1 plus several complecting moves.
  await button('knot 5_1').click();
  for (let i = 0; i < 6; i++) {
    const opts = await page.$$eval('#complecting-moves option', (os) => os.map((o) => o.value));
    if (!opts.length) break;
    const pick = opts.find((o) => /reid2|bulge/i.test(o)) || opts[0];
    const input = page.locator('input[list="complecting-moves"]');
    await input.fill(pick);
    await input.press('Enter');
    await page.waitForTimeout(100);
  }
  await shot('notation-large-ascii');
  await radio('picture').check();
  await shot('notation-large-svg');

  // 5. Error states.
  await textareas().first().fill('(0 (2 xx');
  await shot('notation-diagram-error');
  measurements.notationErrorTextareaTop = await topOf(textareas().first());
  await button('trefoil').click();
  await textareas().nth(1).fill('not a move');
  await shot('notation-moves-error');
  await textareas().nth(1).fill('');

  // 6. Many snapshots.
  const knots = ['unknot', 'trefoil', 'square knot', 'knot 5_1'];
  for (let i = 0; i < 9; i++) {
    await button(knots[i % knots.length]).click();
    if (i >= 4) await button('rotate 90° CCW').click();
    await button('snapshot').click();
  }
  await shot('notation-many-snapshots');

  // 7. Manual mode, empty.
  for (let i = 0; i < 9; i++) await button('delete').first().click();
  await textareas().first().fill('');
  await radio('manual').check();
  await page.locator('textarea.manual-input').fill('');
  await shot('manual-empty');
  measurements.manualEmptyTextareaTop = await topOf(page.locator('textarea.manual-input'));

  // 8. Manual trefoil, plain and bordered.
  await page.locator('textarea.manual-input').fill(TREFOIL_TEXT);
  await shot('manual-trefoil');
  measurements.manualValidTextareaTop = await topOf(page.locator('textarea.manual-input'));
  await radio('bordered').check();
  await shot('manual-trefoil-bordered');
  await radio('plain').check();

  // 9. Manual error (keeps stale render).
  await page.locator('textarea.manual-input').fill(TREFOIL_TEXT + '\n?');
  await shot('manual-error');
  measurements.manualErrorTextareaTop = await topOf(page.locator('textarea.manual-input'));

  // 10. Manual many snapshots.
  await page.locator('textarea.manual-input').fill(TREFOIL_TEXT);
  for (let i = 0; i < 9; i++) {
    await page.locator('textarea.manual-input').fill('.'.repeat(i + 1) + '\n' + TREFOIL_TEXT);
    await button('snapshot').click();
  }
  await shot('manual-many-snapshots');

  // 11. Corrupt storage on load.
  await page.evaluate(() => localStorage.setItem('knotty_state', '{not json'));
  await page.reload();
  await page.waitForSelector('button');
  await shot('storage-error');

  fs.writeFileSync(path.join(OUT, `${name}-measurements.json`), JSON.stringify(measurements, null, 2));
  await browser.close();
}

(async () => {
  for (const [name, opts] of Object.entries(VIEWPORTS)) await run(name, opts);
})().catch((e) => { console.error(e); process.exit(1); });
