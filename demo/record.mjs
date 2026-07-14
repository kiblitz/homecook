// Homecook demo recorder.
//
//   node demo/record.mjs                # every scenario, both viewports
//   node demo/record.mjs practice       # one scenario, both viewports
//   node demo/record.mjs a b c          # several scenarios
//
// For each scenario it records BOTH a mobile and a desktop video (a hard
// project rule — every feature demo ships both form factors) into demo/out/.
//
// Which build gets recorded (first that exists, or $HOMECOOK_DIST):
//   1. $HOMECOOK_DIST
//   2. ../_build/install/default/bin   (a local `dune build`)
//   3. /tmp/ghpages                    (bundle fetched from the gh-pages branch)
import { chromium } from 'playwright';
import { spawn } from 'node:child_process';
import { readdir, mkdir, rename, rm, access } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { serveDir } from './lib/server.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = join(HERE, 'out');
const CHROME = '/opt/pw-browsers/chromium-1194/chrome-linux/chrome';
// Prefer a full ffmpeg (h264/mp4) if present; Playwright's bundled build is
// webm-only. mp4 plays everywhere (notably iOS), so it is the shareable output.
const FFMPEG = process.env.FFMPEG || 'ffmpeg';

const VIEWPORTS = [
  { name: 'desktop', viewport: { width: 1280, height: 800 }, isMobile: false, hasTouch: false, deviceScaleFactor: 1 },
  { name: 'mobile', viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true, deviceScaleFactor: 2 },
];

async function exists(p) {
  try { await access(p); return true; } catch { return false; }
}

async function resolveDist() {
  const candidates = [
    process.env.HOMECOOK_DIST,
    join(HERE, '..', '_build', 'install', 'default', 'bin'),
    '/tmp/ghpages',
  ].filter(Boolean);
  for (const c of candidates) {
    if (await exists(join(c, 'index.html'))) return c;
  }
  throw new Error(
    'No built site found. Set $HOMECOOK_DIST, run `dune build`, or fetch the ' +
      'gh-pages bundle into /tmp/ghpages.',
  );
}

async function loadScenarios(filter) {
  const dir = join(HERE, 'scenarios');
  const files = (await readdir(dir)).filter((f) => f.endsWith('.mjs'));
  const scenarios = [];
  for (const f of files) {
    const mod = await import(join(dir, f));
    const s = mod.default;
    if (!s?.name || typeof s.run !== 'function') continue;
    if (filter.length === 0 || filter.includes(s.name)) scenarios.push(s);
  }
  scenarios.sort((a, b) => a.name.localeCompare(b.name));
  return scenarios;
}

function toMp4(webm, mp4) {
  return new Promise((resolve) => {
    const ff = spawn(FFMPEG, [
      '-y', '-i', webm,
      '-movflags', 'faststart',
      '-pix_fmt', 'yuv420p',
      // pad to even dimensions (h264 requirement) without cropping
      '-vf', 'pad=ceil(iw/2)*2:ceil(ih/2)*2',
      mp4,
    ], { stdio: 'ignore' });
    ff.on('close', (code) => resolve(code === 0));
    ff.on('error', () => resolve(false));
  });
}

async function recordOne(browser, baseUrl, scenario, vp) {
  const tmpDir = join(OUT, `.tmp-${scenario.name}-${vp.name}`);
  await rm(tmpDir, { recursive: true, force: true });
  const context = await browser.newContext({
    viewport: vp.viewport,
    isMobile: vp.isMobile,
    hasTouch: vp.hasTouch,
    deviceScaleFactor: vp.deviceScaleFactor,
    recordVideo: { dir: tmpDir, size: vp.viewport },
  });
  const page = await context.newPage();
  try {
    await page.goto(baseUrl, { waitUntil: 'domcontentloaded' });
    await scenario.run(page, { viewport: vp });
  } finally {
    await context.close(); // flushes the video file
  }
  // Playwright names the video randomly; move the single file to a stable name.
  const [vid] = (await readdir(tmpDir)).filter((f) => f.endsWith('.webm'));
  const webm = join(OUT, `${scenario.name}.${vp.name}.webm`);
  await rename(join(tmpDir, vid), webm);
  await rm(tmpDir, { recursive: true, force: true });
  const mp4 = join(OUT, `${scenario.name}.${vp.name}.mp4`);
  const ok = await toMp4(webm, mp4);
  return ok ? mp4 : webm;
}

async function main() {
  const filter = process.argv.slice(2);
  const dist = await resolveDist();
  const scenarios = await loadScenarios(filter);
  if (scenarios.length === 0) {
    console.error('No scenarios matched:', filter.join(', ') || '(all)');
    process.exit(1);
  }
  await mkdir(OUT, { recursive: true });
  const server = await serveDir(dist);
  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });
  console.log(`serving ${dist} at ${server.url}`);
  const made = [];
  try {
    for (const scenario of scenarios) {
      for (const vp of VIEWPORTS) {
        process.stdout.write(`recording ${scenario.name} [${vp.name}] ... `);
        const out = await recordOne(browser, server.url, scenario, vp);
        console.log(out.replace(HERE + '/', ''));
        made.push(out);
      }
    }
  } finally {
    await browser.close();
    await server.close();
  }
  console.log('\nDone. Videos:');
  for (const m of made) console.log('  ' + m);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
