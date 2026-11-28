// The picture a link to the site shows in a chat or a feed, web/og.jpg, 1200
// by 630: the site's mark, its name, The Grassroots Sphere, and its one line
// on the left, and on the right the globe of the contracts as the live site
// draws it, whole, its keys left out.
// Usage, with node and Playwright: node tools/og.js <a directory for the
// globe's picture and og.jpg> [zoom, a fraction of the page's own, 0.74]
// [tilt, 0.55, or open for the opening screen's]; then copy og.jpg to web/og.jpg.  Made again when the globe or
// the name changes much (Udi, 2026-11-12, to GSA #5, the name).
const { chromium } = require('playwright');
const fs = require('fs');
(async () => {
  const out = process.argv[2];
  const b = await chromium.launch();
  const p = await b.newPage({ viewport: { width: 1280, height: 1000 }, deviceScaleFactor: 2 });
  // OG_URL, OG_TITLE and OG_LINE make another site's picture: the Villagers
  // site's, web/og-villagers.jpg, from peoplesweb.org/villagers/ (Udi,
  // 2026-11-25: "link preview of villagers should reflect what it is").
  await p.goto(process.env.OG_URL || 'https://peoplesweb.org/', { waitUntil: 'load' }); await p.waitForTimeout(1500);
  // Tilt 'open' keeps the opening screen's tilt: the Villagers site's picture
  // shows its south pole, as its opening screen does (Udi, 2026-11-25).
  const Z = parseFloat(process.argv[3] || '0.5'), T = process.argv[4] === 'open' ? null : parseFloat(process.argv[4] || '0.55');
  console.log('view', await p.evaluate(([z, t]) => { const v0 = JSON.stringify(V); V.zoom = V.zoom * z; if (t !== null) V.tilt = t; drawGlobe();
    document.head.insertAdjacentHTML('beforeend', '<style>#graph .key{display:none}</style>');
    document.body.style.background = 'transparent'; document.documentElement.style.background = 'transparent';
    document.querySelector('#mapcard').style.cssText += ';box-shadow:none;background:transparent;border-radius:0'; return v0 + ' -> ' + JSON.stringify(V); }, [Z, T]));
  const g = await p.$('#graph');
  await g.screenshot({ path: out + '/globe.png', omitBackground: true });
  const box = await g.boundingBox(); console.log('globe', Math.round(box.width), Math.round(box.height));
  const img = fs.readFileSync(out + '/globe.png').toString('base64');
  const q = await b.newPage({ viewport: { width: 1200, height: 630 }, deviceScaleFactor: 1 });
  await q.setContent(`<!DOCTYPE html><html><head><meta charset="utf-8">
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;800&display=swap" rel="stylesheet">
<style>html,body{margin:0}body{width:1200px;height:630px;overflow:hidden;background:linear-gradient(135deg,#f7f7f9,#e9eef6);font-family:Inter,-apple-system,"Segoe UI",sans-serif;color:#1d1d1f;position:relative}
.l{position:absolute;left:64px;top:146px;width:540px}
.mark{width:64px;height:64px;border-radius:18px;background:conic-gradient(from 200deg,#4f7bd9,#2bb3a0,#9b7bd4,#f0b54a,#4f7bd9)}
h1{font-size:62px;line-height:1.08;letter-spacing:-.025em;font-weight:800;margin:26px 0 22px}
p{font-size:25px;line-height:1.4;color:#5f6b78;margin:0;max-width:520px}
img{position:absolute;right:-70px;top:50%;transform:translateY(-50%);height:760px}</style></head>
<body><div class="l"><div class="mark"></div><h1>${process.env.OG_TITLE || 'The Grassroots Sphere'}</h1><p>${process.env.OG_LINE || 'Social contracts that run: apps owned and run by the people who use them, on their own phones.'}</p></div>
<img src="data:image/png;base64,${img}"></body></html>`, { waitUntil: 'load' });
  await q.waitForTimeout(1200);
  await q.screenshot({ path: out + '/og.jpg', type: 'jpeg', quality: 88 });
  await b.close();
})();
