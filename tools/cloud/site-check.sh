#!/bin/bash
# The check before every merge to GLP main (Udi, 2026-11-24, through the coordinator: "before every merge to GLP main, build the site and check it in a browser"): build the site of this tree into a scratch directory, serve it, load it in Chromium, and fail on a console or page error, a globe not drawn, or no contract listed.
#   bash tools/cloud/site-check.sh      exit 0 if the site is whole
set -e
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
OUT=$(mktemp -d)
trap 'kill $SRV 2>/dev/null; rm -rf "$OUT"' EXIT
GLP_ROOT="$ROOT" BUILD_DIR="$OUT" CERTIFY=0 python3 -I "$ROOT/servers/peoplesweb/tools/build.py" > "$OUT/build.log" 2>&1 || { tail -20 "$OUT/build.log"; echo "site-check: the build failed"; exit 1; }
PORT=$((20000 + RANDOM % 20000))
(cd "$OUT/www" && exec python3 -m http.server $PORT > /dev/null 2>&1) & SRV=$!
sleep 1
cat > "$OUT/check.js" <<JS
const { chromium } = require('playwright');
(async () => { const b = await chromium.launch(); const p = await b.newPage(); const errs = [];
  p.on('console', m => { if (m.type() === 'error') errs.push(m.text()); });
  p.on('pageerror', e => errs.push('page error: ' + e.message));
  await p.goto('http://localhost:$PORT/index.html'); await p.waitForTimeout(4000);
  const svg = await p.evaluate(() => document.querySelectorAll('svg *').length);
  const n = await p.evaluate(() => (window.DATA && window.DATA.contracts || []).length);
  await b.close();
  console.log('site-check: ' + n + ' contracts, ' + svg + ' svg elements, ' + errs.length + ' errors');
  errs.forEach(e => console.log('  ' + e));
  process.exit(errs.length || n === 0 || svg < 50 ? 1 : 0); })();
JS
cd /tmp && node "$OUT/check.js"
