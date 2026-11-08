// render.mjs --- one page of the store site rendered by headless Chrome over
// the DevTools protocol, for part 4 of the live suite (site_check.py render).
//
//   node render.mjs <url> <seconds> [<text to wait for>]
//
// Loads the page in a fresh profile, runs it for up to <seconds> of wall
// time --- the app page long-polls its world's state, so it is never idle ---
// or until the page's text contains <text to wait for>, and prints one JSON
// line: the page's visible text, every uncaught exception, every console
// error, and every request that failed or answered 400 or more.
import { spawn } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

const [url, secs, waitFor] = process.argv.slice(2);
const CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const prof = mkdtempSync(join(tmpdir(), 'chrome-live-'));
const chrome = spawn(CHROME, ['--headless=new', '--disable-gpu', '--no-first-run',
  '--no-default-browser-check', `--user-data-dir=${prof}`, '--remote-debugging-port=0',
  'about:blank'], { stdio: 'ignore' });
const sleep = ms => new Promise(r => setTimeout(r, ms));
const out = { url, text: '', exceptions: [], consoleErrors: [], failedRequests: [] };
let done = false;
const finish = code => {
  if (done) return; done = true;
  console.log(JSON.stringify(out));
  chrome.kill('SIGKILL');
  setTimeout(() => { try { rmSync(prof, { recursive: true, force: true }); } catch {} process.exit(code); }, 300);
};
setTimeout(() => { out.exceptions.push('render.mjs: no result within the deadline'); finish(1); },
  (Number(secs) + 40) * 1000);

let port;
for (let i = 0; i < 100 && !port; i++) {
  const f = join(prof, 'DevToolsActivePort');
  if (existsSync(f)) port = readFileSync(f, 'utf8').split('\n')[0];
  else await sleep(100);
}
if (!port) { out.exceptions.push('render.mjs: Chrome gave no debugging port'); finish(1); }
const targets = await (await fetch(`http://127.0.0.1:${port}/json/list`)).json();
const page = targets.find(t => t.type === 'page');
const ws = new WebSocket(page.webSocketDebuggerUrl);
let id = 0;
const pending = new Map();
const send = (method, params = {}) => new Promise(r => { const i = ++id; pending.set(i, r); ws.send(JSON.stringify({ id: i, method, params })); });
const reqs = new Map();
ws.onmessage = ev => {
  const m = JSON.parse(ev.data);
  if (m.id && pending.has(m.id)) { pending.get(m.id)(m.result); pending.delete(m.id); return; }
  const p = m.params || {};
  if (m.method === 'Runtime.exceptionThrown') {
    const d = p.exceptionDetails || {};
    out.exceptions.push(((d.exception && d.exception.description) || d.text || '').split('\n')[0]);
  } else if (m.method === 'Runtime.consoleAPICalled' && (p.type === 'error' || p.type === 'assert')) {
    out.consoleErrors.push(p.args.map(a => a.value ?? a.description ?? '').join(' '));
  } else if (m.method === 'Log.entryAdded' && p.entry.level === 'error') {
    out.consoleErrors.push(`${p.entry.text} ${p.entry.url || ''}`.trim());
  } else if (m.method === 'Network.requestWillBeSent') {
    reqs.set(p.requestId, p.request.url);
  } else if (m.method === 'Network.responseReceived' && p.response.status >= 400) {
    out.failedRequests.push(`${p.response.status} ${p.response.url}`);
  } else if (m.method === 'Network.loadingFailed' && !p.canceled) {
    out.failedRequests.push(`${p.errorText} ${reqs.get(p.requestId) || ''}`);
  }
};
await new Promise(r => { ws.onopen = r; });
await send('Runtime.enable'); await send('Log.enable'); await send('Network.enable'); await send('Page.enable');
await send('Page.navigate', { url });
const end = Date.now() + Number(secs) * 1000;
for (;;) {
  await sleep(1000);
  const r = await send('Runtime.evaluate', { expression: 'document.body ? document.body.innerText : ""', returnByValue: true });
  out.text = (r && r.result && r.result.value) || '';
  if ((waitFor && out.text.includes(waitFor)) || Date.now() > end) break;
}
finish(0);
