// GAS2's pages: the shared helpers.
const $ = s => document.querySelector(s);
const esc = s => String(s ?? '').replace(/[&<>"]/g, c => ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;'}[c]));

async function load() {
  for (let i = 0; i < 1000; i++) {
    const r = await fetch('data.json', {cache: 'no-cache'});
    if (r.ok) return r.json();
    if (r.status === 503) {
      const b = document.querySelector('#build');
      if (b) b.textContent = 'The first build of every contract is under way (a few minutes)…';
      await new Promise(res => setTimeout(res, 10000));
      continue;
    }
    throw new Error('Could not load the contracts (' + r.status + ').');
  }
}

function ago(iso) {
  const s = (Date.now() - Date.parse(iso)) / 1000;
  if (s < 90) return 'just now';
  if (s < 5400) return Math.round(s / 60) + ' minutes ago';
  if (s < 129600) return Math.round(s / 3600) + ' hours ago';
  return Math.round(s / 86400) + ' days ago';
}

function buildLine(D) {
  const c = D.commit;
  return `From GLP's main branch at <a href="https://github.com/EShapiro2/GLP/commit/${esc(c.sha)}" target="_blank" rel="noopener">${esc(c.sha.slice(0, 8))}</a>, committed ${esc(ago(c.date))}; built ${esc(ago(D.built))}, checked again every hour.`;
}

// Where a contract stands on its way down the cascade.
function status(c) {
  const run = c.checked && c.checked.gas2;
  if (c.artefact && run && run.settled && run.lines)
    return {cls: 'runs', short: 'mini-app', rank: 5, text: 'A certified mini-app; it runs on two super-apps.'};
  if (c.artefact)
    return {cls: 'warn', short: 'mini-app, run stalls', rank: 4, text: 'A certified mini-app, but its run on two super-apps did not settle.'};
  if (c.app)
    return {cls: 'warn', short: 'compiled', rank: 3, text: 'Compiled, but not certified: ' + (c.artefactWhy || '')};
  const why = c.why || '';
  if (c.step === 'check' && /^the checker: syntactically_grassroots_with_wards/.test(why))
    return {cls: 'stuck', short: 'with wards', rank: 2, text: 'Grassroots with wards; the compiler does not take contracts with wards yet.'};
  if (c.step === 'check' && /not_grassroots/.test(why))
    return {cls: 'stuck', short: 'not grassroots', rank: 1, text: 'The checker finds it not grassroots.'};
  if (c.step === 'pragmas') return {cls: 'stuck', short: 'pragmas', rank: 2, text: 'Its pragmas, the words of its app, do not read.'};
  if (c.step === 'compile') return {cls: 'stuck', short: 'compiler stops', rank: 2, text: 'The compiler does not print its platform.'};
  if (c.step === 'schemas') return {cls: 'stuck', short: 'no schemas', rank: 0, text: 'It has no act schemas of its own.'};
  return {cls: 'stuck', short: 'stuck', rank: 0, text: why};
}
