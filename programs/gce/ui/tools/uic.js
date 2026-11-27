/* The UI compiler: a contract and an interface description in the UI language, to what an app reads.
 *
 *     node uic.js SIM.json NAME.ui [platform.vglp] [--style NAME.style] > NAME.ui.json
 *
 * Given the platform the contract compiles to, it numbers each place as the platform's request does and takes
 * the proposer's place from the platform's request widget (platform.js); otherwise from the contract's schemas.
 * Given a UI style, it refuses one its checker refuses (Proposition 3: no UI style loses an option) and writes
 * the look of each typed place, each list and each action beside it, and the look of the whole app.
 * It refuses a description with errors, and one that is not complete (Proposition 1).  What it writes names
 * each act by its schema and each place by its number in the act's request, the I-th argument of the
 * request being the I-th of the act's places; the app finds the rest in the contract its platform declares.
 */
const fs = require('fs'), G = require('./ui.js');

function compile(sim, text, platformText, lookText) {
  const plat = platformText ? require('./platform.js').read(platformText) : null;
  const warnings = [];
  const spec = {acts: G.acts(sim), vocab: sim.vocab, env: sim.env, personal: G.personal(sim)};
  const P = G.resolve(spec, G.read(text)), R = G.check(spec, P.ui);
  if (P.errors.length) throw new Error(P.errors.map(e => 'line ' + e.line + ': ' + e.msg).join('\n'));
  if (!R.complete) throw new Error('not complete: ' + Object.values(R.acts).filter(a => !a.covered).map(a => a.button).join(', '));
  const LK = G.readLook(lookText || ''), LR = G.checkLook(spec, P.ui, LK, require('./pictures.js'));
  if (!LR.ok) throw new Error('the look: ' + LR.errors.map(e => 'line ' + e.line + ': ' + e.msg).join('\n'));
  let AT = null;
  /* a button's look: the style's sentences on it, then those kept to the screen it is on ("Join ON Group") */
  const lookOf = n => { const r = Object.assign({}, LK.of[n] || {}, (AT && LK.of[n + ' ON ' + AT]) || {}); if (!Object.keys(r).length) return null; return Object.fromEntries(Object.entries(r).filter(([k]) => k !== 'line' && !k.endsWith('Line'))); };
  /* the positions of each predicate's facts that hold persons, from the acts that add them */
  const personPos = {};
  for (const s of sim.schemas) {
    const ps = new Set(s.persons.concat(Object.keys(s.one)));
    for (const c of s.effects) if (c.p && !c.neg) c.args.slice(1).forEach((x, i) => { if (ps.has(x)) (personPos[c.p] = personPos[c.p] || new Set()).add(i + 1); });
  }
  const screens = {};
  for (const [name, C] of Object.entries(P.ui.containers)) {
    AT = name;
    const comps = [];
    /* an action as the app reads it: its act by its schema, each place by its number in the act's request */
    const action = c => {
      const a = spec.acts.find(x => x.name === c.act), places = {};
      /* the places as the platform's request numbers them, and its proposer's */
      const pa = plat && plat.acts[a.name];
      const placeOf = r => pa ? pa.names.indexOf(r) + 1 : a.names.indexOf(r) + 1;
      let me = a.me, arity = a.names.length;
      if (pa) {
        arity = pa.arity;
        const mine = placeOf(a.names[a.me - 1]);
        me = pa.proposers.includes(mine) ? mine : pa.proposers[0] || mine;
      } else if (plat) warnings.push(a.name + ': not in the platform');
      for (const [r, v] of Object.entries(c.bind)) {
        const k = placeOf(r);
        if (k < 1 || k === me) { if (k < 1) warnings.push(a.name + ': ' + r + ' is no place of the request'); continue; }
        if (v === 'it') places[k] = {subject: null};
        else if (v.param) places[k] = {subject: v.pos};
        else if (v.field) places[k] = {typed: true, look: lookOf((c.names || {})[r] || r)};
        else if (v.select) { const l = C.components.find(x => x.id === v.select); places[k] = {from: l.items.named ? 'everyone' : l.items.pred}; if (v.same !== undefined) places[k].same = v.same; }
        /* the name the sentence gives the place, which the steps of a chain share */
        if (places[k]) places[k].name = (c.names || {})[r] || r;
      }
      return {kind: 'action', schema: a.name, button: c.button, sentence: c.said || (C.components.find(x => x.act === c.act && x.said) || {}).said || null, then: c.then || null, thenIt: c.thenIt ? {screen: c.thenIt.screen, pred: c.thenIt.pred, of: c.thenIt.of, from: c.thenIt.from} : null, me, arity, places, covers: !!(R.comps[c.id] && R.comps[c.id].ok), where: (lookOf(c.button) || {}).as || null, label: (lookOf(c.button) || {}).label || null, important: !!(lookOf(c.button) || {}).important};
    };
    for (const c of C.components) {
      if (c.hidden) continue;
      if (c.kind === 'inbox') comps.push({kind: 'questions'});
      else if (c.kind === 'details') comps.push({kind: 'show', of: c.about, pos: c.pos, preds: c.shows});
      else if (c.kind === 'thread') comps.push({kind: 'thread', preds: c.shows});
      else if (c.kind === 'list') comps.push({kind: 'list', title: c.title, items: c.items.named ? {everyone: true} : c.items.pred ? {from: c.items.pred} : {facts: c.items.atoms, keep: c.items.within ? c.items.within.pos : null}, with: c.shows || [], opens: c.opens || null, look: (lookOf(c.title) || {}).as || null, order: (lookOf(c.title) || {}).order || null, told: !!(lookOf(c.title) || {}).told});
      else if (c.kind === 'chain') comps.push({kind: 'chain', button: c.button, label: (lookOf(c.button) || {}).label || null, steps: (c.stepIds || []).map(id => action(C.components.find(x => x.id === id))), then: c.then || null, thenIt: c.thenIt ? {screen: c.thenIt.screen, pred: c.thenIt.pred, of: c.thenIt.of, from: c.thenIt.from} : null, important: !!(lookOf(c.button) || {}).important, where: (lookOf(c.button) || {}).as || null, places: action(C.components.find(x => x.id === c.stepIds[0])).places});
      else if (c.kind === 'action' && c.act) comps.push(action(c));
    }
    screens[name] = {about: C.aboutPred ? {fact: C.aboutPred} : C.aboutName ? {name: C.aboutName} : null, components: comps};
  }
  /* the contract's acts as operators on predicates, for the way to an important act: what each adds and removes
     for the person who takes it, and, for each predicate, the acts that add it, by her or by someone else */
  const effects = {}, addedBy = {};
  for (const a of spec.acts) {
    effects[a.name] = {button: a.button, adds: a.adds, removes: a.removes};
    for (const pr of a.addsAny) (addedBy[pr] = addedBy[pr] || []).push({schema: a.name, button: a.button, mine: a.adds.includes(pr)});
  }
  /* the contract's phrase of each predicate, its first variable the holder: how the app says a fact in words */
  const vocab = Object.fromEntries(Object.entries(spec.vocab || {}).filter(([, v]) => v && v.phrase).map(([p, v]) => [p, {phrase: v.phrase, vars: v.vars}]));
  return {version: 1, look: LK.global, effects, addedBy, vocab, start: P.ui.start, landmarks: P.ui.landmarks || [], screens, personPos: Object.fromEntries(Object.entries(personPos).map(([p, s]) => [p, [...s].sort()])), complete: R.complete, visible: R.visible, warnings: [...new Set(warnings)]};
}
module.exports = compile;
if (require.main === module) {
  const args = process.argv.slice(2), li = args.indexOf('--style'), look = li >= 0 ? fs.readFileSync(args.splice(li, 2)[1], 'utf8') : null;
  process.stdout.write(JSON.stringify(compile(JSON.parse(fs.readFileSync(args[0])), fs.readFileSync(args[1], 'utf8'), args[2] ? fs.readFileSync(args[2], 'utf8') : null, look), null, 1) + '\n');
}
