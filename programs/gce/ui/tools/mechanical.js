/* A description of any contract, written by a machine: complete and visible by Propositions 1 and 2.
 *
 *     node mechanical.js SIM.json > NAME.mechanical.ui
 *
 * The start screen holds the questions; Alice's facts with no arguments; everyone named in her facts, with
 * each predicate whose facts each name a person, opening a screen of that person; a list of the facts of
 * each other predicate; and an action for each act, each place's value from a list of the predicate of a
 * required fact that holds it, or typed if no required fact does.
 */
const G = require('../ui.js'), E = require('./emit.js');

function mechanical(sim) {
  const acts = G.acts(sim), env = new Set(sim.env || []), personal = new Set(G.personal(sim));
  const arity = {};
  /* the predicates of Alice's facts, in conditions however nested */
  const walk = cs => { for (const c of cs) { if (c.p && !env.has(c.p)) arity[c.p] = c.args.length - 1;
    if (c.forall) { walk([c.over]); walk(c.then); } if (c.or) c.or.forEach(walk); if (c.exists) walk(c.body); if (c.and) walk(c.and); if (c.not) walk([c.not]); } };
  for (const s of sim.schemas) walk(s.pre.concat(s.effects));
  for (const p of Object.keys(sim.vocab)) if (!env.has(p) && !(p in arity)) arity[p] = sim.vocab[p].vars.length - 1;
  const preds = Object.keys(arity).filter(p => p !== 'today');
  const zero = preds.filter(p => arity[p] === 0), withP = preds.filter(p => arity[p] > 0 && personal.has(p)), other = preds.filter(p => arity[p] > 0 && !personal.has(p));
  const out = ['# Mechanical: what a machine writes for any contract. Complete and visible, and not pleasant.', '', 'SCREEN Home ABOUT Alice:', '    QUESTIONS'];
  if (zero.length) out.push('    SHOW Alice: ' + zero.join(', '));
  out.push('    LIST People: PERSONS' + (withP.length ? ' WITH ' + withP.join(', ') : '') + ' OPENS Person');
  for (const p of other) out.push('    LIST ' + p.charAt(0).toUpperCase() + p.slice(1).replace(/_/g, ' ') + ': "' + E.factText(sim.vocab, p, ['Alice'].concat(E.factNames(sim, p))) + '"');
  for (const a of acts) {
    const anc = {};
    for (const [p, ks] of a.req) ks.forEach(k => { if (k !== null) (anc[k] = anc[k] || []).push(p); });
    const me = a.names[a.me - 1], speak = E.sayable(a), map = {}, taken = new Set(['Alice']);
    if (speak) {
      map[me] = 'Alice';
      const inS = E.said(a);
      for (const r of a.all.concat(a.names)) if (!(r in map)) {
        if (!inS.has(r)) { map[r] = r; continue; } const n = taken.has(r) ? E.fresh(taken, r, a.pnames.includes(r)) : r; map[r] = n; taken.add(n);
      }
    }
    const src = [];
    a.names.forEach((r, i) => { if (i + 1 !== a.me) src.push((map[r] || r) + (anc[i + 1] ? ' FROM ' + anc[i + 1][0] : ' TYPED')); });
    out.push('    ' + a.button + (speak ? ': ' + E.say(a, map) + (src.length ? ', ' + src.join(', ') : '') : src.length ? ': ' + src.join(', ') : ''));
  }
  out.push('', 'SCREEN Person ABOUT Bob:', '    QUESTIONS', '    SHOW Bob: ' + (withP.join(', ') || zero[0] || 'person'));
  return out.join('\n') + '\n';
}
module.exports = mechanical;
if (require.main === module) process.stdout.write(mechanical(JSON.parse(require('fs').readFileSync(process.argv[2]))));
