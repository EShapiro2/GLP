/* Writing the UI language: a contract's sentence and its facts, with names of one's choosing.
 * Used by mechanical.js, which writes a description of any contract.
 */
(function (G) {
const FEMALE = ['Alice', 'Carol', 'Erin', 'Grace'], PERSONS = ['Bob', 'Carol', 'Dave', 'Erin', 'Frank', 'Grace'];

/* a name not yet taken: the next person's name, or the value's name with a number */
function fresh(taken, base, person) {
  if (person) { const x = PERSONS.find(n => !taken.has(n)); if (x) return x; }
  for (let i = 2; ; i++) if (!taken.has(base + i)) return base + i;
}
const template = a => a.sentence.split(/\.\s/)[0].replace(/\.$/, '');
/* a sentence one can say: one with "may"; a clause that only points to another, such as "Notes:3, ...", is not */
const sayable = a => /\bmay\b/.test(template(a));

/* the act's sentence with its names renamed by map; a pronoun follows the person named last before it */
function say(a, map) {
  const slots = new Set(a.all.concat(a.names));
  let last = null;
  return template(a).replace(/\b[A-Za-z]\w*\b/g, w => {
    if (slots.has(w)) { const n = map[w] || w; if (a.pnames.includes(w)) last = n; return n; }
    if (last && /^(his|her|him)$/.test(w)) {
      const f = FEMALE.includes(last);
      return w === 'his' ? (f ? 'her' : 'his') : w === 'him' ? (f ? 'her' : 'him') : (f ? 'her' : 'his');
    }
    return w;
  });
}
/* a fact in the vocabulary's words: its holder, then its arguments' names */
function factText(vocab, pred, names) {
  const v = vocab[pred], val = {};
  if (!v) return pred + '(' + names.join(', ') + ')';
  v.vars.forEach((x, j) => val[x] = names[j]);
  return v.phrase.replace(/\b[a-z]\w*\b/g, w => w in val ? val[w] : w);
}
/* names for the arguments of a predicate's facts, from an act that adds or requires one, with Alice the holder */
function factNames(sim, pred) {
  for (const s of sim.schemas) for (const c of s.effects.concat(s.pre)) {
    if (c.p !== pred || !c.args.every(x => typeof x === 'string')) continue;
    const ps = s.persons.concat(Object.keys(s.one)), h = c.args[0], taken = new Set(['Alice']), map = {[h]: 'Alice'};
    const out = c.args.slice(1).map(x => {
      /* a set of coins is named as the number of them: SumSet as Sum */
      const y = x.replace(/(\w)Set$/, '$1').replace(/1$/, '');
      if (!(x in map)) { map[x] = y === 'Alice' || taken.has(y) ? fresh(taken, y, ps.includes(x)) : y; taken.add(map[x]); }
      return map[x];
    });
    return out;
  }
  const taken = new Set(['Alice']);
  if (!sim.vocab[pred]) {
    const n = sim.schemas.flatMap(s => s.pre.concat(s.effects)).find(c => c.p === pred).args.length - 1;
    return Array.from({length: n}, () => { const x = fresh(taken, 'X', true); taken.add(x); return x; });
  }
  return sim.vocab[pred].vars.slice(1).map(x => { const n = fresh(taken, x.toUpperCase(), false); taken.add(n); return n; });
}
/* the act's names its sentence says */
const said = a => { const xs = new Set(a.all.concat(a.names)); return new Set((template(a).match(/\b[A-Za-z]\w*\b/g) || []).filter(w => xs.has(w))); };
const api = {said, sayable, say, factText, factNames, fresh, template, PERSONS};
if (typeof module !== 'undefined') module.exports = api; else G.EMIT = api;
})(this);
