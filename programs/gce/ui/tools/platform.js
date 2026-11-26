/* Reading a canonical platform (platform.vglp, as vGLP's compiler prints it) for what the UI compiler needs:
 * for each act, the names of the places of its request in order, their types, its arity, and the place of its proposer at
 * each guarding role the platform proposes it at.
 *
 * The places of an act's request are the roles of its schema, but for a listed role, then its variables, as many
 * as the request has places (programs/vglp/cva/compiler/README.md, 6: "a place I of the request being the I-th
 * argument of F(...), the schema's head less its listed roles").
 */
function terms(text) {
  let i = 0;
  const ws = () => { while (i < text.length && /\s/.test(text[i])) i++; };
  function term() {
    ws();
    const c = text[i];
    if (c === '[') {
      i++; const xs = []; ws();
      if (text[i] === ']') { i++; return {list: xs}; }
      for (;;) { xs.push(term()); ws(); if (text[i] === ',') { i++; continue; } if (text[i] === '|') { i++; term(); ws(); } if (text[i] === ']') { i++; return {list: xs}; } throw new Error('list at ' + i); }
    }
    if (c === '"') { let j = i + 1, s = ''; while (text[j] !== '"') { if (text[j] === '\\') { s += text[j + 1]; j += 2; } else s += text[j++]; } i = j + 1; return {str: s}; }
    if (c === "'") { let j = i + 1, s = ''; while (text[j] !== "'") { if (text[j] === '\\') { s += text[j + 1]; j += 2; } else s += text[j++]; } i = j + 1; return args({atom: s}); }
    const m = /^-?[A-Za-z0-9_]+/.exec(text.slice(i));
    if (!m) throw new Error('term at ' + i + ': ' + text.slice(i, i + 30));
    i += m[0].length;
    return args(/^-?\d+$/.test(m[0]) ? {num: +m[0]} : {atom: m[0]});
  }
  function args(t) {
    if (text[i] !== '(' || !t.atom) return t;
    i++; const xs = [];
    for (;;) { xs.push(term()); ws(); if (text[i] === ',') { i++; continue; } if (text[i] === ')') { i++; return {f: t.atom, args: xs}; } throw new Error('args at ' + i); }
  }
  return {term, at: k => { i = k; }, pos: () => i};
}
const nameOf = t => t.atom !== undefined ? t.atom : t.f && t.args.length && t.args[0].atom !== undefined ? t.args[0].atom : null;

function read(text) {
  const out = {acts: {}};
  /* the arity of each request, from the request type */
  const prop = /^Proposal ::=([\s\S]*?)\.\s*$/m.exec(text);
  const arity = {}, types = {};
  if (prop) for (const alt of prop[1].split(/\n;\n|\s;\s/)) { const m = /([a-z]\w*)(?:\((.*)\))?/s.exec(alt.trim()); if (m) { types[m[1]] = m[2] ? m[2].split(/,(?![^()]*\))/).map(x => x.trim()) : []; arity[m[1]] = types[m[1]].length; } }
  /* the schemas: their roles, which of them are listed, and their variables */
  const ci = text.search(/^contract\("/m);
  const names = {};
  if (ci >= 0) {
    const P = terms(text); P.at(ci);
    const c = P.term();
    const walk = t => {
      if (!t || typeof t !== 'object') return;
      if (/^[a-z]?schema$/.test(t.f || '') && t.args.length >= 4 && t.args[1].list && t.args[1].list.length && t.args[2].list) {
        const roles = t.args[1].list, vars = t.args[2].list || [], specs = t.args[t.args.length - 1].list || [];
        const listed = new Set(specs.map((s, k) => s.f === 'crs' && s.args[0].f === 'listed' ? k : -1).filter(k => k >= 0));
        const ns = [], vs = vars.map(nameOf).filter(n => n);
        /* a role is a place but for a listed one, and but for one the schema also names among its variables, as
           one('Erin', ['Alice', 'Carol']), a role possibly one of the others, whose place is the variable's */
        roles.forEach((r, k) => {
          const n = r.atom !== undefined ? r.atom : r.f === 'one' && r.args.length && r.args[0].atom !== undefined ? r.args[0].atom : null;
          if (n && !listed.has(k) && !vs.includes(n)) ns.push(n);
        });
        ns.push(...vs);
        if (!names[t.args[0].atom]) names[t.args[0].atom] = ns;
        return;
      }
      (t.args || []).forEach(walk); (t.list || []).forEach(walk);
    };
    walk(c);
  }
  /* the acts of the request widget's declaration, each with its proposer's place */
  const mi = text.search(/=::= social_menu\(/);
  const prop2 = {};
  if (mi >= 0) {
    const P = terms(text); P.at(text.indexOf('social_menu(', mi));
    const walk = t => {
      if (!t || typeof t !== 'object') return;
      if (t.f === 'act' && t.args.length === 8 && t.args[1].num !== undefined) { (prop2[t.args[0].atom] = prop2[t.args[0].atom] || []).push(t.args[1].num); return; }
      (t.args || []).forEach(walk); (t.list || []).forEach(walk);
    };
    walk(P.term());
  }
  for (const s of Object.keys(arity)) {
    const n = arity[s], ns = (names[s] || []).slice(0, n);
    out.acts[s] = {arity: n, names: ns, types: types[s], proposers: prop2[s] || []};
  }
  return out;
}
module.exports = {read};
