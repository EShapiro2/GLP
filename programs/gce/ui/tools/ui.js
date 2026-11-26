/* The UI language of the prototype: a reader, and the checker of the paper's two propositions.
 *
 * Alice is the person whose phone it is.  Keywords are in capitals; everything else is the contract's.
 *
 *   SCREEN Chats ABOUT Alice:                        the first screen is the start screen, about Alice herself
 *   SCREEN Chat ABOUT Bob:                           a screen about a person or a value, named Bob here
 *   SCREEN Group ABOUT "Alice is a member of Bob's Hikers":  or about a fact of Alice's, in the contract's words
 *       QUESTIONS                                    the questions put to Alice, with yes and no
 *       SHOW Alice: identity, person                 facts of these predicates about Alice, about a name of the
 *       SHOW Bob: knows, friend                      screen's subject, or (SHOW:) about the whole subject
 *       THREAD: message, sent_message                the same, drawn as a conversation
 *       LIST Contacts: EVERYONE WITH knows OPENS Chat        everyone named in Alice's facts
 *       LIST Groups: "Alice is a member of Bob's Hikers" OPENS Group  Alice's facts of a predicate; on a screen
 *                                                    about a fact, a name they share with it keeps the list to it
 *       LIST Markets: FROM keeper OPENS Market       the values in Alice's facts of a predicate
 *       Introduce: Alice may introduce Carol to Bob, Carol FROM knows
 *           an action: its button, then the contract's sentence for the act, its names renamed as one likes
 *           (Alice for the one who acts); a name of the screen's subject takes its value from it, and any other
 *           name is TYPED, FROM EVERYONE, FROM a predicate's values, or IS a name of the subject; an act whose
 *           clause only points to another (Pay issuer: Notes:3, ...) is written with no sentence, in the act's names
 *   A line indented deeper than the lines of its screen goes on the line before it.
 */
(function (G) {
  const SRC = /^([A-Z]\w*)\s+(TYPED|FROM\s+EVERYONE|FROM\s+[a-z]\w*|IS\s+[A-Z]\w*)$/;
  const KEYWORDS = /^(SCREEN|SHOW|LIST|THREAD|QUESTIONS|ABOUT|WITH|OPENS|FROM|TYPED|EVERYONE|IS)\b/;
  function read(text) {
    const ui = {name: '', start: null, containers: {}}, lines = [], errors = [];
    let scr = null, n = 0;
    const err = (i, m) => errors.push({line: i, msg: m});
    /* a line indented deeper than the lines of its screen goes on the line before it */
    const logical = [];
    let inner = null;
    for (const raw of text.split('\n')) {
      n++;
      const t = raw.replace(/#.*$/, '').trimEnd(), ind = t.length - t.trimStart().length, L = {n, raw, kind: 'blank'};
      lines.push(L);
      if (!t.trim()) { if (raw.trim().startsWith('#')) L.kind = 'comment'; continue; }
      if (ind === 0) inner = null; else if (inner === null) inner = ind;
      const prev = logical[logical.length - 1];
      if (ind > 0 && ind > inner && prev && prev.ind > 0) { prev.s += ' ' + t.trim(); prev.more.push(L); continue; }
      logical.push({n, ind, s: t.trim(), L, more: []});
    }
    for (const {n, ind, s, L, more} of logical) {
      const mark = o => { Object.assign(L, o); more.forEach(x => Object.assign(x, o)); };
      let m;
      if (ind === 0) {
        if (!(m = s.match(/^SCREEN\s+([A-Za-z][\w-]*)(?:\s+ABOUT\s+(?:"([^"]+)"|([A-Z]\w*)))?\s*:$/))) { err(n, 'expected: SCREEN Name: or SCREEN Name ABOUT Bob: or SCREEN Name ABOUT "a fact":'); continue; }
        scr = m[1];
        if (ui.containers[scr]) { err(n, 'screen ' + scr + ' twice'); scr = null; continue; }
        /* every screen is about something: the start screen about Alice herself, whose phone it is */
        const first = !ui.start, me = m[3] === 'Alice';
        ui.containers[scr] = {params: (m[2] || m[3]) && !(first && me) ? ['it'] : [], components: [], line: n, aboutText: first ? null : m[2] || null, aboutName: first || me ? null : m[3] || null};
        if (first) { ui.start = scr; if (m[2] || (m[3] && !me)) err(n, 'the start screen is about Alice: SCREEN ' + scr + ' ABOUT Alice:'); }
        else if (me) err(n, 'only the start screen is about Alice');
        else if (!m[2] && !m[3]) err(n, 'a screen other than the start is ABOUT something');
        mark({kind: 'screen', screen: scr});
        continue;
      }
      if (!scr) { err(n, 'a line before the first screen'); continue; }
      const C = ui.containers[scr], id = 'l' + n, base = {id, line: n};
      const push = c => { C.components.push(c); mark({kind: 'comp', screen: scr, id}); };
      if (s === 'QUESTIONS') { push({...base, zone: 'top', kind: 'inbox'}); continue; }
      if ((m = s.match(/^SHOW(?:\s+([A-Z]\w*))?\s*:\s*([\w,\s]+)$/))) { push({...base, zone: 'middle', kind: 'details', name: m[1] || null, shows: m[2].split(',').map(x => x.trim()).filter(Boolean)}); continue; }
      if ((m = s.match(/^THREAD\s*:\s*([\w,\s]+)$/))) { push({...base, zone: 'middle', kind: 'thread', shows: m[1].split(',').map(x => x.trim()).filter(Boolean)}); continue; }
      if ((m = s.match(/^LIST\s+([^:"]+):\s*(EVERYONE|FROM\s+[a-z]\w*|"[^"]+")(?:\s+WITH\s+([\w\s,]+?))?(?:\s+OPENS\s+([\w-]+))?$/))) {
        const c = {...base, zone: 'middle', kind: 'list', title: m[1].trim(), items: m[2] === 'EVERYONE' ? {named: true} : m[2].startsWith('FROM') ? {pred: m[2].replace(/^FROM\s+/, '')} : {fact: m[2].slice(1, -1)}};
        if (m[3]) c.shows = m[3].split(',').map(x => x.trim()).filter(Boolean);
        if (m[4]) c.opens = m[4];
        push(c); continue;
      }
      if (KEYWORDS.test(s)) { err(n, 'not understood'); continue; }
      if ((m = s.match(/^([A-Z][\w' -]*?):\s*(.*)$/)) || (m = s.match(/^([A-Z][\w' -]*)()$/))) {
        const parts = m[2].replace(/\.\s*$/, '').split(/,\s*/), sources = {};
        while (parts.length && SRC.test(parts[parts.length - 1].trim())) {
          const q = parts.pop().trim().match(SRC);
          sources[q[1]] = q[2] === 'TYPED' ? {field: true} : /^IS\s/.test(q[2]) ? {is: q[2].replace(/^IS\s+/, '')} : /^FROM\s+EVERYONE$/.test(q[2]) ? {from: 'everyone'} : {from: q[2].replace(/^FROM\s+/, '')};
        }
        push({...base, zone: 'bottom', kind: 'action', button: m[1].trim(), said: parts.join(', ').trim(), sources, bind: {}});
        continue;
      }
      err(n, 'not understood');
    }
    if (!ui.start) err(1, 'no screen');
    return {ui, lines, errors};
  }

  /* a sentence of the contract as a pattern: its names (of the act's places, or of a phrase's variables)
     match any capitalized names, the same name where it repeats, and his, her, its or their one another */
  const PRON = /\b(?:his|her|hers|him|its|their)\b/g;
  const reEsc = x => x.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  function pattern(text, slots) {
    let re = '', last = 0;
    const order = [], words = /\b[A-Za-z]\w*\b/g, lit = x => reEsc(x).replace(PRON, '(?:his|her|hers|him|its|their)').replace(/\s+/g, '\\s+');
    let w;
    while ((w = words.exec(text))) {
      if (!slots.includes(w[0])) continue;
      re += lit(text.slice(last, w.index));
      const j = order.indexOf(w[0]);
      re += j < 0 ? '([A-Z]\\w*)' : '\\' + (j + 1);
      if (j < 0) order.push(w[0]);
      last = w.index + w[0].length;
    }
    re += lit(text.slice(last));
    return {re: new RegExp('^' + re + '\\.?$'), order};
  }
  function matchWith(pat, text) {
    const m = text.trim().match(pat.re);
    if (!m) return null;
    const map = {};
    pat.order.forEach((x, j) => map[x] = m[j + 1]);
    return new Set(Object.values(map)).size === pat.order.length ? map : null;
  }
  /* a fact of Alice's, in the words of the contract's vocabulary: its predicate and the names of its arguments */
  function fact(spec, text) {
    /* or in the Logic pragmas' form, pred(Alice, Bob), which every predicate has, a phrase or not */
    const m = text.trim().match(/^([a-z]\w*)\(([^()]*)\)$/);
    if (m) {
      const xs = m[2].split(',').map(x => x.trim()), known = spec.vocab[m[1]] ? spec.vocab[m[1]].vars.length : (spec.acts.flatMap(a => a.req.concat(a.forb)).find(([p]) => p === m[1]) || [null, null])[1];
      if (known === null || (typeof known === 'number' ? known : known.length + 1) !== xs.length || !xs.every(x => /^[A-Z]\w*$/.test(x))) return null;
      return {pred: m[1], holder: xs[0], args: xs.slice(1)};
    }
    for (const [p, v] of Object.entries(spec.vocab)) {
      if ((spec.env || []).includes(p)) continue;
      const m = matchWith(pattern(v.phrase, v.vars), text);
      if (m) return {pred: p, holder: m[v.vars[0]], args: v.vars.slice(1).map(x => m[x])};
    }
    return null;
  }
  const template = a => a.sentence.split(/\.\s/)[0].replace(/\.$/, '');

  function resolve(spec, parsed) {
    const {ui, errors} = parsed, err = (i, m) => errors.push({line: i, msg: m});
    let picks = 0;
    for (const C of Object.values(ui.containers)) {
      /* the subject: its names, each the whole subject or an argument of the fact at its position */
      C.names = {};
      if (C.aboutName) C.names[C.aboutName] = null;
      if (C.aboutText) {
        const f = fact(spec, C.aboutText);
        if (!f) err(C.line, 'no fact of the contract reads "' + C.aboutText + '"');
        else if (f.holder !== 'Alice') err(C.line, 'a screen is about a fact of Alice\'s: it starts with Alice');
        else { C.aboutPred = f.pred; f.args.forEach((x, j) => { if (!(x in C.names)) C.names[x] = j + 1; }); }
      }
      for (const c of [...C.components]) {
        if (c.kind === 'details') {
          if (!c.name) { if (!C.params.length) err(c.line, 'SHOW: shows the subject, and this screen is about nothing'); c.about = 'it'; c.pos = null; }
          else if (c.name === 'Alice') { c.about = 'me'; c.pos = null; }
          else if (c.name in C.names) { c.about = 'it'; c.pos = C.names[c.name]; }
          else err(c.line, c.name + ' is neither Alice nor a name of the screen\'s subject');
        }
        if (c.kind === 'list' && c.items.fact) {
          const f = fact(spec, c.items.fact);
          if (!f) { err(c.line, 'no fact of the contract reads "' + c.items.fact + '"'); continue; }
          if (f.holder !== 'Alice') { err(c.line, 'a list is of Alice\'s facts: it starts with Alice'); continue; }
          c.items = {atoms: f.pred, args: f.args};
          const shared = f.args.filter(x => x in C.names);
          if (shared.length) {
            if (C.names[shared[0]] === null) err(c.line, 'a list is kept only to a part of a fact the screen is about');
            else c.items.within = {param: 'it', pos: C.names[shared[0]]};
          }
        }
        if (c.kind !== 'action') continue;
        const cands = spec.acts.filter(a => a.button === c.button);
        if (!cands.length) { err(c.line, 'the contract has no act of the button ' + c.button); continue; }
        let a = null, map = null;
        for (const x of cands) {
          const slots = [...new Set(x.all.concat(x.names))];
          const mm = c.said ? matchWith(pattern(template(x), slots), c.said) : Object.fromEntries(x.names.map(r => [r, r]));
          if (mm) { a = x; map = mm; break; }
        }
        if (!a) { err(c.line, 'not the contract\'s sentence for ' + c.button + ', "' + template(cands[0]) + '", with its names renamed'); continue; }
        c.act = a.name;
        c.names = Object.fromEntries(a.names.map(r => [r, map[r] || r]));
        const nm = r => map[r] || r, me = a.names[a.me - 1];
        /* in a sentence Alice is the one who acts; a name the sentence does not say, and a line with no sentence, are the act's own */
        if (c.said && me in map && map[me] !== 'Alice') { err(c.line, map[me] + ' acts here; the one who acts is Alice'); continue; }
        const used = new Set();
        for (const r of a.names) {
          if (r === me) continue;
          const N = nm(r);
          used.add(N);
          if (c.said && r in map && N === 'Alice') { err(c.line, 'Alice is the one who acts; ' + r + ' needs another name'); continue; }
          const src = c.sources[N];
          if (src && src.is) {
            if (src.is in C.names) c.bind[r] = C.names[src.is] === null ? 'it' : {param: 'it', pos: C.names[src.is]};
            else err(c.line, src.is + ' is not a name of the screen\'s subject');
          } else if (src) {
            if (src.field) c.bind[r] = {field: true};
            else {
              const pid = 'pick' + (++picks);
              C.components.push({id: pid, zone: 'middle', line: c.line, kind: 'list', hidden: true, title: N, items: src.from === 'everyone' ? {named: true} : {pred: src.from}});
              c.bind[r] = {select: pid};
            }
          } else if (N in C.names) c.bind[r] = C.names[N] === null ? 'it' : {param: 'it', pos: C.names[N]};
        }
        for (const N of Object.keys(c.sources)) if (!used.has(N)) err(c.line, N + ' is not a name of this sentence');
      }
    }
    for (const d of Object.values(ui.containers)) for (const c of d.components) if (c.opens) {
      const to = ui.containers[c.opens];
      if (!to) { err(c.line, 'no screen ' + c.opens); continue; }
      if (to.aboutPred && c.items.atoms !== to.aboutPred) err(c.line, c.opens + ' is about a ' + to.aboutPred + ' fact, and this list is not of them');
      else if (!to.aboutPred && c.items.atoms) err(c.line, c.opens + ' is about a person or a value, and this list is of facts');
      else c.event = {navigate: c.opens, bind: {it: 'item'}};
    }
    return parsed;
  }

  /* the acts of a simulated contract, in the checker's terms: places, the proposer's place, and the
     proposer's local precondition, required and forbidden atoms over places (null: another value) */
  function acts(sim) {
    return sim.schemas.filter(s => !s.auto).map(s => {
      const env = new Set();
      for (const c of s.pre) if (c.p === 'today') env.add(c.args[1]);
      const sets = new Set(Object.keys(s.types || {}).filter(x => s.types[x] === 'set'));
      const derived = new Set(s.derived || []);
      const names = [...new Set(s.persons.concat(Object.keys(s.one), s.values))].filter(x => !env.has(x) && !sets.has(x) && !derived.has(x));
      const me = s.by[0], loc = (c) => [c.p, c.args.slice(1).map(x => names.includes(x) ? names.indexOf(x) + 1 : null)];
      const mine = s.pre.filter(c => c.p && c.args[0] === me && !sim.env.includes(c.p));
      return {name: s.name, button: s.button, names, all: s.persons.concat(Object.keys(s.one), s.values), pnames: s.persons.concat(Object.keys(s.one)), me: names.indexOf(me) + 1, args: names, roles: s.persons.map(x => names.indexOf(x) + 1), people: s.persons.concat(Object.keys(s.one)).map(x => names.indexOf(x) + 1),
        values: s.values.filter(x => !env.has(x) && !s.persons.includes(x) && !sets.has(x) && !derived.has(x)), types: s.types || {}, field: s.field, guard: s.guard, sentence: s.sentence, questions: s.questions,
        req: mine.filter(c => !c.neg).map(loc), forb: mine.filter(c => c.neg).map(loc)};
    });
  }
  const placeOf = (a, r) => a.names.indexOf(r) + 1;
  function anchors(a) {
    const out = {};
    for (const [p, ks] of a.req) for (const k of ks) if (k !== null) (out[k] = out[k] || new Set()).add(p);
    return out;
  }
  const itemKinds = l => l.items.named ? ['named'] : l.items.atoms ? ['atom:' + l.items.atoms] : ['pred:' + l.items.pred];
  /* the ways into a screen: each a path of lists from the start screen, to a depth of four, every list
     after the first kept, by `with`, to the atoms that name a position of its screen's parameter */
  function paths(ui) {
    const out = {[ui.start]: [null]}, todo = [[ui.start, null, 0]];
    while (todo.length) {
      const [c, d, depth] = todo.shift();
      if (depth >= 4) continue;
      /* past the start screen, a way goes on only through a list kept to its screen's parameter */
      for (const x of ui.containers[c].components) if (x.event && (c === ui.start || x.items.within)) {
        const nd = {kind: itemKinds(x)[0], list: x.id, via: x.items.within ? {desc: d, pos: x.items.within.pos} : null};
        const to = x.event.navigate, key = JSON.stringify(nd);
        out[to] = out[to] || [];
        if (out[to].some(y => JSON.stringify(y) === key)) continue;
        out[to].push(nd); todo.push([to, nd, depth + 1]);
      }
    }
    return out;
  }
  function kindsOf(v, d, comps) {
    if (v === 'me') return ['me'];
    if (typeof v === 'string') return d ? [d.kind.startsWith('atom:') ? 'whole:' + d.kind : d.kind] : [];
    if (v.param) return d && d.kind.startsWith('atom:') ? ['pos:' + d.kind.slice(5) + ':' + v.pos] : ['bad'];
    if (v.select) return itemKinds(comps.find(x => x.id === v.select));
    return ['any'];
  }
  function reaches(kinds, k, anc, roles) {
    if (!kinds.length) return false;
    if (kinds.includes('any')) return true;
    const need = anc[k] || new Set();
    return kinds.every(kd => (kd === 'named' && need.size > 0 && (!roles || roles.includes(k))) || (kd.startsWith('pred:') && need.has(kd.slice(5))) || kd.startsWith('pos:'));
  }
  /* a path through lists `with` a parameter reaches a fact only if the act also requires the facts the
     earlier lists were made of: one of each, naming the value the next list keeps */
  function chainOK(a, d, req) {
    for (; d && d.via; ) {
      const up = d.via.desc;
      if (!up) return false;
      if (up.kind === 'named') return d.via.pos === null;
      if (!up.kind.startsWith('atom:')) return false;
      const q = up.kind.slice(5), r = a.req.find(([p, pl]) => p === q && (d.via.pos === null ? pl.some(k => k !== null && req[1].includes(k)) : pl[d.via.pos - 1] !== null && req[1].includes(pl[d.via.pos - 1])));
      if (!r) return false;
      d = up; req = r;
    }
    return true;
  }
  function check(spec, ui) {
    const ways = paths(ui), reach = new Set(Object.keys(ways)), acts = {}, comps = {};
    for (const a of spec.acts) acts[a.name] = {button: a.button, covered: false};
    for (const c of reach) for (const x of ui.containers[c].components) if (x.kind === 'action' && x.act) {
      const a = spec.acts.find(y => y.name === x.act), anc = anchors(a), bind = {[a.me]: 'me'};
      for (const [r, v] of Object.entries(x.bind)) bind[placeOf(a, r)] = v;
      const ps = ui.containers[c].params;
      /* the action covers its act if, on one way into its screen, every place is reached */
      let best = null;
      for (const d of ways[c]) {
        const why = [];
        for (let k = 1; k <= a.args.length; k++) {
          if (k === a.me) continue;
          const r = a.names[k - 1];
          if (!(k in bind)) why.push(r + ' gets no value');
          else if (!reaches(kindsOf(bind[k], d, ui.containers[c].components), k, anc, a.roles)) why.push(r + ' may be someone this source does not reach');
        }
        /* places filled from the parameter's atom: all of them the positions of one required atom of its predicate */
        const fromAtom = Object.entries(x.bind).filter(([, v]) => v && v.param);
        if (fromAtom.length) {
          const ks = kindsOf(fromAtom[0][1], d, []), pred = ks.length === 1 && ks[0].startsWith('pos:') ? ks[0].split(':')[1] : null;
          const fit = pred && a.req.find(([p, pl]) => p === pred && fromAtom.every(([r, v]) => pl[v.pos - 1] === placeOf(a, r)));
          if (!fit) why.push('the places taken from the parameter are not the positions of one required ' + (pred || 'atom') + ' fact');
          else if (!chainOK(a, d, fit)) why.push('the act does not require the facts of the lists that lead to this screen');
        } else if (d && d.via) why.push('the act does not require the facts of the lists that lead to this screen');
        if (ps.length && !Object.values(x.bind).some(v => ps.includes(v) || (v && ps.includes(v.param)))) why.push('this screen opens only from a list, and the action does not use its parameter');
        if (!best || why.length < best.length) best = why;
        if (!why.length) break;
      }
      comps[x.id] = {ok: !best.length, why: best};
      if (!best.length) acts[a.name].covered = true;
    }
    for (const d of Object.values(ui.containers)) for (const x of d.components) if (x.kind === 'action' && !(x.id in comps)) comps[x.id] = {ok: false, why: ['no list opens this screen']};
    const preds = new Set(Object.keys(spec.vocab).filter(p => !(spec.env || []).includes(p)));
    for (const a of spec.acts) for (const [p] of a.req.concat(a.forb)) preds.add(p);
    /* Proposition 2: a predicate with arguments is shown by a list of its atoms on the start screen or, if
       its atoms each name a person, next to everyone there or by the details of a person whose screen that
       list opens; one without arguments, by the person's details on the start screen */
    const st = ui.containers[ui.start].components, shown = new Set(), arity = p => spec.vocab[p] ? spec.vocab[p].vars.length - 1 : (spec.acts.flatMap(a => a.req.concat(a.forb)).find(([q]) => q === p) || [null, []])[1].length;
    const everyone = st.filter(x => x.kind === 'list' && !x.hidden && x.items.named);
    const opened = new Set(everyone.filter(x => x.event).map(x => x.event.navigate));
    for (const p of preds) {
      const ok = arity(p) === 0 ? st.some(x => x.kind === 'details' && x.about === 'me' && x.shows.includes(p))
        : st.some(x => x.kind === 'list' && !x.hidden && x.items.atoms === p) || (spec.personal || []).includes(p) && (everyone.some(x => (x.shows || []).includes(p)) || [...opened].some(c => ui.containers[c].components.some(x => (x.kind === 'thread' || x.kind === 'details' && x.about !== 'me' && !x.pos) && x.shows.includes(p))));
      if (ok) shown.add(p);
    }
    const inbox = [...reach].some(c => ui.containers[c].components.some(x => x.kind === 'inbox'));
    const hidden = [...preds].filter(p => !shown.has(p)).sort(), somewhere = new Set();
    for (const c of reach) for (const x of ui.containers[c].components) {
      (x.shows || []).forEach(p => somewhere.add(p));
      if (x.kind === 'list' && !x.hidden && x.items.atoms) somewhere.add(x.items.atoms);
    }
    /* inner: shown, but only on screens reached through other facts; that each of them is found there
       depends on what else the person holds, which Proposition 2 does not check */
    return {acts, comps, hidden: hidden.filter(p => !somewhere.has(p)), inner: hidden.filter(p => somewhere.has(p)), inbox, complete: inbox && Object.values(acts).every(a => a.covered), visible: !hidden.length};
  }

  /* every state of one viewer among the persons, the allowed answers, and those the interface does not offer */
  function offered(spec, ui, st, me, persons) {
    const named = [...new Set(st.flatMap(([, xs]) => xs))].sort(), out = new Set(), seen = new Set(), todo = [[ui.start, {}]];
    while (todo.length) {
      const [c, env] = todo.pop(), key = c + JSON.stringify(env);
      if (seen.has(key)) continue;
      seen.add(key);
      const cs = ui.containers[c].components, items = {};
      for (const x of cs) if (x.kind === 'list') items[x.id] = x.items.named ? named : [...new Set(st.filter(([p]) => p === x.items.pred).flatMap(([, xs]) => xs))];
      for (const x of cs) {
        if (x.event) for (const it of items[x.id]) todo.push([x.event.navigate, Object.fromEntries(Object.entries(x.event.bind).map(([k, v]) => [k, v === 'item' ? it : env[v]]))]);
        if (x.kind !== 'action' || !x.act) continue;
        const a = spec.acts.find(y => y.name === x.act), ch = {[a.me]: [me]};
        for (const [r, v] of Object.entries(x.bind)) ch[placeOf(a, r)] = typeof v === 'string' ? [env[v]] : v.select ? items[v.select] : persons.filter(p => p !== me);
        let vals = [[]];
        for (let k = 1; k <= a.args.length; k++) { if (!ch[k]) { vals = []; break; } vals = vals.flatMap(v => ch[k].map(y => v.concat([y]))); }
        for (const v of vals) out.add(a.name + '(' + v.join(',') + ')');
      }
    }
    return out;
  }
  function brute(spec, ui, persons) {
    const me = persons[0], rest = persons.slice(1), preds = {};
    for (const a of spec.acts) for (const [p, ks] of a.req.concat(a.forb)) preds[p] = ks.length;
    const atoms = [];
    for (const [p, n] of Object.entries(preds).sort()) { if (n === 0) atoms.push([p, []]); else for (const x of rest) atoms.push([p, [x]]); }
    let states = 0, answers = 0, missed = 0;
    const examples = [], has = (st, p, xs) => st.some(([q, ys]) => q === p && ys.join() === xs.join());
    for (let bits = 0; bits < (1 << atoms.length); bits++) {
      const st = atoms.filter((_, i) => bits >> i & 1);
      states++;
      const offer = offered(spec, ui, st, me, persons);
      for (const a of spec.acts) {
        const ps = []; for (let k = 1; k <= a.args.length; k++) if (k !== a.me) ps.push(k);
        let vals = [[]]; for (const _ of ps) vals = vals.flatMap(v => rest.filter(x => !v.includes(x)).map(x => v.concat([x])));
        for (const vs of vals) {
          const v = {[a.me]: me}; ps.forEach((k, j) => v[k] = vs[j]);
          const ok = a.req.every(([p, ks]) => has(st, p, ks.map(k => v[k]))) && !a.forb.some(([p, ks]) => has(st, p, ks.map(k => v[k])));
          if (!ok) continue;
          answers++;
          const b = a.name + '(' + a.args.map((_, k) => v[k + 1]).join(',') + ')';
          if (!offer.has(b)) { missed++; if (examples.length < 3) examples.push({state: st.map(([p, xs]) => xs.length ? p + '(' + xs + ')' : p), answer: b}); }
        }
      }
    }
    return {states, answers, missed, examples};
  }

  /* the predicates whose atoms each name a person: every atom an act adds names one */
  function personal(sim) {
    const out = {};
    for (const s of sim.schemas) {
      const ps = new Set(s.persons.concat(Object.keys(s.one)));
      for (const c of s.effects) if (c.p && !c.neg && !(sim.env || []).includes(c.p)) out[c.p] = (out[c.p] ?? true) && c.args.slice(1).some(x => ps.has(x));
    }
    return Object.keys(out).filter(p => out[p]);
  }
  /* The look: how the app is drawn, one sentence a line, keyed by the names the description uses.
   *
   *   STYLE Chats:                               optional first line
   *   Text IS LONG.                             a typed value: a large box (SHORT, a line)
   *   Sum IS A SLIDER FROM 1 TO 100.            a typed number: a slider; ", OR TYPED" adds a box beside it
   *   Day IS A DATE.                            a typed number: a calendar
 *   Bob IS SCANNED.                           a typed person: her key scanned from her phone's code, or typed
 *   Board IS A CHESSBOARD.                    a value: drawn as a board, here a position of chess
 *   Move IS PICKED ON THE CHESSBOARD.         a typed value: picked by tapping the board drawn beside it, or typed
   *   Groups ARE A GRID.                        a list: a grid of cards (ROWS, the default; CHIPS)
   *   Unfriend IS IN THE MENU.                  an action: in the menu (IN THE BAR; PRIMARY, first and marked)
   *   COLOR IS night.  CORNERS ARE SQUARE.  SPACING IS COMPACT.  LISTS ARE TABS.
   *   CONVERSATIONS ARE PLAIN.  QUESTIONS ARE IN A BELL.
   *
   * No sentence hides anything; the checks are that each name is the description's, that each widget fits its
   * value, and that no look loses an option: a slider covers every value the contract allows, or is OR TYPED. */
  const GLOBAL = {
    COLOR: {key: 'theme', vals: {FRESH: 'fresh', NIGHT: 'night', PAPER: 'paper', CANDY: 'candy'}},
    CORNERS: {key: 'shape', vals: {ROUND: 'round', SQUARE: 'square'}},
    SPACING: {key: 'density', vals: {COZY: 'cozy', COMPACT: 'compact'}},
    LISTS: {key: 'home', vals: {SECTIONS: 'sections', TABS: 'tabs'}},
    CONVERSATIONS: {key: 'convo', vals: {BUBBLES: 'bubbles', PLAIN: 'plain'}},
    QUESTIONS: {key: 'questions', vals: {'ON TOP': 'top', 'IN A BELL': 'bell'}},
    ACTIONS: {key: 'actions', vals: {'IN THE BAR': 'bar', 'IN THE MENU': 'menu', 'ON A BUTTON': 'button'}},
  };
  function readLook(text) {
    const out = {name: null, global: {}, of: {}, lines: [], errors: []};
    let n = 0;
    for (const raw of (text || '').split('\n')) {
      n++;
      const s = raw.replace(/#.*$/, '').trim().replace(/\.$/, '');
      const L = {n, raw, kind: s ? 'rule' : raw.trim().startsWith('#') ? 'comment' : 'blank'};
      out.lines.push(L);
      if (!s) continue;
      const err = m => { out.errors.push({line: n, msg: m}); L.kind = 'bad'; };
      let m;
      if (n === out.lines.filter(x => x.kind !== 'blank' && x.kind !== 'comment').length && (m = s.match(/^STYLE\s+([A-Za-z][\w-]*)\s*:$/))) { out.name = m[1]; L.kind = 'head'; continue; }
      if ((m = s.match(/^([A-Z]+)\s+(?:IS|ARE)\s+(.+)$/)) && GLOBAL[m[1]]) {
        const g = GLOBAL[m[1]], v = g.vals[m[2].toUpperCase()];
        if (!v) { err(m[1] + ' is one of ' + Object.keys(g.vals).join(', ')); continue; }
        out.global[g.key] = v; L.global = g.key; continue;
      }
      if (!(m = s.match(/^([A-Z][\w' -]*?)\s+(IS|ARE)\s+(.+)$/))) { err('expected: Name IS ... or Name ARE ...'); continue; }
      const name = m[1].trim(), what = m[3].trim();
      let r = null, q;
      if (/^(LONG|SHORT)$/.test(what)) r = {as: what.toLowerCase()};
      else if ((q = what.match(/^A SLIDER FROM (-?\d+) TO (-?\d+)(,\s*OR TYPED)?$/))) r = {as: 'slider', from: +q[1], to: +q[2], typed: !!q[3]};
      else if (what === 'A DATE') r = {as: 'date'};
      else if (what === 'A CHESSBOARD') r = {as: 'chessboard'};
      else if (what === 'SCANNED') r = {as: 'scanned'};
      else if (what === 'PICKED ON THE CHESSBOARD') r = {as: 'board-pick'};
      else if (/^(A GRID|ROWS|CHIPS)$/.test(what)) r = {as: what === 'A GRID' ? 'grid' : what.toLowerCase()};
      else if (/^(IN THE MENU|IN THE BAR|PRIMARY)$/.test(what)) r = {as: what === 'PRIMARY' ? 'primary' : what === 'IN THE MENU' ? 'menu' : 'bar'};
      else { err('not a style: LONG, SHORT, A SLIDER FROM 1 TO 100, A DATE, SCANNED, A CHESSBOARD, PICKED ON THE CHESSBOARD, A GRID, ROWS, CHIPS, IN THE MENU, IN THE BAR or PRIMARY'); continue; }
      if (r.as === 'slider' && r.from >= r.to) { err('a slider goes from a smaller number to a larger one'); continue; }
      if (out.of[name]) { err(name + ' has a style already'); continue; }
      out.of[name] = {...r, line: n}; L.name = name;
    }
    return out;
  }
  /* the numbers of a schema's constraint, and the bounds on each that are numerals */
  function numbersOf(g, out = {names: new Set(), lo: {}, hi: {}}, top = true) {
    if (!g || typeof g !== 'object') return out;
    if (g.f === 'num' && typeof g.a[0] === 'string') out.names.add(g.a[0]);
    if (g.f === 'eq' && g.a.some(e => e && e.f === 'size')) for (const e of g.a) if (e && e.f === 'num' && typeof e.a[0] === 'string') out.lo[e.a[0]] = Math.max(out.lo[e.a[0]] ?? -Infinity, 0);   /* a count of things is never negative */
    const lit = e => e && e.f === 'num' && typeof e.a[0] === 'number' ? e.a[0] : null, nm = e => e && e.f === 'num' && typeof e.a[0] === 'string' ? e.a[0] : null;
    if (top && ['ge', 'gt', 'le', 'lt'].includes(g.f)) {
      const [x, y] = g.a, X = nm(x), Y = lit(y), Xl = lit(x), Yn = nm(y);
      const bound = (v, f, k) => { if (f === 'ge') out.lo[v] = Math.max(out.lo[v] ?? -Infinity, k); if (f === 'gt') out.lo[v] = Math.max(out.lo[v] ?? -Infinity, k + 1); if (f === 'le') out.hi[v] = Math.min(out.hi[v] ?? Infinity, k); if (f === 'lt') out.hi[v] = Math.min(out.hi[v] ?? Infinity, k - 1); };
      if (X && Y !== null) bound(X, g.f, Y);
      if (Yn && Xl !== null) bound(Yn, {ge: 'le', gt: 'lt', le: 'ge', lt: 'gt'}[g.f], Xl);
    }
    for (const a of g.a || []) (Array.isArray(a) ? a : [a]).forEach(e => numbersOf(e, out, top && g.f === 'and'));
    return out;
  }
  function checkLook(spec, ui, look) {
    const errors = [...look.errors], err = (i, m) => errors.push({line: i, msg: m});
    const fields = {}, lists = {}, buttons = {};
    for (const C of Object.values(ui.containers)) for (const c of C.components) {
      if (c.kind === 'list' && !c.hidden) lists[c.title] = c;
      if (c.kind !== 'action' || !c.act) continue;
      (buttons[c.button] = buttons[c.button] || []).push(c);
      const a = spec.acts.find(x => x.name === c.act);
      for (const [r, v] of Object.entries(c.bind)) if (v && v.field) (fields[(c.names || {})[r] || r] = fields[(c.names || {})[r] || r] || []).push({a, r});
    }
    const loses = [];
    for (const [name, r] of Object.entries(look.of)) {
      if (r.as === 'chessboard') {
        /* a value the description names, in a fact a screen is about or as a typed value; drawing it shows it */
        const named = Object.values(ui.containers).some(C => C.names && name in C.names) || fields[name];
        if (!named) err(r.line, name + ' is no value the description names');
        continue;
      }
      if (['long', 'short', 'slider', 'date', 'board-pick', 'scanned'].includes(r.as)) {
        if (!fields[name]) { err(r.line, name + ' is no value the description has TYPED'); continue; }
        for (const {a, r: place} of fields[name]) {
          const ns = numbersOf(a.guard), num = ns.names.has(place) || (a.types || {})[place] === 'num';
          if ((r.as === 'slider' || r.as === 'date') && !num) { err(r.line, name + ' is not a number in ' + a.button + ', so it is no ' + r.as); continue; }
          if ((r.as === 'long' || r.as === 'short' || r.as === 'board-pick') && num) { err(r.line, name + ' is a number in ' + a.button + ', so it is no text'); continue; }
          if (r.as === 'scanned' && !(a.pnames || []).includes(place)) { err(r.line, name + ' is not a person in ' + a.button + ', so it has no code to scan'); continue; }
          if (r.as === 'board-pick' && !Object.values(look.of).some(x => x.as === 'chessboard')) { err(r.line, 'there is no CHESSBOARD to pick ' + name + ' on'); continue; }
          if (r.as === 'slider' && !r.typed) {
            const lo = ns.lo[place] ?? -Infinity, hi = ns.hi[place] ?? Infinity, lost = [];
            const span = (x, y) => x === y ? String(x) : 'from ' + x + ' to ' + y;
            if (lo < r.from) lost.push(lo === -Infinity ? 'below ' + r.from : span(lo, r.from - 1));
            if (hi > r.to) lost.push(hi === Infinity ? 'above ' + r.to : span(r.to + 1, hi));
            if (lost.length) loses.push({line: r.line, name, act: a.button, lost: lost.join(' and ')});
          }
        }
        const mine = loses.filter(x => x.line === r.line);
        if (mine.length) err(r.line, 'this slider loses values the contract allows, ' + mine[0].lost + ', in ' + [...new Set(mine.map(x => x.act))].join(', ') + ': with ", OR TYPED" they can still be typed');
      } else if (['grid', 'rows', 'chips'].includes(r.as)) { if (!lists[name]) err(r.line, name + ' is no list of the description'); }
      else if (!buttons[name]) err(r.line, name + ' is no button of the description');
    }
    return {errors, loses, ok: !errors.length};
  }
  const api = {read, resolve, check, brute, acts, personal, fact, template, readLook, checkLook, GLOBAL};
  if (typeof module !== 'undefined') module.exports = api; else G.GUI = api;
})(this);
