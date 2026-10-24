#!/usr/bin/env python3
# measure.py --- the monitor and the measures of CSSN's simulation (CSSN's paper,
# Section 5, "What is measured"; CSSN #6 Cowork, Code_inbox 2026-10-19 13:46 UTC,
# items 1 and 2), read from the log of a run under sGLP (sGLP's paper, Section 3,
# Definition "Log"; Section 4, "The log": one line per answer, "<time>\t<agent>\t
# <type>\t<term>", one line per element of the program's output each agent's
# screen shows, "<time>\t<agent>\toutput\t<element>", and the clock last).  The
# monitor and the measures are measurement, no part of the app.
#
#   python3 measure.py <log> <CSSN.glp> [--edges <file>] [--json <file>] [--months <k>] [--horizon <s>]
#
# <CSSN.glp> is the contract the run's platform was compiled from (the cascade
# record's copy); --months the months measured (12); --horizon the run's horizon
# in seconds, the state at the end of the log being the state at it.
#
# The agents: agent a's key is the key its first menu shows as its own,
# menu(Me, Ks, R), or, for a child, which has no menu, the child of the enter
# whose hello it answers, failing that its place in the census's order; its role
# is the census's, read off the key, v<village>f<family>p<n> a parent and
# v<village>f<family>c<n> a child, or, for a key of another form, a child where
# a view of the agent records it one.  Written for the platform of
# cascade/CSSN/2026-10-24 (any number of parents: enter, make_parent,
# end_parent, of_age, retire; created, creator, parenting, made; the relay acts
# child_meet and child_meet_2), whose view is view(As, Ks, Approved, Tabs) and
# whose approval is approval(Asking, Words, YesNo), Asking an attempt
# attempt(I, N, Rk, C, Names) or a meeting's hello(I, C), Words printing text(...)
# unquoted.
#
# The commits: an agent's view, view(As, Ks, ...), is its machine state As and the
# persons it knows Ks (vGLP's canonical compilation writes it where the state
# changes); a change of As from one view of an agent to its next is the agent's
# effect in a committed transaction.  Each such change is matched to a request,
# propose(C) answered to the menu of an agent I, whose schema (CSSN.glp, the
# repository's copy of the contract) at C's binding has exactly that effect at
# that agent's role, a listed role (cschema, crs(listed(J, P, Args), ...)) being
# every agent X with P(X, Args) in the state of the agent at role J; the
# transaction is committed once every participant whose effect is not empty has
# shown it.  A change matched to no request is reported.
#
# The monitor (CSSN #6, item 1): at the commit of every act a parent guards ---
# the children's schemas, enter, make_parent, end_parent, of_age, retire,
# child_meet, child_meet_2, child_befriend, child_adult_befriend, child_unfriend,
# child_join, child_join_2, child_leave, child_close --- it checks that each of
# its guards approved, the initiator by the request and every other guard by an
# answer yes, between the request and the commit, to an attempt (I, N, C) of
# the initiator I or to its meeting's hello(I, C), and that its precondition
# held: each atom the schema requires at a role in the state of the agent at
# that role, and each it forbids not, friend and knows being reflexive, in the
# states the agents' last views before the commit show.  Every exception is
# reported.
#
# The measures (Section 5, items 2 to 4): the child friendships at the end of
# each week and each month, a run that ends within one giving its state at the
# horizon and the fraction covered, a month being the twelfth of a year of 365.25 days, 2,629,800 s,
# as sGLP's reader of the social graph has it; the parents' requests for a
# child friendship, child_befriend or child_adult_befriend, those that commit,
# and the days from such a request to its commit; the approvals each parent
# answers each month, the hellos of meetings among them; the month by whose end the parents' friendship graph,
# and the children's, is one connected component; and the acquaintance graph at
# the end of the run, the persons each agent's last view shows it knows.
import sys, json, re
from collections import Counter, defaultdict

MONTH = 2629800.0
WEEK = 604800.0
# The acts a parent guards: the children's schemas of CSSN.glp at cascade/CSSN/2026-10-24 (any number of parents).
MONITORED = ["enter", "make_parent", "end_parent", "of_age", "retire", "child_meet", "child_meet_2",
             "child_befriend", "child_adult_befriend", "child_unfriend", "child_join", "child_join_2",
             "child_leave", "child_close"]

# ---- GLP terms as the log prints them ----------------------------------------
# A text(...) is the platform's words, printed unquoted, spaces and all (text( asks that )): read whole.
TOK = re.compile(r"\s*(?:(?P<text>text\([^)]*\))|(?P<num>\d+\.\d+(?:[eE][-+]?\d+)?|\d+)|(?P<q>'(?:[^'\\]|\\.)*')|(?P<id>[A-Za-z_][A-Za-z0-9_]*)|(?P<p>.))")

def tokens(s):
    out = []
    pos = 0
    while pos < len(s):
        m = TOK.match(s, pos)
        if not m:
            break
        pos = m.end()
        if m.group("text"): out.append(("text", m.group("text")[5:-1]))
        elif m.group("num"): out.append(("num", m.group("num")))
        elif m.group("q"): out.append(("atom", m.group("q")[1:-1]))
        elif m.group("id"): out.append(("atom", m.group("id")))
        elif m.group("p") and not m.group("p").isspace(): out.append(("p", m.group("p")))
    return out

class P:
    def __init__(self, s):
        self.t = tokens(s); self.i = 0
    def peek(self): return self.t[self.i] if self.i < len(self.t) else ("eof", "")
    def take(self):
        x = self.peek(); self.i += 1; return x
    def term(self):
        k, v = self.take()
        if k == "text": return ("text", (v,))
        if k == "num": return float(v) if "." in v else int(v)
        if k == "atom":
            if self.peek() == ("p", "("):
                self.take(); args = [self.term()]
                while self.peek() == ("p", ","):
                    self.take(); args.append(self.term())
                assert self.take() == ("p", ")")
                return (v, tuple(args))
            if self.peek() == ("p", "?"):
                self.take()
            return v
        if (k, v) == ("p", "["):
            items = []
            if self.peek() == ("p", "]"):
                self.take(); return tuple(["$list"])
            items.append(self.term())
            tail = None
            while True:
                n = self.take()
                if n == ("p", ","): items.append(self.term())
                elif n == ("p", "|"): tail = self.term()
                elif n == ("p", "]"): break
                else: raise ValueError("list: %r" % (n,))
            return tuple(["$list"] + items) if tail is None else tuple(["$list"] + items + [("$tail", tail)])
        raise ValueError("term: %r %r" % (k, v))

def parse(s): return P(s).term()
def is_list(x): return isinstance(x, tuple) and len(x) >= 1 and x[0] == "$list"
def elems(x): return [e for e in x[1:] if not (isinstance(e, tuple) and e and e[0] == "$tail")]
def show(x):
    if is_list(x): return "[" + ", ".join(show(e) for e in elems(x)) + "]"
    if isinstance(x, tuple): return "%s(%s)" % (x[0], ", ".join(show(a) for a in x[1]))
    return str(x)

# ---- The schemas of the contract (CSSN.glp) ----------------------------------
# A cschema's listed role, crs(listed(J, Pred, Args), ...), is every agent X with Pred(X, Args) in the state of
# the agent at role J (GSC, Appendix D.4).  A request names the class by its other roles in order, then the
# schema's variables in the order of its second argument (the platform's binding_of and request_name).
def schemas(path):
    text = open(path).read()
    text = "\n".join(l.split("%%")[0] if not l.lstrip().startswith("%") else "" for l in text.split("\n"))
    out = {}
    for m in re.finditer(r"\b(c?schema)\(", text):
        p = P(text[m.start():]); t = p.term()
        if not (isinstance(t, tuple) and len(t[1]) >= 3 and is_list(t[1][1]) and is_list(t[1][-1])): continue
        if t[0] == "schema": name, args, roles = t[1][0], t[1][1], t[1][2]
        else: name, args, roles = t[1][0], t[1][1], t[1][3]
        rs = []
        for r in elems(roles):
            listed = None
            if r[0] == "rs": g, adds, dels, req, forb = r[1]
            else:
                kind = r[1][0]
                g, adds, dels, req, forb = r[1][1:]
                if isinstance(kind, tuple) and kind[0] == "listed":
                    listed = (kind[1][0], kind[1][1], elems(kind[1][2]))
                else:
                    listed = ("unsupported", kind, [])
            rs.append({"guarded": g == "guarded", "add": elems(adds), "del": elems(dels), "req": elems(req),
                       "forb": elems(forb), "listed": listed})
        out[name] = {"args": elems(args), "roles": rs}
    return out

def env_of(sch, binding):
    """The request's binding as {('role', i): agent, ('var', name): value}; None if it does not fit."""
    fixed = [i for i, r in enumerate(sch["roles"]) if r["listed"] is None]
    if len(binding) != len(fixed) + len(sch["args"]): return None
    env = {}
    for j, i in enumerate(fixed): env[("role", i + 1)] = binding[j]
    for j, v in enumerate(sch["args"]): env[("var", v[1][0])] = binding[len(fixed) + j]
    return env

def subst_arg(a, env):
    if isinstance(a, tuple) and a[0] == "role": return env.get(("role", a[1][0]))
    if isinstance(a, tuple) and a[0] in ("pvar", "svar"): return env.get(("var", a[1][0]))
    return None   # a name term (a group's community name): not read here

def subst(atom, env):
    # atom(friend, [role(2)]) at env {role 2: k2} -> "friend(k2)"
    name, args = atom[1]
    vals = []
    for a in elems(args):
        v = subst_arg(a, env)
        if v is None: return None
        vals.append(str(v))
    return name if not vals else "%s(%s)" % (name, ", ".join(vals))

def participants(sch, binding, state):
    """[(role index, agent, role spec)], a listed role expanded to every agent the state of its lister lists."""
    env = env_of(sch, binding)
    if env is None: return None, None
    out = []
    for i, r in enumerate(sch["roles"]):
        if r["listed"] is None:
            out.append((i, env[("role", i + 1)], r)); continue
        j, pred, largs = r["listed"]
        if j == "unsupported": return None, None
        lister = env.get(("role", j))
        rest = [subst_arg(a, env) for a in largs]
        if lister is None or any(v is None for v in rest): return None, None
        suffix = "".join(", %s" % v for v in rest)
        for atom, n in state.get(lister, Counter()).items():
            if n > 0 and atom.startswith(pred + "(") and atom.endswith(suffix + ")"):
                x = atom[len(pred) + 1:len(atom) - len(suffix) - 1]
                if x and "," not in x: out.append((i, x, r))
    return env, out

def effects(sch, binding, state):
    """Per agent: (adds Counter, dels Counter), the join where one agent holds two roles."""
    env, parts = participants(sch, binding, state)
    if parts is None: return None
    eff = defaultdict(lambda: [Counter(), Counter()])
    for i, k, r in parts:
        e = eff[k]
        for a in r["add"]:
            s = subst(a, env)
            if s is None: return None
            e[0][s] += 1
        for a in r["del"]:
            s = subst(a, env)
            if s is None: return None
            e[1][s] += 1
    return {k: (v[0], v[1]) for k, v in eff.items()}

def atom_str(a):
    return show(a)

# ---- The log ------------------------------------------------------------------
def read_log(path):
    lines = []
    clock = None
    for raw in open(path):
        raw = raw.rstrip("\n")
        f = raw.split("\t")
        if len(f) == 4:
            lines.append((float(f[0]), int(f[1]), f[2], f[3]))
        elif len(f) == 1 and f[0]:
            try: clock = float(f[0])
            except ValueError: pass
    return lines, clock

def role_of_key(k):
    # v<village>f<family>p<n> a parent (a<n> on the platform of cascade/CSSN/2026-10-21-3), v<village>f<family>c<n> a child.
    m = re.match(r"^v(\d+)f(\d+)([apc])(\d+)$", str(k))
    if not m: return None, None, None
    return ("parent" if m.group(3) in "ap" else "child"), int(m.group(1)), int(m.group(2))

def components(nodes, edges):
    par = {n: n for n in nodes}
    def f(x):
        while par[x] != x:
            par[x] = par[par[x]]; x = par[x]
        return x
    for a, b in edges:
        if a in par and b in par:
            ra, rb = f(a), f(b)
            if ra != rb: par[ra] = rb
    return len({f(n) for n in nodes})

def main():
    args = sys.argv[1:]
    logp, glp = args[0], args[1]
    opt = {}
    i = 2
    while i < len(args):
        opt[args[i]] = args[i + 1]; i += 2
    S = schemas(glp)
    lines, clock = read_log(logp)
    months = int(opt.get("--months", "12"))

    key_of = {}
    for t, a, ty, term in lines:
        if ty == "Menu" and a not in key_of:
            key_of[a] = parse(term)[1][0]
    # An agent with no menu (a child of the census, which answers only approvals) is the child its creator's enter
    # names: it answers the hello of enter(I, C) as C.  Failing that, it is keyed by its place in the census's order,
    # a family's parents then its children, v<village>f<family>c<n> numbered from 1 after the family's last parent.
    census_key = {}
    seen = sorted({a for t, a, ty, term in lines if a}, key=lambda s: int(s) if str(s).isdigit() else 0)
    fam, kid = None, 0
    for a in seen:
        if a in key_of:
            m = re.match(r"(v\d+f\d+)p\d+$", str(key_of[a]))
            if m and m.group(1) != fam: fam, kid = m.group(1), 0
        elif fam is not None:
            kid += 1; census_key[a] = "%sc%d" % (fam, kid)
    key_mismatch = []
    for t, a, ty, term in lines:
        if ty == "Approval" and a not in key_of:
            x = parse(term)[1][0]
            if isinstance(x, tuple) and x[0] == "hello" and isinstance(x[1][1], tuple) and x[1][1][0] == "enter":
                key_of[a] = str(x[1][1][1][1])
    for a, kk in census_key.items():
        if a not in key_of: key_of[a] = kk
        elif key_of[a] != kk: key_mismatch.append((a, key_of[a], kk))
    agents = sorted(key_of)
    role = {key_of[a]: role_of_key(key_of[a])[0] for a in agents}
    village = {key_of[a]: role_of_key(key_of[a])[1] for a in agents}
    # A key not of the census's form: a child is an agent a view of which records it a child.
    if any(r is None for r in role.values()):
        kids = set()
        for t, a, ty, term in lines:
            if ty == "output" and term.startswith("view(") and a in key_of:
                if "child" in (atom_str(e) for e in elems(parse(term)[1][0])): kids.add(key_of[a])
        for k in role:
            if role[k] is None: role[k] = "child" if k in kids else "parent"
    parents = [k for k in role if role[k] == "parent"]
    children = [k for k in role if role[k] == "child"]

    state = {k: Counter() for k in role}          # the machine state the agent's last view shows
    knows = {k: set() for k in role}
    requests = []                                  # dicts: idx t I C cls binding committed
    pending = defaultdict(list)                    # (I, C) -> requests not yet committed
    approvals = defaultdict(dict)                  # (I, N, C) -> {guard: (t, yes/no)}
    approvals_by_parent_month = Counter()
    hellos_by_parent_month = Counter()
    week_child_edges = {}
    week_end_idx = 1
    open_commits = {}                              # id(request) -> {"need": set(agents), "snap": {...}, "t": t}
    commits = []
    exceptions = []
    unexplained = []
    notices = Counter()
    month_child_edges = {}
    month_parent_one = {}
    month_child_one = {}
    month_end_idx = 1

    def child_edges():
        e = set()
        for r in children:
            for a, n in state[r].items():
                if n > 0 and a.startswith("friend("):
                    s = a[7:-1]
                    if role.get(s) == "child" and s != r: e.add(tuple(sorted((r, s))))
        return e

    def parent_edges():
        e = set()
        for p in parents:
            for a, n in state[p].items():
                if n > 0 and a.startswith("friend("):
                    q = a[7:-1]
                    if role.get(q) == "parent" and q != p: e.add(tuple(sorted((p, q))))
        return e

    def snapshot_months(upto_t):
        nonlocal month_end_idx, week_end_idx
        while week_end_idx * WEEK <= upto_t and week_end_idx <= 60:
            week_child_edges[week_end_idx] = len(child_edges())
            week_end_idx += 1
        while month_end_idx <= months and month_end_idx * MONTH <= upto_t:
            ce = child_edges(); pe = parent_edges()
            month_child_edges[month_end_idx] = len(ce)
            month_parent_one[month_end_idx] = components(parents, pe) == 1
            month_child_one[month_end_idx] = components(children, ce) == 1
            month_end_idx += 1

    def check_pre(cls, binding, snap):
        # friend and knows are reflexive (CSSN.glp, reflexive([friend, knows], C)).
        sch = S[cls]; bad = []
        env, parts = participants(sch, binding, snap)
        if parts is None: return ["the binding %s does not fit %s" % (binding, cls)]
        for i, k, r in parts:
            st = snap.get(k, Counter())
            refl = ("friend(%s)" % k, "knows(%s)" % k)
            for a in r["req"]:
                s = subst(a, env)
                if s is None: bad.append("%s: %s not read" % (k, show(a))); continue
                if s in refl: continue
                if st[s] <= 0: bad.append("%s lacks %s" % (k, s))
            for a in r["forb"]:
                s = subst(a, env)
                if s is None: bad.append("%s: %s not read" % (k, show(a))); continue
                if s in refl or st[s] > 0: bad.append("%s holds %s" % (k, s))
        return bad

    for idx, (t, a, ty, term) in enumerate(lines):
        snapshot_months(t)
        k = key_of.get(a)
        if ty == "Menu":
            x = parse(term)
            rq = x[1][2]
            if isinstance(rq, tuple) and rq[0] == "propose":
                c = rq[1][0]
                cls = c[0] if isinstance(c, tuple) else c
                binding = [str(v) for v in (c[1] if isinstance(c, tuple) else ())]
                C = show(c)
                r = {"idx": idx, "t": t, "I": k, "C": C, "cls": cls, "binding": binding, "commit": None}
                requests.append(r)
                pending[(k, C)].append(r)
        elif ty == "Approval":
            x = parse(term)
            # approval(Asking, Words, YesNo) on the platform of cascade/CSSN/2026-10-24, Asking an attempt
            # attempt(I, N, Rk, C, Names) or the hello of a meeting, hello(I, C), which names no attempt.
            at, yn = x[1][0], x[1][-1]
            if at[0] == "attempt":
                I, N, Rk, c = at[1][:4]
                approvals[(str(I), N, show(c))][k] = (t, yn)
                if role.get(k) == "parent":
                    approvals_by_parent_month[min(int(t // MONTH) + 1, months)] += 1
            elif at[0] == "hello":
                I, c = at[1][:2]
                approvals[(str(I), "hello", show(c))][k] = (t, yn)
                if role.get(k) == "parent":
                    hellos_by_parent_month[min(int(t // MONTH) + 1, months)] += 1
        elif ty == "output":
            x = parse(term)
            if x[0] == "view":
                As, Ks = x[1][:2]   # view(As, Ks, Approved, Tabs) on the platform of cascade/CSSN/2026-10-24
                new = Counter(atom_str(e) for e in elems(As))
                knows[k] = set(str(e) for e in elems(Ks))
                adds = new - state[k]; dels = state[k] - new
                if adds or dels:
                    # The commit this change belongs to: an open one awaiting k, or a request
                    # whose schema has this effect at k.
                    found = None
                    for rid, oc in list(open_commits.items()):
                        r = oc["req"]
                        if k in oc["need"] and oc["eff"][k] == (adds, dels):
                            found = rid; break
                    if found is None:
                        cands = []
                        for (I, C), lst in pending.items():
                            for r in lst:
                                if r["t"] > t or r["cls"] not in S: continue
                                eff = effects(S[r["cls"]], r["binding"], state)
                                if eff is None or k not in eff: continue
                                if eff[k] == (adds, dels):
                                    cands.append((r["t"], r, eff))
                        if cands:
                            cands.sort(key=lambda z: z[0])
                            _, r, eff = cands[-1]   # the latest request before the change
                            need = {q for q, e in eff.items() if e[0] or e[1]}
                            snap = {q: Counter(v) for q, v in state.items()}
                            rid = id(r)
                            open_commits[rid] = {"req": r, "need": need, "eff": eff, "snap": snap, "t": t}
                            pending[(r["I"], r["C"])].remove(r)
                            found = rid
                    if found is None:
                        unexplained.append({"t": t, "agent": k, "add": dict(adds), "del": dict(dels)})
                    else:
                        oc = open_commits[found]
                        oc["need"].discard(k)
                        if not oc["need"]:
                            r = oc["req"]; del open_commits[found]
                            r["commit"] = t
                            commits.append(r)
                            if r["cls"] in MONITORED:
                                sch = S[r["cls"]]
                                guards = []
                                _, parts = participants(sch, r["binding"], oc["snap"])
                                for i2, g, ro in (parts or []):
                                    if ro["guarded"] and g not in guards: guards.append(g)
                                others = [g for g in guards if g != r["I"]]
                                # Each other guard answered yes, between the request and the commit, to an attempt
                                # (I, N, C) of the initiator or to the hello of its meeting, hello(I, C).
                                missing = []
                                for g in others:
                                    if not any(I2 == r["I"] and C2 == r["C"] and g in ans and ans[g][1] == "yes" and r["t"] <= ans[g][0] <= t
                                               for (I2, N2, C2), ans in approvals.items()):
                                        missing.append(g)
                                why = []
                                if r["I"] not in guards: why.append("initiator %s is no guard" % r["I"])
                                if missing: why.append("no yes of %s to an attempt or hello of %s" % (", ".join(missing), r["I"]))
                                why += check_pre(r["cls"], r["binding"], oc["snap"])
                                if why:
                                    exceptions.append({"t": t, "class": r["C"], "why": why})
                    state[k] = new
            elif x[0] in ("refused", "yielded"):
                notices[x[0]] += 1
    # A run whose monitor stopped at its horizon ends in the state at the horizon.
    end_t = float(opt["--horizon"]) if "--horizon" in opt else (float("inf") if clock is None else clock + 1)
    snapshot_months(end_t)
    # A run shorter than its last month or week ends in it: that month's and that week's figures are the state at the
    # horizon, and the fraction of it the run covered is reported.
    partial_month = partial_week = None
    if end_t != float("inf"):
        if month_end_idx <= months and (month_end_idx - 1) * MONTH < end_t:
            partial_month = round(end_t / MONTH - (month_end_idx - 1), 3)
            ce = child_edges(); pe = parent_edges()
            month_child_edges[month_end_idx] = len(ce)
            month_parent_one[month_end_idx] = components(parents, pe) == 1
            month_child_one[month_end_idx] = components(children, ce) == 1
        if (week_end_idx - 1) * WEEK < end_t:
            partial_week = round(end_t / WEEK - (week_end_idx - 1), 3)
            week_child_edges[week_end_idx] = len(child_edges())

    # ---- The measures ----
    cb = [r for r in requests if role.get(r["I"]) == "parent" and r["cls"] in ("child_befriend", "child_adult_befriend")]
    cbc = [r for r in cb if r["commit"] is not None]
    days = [(r["commit"] - r["t"]) / 86400.0 for r in cbc]
    horizon = clock if clock is not None else (lines[-1][0] if lines else 0)
    nmonths = max(1, min(months, int(-(-horizon // MONTH))))
    appr = [(approvals_by_parent_month[m] + hellos_by_parent_month[m]) / max(1, len(parents)) for m in range(1, nmonths + 1)]
    appr_att = [approvals_by_parent_month[m] / max(1, len(parents)) for m in range(1, nmonths + 1)]
    def first_one(d):
        for m in range(1, months + 1):
            if d.get(m): return m
        return None
    acq = set()
    for k2 in role:
        for q in knows[k2]:
            if q in role and q != k2: acq.add(tuple(sorted((k2, q))))
    deg = Counter()
    for x, y in acq: deg[x] += 1; deg[y] += 1
    within = sum(1 for x, y in acq if village[x] == village[y])
    res = {
        "agents": len(role), "parents": len(parents), "children": len(children), "clock": clock,
        "answers": sum(1 for l in lines if l[2] != "output"), "outputs": sum(1 for l in lines if l[2] == "output"),
        "requests": len(requests), "commits": len(commits),
        "commits_by_class": dict(Counter(r["cls"] for r in commits)),
        "requests_by_class": dict(Counter(r["cls"] for r in requests)),
        "notices": dict(notices),
        "monitor_exceptions": exceptions, "unexplained_changes": unexplained[:50], "unexplained_count": len(unexplained),
        "open_commits_at_end": len(open_commits),
        "child_requests": len(cb), "child_requests_committed": len(cbc),
        "child_commit_fraction": (len(cbc) / len(cb)) if cb else None,
        "days_to_commit_mean": (sum(days) / len(days)) if days else None,
        "days_to_commit_max": max(days) if days else None,
        "approvals_per_parent_month": appr,
        "approvals_per_parent_month_attempts_only": appr_att,
        "child_friendships_by_week": [week_child_edges.get(w) for w in range(1, len(week_child_edges) + 1)],
        "last_week_partial": partial_week, "last_month_partial": partial_month,
        "child_friendship_requests_by_class": dict(Counter(r["cls"] for r in requests if r["cls"] in ("child_befriend", "child_adult_befriend"))),
        "key_census_mismatch": key_mismatch,
        "approvals_per_parent_month_mean": sum(appr) / len(appr) if appr else None,
        "child_friendships_by_month": [month_child_edges.get(m) for m in range(1, months + 1)],
        "child_friendships_end": len(child_edges()),
        "parent_friendships_end": len(parent_edges()),
        "parents_one_component_month": first_one(month_parent_one),
        "children_one_component_month": first_one(month_child_one),
        "parents_components_end": components(parents, parent_edges()),
        "children_components_end": components(children, child_edges()),
        "retired_children": sum(1 for r in children if state[r]["retired"] > 0),
        "entered_children": sum(1 for r in children if state[r]["identity"] > 0),
        "acquaintance_edges": len(acq),
        "acquaintance_within_village": within,
        "acquaintance_degree_mean": (2.0 * len(acq) / len(role)) if role else None,
        "acquaintance_degree_min": min((deg[k2] for k2 in role), default=None),
        "acquaintance_degree_max": max((deg[k2] for k2 in role), default=None),
    }
    if "--edges" in opt:
        with open(opt["--edges"], "w") as f:
            for x, y in sorted(acq): f.write("%s %s\n" % (x, y))
    out = json.dumps(res, indent=1, default=str)
    if "--json" in opt:
        open(opt["--json"], "w").write(out + "\n")
    print(out)

if __name__ == "__main__":
    main()
