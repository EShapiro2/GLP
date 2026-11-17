#!/usr/bin/env python3
# measure.py --- the monitor and the measures of CSSN's simulation (CSSN's paper,
# Section 5, "What is measured"; CSSN #6 Cowork, Code_inbox 2026-10-19 13:46 UTC,
# items 1 and 2), read from the log of a run under sGLP (sGLP's paper, Section 3,
# Definition "Log"; Section 4, "The log": one line per answer, "<time>\t<agent>\t
# <type>\t<term>", one line per element of the program's output each agent's
# screen shows, "<time>\t<agent>\toutput\t<element>", and the clock last).  The
# monitor and the measures are measurement, no part of the app.
#
#   python3 measure.py <log> <Children.glp> [--edges <file>] [--json <file>] [--months <k>] [--horizon <s>]
#
# <Children.glp> is the contract the run's platform was compiled from (the cascade
# record's copy); --months the months measured (12); --horizon the run's horizon
# in seconds, the state at the end of the log being the state at it.
#
# The agents: agent a's key is the first argument of its views, view(Key, Atoms,
# Known, Approved, Tabs); its role is the census's, read off the key,
# v<village>f<family>p<n> a parent and v<village>f<family>c<n> a child, or, for a
# key of another form, a child where a view of the agent records it one.  Written
# for the platform of cascade/CSSN/2026-11-16, the contract adopted from the
# Calculus, whose request question is Request?, propose(C), and whose approval is
# approval(Asking, Words, YesNo), Asking an attempt attempt(I, N, Rk, Class, Names),
# the class carrying the lists of its listed roles, or a meeting's hello(I, C).
#
# The commits: an agent's view is its machine state Atoms, the persons it knows
# Known and the approvals its person made that are pending, Approved, each by its
# request name (vGLP's canonical compilation writes it where any of them
# changes).  A request propose(C) of agent I is pending once I's view shows it,
# and ends when I's view no longer shows it: it was refused where a notice
# refused(C, Q, D) to I follows at that instant, and committed otherwise, at that
# instant.  A request refused before it is shown pending is refused by the
# notice alone.
#
# The monitor (CSSN #6, item 1), at the commit of every act: each of its guards
# approved it, the initiator by the request and every other guard by an answer
# yes, between the request and the commit, to an attempt or a hello of the same
# part, whoever's attempt that was; and its precondition held in the states the
# agents' views showed before the instant of the commit, each condition of the
# schema (Children.glp, the repository's copy of the contract) read at the request's
# binding: atom, not, and, or, ne and forall, the reflexive predicates holding of
# their holder, and a listed role's agents those its forall ranges over.  Every
# exception is reported.
#
# The measures (Section 5, items 2 to 4): the child friendships at the end of
# each week and each month, a run that ends within one giving its state at the
# horizon and the fraction covered, a month being the twelfth of a year of 365.25 days, 2,629,800 s,
# as sGLP's reader of the social graph has it; the parents' requests for a
# child friendship, children_befriend, children_of_one_parent_befriend or
# child_befriend, those that commit and those refused, and the days from such a
# request to its commit; the approvals each parent answers each month, the
# hellos of meetings among them; the month by whose end the parents' friendship
# graph, and the children's, is one connected component; and the acquaintance
# graph at the end of the run, the persons each agent's last view shows it knows.
import sys, json, re
from collections import Counter, defaultdict

MONTH = 2629800.0
WEEK = 604800.0
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

# ---- The schemas of the contract (Children.glp) ----------------------------------
# The contract as adopt.py prints it: contract_named(children, reflexive(Preds, [sas(Name, Roles, Vars, Effects, Pre,
# Constraint, Guards), ...])), an atom written atom(Pred, [Holder | Args]) over the role names, and the agent's
# state holding Pred(Args).  A request propose(C) names the roles of C in the order of Roles, then the variables
# in the order of Vars; a role Roles names after them is listed, its agents those a forall of Pre ranges over.
def schemas(path):
    text = open(path).read()
    text = "\n".join("" if l.lstrip().startswith("%") else l for l in text.split("\n"))
    out = {}
    refl = []
    m = re.search(r"reflexive\(", text)
    if m:
        p = P(text[m.end():]); refl = [str(x) for x in elems(p.term())]
    for m in re.finditer(r"\bsas\(", text):
        t = P(text[m.start():]).term()
        name, roles, vars_, eff, pre, cons, guards = t[1]
        rn = [(r[1][0], r[1][1]) if isinstance(r, tuple) and r[0] == "one" else (r, None) for r in elems(roles)]
        out[name] = {"roles": rn, "vars": [str(v) for v in elems(vars_)], "eff": elems(eff), "pre": elems(pre),
                     "cons": cons, "guards": [str(g) for g in elems(guards)]}
    return out, set(refl)

def env_of(sch, args):
    """The request's arguments as {name: value}, the roles named first and then the variables; None if they do not fit."""
    nr = len(args) - len(sch["vars"])
    if nr < 1 or nr > len(sch["roles"]): return None
    env = {}
    for (r, _), v in zip(sch["roles"][:nr], args[:nr]): env[str(r)] = v
    for x, v in zip(sch["vars"], args[nr:]): env[x] = v
    return env

def val(a, env):
    a = str(a) if not isinstance(a, tuple) else show(a)
    return env.get(a, a)

def pattern(atom, env):
    """atom(Pred, [Holder | Args]) at env: the holder and the atom's pattern, '_' and unbound names as wildcards."""
    pred, hargs = atom[1]
    hs = elems(hargs)
    return val(hs[0], env), str(pred), [val(x, env) for x in hs[1:]]

def matches(st, pred, args, bound):
    """The atoms of st matching pred(args), each arg a value, '_' or a name in bound (None: free); yields bindings."""
    for a, n in st.items():
        if n <= 0: continue
        x = parse(a)
        if not args:
            if x == pred: yield {}
            continue
        if not (isinstance(x, tuple) and x[0] == pred and len(x[1]) == len(args)): continue
        b = {}; ok = True
        for want, have in zip(args, x[1]):
            h = show(have)
            if want == "_": continue
            if want in bound and bound[want] is None:
                if want in b and b[want] != h: ok = False; break
                b[want] = h
            elif want != h: ok = False; break
        if ok: yield b

def holds(c, env, snap, refl):
    """The condition c of a schema at env in the states snap: atom, not, and, or, ne, forall; a list is a conjunction."""
    if is_list(c): return all(holds(e, env, snap, refl) for e in elems(c))
    if c == "true": return True
    k = c[0]
    if k == "atom":
        h, pred, args = pattern(c, env)
        if pred in refl and len(args) == 1 and args[0] == h: return True
        return any(True for _ in matches(snap.get(h, Counter()), pred, args, {}))
    if k == "not": return not holds(c[1][0], env, snap, refl)
    if k == "and": return holds(c[1][0], env, snap, refl) and holds(c[1][1], env, snap, refl)
    if k == "or": return holds(c[1][0], env, snap, refl) or holds(c[1][1], env, snap, refl)
    if k == "ne": return val(c[1][0], env) != val(c[1][1], env)
    if k == "forall":
        vs, cond, cons, then = c[1]
        for b in forall_bindings(vs, cond, env, snap):
            e2 = dict(env); e2.update(b)
            if holds(cons, e2, snap, refl) and not holds(then, e2, snap, refl): return False
        return True
    raise ValueError("condition %s" % show(c))

def forall_bindings(vs, cond, env, snap):
    names = [str(v) for v in elems(vs)]
    h, pred, args = pattern(cond, {k: v for k, v in env.items() if k not in names})
    bound = {n: None for n in names}
    return list(matches(snap.get(h, Counter()), pred, args, bound))

def guards_of(sch, env, snap):
    """The agents of the schema's guards, a listed role expanded to every agent a forall of Pre ranges it over."""
    out = []
    for g in sch["guards"]:
        if g in env:
            out.append(env[g]); continue
        for c in sch["pre"]:
            if isinstance(c, tuple) and c[0] == "forall" and g in [str(v) for v in elems(c[1][0])]:
                for b in forall_bindings(c[1][0], c[1][1], env, snap): out.append(b[g])
                break
    seen = []
    for x in out:
        if x not in seen: seen.append(x)
    return seen

def check_pre(sch, env, snap, refl):
    bad = []
    for c in sch["pre"]:
        try:
            if not holds(c, env, snap, refl): bad.append("fails %s" % show(c))
        except ValueError as e:
            bad.append("not read: %s" % e)
    return bad

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
    S, REFL = schemas(glp)
    lines, clock = read_log(logp)
    months = int(opt.get("--months", "12"))

    # An agent's key is the first argument of its views, view(Key, Atoms, Known, Approved, Tabs).
    key_of = {}
    for t, a, ty, term in lines:
        if ty == "output" and term.startswith("view(") and a not in key_of:
            key_of[a] = str(parse(term)[1][0])
    agents = sorted(key_of)
    role = {key_of[a]: role_of_key(key_of[a])[0] for a in agents}
    village = {key_of[a]: role_of_key(key_of[a])[1] for a in agents}
    # A key not of the census's form: a child is an agent a view of which records it a child.
    if any(r is None for r in role.values()):
        kids = set()
        for t, a, ty, term in lines:
            if ty == "output" and term.startswith("view(") and a in key_of:
                if "child" in (atom_str(e) for e in elems(parse(term)[1][1])): kids.add(key_of[a])
        for k in role:
            if role[k] is None: role[k] = "child" if k in kids else "parent"
    parents = [k for k in role if role[k] == "parent"]
    children = [k for k in role if role[k] == "child"]

    state = {k: Counter() for k in role}          # the machine state the agent's last view shows
    before = {k: Counter() for k in role}         # the states before the current instant
    now_t = None
    knows = {k: set() for k in role}
    requests = []                                  # dicts: t I C cls args commit refused
    unresolved = defaultdict(list)                 # (I, C) -> its requests not yet committed or refused, in order
    shown = Counter()                              # (I, C) -> how many of them the initiator's view shows pending
    approvals = []                                 # (t, guard, part C, yes/no)
    asked_at = defaultdict(list)                   # (agent, part C) -> the times it requested the part
    approvals_by_parent_month = Counter()
    hellos_by_parent_month = Counter()
    week_child_edges = {}
    week_end_idx = 1
    commits = []
    exceptions = []
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
                    s2 = a[7:-1]
                    if role.get(s2) == "child" and s2 != r: e.add(tuple(sorted((r, s2))))
        return e

    def parent_edges():
        e = set()
        for p2 in parents:
            for a, n in state[p2].items():
                if n > 0 and a.startswith("friend("):
                    q = a[7:-1]
                    if role.get(q) == "parent" and q != p2: e.add(tuple(sorted((p2, q))))
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

    def part_of(c):
        """An attempt's class as the request that names its part: the class without the lists of its listed roles."""
        if not isinstance(c, tuple): return str(c)
        return show((c[0], tuple(a for a in c[1] if not is_list(a))))

    ended = []                                     # requests that left the pending list at the current instant

    def settle():
        # A request that left its initiator's pending list with no refusal at that instant was committed.
        while ended:
            r = ended.pop(0); committed(r, r["ended"], r["snap"])

    def committed(r, t, snap):
        # The monitor: each guard of the act willed it before the commit, by its request or by an answer yes to an
        # attempt or a hello of the same part, whoever's attempt that was; and its precondition held in the states
        # the agents' views showed before the instant of the commit.
        r["commit"] = t
        commits.append(r)
        sch = S.get(r["cls"])
        if sch is None:
            exceptions.append({"t": t, "class": r["C"], "why": ["no schema %s" % r["cls"]]}); return
        env = env_of(sch, r["args"])
        if env is None:
            exceptions.append({"t": t, "class": r["C"], "why": ["the arguments do not fit %s" % r["cls"]]}); return
        guards = guards_of(sch, env, snap)
        why = []
        if r["I"] not in guards: why.append("initiator %s is not a guard" % r["I"])
        missing = [g for g in guards
                   if not (any(ta <= t for ta in asked_at[(g, r["C"])])
                           or any(g2 == g and C2 == r["C"] and yn == "yes" and ta <= t for ta, g2, C2, yn in approvals))]
        if missing: why.append("%s neither requested %s nor answered yes to an attempt or hello of it" % (", ".join(missing), r["C"]))
        why += check_pre(sch, env, snap, REFL)
        if why: exceptions.append({"t": t, "class": r["C"], "why": why})

    for idx, (t, a, ty, term) in enumerate(lines):
        if t != now_t:
            settle()
            before = {k: Counter(v) for k, v in state.items()}; now_t = t
        snapshot_months(t)
        k = key_of.get(a)
        if ty == "Request?":
            x = parse(term)
            if isinstance(x, tuple) and x[0] == "propose":
                c = x[1][0]
                cls = c[0] if isinstance(c, tuple) else c
                cargs = [show(v) for v in (c[1] if isinstance(c, tuple) else ())]
                C = show(c)
                r = {"t": t, "I": k, "C": C, "cls": cls, "args": cargs, "commit": None, "refused": None}
                requests.append(r)
                unresolved[(k, C)].append(r)
                asked_at[(k, C)].append(t)
        elif ty == "Approval":
            x = parse(term)
            # approval(Asking, Words, YesNo): Asking an attempt attempt(I, N, Rk, Class, Names), the class carrying
            # after the request's arguments the lists of a listed role, or the hello of a meeting, hello(I, Proposal).
            at, yn = x[1][0], x[1][-1]
            if at[0] == "attempt":
                c = at[1][3]
                approvals.append((t, k, part_of(c), yn))
                if role.get(k) == "parent":
                    approvals_by_parent_month[min(int(t // MONTH) + 1, months)] += 1
            elif at[0] == "hello":
                approvals.append((t, k, show(at[1][1]), yn))
                if role.get(k) == "parent":
                    hellos_by_parent_month[min(int(t // MONTH) + 1, months)] += 1
        elif ty == "output":
            x = parse(term)
            if x[0] == "view":
                _, As, Ks, Ap = x[1][:4]
                state[k] = Counter(atom_str(e) for e in elems(As))
                knows[k] = set(str(e) for e in elems(Ks))
                pend = Counter(show(e) for e in elems(Ap))
                for (I, C), lst in unresolved.items():
                    if I != k: continue
                    n = pend[C]
                    while shown[(I, C)] < min(n, len(lst)): shown[(I, C)] += 1
                    while shown[(I, C)] > n and lst:
                        r = lst.pop(0); shown[(I, C)] -= 1
                        r["ended"] = t; r["snap"] = before; ended.append(r)
            elif x[0] == "refused":
                notices["refused"] += 1
                C = show(x[1][0])
                e = [r for r in ended if r["I"] == k and r["C"] == C]
                lst = unresolved.get((k, C))
                if e:
                    ended.remove(e[0]); e[0]["refused"] = t
                elif lst:
                    r = lst.pop(0); r["refused"] = t
                    if shown[(k, C)] > 0: shown[(k, C)] -= 1
            else:
                notices[x[0]] += 1
    settle()
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
    FRIENDSHIP = ("children_befriend", "children_of_one_parent_befriend", "child_befriend")
    cb = [r for r in requests if role.get(r["I"]) == "parent" and r["cls"] in FRIENDSHIP]
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
        "monitor_exceptions": exceptions, "monitored_commits": len(commits),
        "requests_refused": sum(1 for r in requests if r["refused"] is not None),
        "requests_open_at_end": sum(len(v) for v in unresolved.values()),
        "child_requests": len(cb), "child_requests_committed": len(cbc),
        "child_commit_fraction": (len(cbc) / len(cb)) if cb else None,
        "days_to_commit_mean": (sum(days) / len(days)) if days else None,
        "days_to_commit_max": max(days) if days else None,
        "approvals_per_parent_month": appr,
        "approvals_per_parent_month_attempts_only": appr_att,
        "child_friendships_by_week": [week_child_edges.get(w) for w in range(1, len(week_child_edges) + 1)],
        "last_week_partial": partial_week, "last_month_partial": partial_month,
        "child_friendship_requests_by_class": dict(Counter(r["cls"] for r in requests if r["cls"] in FRIENDSHIP)),
        "child_requests_refused": sum(1 for r in cb if r["refused"] is not None),
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
