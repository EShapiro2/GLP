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
# menu(Me, Ks, R); its role is the census's, read off the key the census gives
# it, v<village>f<family>a<n> a parent and v<village>f<family>c<n> a child, or,
# for a key of another form, a child where a view of the agent records it one.
#
# The commits: an agent's view, view(As, Ks), is its machine state As and the
# persons it knows Ks (vGLP's canonical compilation writes it where the state
# changes); a change of As from one view of an agent to its next is the agent's
# effect in a committed transaction.  Each such change is matched to a request,
# propose(C) answered to the menu of an agent I, whose schema (CSSN.glp, the
# repository's copy of the contract) at C's binding has exactly that effect at
# that agent's role; the transaction is committed once every participant whose
# effect is not empty has shown it.  A change matched to no request is reported.
#
# The monitor (CSSN #6, item 1): at the commit of every act a parent guards ---
# child_befriend, child_adult_befriend, child_unfriend, child_join, child_join_2,
# child_leave, child_close, enter, enter_2, of_age, of_age_2, parent_unfriend,
# parent_unfriend_2 --- it checks that each of its guards approved, the initiator
# by the request and every other guard by an answer yes to the approval of an
# attempt (I, N, C) of the initiator I, and that its precondition held: each
# atom the schema requires at a role in the state of the agent at that role,
# and each it forbids not, friend being reflexive, in the states the agents'
# last views before the commit show.  Every exception is reported.
#
# The measures (Section 5, items 2 to 4): the child friendships at the end of
# each month, a month being the twelfth of a year of 365.25 days, 2,629,800 s,
# as sGLP's reader of the social graph has it; the parents' requests for a
# child friendship, child_befriend or child_adult_befriend, those that commit,
# and the days from such a request to its commit; the approvals each parent
# answers each month; the month by whose end the parents' friendship graph,
# and the children's, is one connected component; and the acquaintance graph at
# the end of the run, the persons each agent's last view shows it knows.
import sys, json, re
from collections import Counter, defaultdict

MONTH = 2629800.0
MONITORED = ["child_befriend", "child_adult_befriend", "child_unfriend", "child_join", "child_join_2",
             "child_leave", "child_close", "enter", "enter_2", "of_age", "of_age_2",
             "parent_unfriend", "parent_unfriend_2"]

# ---- GLP terms as the log prints them ----------------------------------------
TOK = re.compile(r"\s*(?:(\d+\.\d+(?:[eE][-+]?\d+)?|\d+)|('(?:[^'\\]|\\.)*')|([A-Za-z_][A-Za-z0-9_]*)|(.))")

def tokens(s):
    out = []
    pos = 0
    while pos < len(s):
        m = TOK.match(s, pos)
        if not m:
            break
        pos = m.end()
        if m.group(1): out.append(("num", m.group(1)))
        elif m.group(2): out.append(("atom", m.group(2)[1:-1]))
        elif m.group(3): out.append(("atom", m.group(3)))
        elif m.group(4) and not m.group(4).isspace(): out.append(("p", m.group(4)))
    return out

class P:
    def __init__(self, s):
        self.t = tokens(s); self.i = 0
    def peek(self): return self.t[self.i] if self.i < len(self.t) else ("eof", "")
    def take(self):
        x = self.peek(); self.i += 1; return x
    def term(self):
        k, v = self.take()
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
            if r[0] == "rs": g, adds, dels, req, forb = r[1]
            else: g, adds, dels, req, forb = r[1][1:]
            rs.append({"guarded": g == "guarded", "add": elems(adds), "del": elems(dels), "req": elems(req), "forb": elems(forb)})
        out[name] = {"args": elems(args), "roles": rs}
    return out

def subst(atom, binding):
    # atom(friend, [role(2)]) at binding [k1, k2, ...] -> "friend(k2)"
    name, args = atom[1]
    vals = []
    for a in elems(args):
        if isinstance(a, tuple) and a[0] == "role": vals.append(binding[a[1][0] - 1])
        else: return None   # a speech-act or name variable: not an agent schema
    return name if not vals else "%s(%s)" % (name, ", ".join(vals))

def effects(sch, binding):
    """Per agent: (adds Counter, dels Counter), the join where one agent holds two roles."""
    eff = defaultdict(lambda: [Counter(), Counter()])
    for i, r in enumerate(sch["roles"]):
        k = binding[i]
        for a in r["add"]:
            s = subst(a, binding)
            if s is None: return None
            eff[k][0][s] += 1
        for a in r["del"]:
            s = subst(a, binding)
            if s is None: return None
            eff[k][1][s] += 1
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
    m = re.match(r"^v(\d+)f(\d+)([ac])(\d+)$", k)
    if not m: return None, None, None
    return ("parent" if m.group(3) == "a" else "child"), int(m.group(1)), int(m.group(2))

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
        nonlocal month_end_idx
        while month_end_idx <= months and month_end_idx * MONTH <= upto_t:
            ce = child_edges(); pe = parent_edges()
            month_child_edges[month_end_idx] = len(ce)
            month_parent_one[month_end_idx] = components(parents, pe) == 1
            month_child_one[month_end_idx] = components(children, ce) == 1
            month_end_idx += 1

    def holds(k, atom):
        if atom == "friend(%s)" % k: return True
        return state[k][atom] > 0

    def check_pre(cls, binding, snap):
        sch = S[cls]; bad = []
        for i, r in enumerate(sch["roles"]):
            k = binding[i]
            st = snap[k]
            for a in r["req"]:
                s = subst(a, binding)
                if s == "friend(%s)" % k: continue
                if st[s] <= 0: bad.append("%s lacks %s" % (k, s))
            for a in r["forb"]:
                s = subst(a, binding)
                if s == "friend(%s)" % k or st[s] > 0: bad.append("%s holds %s" % (k, s))
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
            at, yn = x[1]
            I, N, Rk, c = at[1]
            approvals[(str(I), N, show(c))][k] = (t, yn)
            if role.get(k) == "parent":
                approvals_by_parent_month[min(int(t // MONTH) + 1, months)] += 1
        elif ty == "output":
            x = parse(term)
            if x[0] == "view":
                As, Ks = x[1]
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
                                eff = effects(S[r["cls"]], r["binding"])
                                if eff is None or k not in eff: continue
                                if eff[k] == (adds, dels):
                                    cands.append((r["t"], r, eff))
                        if cands:
                            cands.sort(key=lambda z: z[0])
                            _, r, eff = cands[-1]   # the latest request before the change
                            need = {q for q, e in eff.items() if e[0] or e[1]}
                            snap = {q: Counter(state[q]) for q in set(r["binding"])}
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
                                for i2, ro in enumerate(sch["roles"]):
                                    if ro["guarded"] and r["binding"][i2] not in guards: guards.append(r["binding"][i2])
                                others = [g for g in guards if g != r["I"]]
                                ok_att = None
                                if others:
                                    for (I2, N2, C2), ans in approvals.items():
                                        if I2 == r["I"] and C2 == r["C"] and all(g in ans and ans[g][1] == "yes" and ans[g][0] <= t for g in others):
                                            ok_att = N2
                                why = []
                                if r["I"] not in guards: why.append("initiator %s is no guard" % r["I"])
                                if others and ok_att is None: why.append("no attempt of %s with a yes of each of %s" % (r["I"], ", ".join(others)))
                                why += check_pre(r["cls"], r["binding"], oc["snap"])
                                if why:
                                    exceptions.append({"t": t, "class": r["C"], "why": why})
                    state[k] = new
            elif x[0] in ("refused", "yielded"):
                notices[x[0]] += 1
    # A run whose monitor stopped at its horizon ends in the state at the horizon.
    snapshot_months(float(opt["--horizon"]) if "--horizon" in opt else (float("inf") if clock is None else clock + 1))

    # ---- The measures ----
    cb = [r for r in requests if role.get(r["I"]) == "parent" and r["cls"] in ("child_befriend", "child_adult_befriend")]
    cbc = [r for r in cb if r["commit"] is not None]
    days = [(r["commit"] - r["t"]) / 86400.0 for r in cbc]
    horizon = clock if clock is not None else (lines[-1][0] if lines else 0)
    nmonths = max(1, min(months, int(-(-horizon // MONTH))))
    appr = [approvals_by_parent_month[m] / max(1, len(parents)) for m in range(1, nmonths + 1)]
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
