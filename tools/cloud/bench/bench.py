#!/usr/bin/env python3
"""bench.py: time a fixed set of GLP runs through glpc, N repetitions each, and print the medians.

Usage: python3 -I bench.py [reps] [case-name ...]
Runs from glp_runtime.  Each case is a REPL session fed on stdin (or a shell command); wall, user and
system seconds and the maximum resident set come from os.wait4.  Output: one line per case with the
median of the repetitions, and a JSON file of every measurement beside the script.
"""
import json, os, statistics, subprocess, sys, time

RT = os.environ.get("GLP_RT", "/home/user/glp/glp_runtime")
GROOT = os.path.dirname(RT)
P = "../programs"
LIMIT = ":limit 1000000000\n"

def repl(*lines):
    return {"kind": "repl", "stdin": "".join(l + "\n" for l in lines) + ":quit\n"}

def sh(cmd):
    return {"kind": "sh", "cmd": cmd}

CASES = {
    # the REPL alone: process start, root self.glp loaded
    "startup":        repl(),
    # the engine: reductions
    "fib_23":         repl(f"load {P}/book/recursive/arithmetic_trees/fibonacci.glp", LIMIT.strip(), "fib(23, F)."),
    "fib_linear_1e5": repl(f"load {P}/book/recursive/arithmetic_trees/fibonacci.glp", LIMIT.strip(), "fib_linear(100000, F)."),
    "primes_5000":    repl(f"load {P}/book/recursive/arithmetic_trees/primes.glp", LIMIT.strip(), "primes(5000, Ps)."),
    # the pipeline: lex, parse, SRSW, partial evaluation, type check, compile, of a directory as a project
    "load_gsc":       repl(f"{P}/gsc"),
    "load_graph":     repl(f"{P}/social/graph"),
    "load_sglp":      repl(f"{P}/sglp"),
    # an app: the contract compiled by vGLP's compiler to its platform on load, then the whole program
    "load_app_gsn":   repl(f"{P}/apps/gsn"),
    # the checker on a contract
    "check_gsn":      repl(f"{P}/gsc", LIMIT.strip(), "contract_named(gsn, C), check(C?, V)."),
    # ten goals posted after one load: what a session that keeps asking pays per goal
    "check_gsn_x10":  repl(f"{P}/gsc", LIMIT.strip(), *(["contract_named(gsn, C), check(C?, V)."] * 10)),
    "fib_20_x10":     repl(f"{P}/book/recursive/arithmetic_trees/fibonacci.glp", LIMIT.strip(), *(["fib(20, F)."] * 10)),
    # sGLP's simulations: the social graph with 100 agents, a day (the population's setup) and 90 days (the run)
    "sglp_sg_100_1d":  repl(f"{P}/sglp", ":limit 1000000000000000", "social_graph(100, 1, day, 20260927)."),
    "sglp_sg_100_90d": repl(f"{P}/sglp", ":limit 1000000000000000", "social_graph(100, 90, days, 20260927)."),
    # the multi-isolate runtime: a boot of three agents
    "boot_held_links": repl(f":boot {P}/tests/mad_held_links_boot.glp"),
    # the checker's whole test script
    "test_gsc_sh":    sh("bash ../programs/gsc/test_gsc.sh"),
}

def plays(d, limit=6000000):
    """A session running every play a plays directory exports, as cascade.sh's step 7 does: one goal per
    exported play_* procedure, a fresh variable per argument."""
    import re
    goals = []
    for line in open(f"{GROOT}/programs/{d}/self.glp"):
        m = re.match(r"exported procedure (play_[a-z0-9_]*)\((.*)\)\.\s*$", line)
        if not m: continue
        depth, n = 0, 1
        for ch in m.group(2):
            depth += ch == "("; depth -= ch == ")"; n += ch == "," and depth == 0
        goals.append(f"{m.group(1)}({', '.join('V%d' % i for i in range(1, n + 1))}).")
    return repl(f"{P}/{d}/", f":limit {limit}", *goals)

CASES.update({
    # the platforms run with scripted persons: the plays the cascade's step 7 runs
    "play_gkk":   plays("apps/gkk"),
    "play_sg":    plays("vglp/cva/social_graph"),
    "play_sn":    plays("vglp/cva/social_network"),
    "play_gsn":   plays("apps/gsn"),
    "play_gai":   plays("apps/gai"),
    "play_cssn":  plays("cssn"),
    # the cascade whole, checked against its record: checker, VMAT, vGLP compiler, the platform and its run
    "cascade_gkk":  sh(f"cd {GROOT} && bash cascade.sh gkk --check"),
    "cascade_gsg":  sh(f"cd {GROOT} && bash cascade.sh gsg --check"),
    "cascade_gsn":  sh(f"cd {GROOT} && bash cascade.sh gsn --check"),
    "cascade_gai":  sh(f"cd {GROOT} && bash cascade.sh gai --check"),
    "cascade_cssn": sh(f"cd {GROOT} && bash cascade.sh cssn --check"),
})

def run_once(case):
    if case["kind"] == "repl":
        args = ["bash", "bin/glpc"]; stdin = case["stdin"]
    else:
        args = ["bash", "-c", case["cmd"]]; stdin = ""
    t0 = time.monotonic()
    p = subprocess.Popen(args, cwd=RT, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    out, _ = p.communicate(stdin)
    _, status, ru = os.wait4(p.pid, 0) if False else (None, p.returncode, None)
    wall = time.monotonic() - t0
    return wall, out

def run_once_ru(case):
    """Like run_once, with rusage: Popen then wait4 for the child's own user, system and maxrss."""
    if case["kind"] == "repl":
        args = ["bash", "bin/glpc"]; stdin = case["stdin"]
    else:
        args = ["bash", "-c", case["cmd"]]; stdin = ""
    t0 = time.monotonic()
    p = subprocess.Popen(args, cwd=RT, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    p.stdin.write(stdin); p.stdin.close()
    out = p.stdout.read()
    _, status, ru = os.wait4(p.pid, 0)
    p.returncode = status
    wall = time.monotonic() - t0
    return {"wall": wall, "user": ru.ru_utime, "sys": ru.ru_stime, "maxrss_mb": ru.ru_maxrss / 1024, "status": status, "tail": out.strip().splitlines()[-3:]}

def main():
    reps = int(sys.argv[1]) if len(sys.argv) > 1 and sys.argv[1].isdigit() else 3
    names = [a for a in sys.argv[1:] if not a.isdigit()] or list(CASES)
    results = {}
    print(f"{'case':18s} {'wall s':>8s} {'user s':>8s} {'sys s':>7s} {'maxrss MB':>10s}  reps  last lines")
    for name in names:
        case = CASES[name]
        runs = [run_once_ru(case) for _ in range(reps)]
        med = lambda k: statistics.median(r[k] for r in runs)
        results[name] = runs
        tail = " | ".join(runs[-1]["tail"])[:90]
        print(f"{name:18s} {med('wall'):8.2f} {med('user'):8.2f} {med('sys'):7.2f} {med('maxrss_mb'):10.0f}  {reps:4d}  {tail}", flush=True)
    with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "bench_results.json"), "w") as f:
        json.dump(results, f, indent=1)

if __name__ == "__main__":
    main()
