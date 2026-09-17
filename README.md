# GLP — Grassroots Logic Programs

GLP is a concurrent logic programming language for grassroots systems: a program is a set of guarded clauses over single-reader single-writer variables, and a goal reduces by tentative head unification, then pure three-valued guards, then a body that binds writers.  A goal whose reader is unbound suspends and reactivates when the writer is bound, so a program is a network of processes communicating through shared variables rather than a sequence of calls.  Agents run in separate Dart isolates and reach one another by message-passing over the same variables, which is what makes a grassroots platform — no server, no global state — expressible as one typed program.

The language is defined by the papers, not by this tree; the implementation here follows them.  Ownership of each directory is Coordination Appendix B.

## Directory map

```
GLP/
├── CLAUDE.md                 # how to work in this repository — read first
├── docs/                     # specifications and references (docs/README.md indexes them)
├── glp_runtime/              # the Dart implementation
│   ├── lib/                  # parser, partial evaluator, type checker, compiler, runtime, multiagent
│   ├── bin/                  # glpc (the AOT REPL) and glp_repl.dart (its source)
│   └── test/                 # Dart unit tests
├── glp_multiagent/           # the Flutter app (GrassApp); assets/glp/ is generated
├── networking/               # the transport layer
├── servers/gpw/              # grassroots personal websites
├── test/run_all_tests.sh     # the canonical suite
└── programs/                 # all GLP and vGLP sources
    ├── self.glp              # the root scope: the types and procedures every program inherits
    ├── lib/                  # the standard modules the root scope exposes
    ├── system/               # system predicates
    ├── book/                 # The Art of GLP examples
    ├── examples/             # small programs by topic
    ├── p99/                  # the 99 Prolog problems in GLP
    ├── social/               # the social layer
    │   ├── graph/            # the social graph and the Grassroots Super-App (core/, pingapp/, ui/, routing/)
    │   └── spm/              # securing people and their machines (cva/, gsg/, secure_gsg/)
    ├── currencies/           # the grassroots currencies: bonds_v2/, coins/, bonds/, sovereign/
    ├── cssn/                 # the child-safe social network
    ├── federation/           # the grassroots federation platform (GFWC)
    ├── grassapp/             # the GrassApp programs
    ├── jurix/                # legal contracts
    ├── vglp/                 # vGLP sources
    └── tests/                # the suite's program fixtures (linkprobes/, multiagent/, vglp/, module/, …)
```

`programs/social/`, `programs/currencies/` and `programs/tests/` are containers with no `self.glp` of their own, so they add nothing to the ancestor scope chain: a program under them inherits the root scope directly.

## Running and testing

`CLAUDE.md` is the authority on both: how to invoke the REPL (`bin/glpc` from `glp_runtime/`, never `dart run` for routine checks), how to load a file or a project, how to run the suite, and what the commit gate is.

## Documentation

- `CLAUDE.md` — how to work here
- `docs/README.md` — the documentation index
- `docs/DISCIPLINE.md` — spec-first discipline
- `docs/typed-glp-manual.md` — typed GLP
- `docs/glp-cheat-sheet.md` — patterns and idioms ("GLP is NOT Prolog")
- `docs/known-issues.md` — the running issue record
