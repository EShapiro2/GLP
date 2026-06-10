# Overview GLP - Project Map

**Created**: 2026-01-18  
**Purpose**: Comprehensive map of the GLP ecosystem for Overview GLP reference  
**Update Policy**: Update this file as understanding increases

---

## Repository Overview

The GLP ecosystem spans three active repositories in `/Users/udi/Grassroots/`:

| Repository | Purpose | Status |
|------------|---------|--------|
| **GLP** | Main codebase: runtime, compiler, type checker, programs, specs | Active development |
| **Moded-Types** | Type system paper (LaTeX) | Active writing |
| **GLP-ICLP-2026** | ICLP 2026 submission paper (LaTeX) | Active writing |

---

## GLP Repository Structure

### Core Dart Projects

**glp_runtime/** — Main Dart package containing the GLP implementation

```
glp_runtime/
├── lib/
│   ├── analysis/type_checker/    # Type checking implementation
│   │   ├── type_checker.dart     # Main entry point
│   │   ├── clause_validation.dart
│   │   ├── mode.dart
│   │   ├── moded_term.dart
│   │   ├── well_typed_clause.dart
│   │   └── well_typed_term.dart
│   ├── bytecode/                 # VM implementation
│   ├── compiler/                 # GLP→bytecode compiler
│   │   ├── parser.dart
│   │   ├── lexer.dart
│   │   ├── ast.dart
│   │   ├── codegen.dart
│   │   └── compiler.dart
│   ├── runtime/                  # Heap, scheduler, terms
│   │   ├── runtime.dart
│   │   ├── scheduler.dart
│   │   ├── terms.dart
│   │   ├── cells.dart
│   │   └── suspension.dart
│   └── multiagent/               # Multiagent support
│       ├── irma_agent.dart       # Agent wrapper
│       ├── irma_context.dart     # V_p/M_p context
│       └── payload_serializer.dart
├── bin/
│   ├── glp_repl.dart             # REPL entry point
│   ├── check_types.dart          # Type checker CLI
│   └── glpc.dart                 # Compiler CLI
└── test/                         # Unit tests
    ├── analysis/type_checker/
    ├── bytecode/
    ├── compiler/
    └── multiagent/
```

**glp_multiagent/** — Flutter application for multiagent simulation

```
glp_multiagent/
├── lib/
│   ├── main.dart                 # Flutter app entry
│   └── irma_router.dart          # Message routing
├── macos/                        # macOS build config
└── test/
```

### GLP Programs

```
programs/
├── book/                         # Programs for The Art of GLP
│   ├── streams/                  # Producers, consumers, monitors
│   ├── recursive/                # Arithmetic, list, structure processing
│   ├── social_graph/             # Agent protocols
│   ├── social_networks/          # Network protocols
│   ├── meta/                     # Metainterpreters
│   ├── constants/                # Logic gates, circuits
│   ├── cryptocurrencies/         # GC protocol
│   └── constitutional_consensus/ # Consensus protocols
├── typed_book/                   # Same structure, with type declarations
├── multiagent/                   # Multiagent test programs
│   └── social_agent.glp          # Current agent program
├── tests/                        # REPL test files
├── stdlib/                       # Standard library
├── lib/                          # Reusable modules
└── paper/                        # Paper example programs
```

### Documentation

```
docs/
├── DISCIPLINE.md                 # ★ MASTER: Development discipline, testing, handovers
├── projects/                     # Project descriptions
│   ├── README.md                 # Project overview
│   ├── overview-glp.md           # This project
│   ├── multiagent-glp.md
│   ├── typed-glp.md
│   ├── book-glp.md
│   ├── iclp-glp.md
│   └── overview-map.md           # THIS FILE
├── ma/                           # Multiagent specs and handovers
│   ├── irmaGLP-spec.md
│   ├── irmaGLP-implementation-plan-v2.md
│   └── phase6-handover-2026-01-17.md
├── type system/                  # Type system specs
│   ├── STATUS.md
│   ├── mode.md
│   ├── moded-term.md
│   ├── well-typed-clause.md
│   └── well-typed-term.md
├── SPEC_GUIDE.md                 # GLP execution model overview
├── glp-bytecode-v216-complete.md # Instruction set spec
├── glp-runtime-spec.txt          # Runtime architecture
├── parser-spec.md
├── glp-io-spec.md
├── glp-module-system-v1-spec.md
├── archive/                      # Superseded docs
│   └── DEVELOPMENT_DISCIPLINE_v1.1.md  # (superseded by DISCIPLINE.md)
```

---

## Moded-Types Repository (Type System Paper)

```
Moded-Types/
├── main-moded-types.tex          # Main document
├── bib.bib                       # Bibliography
├── sections/
│   ├── introduction.tex
│   ├── logic-programs.tex
│   ├── glp.tex
│   ├── glp-semantics.tex
│   ├── typed-glp.tex
│   ├── well-typing.tex
│   ├── related-work.tex
│   ├── conclusion.tex
│   └── appendix-examples.tex
├── DD.md                         # Design document
└── CLAUDE.md                     # Claude Code instructions
```

---

## GLP-ICLP-2026 Repository (ICLP Paper)

```
GLP-ICLP-2026/
├── main GLP 2025.tex             # Main document
├── bib.bib                       # Bibliography
├── glp_section_*.tex             # Main sections
│   ├── glp_section_introduction.tex
│   ├── glp_section_logic_programs.tex
│   ├── glp_section_glp.tex
│   ├── glp_section_examples.tex
│   ├── glp_section_multiagent.tex
│   ├── glp_section_social_graph.tex
│   ├── glp_section_implementation.tex
│   ├── glp_section_related_work.tex
│   └── glp_section_conclusion.tex
├── glp_appendix_*.tex            # Appendices
├── Code/                         # Code snippets
├── Figs/                         # Figures
├── new_tlp.cls                   # TPLP document class
└── CLAUDE.md                     # Claude Code instructions
```

---

## Project Relationships

### Five Active Projects

| Project | Mission | Primary Files |
|---------|---------|---------------|
| **Overview GLP** | Consistency, testing, structure, discipline | All (oversight) |
| **Multiagent GLP** | Multiagent implementation | `glp_runtime/lib/multiagent/`, `glp_multiagent/`, `programs/multiagent/` |
| **Typed GLP** | Type system implementation | `glp_runtime/lib/analysis/type_checker/`, `docs/type system/` |
| **Book GLP** | Book writing | `programs/book/`, `programs/typed_book/` |
| **ICLP GLP** | Paper writing | `GLP-ICLP-2026/` |

### Paper-Spec-Code Alignment

| Concept | Paper (Moded-Types) | Spec (docs/) | Code (glp_runtime/) |
|---------|---------------------|--------------|---------------------|
| **Compilation Pipeline** | — | `type system/compilation-pipeline.md` | REPL, GlpCompiler, check_types.dart |
| Type System | `sections/typed-glp.tex` | `type system/*.md` | `lib/analysis/type_checker/` |
| Well-Typing | `sections/well-typing.tex` | `type system/well-typed-*.md` | `well_typed_*.dart` |
| Modes | `sections/glp.tex` | `type system/mode.md` | `mode.dart` |
| GLP Semantics | `sections/glp-semantics.tex` | `SPEC_GUIDE.md` | `lib/runtime/` |

---

## Test Infrastructure

### Test Counts (as of 2026-01-17)

| Suite | Count | Command |
|-------|-------|---------|
| Dart unit tests | 236 | `cd glp_runtime && dart test` |
| REPL tests | 222 | `cd glp_runtime && dart test test/repl/` |
| Multiagent tests | 139 | `cd glp_runtime && dart test test/multiagent/` |

### Test Locations

```
glp_runtime/test/
├── analysis/type_checker/        # Type checker tests
├── bytecode/                     # Bytecode tests
├── compiler/                     # Parser/compiler tests
├── conformance/                  # Language conformance
├── module/                       # Module system tests
├── multiagent/                   # Multiagent tests
└── programs/                     # Program execution tests
```

---

## Key Terminology Standards

| Correct | Incorrect |
|---------|-----------|
| GLP Type System | PMT |
| Typed GLP | PMT (as language name) |
| SRSW (Single Reader Single Writer) | Single-ID |
| writer (lowercase in prose) | Writer |
| reader (lowercase in prose) | Reader |

---

## Current State Summary (2026-01-18)

### Codebase Health
- All tests passing (236 unit + 222 REPL)
- Flutter builds successfully
- No known broken builds

### Active Work
- **Multiagent GLP**: Friend-mediated introduction protocol in progress
- **Typed GLP**: Type checker debugging edge cases
- **Book GLP**: Multiple chapters drafted
- **ICLP GLP**: Paper structure being developed

### Key Decisions (from Decision Log)
- 2026-01-17: Restructured from file-ownership to goal-oriented projects
- Projects: Overview GLP, Multiagent GLP, Typed GLP, Book GLP, ICLP GLP
- Specs updated by each project; Overview GLP ensures no conflicts

---

## Archive Locations

Old/obsolete files should be moved to:
- `GLP/archive/` — Main archive
- `GLP/glp_runtime/archive/` — Runtime-specific archive
- `GLP/docs/archive/` — Documentation archive

---

## External References

### GitHub Repositories
- FCP: https://github.com/EShapiro2/FCP (reference implementation)
- Art-of-GLP-2025: https://github.com/EShapiro2/Art-of-GLP-2025 (book LaTeX)

### Key Documents
- `CLAUDE.md` in each repository — Instructions for Claude Code sessions
- `docs/wam.pdf` — Warren's Abstract Machine paper
- `docs/1-s2.0-0743106689890113-main.pdf` — FCP implementation paper

---

## Update Log

| Date | Change |
|------|--------|
| 2026-01-18 | Initial creation by Overview GLP |
| 2026-01-18 | Created consolidated DISCIPLINE.md; archived old v1.0/v1.1 discipline docs; updated test/README.md |
| 2026-01-18 | Added compilation-pipeline.md to Paper-Spec-Code alignment table (from Typed GLP update) |
| 2026-01-18 | Created the root-scope/stdlib consolidation instructions for Typed GLP |
| 2026-01-18 | Root self.glp/stdlib consolidation COMPLETED: procedure declarations added to root_scope.dart and all stdlib files |

