# Overview GLP

## Mission

Maintain the working codebase, specs, and papers all consistent. Directory structure integrity. Remove old/dated stuff. Maintain all tests. Ensure all projects follow discipline. Ensure no overlap among specs. Restructure projects as needed.

## Responsibilities

### Codebase Health
- All tests passing (Dart unit tests, REPL tests)
- No broken builds
- Clean repository structure
- Archive obsolete files
- Remove old/dated stuff

### Directory Structure Integrity
- Logical organization of files
- No duplicate or scattered files
- Clear separation of concerns
- Programs in `programs/`, specs in `docs/`, etc.

### Consistency & No Overlap
- Paper ↔ spec ↔ code alignment
- Terminology consistency across all documents
- No contradictions between projects' work
- **Specs**: Each project updates their own specs; Overview ensures no conflicts or overlaps between specs

### Discipline Enforcement
- **Types First**: Type declarations before code
- **Testing**: Positive and negative controls for every feature
- **No Shortcuts**: No cutting corners, no bypassing problems, no workarounds for bugs
- **Complete Work**: Every task done completely and correctly

### Project Coordination
- Resolve overlaps between projects
- Restructure projects when missions change
- Track overall progress across all projects
- Maintain project descriptions in `docs/projects/`

## Testing Commands

**READ FIRST:** `docs/DISCIPLINE.md` Part II for complete testing protocol.

**Standard Testing One-Liner (copy-paste this exact command):**

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test > /tmp/glp-tests.txt 2>&1 && cd .. && bash test/full_run_repl_tests.sh >> /tmp/glp-tests.txt 2>&1
```

This runs both Dart tests and REPL tests, writing all output to `/tmp/glp-tests.txt`. After user runs the command, read the results with `Filesystem:read_text_file` on `/tmp/glp-tests.txt`.

**Other test commands:**

```bash
# Flutter build
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos > /tmp/flutter-build.txt 2>&1

# Type check a program
cd /Users/udi/Grassroots/GLP/glp_runtime && dart run bin/check_types.dart <program.glp> > /tmp/typecheck.txt 2>&1

# Book tests
cd /Users/udi/Grassroots/GLP && bash test/run_book_tests.sh > /tmp/book-tests.txt 2>&1
```

## Current Test Status (2026-01-17)

- Dart unit tests: 236 passing
- REPL tests: 222 passing
- Flutter: builds successfully

## Key Locations

- `docs/projects/` - project descriptions
- `docs/ma/` - Multiagent GLP handovers and specs
- `docs/type system/` - Typed GLP handovers
- `archive/` - obsolete files

## Decision Log

### 2026-01-17: Project Restructuring
- Changed from file-ownership to goal-oriented projects
- Four projects: Overview GLP, Multiagent GLP, Typed GLP, Book GLP
- Overview GLP has explicit coordination role
- Specs updated by each project; Overview ensures no conflicts

### Terminology Standards
- "GLP Type System" (not PMT)
- "Typed GLP" (the language with types)
- Project names: Overview GLP, Multiagent GLP, Typed GLP, Book GLP
