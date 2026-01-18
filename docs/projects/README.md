# GLP Projects

Projects are organized by **mission/goal**, not by file ownership. Any project can touch any file as needed to complete its mission.

## Projects

| Project | Mission |
|---------|---------|
| [Overview GLP](overview-glp.md) | Maintain working codebase, specs, papers all consistent. Directory structure integrity. Remove old/dated stuff. Maintain all tests. Ensure discipline. Ensure no overlap among specs. Restructure projects as needed. |
| [Multiagent GLP](multiagent-glp.md) | Get multiagent GLP up and running (all aspects: Dart, GLP programs, specs). |
| [Typed GLP](typed-glp.md) | Finish the type system paper. Get the type checker working. All GLP programs well-typed. |
| [Book GLP](book-glp.md) | Write the book including its programs. |
| [ICLP GLP](iclp-glp.md) | Write the GLP paper for ICLP. |

## Principles

### Goal-Oriented
Each project has a mission to complete. Projects can potentially "finish" when their goal is achieved.

### No File Ownership
Any project can modify any file as needed for its mission. Specs are updated by each project; Overview GLP ensures no conflicts or overlaps.

### Discipline (enforced by Overview GLP)
1. **Types First**: Type declarations → procedure declarations → code
2. **Testing**: All tests must pass. Positive and negative controls for every feature.
3. **Consistency**: Paper ↔ spec ↔ code must align
4. **No Shortcuts**: No cutting corners, no bypassing problems, no workarounds for bugs

### Overlap Handling
When missions overlap (e.g., Multiagent GLP needs type declarations):
- Project can do it themselves, OR
- Ask another project for help
- Overview GLP decides if unclear

## Repository Structure

```
GLP/
├── glp_runtime/          # Dart runtime, compiler, type checker
├── glp_multiagent/       # Flutter multiagent app
├── programs/             # All GLP programs
├── book/                 # Book LaTeX source
├── docs/                 # Specifications and handovers
│   ├── projects/         # Project descriptions (this directory)
│   ├── ma/               # Multiagent documentation
│   └── type system/      # Type system documentation
└── archive/              # Old/obsolete files
```

## Current State (2026-01-17)

- **Overview GLP**: Project restructuring complete
- **Multiagent GLP**: IrmaAgent integrated into Flutter app, friend-mediated introduction next
- **Typed GLP**: Type checker operational, debugging edge cases
- **Book GLP**: Multiple chapters drafted
- **ICLP GLP**: Paper structure being developed
