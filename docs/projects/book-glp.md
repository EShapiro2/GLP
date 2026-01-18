# Book GLP

## Mission

Write the book including its programs.

## Scope

- `book/` - LaTeX source for "The Art of GLP"
- `chapters/` - Individual chapter files
- `programs/book/` - Programs referenced in the book
- Exercises and solutions

## Book Structure (Draft)

### Part I: Foundations
1. Introduction to GLP
2. Streams and Communication
3. Recursive Data Structures

### Part II: Concurrency
4. Monitors and State
5. Meta-Interpreters

### Part III: Types
6. The GLP Type System
7. Typed Programming

### Part IV: Applications
8. Social Graph Protocols
9. Multiagent Systems
10. Grassroots Platforms

## Quality Criteria

1. **Executable**: Every program listing runs
2. **Well-typed**: Every typed program passes type checker
3. **Consistent**: Terminology matches across chapters
4. **Progressive**: Concepts build on each other
5. **Complete**: No gaps in the story

## Key Directories

- `programs/book/` - Organized by chapter/topic
  - `streams/` - Stream programs
  - `recursive/` - Data structure programs
  - `monitors/` - Stateful services
  - `social_graph/` - Social protocols
  - `social_networks/` - Network protocols

## Current State (2026-01-17)

- Multiple chapters drafted
- Social graph chapter covers cold call and friend introduction
- Type system chapter in progress
- Programs scattered - being organized

## Collaboration

### With Typed GLP
- Receive type system explanations
- Ensure book examples are well-typed

### With Multiagent GLP
- Receive working multiagent examples
- Document protocols accurately

### Implicit Oversight Role
If something doesn't fit coherently in the book, it reveals an inconsistency in the underlying work that needs fixing.

## Build Process

```bash
# Build book PDF
cd book && pdflatex main.tex

# Verify all book programs type-check
for f in programs/book/**/*.glp; do
  dart run glp_runtime/bin/check_types.dart "$f"
done

# Verify all book programs execute
# (via REPL tests)
```

## Style Guidelines

- Code listings must be complete and runnable
- Explain before showing code
- Build concepts progressively
- Include exercises at chapter end
- Reference programs by their file path
