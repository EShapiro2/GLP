# The Art of Grassroots Logic Programming - Example Programs

This repository contains example programs for the textbook "The Art of Grassroots Logic Programming" (AoGLP), demonstrating GLP programming techniques from basic stream operations to secure multiagent protocols.

## Source Priority

Programs are drawn from these sources in order of priority:
1. **GLP Papers** - Authoritative source, used exactly as written
2. **FCP/CP Book** - Converted to GLP only when not available in GLP papers
3. **Test Files** - Standard examples cleaned and documented

## Structure

Each program file includes:
- The GLP program with full comments
- Inline `reduce` clauses for metainterpreter support (until module system exists)

## Directory Organization

Directories match book chapter names:

### Part I: Foundations
- `constants/` — Logic gates, unary/binary predicates with constants

### Part II: Programming with Streams
- `streams/` — Producers, consumers, merge, distribution, observation
- `buffered_communication/` — Bounded buffers, flow control
- `monitors/` — Objects and monitors, stateful servers, network switches

### Part III: Recursive Programming
- `lists/` — Append, reverse, member, length
- `sorting/` — Quicksort, mergesort, insertion sort
- `arithmetic/` — Factorial, fibonacci, primes, trees

### Part IV: Inheritance and Delegation
- `inheritance/` — Frame-based inheritance

### Part V: Metaprogramming
- `meta/` — Plain, failsafe, tracing, debugging metainterpreters

### Part VI: Grassroots Protocols
- `social_graph/` — Agent initialization, befriending
- `networking/` — DM channels, feeds, groups, blocklace
- `security/` — Attestation, blockchain properties

### Other
- `puzzles/` — Tower of Hanoi, etc.
- `lib/` — Utility predicates (channels, streams, lookup, broadcast)

## Usage

Each `.glp` file is self-contained with its `reduce` clauses for metainterpreter execution. To run a program under the plain metainterpreter:
```glp
run(merge([1,2,3], [a,b,c], Result)).
```

## References

- GLP Paper: "GLP: A Secure, Multiagent, Grassroots, Concurrent, Logic Programming Language"
- Concurrent Prolog / FCP Book: "Concurrent Prolog: Collected Papers" (MIT Press)
- Art of Prolog: Sterling & Shapiro, "The Art of Prolog" (MIT Press)

## Future Work

- Module system for automatic metainterpreter integration
- Additional examples from FCP book (converted to GLP)
- Test harness integration
