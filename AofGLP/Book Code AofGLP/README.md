# Art of GLP - Code Examples

This directory contains GLP code examples from "The Art of Grassroots Logic Programming" book.

## Directory Structure

```
AofGLP/
├── book_examples/     # Code from main chapters
│   └── streams/       # Chapter 5: Streams
└── exercise_solutions/ # Solutions to exercises
    └── streams/       # Chapter 5 exercises
```

## File Conventions

- `*.glp` - GLP source code
- `*.trace` - Execution traces from the GLP REPL

## Running Examples

Load a file in the GLP REPL:
```
GLP> consult("AofGLP/book_examples/streams/merge_simple.glp").
```

Then run the test goals shown in each file's header comments.
