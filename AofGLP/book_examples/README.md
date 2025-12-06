# The Art of Grassroots Logic Programming - Examples

This directory contains GLP programs and traces for the book.

## Structure

- `book_examples/` — Programs appearing in chapter text
- `exercise_solutions/` — Solutions to chapter exercises (separate to avoid accidental disclosure)

## File Conventions

Each `.glp` file has a header with metadata and test goals:

```glp
% Topic: <descriptive name>
% Section: <book section>
%
% Test goals:
% <goal1>.
% <goal2>.

<program code>
```

Traces are stored in corresponding `.trace` files.

## Regenerating Traces

To regenerate a trace, load the `.glp` file in the GLP REPL and run the commented test goals.
