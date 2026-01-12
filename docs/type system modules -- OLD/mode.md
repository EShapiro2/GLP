# Module: mode

**Version**: 0.1  
**Date**: 2025-01-08  
**Status**: DRAFT  
**Paper References**: Section 4.1 (lines 7-17)

## Purpose

Defines the Mode enum representing the two communication directions in GLP: consume (↓) and produce (↑).

## Dependencies

None (leaf module).

## Definitions

### Mode (Paper lines 7-17)

A **mode** indicates whether a term or subterm is consumed or produced during computation:

- **consume (↓)**: The value is read/consumed by the program
- **produce (↑)**: The value is written/produced by the program

### Mode Complementation (Paper line 17)

Mode complementation flips the direction:
- consume ↔ produce
- The complement operation is an involution: flip(flip(m)) = m

## Public Interface

### Types

#### `enum Mode`

```dart
enum Mode {
  consume,  // ↓ - value is consumed/read
  produce,  // ↑ - value is produced/written
}
```

### Operations

#### `Mode.flip`

Returns the complementary mode.

```dart
extension ModeExtension on Mode {
  Mode get flip => this == Mode.consume ? Mode.produce : Mode.consume;
}
```

**Postconditions:**
- `Mode.consume.flip == Mode.produce`
- `Mode.produce.flip == Mode.consume`
- `m.flip.flip == m` (involution)

## Examples

### Example: Mode Flip

```dart
Mode.consume.flip  // → Mode.produce
Mode.produce.flip  // → Mode.consume
```

### Example: Involution Property

```dart
Mode.consume.flip.flip  // → Mode.consume
Mode.produce.flip.flip  // → Mode.produce
```

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-08 | Initial draft — extracted from moded-term |
