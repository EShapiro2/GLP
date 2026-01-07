/// Tests for path consistency checking
/// Specification: docs/modules/path-consistency.md
/// Paper Reference: Definition 4.3

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/path_consistency.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';

void main() {
  group('Path Consistency', () {
    group('Case 1: Equal length with consistent endings', () {
      // Positive controls
      test('integer constant matches Integer type', () {
        // Term path: (0,↓) -> foo --(1,↑)-> 42
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, '42')
        ]);

        // Type path: (0,↓) -> foo --(1,↑)-> Integer
        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, 'Integer')
        ]);

        expect(areConsistent(termPath, typePath), isTrue);
      });

      test('integer constant matches same integer type', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, '42')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, '42')
        ]);

        expect(areConsistent(termPath, typePath), isTrue);
      });

      test('reader matches _? type', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, 'X?')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, '_?')
        ]);

        expect(areConsistent(termPath, typePath), isTrue);
      });

      test('writer matches _ type', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, 'X')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, '_')
        ]);

        expect(areConsistent(termPath, typePath), isTrue);
      });

      // Negative controls
      test('integer does not match wrong integer', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, '42')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, '99')
        ]);

        expect(areConsistent(termPath, typePath), isFalse);
      });

      test('reader does not match _ type', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, 'X?')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, '_')  // Should be _?
        ]);

        expect(areConsistent(termPath, typePath), isFalse);
      });

      test('writer does not match _? type', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, 'X')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, '_?')  // Should be _
        ]);

        expect(areConsistent(termPath, typePath), isFalse);
      });
    });

    group('Case 2: Term path is prefix (variable at leaf)', () {
      // Positive controls
      test('Case 2(a): reader at consumed position is consistent', () {
        // Term: (0,↓) -> merge --(1,↓)-> "."/2 --(1,↓)-> X?
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'merge/3'),
          PathStep(1, Mode.consume, '.'),
          PathStep(1, Mode.consume, 'X?')
        ]);

        // Type: (0,↓) -> merge --(1,↓)-> Stream? --(1,↓)-> "."/2 --(1,↓)-> _?
        final typePath = TypePath([
          PathStep(0, Mode.consume, 'merge/3'),
          PathStep(1, Mode.consume, 'Stream?'),
          PathStep(1, Mode.consume, '.'),
          PathStep(1, Mode.consume, '_?')
        ]);

        expect(areConsistent(termPath, typePath), isTrue);
      });

      test('Case 2(b): writer at produced position is consistent', () {
        // Term: (0,↓) -> merge --(3,↑)-> "."/2 --(1,↑)-> X
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'merge/3'),
          PathStep(3, Mode.produce, '.'),
          PathStep(1, Mode.produce, 'X')
        ]);

        // Type: (0,↓) -> merge --(3,↑)-> Stream --(1,↑)-> "."/2 --(1,↑)-> _
        final typePath = TypePath([
          PathStep(0, Mode.consume, 'merge/3'),
          PathStep(3, Mode.produce, 'Stream'),
          PathStep(1, Mode.produce, '.'),
          PathStep(1, Mode.produce, '_')
        ]);

        expect(areConsistent(termPath, typePath), isTrue);
      });

      // Negative controls
      test('reader at produced position is inconsistent', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, 'X?')  // Reader at produce position!
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, 'T'),
          PathStep(1, Mode.produce, '_')
        ]);

        expect(areConsistent(termPath, typePath), isFalse);
      });

      test('writer at consumed position is inconsistent', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, 'X')  // Writer at consume position!
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, 'T'),
          PathStep(1, Mode.consume, '_?')
        ]);

        expect(areConsistent(termPath, typePath), isFalse);
      });
    });

    group('Case 3: Type path is prefix (primitive type at leaf)', () {
      // Positive controls
      test('Case 3(a): _? with consume mode is consistent', () {
        // Type path ends in _?, term continues with consumed structure
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, 'bar/1'),
          PathStep(1, Mode.consume, '42')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, '_?')
        ]);

        expect(areConsistent(termPath, typePath), isTrue);
      });

      test('Case 3(b): _ with produce mode is consistent', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, 'bar/1'),
          PathStep(1, Mode.produce, '42')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, '_')
        ]);

        expect(areConsistent(termPath, typePath), isTrue);
      });

      // Negative controls
      test('_? with produce mode is inconsistent', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, 'bar/1'),
          PathStep(1, Mode.produce, '42')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.produce, '_?')  // _? requires consume mode
        ]);

        expect(areConsistent(termPath, typePath), isFalse);
      });

      test('_ with consume mode is inconsistent', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, 'bar/1'),
          PathStep(1, Mode.consume, '42')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, '_')  // _ requires produce mode
        ]);

        expect(areConsistent(termPath, typePath), isFalse);
      });
    });

    group('checkConsistency with variable assignment', () {
      test('returns variable assignment for Case 2(a)', () {
        final termPath = ModedPath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, 'X?')
        ]);

        final typePath = TypePath([
          PathStep(0, Mode.consume, 'foo/1'),
          PathStep(1, Mode.consume, 'T'),
          PathStep(1, Mode.consume, '_?')
        ]);

        final result = checkConsistency(termPath, typePath);

        expect(result.isConsistent, isTrue);
        expect(result.matchCase, equals(ConsistencyCase.termPrefixReader));
        expect(result.variableName, equals('X'));
        expect(result.assignedMode, equals(Mode.consume));
      });
    });
  });
}
