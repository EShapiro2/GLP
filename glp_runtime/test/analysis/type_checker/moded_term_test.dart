// test/analysis/type_checker/moded_term_test.dart
//
// Tests for moded_term.dart
// Specification: docs/modules/moded-term.md v0.5
// Paper Reference: Definition 4.2

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';

void main() {
  group('ModedTerm', () {
    // =========================================================================
    // ModedVariable Tests
    // =========================================================================

    group('ModedVariable', () {
      test('reader has consume mode', () {
        final v = ModedVariable.reader('X', structuralMode: Mode.consume);
        expect(v.name, equals('X'));
        expect(v.isReader, isTrue);
        expect(v.isWriter, isFalse);
        expect(v.mode, equals(Mode.consume));
      });

      test('writer has produce mode', () {
        final v = ModedVariable.writer('X', structuralMode: Mode.produce);
        expect(v.name, equals('X'));
        expect(v.isReader, isFalse);
        expect(v.isWriter, isTrue);
        expect(v.mode, equals(Mode.produce));
      });

      test('toString includes ? for readers', () {
        expect(ModedVariable.reader('X', structuralMode: Mode.consume).toString(), equals('X?'));
        expect(ModedVariable.writer('X', structuralMode: Mode.produce).toString(), equals('X'));
      });

      test('equality based on name and reader status', () {
        expect(ModedVariable.reader('X', structuralMode: Mode.consume), equals(ModedVariable.reader('X', structuralMode: Mode.consume)));
        expect(ModedVariable.writer('X', structuralMode: Mode.produce), equals(ModedVariable.writer('X', structuralMode: Mode.produce)));
        expect(ModedVariable.reader('X', structuralMode: Mode.consume), isNot(equals(ModedVariable.writer('X', structuralMode: Mode.produce))));
        expect(ModedVariable.reader('X', structuralMode: Mode.consume), isNot(equals(ModedVariable.reader('Y', structuralMode: Mode.consume))));
      });
    });

    // =========================================================================
    // ModedConstant Tests
    // =========================================================================

    group('ModedConstant', () {
      test('integer constant with mode', () {
        final c = ModedConstant(Mode.produce, 42);
        expect(c.value, equals(42));
        expect(c.mode, equals(Mode.produce));
        expect(c.isNil, isFalse);
      });

      test('nil constant', () {
        final c = ModedConstant.nil(Mode.consume);
        expect(c.value, equals('[]'));
        expect(c.mode, equals(Mode.consume));
        expect(c.isNil, isTrue);
      });

      test('string constant', () {
        final c = ModedConstant(Mode.produce, 'hello');
        expect(c.value, equals('hello'));
        expect(c.mode, equals(Mode.produce));
      });

      test('toString includes mode symbol', () {
        expect(ModedConstant(Mode.consume, 42).toString(), equals('↓42'));
        expect(ModedConstant(Mode.produce, 42).toString(), equals('↑42'));
        expect(ModedConstant.nil(Mode.consume).toString(), equals('↓[]'));
      });

      test('equality based on mode and value', () {
        expect(ModedConstant(Mode.consume, 42), equals(ModedConstant(Mode.consume, 42)));
        expect(ModedConstant(Mode.consume, 42), isNot(equals(ModedConstant(Mode.produce, 42))));
        expect(ModedConstant(Mode.consume, 42), isNot(equals(ModedConstant(Mode.consume, 43))));
      });
    });

    // =========================================================================
    // ModedCompound Tests
    // =========================================================================

    group('ModedCompound', () {
      test('simple compound with mode', () {
        final c = ModedCompound(Mode.consume, 's', 1, [
          ModedVariable.reader('N', structuralMode: Mode.consume),
        ]);
        expect(c.functor, equals('s'));
        expect(c.arity, equals(1));
        expect(c.mode, equals(Mode.consume));
        expect(c.isListCons, isFalse);
      });

      test('list cons detection', () {
        final cons = ModedCompound.listCons(
          Mode.consume,
          ModedVariable.reader('X', structuralMode: Mode.consume),
          ModedVariable.reader('Xs', structuralMode: Mode.consume),
        );
        expect(cons.functor, equals('[|]'));
        expect(cons.arity, equals(2));
        expect(cons.isListCons, isTrue);
      });

      test('toString for regular compound', () {
        final c = ModedCompound(Mode.consume, 'foo', 2, [
          ModedVariable.reader('X', structuralMode: Mode.consume),
          ModedVariable.writer('Y', structuralMode: Mode.produce),
        ]);
        expect(c.toString(), equals('↓foo(X?, Y)'));
      });

      test('toString for list cons', () {
        final cons = ModedCompound.listCons(
          Mode.consume,
          ModedVariable.reader('X', structuralMode: Mode.consume),
          ModedVariable.reader('Xs', structuralMode: Mode.consume),
        );
        expect(cons.toString(), equals('↓[X?|Xs?]'));
      });

      test('toString for nullary compound (atom)', () {
        final c = ModedCompound(Mode.produce, 'atom', 0, []);
        expect(c.toString(), equals('↑atom'));
      });

      test('equality based on mode, functor, arity, and args', () {
        final c1 = ModedCompound(Mode.consume, 'f', 1, [ModedVariable.reader('X', structuralMode: Mode.consume)]);
        final c2 = ModedCompound(Mode.consume, 'f', 1, [ModedVariable.reader('X', structuralMode: Mode.consume)]);
        final c3 = ModedCompound(Mode.produce, 'f', 1, [ModedVariable.reader('X', structuralMode: Mode.consume)]);
        final c4 = ModedCompound(Mode.consume, 'g', 1, [ModedVariable.reader('X', structuralMode: Mode.consume)]);

        expect(c1, equals(c2));
        expect(c1, isNot(equals(c3))); // different mode
        expect(c1, isNot(equals(c4))); // different functor
      });
    });

    // =========================================================================
    // Complement Tests
    // =========================================================================

    group('complement', () {
      test('complement of reader is writer', () {
        final v = ModedVariable.reader('X', structuralMode: Mode.consume);
        final c = complement(v) as ModedVariable;
        expect(c.name, equals('X'));
        expect(c.isReader, isFalse);
        expect(c.isWriter, isTrue);
      });

      test('complement of writer is reader', () {
        final v = ModedVariable.writer('X', structuralMode: Mode.produce);
        final c = complement(v) as ModedVariable;
        expect(c.name, equals('X'));
        expect(c.isReader, isTrue);
        expect(c.isWriter, isFalse);
      });

      test('complement flips constant mode', () {
        final c1 = ModedConstant(Mode.consume, 42);
        final c2 = complement(c1) as ModedConstant;
        expect(c2.value, equals(42));
        expect(c2.mode, equals(Mode.produce));
      });

      test('complement flips compound mode and recurses', () {
        // ↓f(X?) => ↑f(X)
        final original = ModedCompound(Mode.consume, 'f', 1, [
          ModedVariable.reader('X', structuralMode: Mode.consume),
        ]);
        final comp = complement(original) as ModedCompound;

        expect(comp.mode, equals(Mode.produce));
        expect(comp.functor, equals('f'));
        expect(comp.arity, equals(1));

        final arg = comp.args[0] as ModedVariable;
        expect(arg.isWriter, isTrue);
      });

      test('complement is involution: complement(complement(t)) == t', () {
        // Test with variable
        final v = ModedVariable.reader('X', structuralMode: Mode.consume);
        expect(complement(complement(v)), equals(v));

        // Test with constant
        final c = ModedConstant(Mode.consume, 42);
        expect(complement(complement(c)), equals(c));

        // Test with nested compound
        final compound = ModedCompound(Mode.consume, 'f', 2, [
          ModedVariable.reader('X', structuralMode: Mode.consume),
          ModedCompound(Mode.produce, 'g', 1, [
            ModedConstant(Mode.produce, 'a'),
          ]),
        ]);
        expect(complement(complement(compound)), equals(compound));
      });

      test('complement of merge moded head (paper example)', () {
        // Original: ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
        final original = ModedCompound(Mode.consume, 'merge', 3, [
          ModedCompound.listCons(
            Mode.consume,
            ModedVariable.reader('X', structuralMode: Mode.consume),
            ModedVariable.reader('Xs', structuralMode: Mode.consume),
          ),
          ModedVariable.reader('Ys'),
          ModedCompound.listCons(
            Mode.produce,
            ModedVariable.writer('X', structuralMode: Mode.produce),
            ModedVariable.writer('Zs'),
          ),
        ]);

        // Complement: ↑merge(↑[↑X|Xs], Ys, ↓[↓X?|Zs?])
        final comp = complement(original) as ModedCompound;

        expect(comp.mode, equals(Mode.produce));
        expect(comp.functor, equals('merge'));

        // Arg 1: ↑[↑X|Xs] (was ↓[↓X?|Xs?])
        final arg1 = comp.args[0] as ModedCompound;
        expect(arg1.mode, equals(Mode.produce));
        expect((arg1.args[0] as ModedVariable).isWriter, isTrue);
        expect((arg1.args[1] as ModedVariable).isWriter, isTrue);

        // Arg 2: Ys (was Ys?)
        final arg2 = comp.args[1] as ModedVariable;
        expect(arg2.isWriter, isTrue);

        // Arg 3: ↓[↓X?|Zs?] (was ↑[↑X|Zs])
        final arg3 = comp.args[2] as ModedCompound;
        expect(arg3.mode, equals(Mode.consume));
        expect((arg3.args[0] as ModedVariable).isReader, isTrue);
        expect((arg3.args[1] as ModedVariable).isReader, isTrue);
      });
    });

    // =========================================================================
    // PathStep Tests
    // =========================================================================

    group('PathStep', () {
      test('regular step', () {
        final step = PathStep(
          symbol: 'f/2',
          argIndex: 1,
          mode: Mode.consume,
        );
        expect(step.symbol, equals('f/2'));
        expect(step.argIndex, equals(1));
        expect(step.mode, equals(Mode.consume));
        expect(step.isVariable, isFalse);
      });

      test('variable step', () {
        final step = PathStep(
          symbol: 'X?',
          argIndex: 1,
          mode: Mode.consume,
          isVariable: true,
          isReader: true,
        );
        expect(step.isVariable, isTrue);
        expect(step.isReader, isTrue);
        expect(step.isWriter, isFalse);
      });

      test('equality', () {
        final s1 = PathStep(symbol: 'f/2', argIndex: 1, mode: Mode.consume);
        final s2 = PathStep(symbol: 'f/2', argIndex: 1, mode: Mode.consume);
        final s3 = PathStep(symbol: 'f/2', argIndex: 2, mode: Mode.consume);

        expect(s1, equals(s2));
        expect(s1, isNot(equals(s3)));
      });
    });

    // =========================================================================
    // ModedPath Tests
    // =========================================================================

    group('ModedPath', () {
      test('root and leaf accessors', () {
        final path = ModedPath([
          PathStep(symbol: 'f/2', argIndex: 0, mode: Mode.consume),
          PathStep(symbol: 'X?', argIndex: 1, mode: Mode.consume, isVariable: true, isReader: true),
        ]);

        expect(path.root.symbol, equals('f/2'));
        expect(path.leaf.symbol, equals('X?'));
        expect(path.length, equals(2));
      });

      test('isInputPath and isOutputPath', () {
        final inputPath = ModedPath([
          PathStep(symbol: 'f/1', argIndex: 0, mode: Mode.consume),
          PathStep(symbol: 'X?', argIndex: 1, mode: Mode.consume, isVariable: true, isReader: true),
        ]);
        expect(inputPath.isInputPath, isTrue);
        expect(inputPath.isOutputPath, isFalse);

        final outputPath = ModedPath([
          PathStep(symbol: 'f/1', argIndex: 0, mode: Mode.produce),
          PathStep(symbol: 'X', argIndex: 1, mode: Mode.produce, isVariable: true, isReader: false),
        ]);
        expect(outputPath.isInputPath, isFalse);
        expect(outputPath.isOutputPath, isTrue);
      });
    });

    // =========================================================================
    // paths() Function Tests
    // =========================================================================

    group('paths', () {
      test('single variable term has one path', () {
        final v = ModedVariable.reader('X', structuralMode: Mode.consume);
        final ps = paths(v);

        expect(ps.length, equals(1));
        final p = ps.first;
        expect(p.length, equals(1));
        expect(p.root.symbol, equals('X?'));
        expect(p.root.argIndex, equals(0));
        expect(p.root.isVariable, isTrue);
        expect(p.root.isReader, isTrue);
      });

      test('single constant has one path', () {
        final c = ModedConstant(Mode.consume, 42);
        final ps = paths(c);

        expect(ps.length, equals(1));
        final p = ps.first;
        expect(p.length, equals(1));
        expect(p.root.symbol, equals('42'));
        expect(p.root.mode, equals(Mode.consume));
      });

      test('simple compound has paths to each argument', () {
        // ↓f(X?, Y)
        final compound = ModedCompound(Mode.consume, 'f', 2, [
          ModedVariable.reader('X', structuralMode: Mode.consume),
          ModedVariable.writer('Y', structuralMode: Mode.produce),
        ]);
        final ps = paths(compound);

        expect(ps.length, equals(2));

        // Find path to X?
        final pathToX = ps.firstWhere((p) => p.leaf.symbol == 'X?');
        expect(pathToX.length, equals(2));
        expect(pathToX.root.symbol, equals('f/2'));
        expect(pathToX.root.argIndex, equals(0));
        expect(pathToX.leaf.argIndex, equals(1));

        // Find path to Y
        final pathToY = ps.firstWhere((p) => p.leaf.symbol == 'Y');
        expect(pathToY.length, equals(2));
        expect(pathToY.leaf.argIndex, equals(2));
      });

      test('nested compound has paths through structure', () {
        // ↓f(↑g(X?))
        final compound = ModedCompound(Mode.consume, 'f', 1, [
          ModedCompound(Mode.produce, 'g', 1, [
            ModedVariable.reader('X', structuralMode: Mode.consume),
          ]),
        ]);
        final ps = paths(compound);

        expect(ps.length, equals(1));
        final p = ps.first;
        expect(p.length, equals(3));

        // Root: f/1
        expect(p.steps[0].symbol, equals('f/1'));
        expect(p.steps[0].argIndex, equals(0));
        expect(p.steps[0].mode, equals(Mode.consume));

        // Middle: g/1
        expect(p.steps[1].symbol, equals('g/1'));
        expect(p.steps[1].argIndex, equals(1));
        expect(p.steps[1].mode, equals(Mode.produce));

        // Leaf: X?
        expect(p.steps[2].symbol, equals('X?'));
        expect(p.steps[2].argIndex, equals(1));
        expect(p.steps[2].mode, equals(Mode.consume));
        expect(p.steps[2].isVariable, isTrue);
      });

      test('merge moded head paths (paper example)', () {
        // H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
        final modedHead = ModedCompound(Mode.consume, 'merge', 3, [
          ModedCompound.listCons(
            Mode.consume,
            ModedVariable.reader('X', structuralMode: Mode.consume),
            ModedVariable.reader('Xs', structuralMode: Mode.consume),
          ),
          ModedVariable.reader('Ys'),
          ModedCompound.listCons(
            Mode.produce,
            ModedVariable.writer('X', structuralMode: Mode.produce),
            ModedVariable.writer('Zs'),
          ),
        ]);

        final ps = paths(modedHead);

        // Should have 5 paths (one to each leaf)
        expect(ps.length, equals(5));

        // Path to X? in first arg: merge/3 → [|]/2 → X?
        final pathToX1 = ps.firstWhere(
          (p) => p.leaf.symbol == 'X?' && p.steps[1].argIndex == 1
        );
        expect(pathToX1.length, equals(3));
        expect(pathToX1.steps[0].symbol, equals('merge/3'));
        expect(pathToX1.steps[0].mode, equals(Mode.consume));
        expect(pathToX1.steps[1].symbol, equals('[|]/2'));
        expect(pathToX1.steps[1].argIndex, equals(1)); // first arg of merge
        expect(pathToX1.steps[1].mode, equals(Mode.consume));
        expect(pathToX1.steps[2].symbol, equals('X?'));
        expect(pathToX1.steps[2].argIndex, equals(1)); // head of list

        // Path to Xs?: merge/3 → [|]/2 → Xs?
        final pathToXs = ps.firstWhere((p) => p.leaf.symbol == 'Xs?');
        expect(pathToXs.length, equals(3));
        expect(pathToXs.steps[2].argIndex, equals(2)); // tail of list

        // Path to Ys?: merge/3 → Ys?
        final pathToYs = ps.firstWhere((p) => p.leaf.symbol == 'Ys?');
        expect(pathToYs.length, equals(2));
        expect(pathToYs.steps[1].argIndex, equals(2)); // second arg of merge

        // Path to X (writer) in third arg: merge/3 → [|]/2 → X
        final pathToX2 = ps.firstWhere(
          (p) => p.leaf.symbol == 'X' && p.steps[1].argIndex == 3
        );
        expect(pathToX2.length, equals(3));
        expect(pathToX2.steps[1].mode, equals(Mode.produce));
        expect(pathToX2.steps[2].isVariable, isTrue);
        expect(pathToX2.steps[2].isReader, isFalse); // writer

        // Path to Zs: merge/3 → [|]/2 → Zs
        final pathToZs = ps.firstWhere((p) => p.leaf.symbol == 'Zs');
        expect(pathToZs.length, equals(3));
        expect(pathToZs.steps[2].isWriter, isTrue);
      });

      test('list with nil and cons', () {
        // ↓[↓X?|↓[]]
        final term = ModedCompound.listCons(
          Mode.consume,
          ModedVariable.reader('X', structuralMode: Mode.consume),
          ModedConstant.nil(Mode.consume),
        );

        final ps = paths(term);
        expect(ps.length, equals(2));

        // Path to X?
        final pathToX = ps.firstWhere((p) => p.leaf.symbol == 'X?');
        expect(pathToX.length, equals(2));

        // Path to []
        final pathToNil = ps.firstWhere((p) => p.leaf.symbol == '[]');
        expect(pathToNil.length, equals(2));
        expect(pathToNil.leaf.isVariable, isFalse);
      });
    });

    // =========================================================================
    // Negative Controls - Structural Inequalities
    // =========================================================================

    group('Negative Controls', () {
      test('different variable names are not equal', () {
        expect(ModedVariable.reader('X', structuralMode: Mode.consume), isNot(equals(ModedVariable.reader('Y', structuralMode: Mode.consume))));
      });

      test('reader and writer of same name are not equal', () {
        expect(ModedVariable.reader('X', structuralMode: Mode.consume), isNot(equals(ModedVariable.writer('X', structuralMode: Mode.produce))));
      });

      test('constants with different modes are not equal', () {
        expect(
          ModedConstant(Mode.consume, 42),
          isNot(equals(ModedConstant(Mode.produce, 42))),
        );
      });

      test('compounds with different modes are not equal', () {
        final c1 = ModedCompound(Mode.consume, 'f', 0, []);
        final c2 = ModedCompound(Mode.produce, 'f', 0, []);
        expect(c1, isNot(equals(c2)));
      });

      test('compounds with different functors are not equal', () {
        final c1 = ModedCompound(Mode.consume, 'f', 0, []);
        final c2 = ModedCompound(Mode.consume, 'g', 0, []);
        expect(c1, isNot(equals(c2)));
      });

      test('compounds with different arities are not equal', () {
        final c1 = ModedCompound(Mode.consume, 'f', 1, [ModedVariable.reader('X', structuralMode: Mode.consume)]);
        final c2 = ModedCompound(Mode.consume, 'f', 2, [
          ModedVariable.reader('X', structuralMode: Mode.consume),
          ModedVariable.reader('Y', structuralMode: Mode.consume),
        ]);
        expect(c1, isNot(equals(c2)));
      });

      test('path steps with different argIndex are not equal', () {
        final s1 = PathStep(symbol: 'f/2', argIndex: 1, mode: Mode.consume);
        final s2 = PathStep(symbol: 'f/2', argIndex: 2, mode: Mode.consume);
        expect(s1, isNot(equals(s2)));
      });

      test('path steps with different modes are not equal', () {
        final s1 = PathStep(symbol: 'f/2', argIndex: 1, mode: Mode.consume);
        final s2 = PathStep(symbol: 'f/2', argIndex: 1, mode: Mode.produce);
        expect(s1, isNot(equals(s2)));
      });
    });

    // =========================================================================
    // Classification Methods Tests (isConsumed, isProduced, isIO)
    // Spec: moded-term.md v0.5
    // =========================================================================

    group('isConsumed', () {
      test('returns true for fully consumed term', () {
        // ↓merge(↓[↓X?|Xs?], Ys?) - all modes are ↓
        final term = ModedCompound(Mode.consume, 'merge', 2, [
          ModedCompound.listCons(
            Mode.consume,
            ModedVariable.reader('X', structuralMode: Mode.consume),
            ModedVariable.reader('Xs', structuralMode: Mode.consume),
          ),
          ModedVariable.reader('Ys'),
        ]);
        expect(isConsumed(term), isTrue);
      });

      test('returns true for single consumed constant', () {
        final term = ModedConstant(Mode.consume, 42);
        expect(isConsumed(term), isTrue);
      });

      test('returns true for single consumed variable (reader)', () {
        // Variables have implicit mode based on reader/writer
        final term = ModedVariable.reader('X', structuralMode: Mode.consume);
        expect(isConsumed(term), isTrue);
      });

      test('returns false when any annotation is produce', () {
        // ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs]) - has ↑ in output arg
        final term = ModedCompound(Mode.consume, 'merge', 3, [
          ModedCompound.listCons(
            Mode.consume,
            ModedVariable.reader('X', structuralMode: Mode.consume),
            ModedVariable.reader('Xs', structuralMode: Mode.consume),
          ),
          ModedVariable.reader('Ys'),
          ModedCompound.listCons(
            Mode.produce, // This makes it not fully consumed
            ModedVariable.writer('X', structuralMode: Mode.produce),
            ModedVariable.writer('Zs'),
          ),
        ]);
        expect(isConsumed(term), isFalse);
      });

      test('returns false for produced constant', () {
        final term = ModedConstant(Mode.produce, 42);
        expect(isConsumed(term), isFalse);
      });
    });

    group('isProduced', () {
      test('returns true for fully produced term', () {
        // ↑merge(↑[↑X|Xs], Ys) - all modes are ↑
        final term = ModedCompound(Mode.produce, 'merge', 2, [
          ModedCompound.listCons(
            Mode.produce,
            ModedVariable.writer('X', structuralMode: Mode.produce),
            ModedVariable.writer('Xs', structuralMode: Mode.produce),
          ),
          ModedVariable.writer('Ys'),
        ]);
        expect(isProduced(term), isTrue);
      });

      test('returns true for single produced constant', () {
        final term = ModedConstant(Mode.produce, 42);
        expect(isProduced(term), isTrue);
      });

      test('returns true for single produced variable (writer)', () {
        final term = ModedVariable.writer('X', structuralMode: Mode.produce);
        expect(isProduced(term), isTrue);
      });

      test('returns false when any annotation is consume', () {
        // ↑merge(↓X?, Y) - has ↓ in first arg
        final term = ModedCompound(Mode.produce, 'merge', 2, [
          ModedVariable.reader('X', structuralMode: Mode.consume), // reader has consume mode
          ModedVariable.writer('Y', structuralMode: Mode.produce),
        ]);
        expect(isProduced(term), isFalse);
      });

      test('returns false for consumed constant', () {
        final term = ModedConstant(Mode.consume, 42);
        expect(isProduced(term), isFalse);
      });
    });

    group('isIO', () {
      test('returns true for valid I/O term (paper example)', () {
        // H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
        // Root ↓, args 1-2 stay ↓, arg 3 flips to ↑ and stays ↑
        final term = ModedCompound(Mode.consume, 'merge', 3, [
          ModedCompound.listCons(
            Mode.consume,
            ModedVariable.reader('X', structuralMode: Mode.consume),
            ModedVariable.reader('Xs', structuralMode: Mode.consume),
          ),
          ModedVariable.reader('Ys'),
          ModedCompound.listCons(
            Mode.produce,
            ModedVariable.writer('X', structuralMode: Mode.produce),
            ModedVariable.writer('Zs'),
          ),
        ]);
        expect(isIO(term), isTrue);
      });

      test('returns true for fully consumed term (no mode inversions)', () {
        // ↓[↓X?|Xs?] - all ↓, valid I/O (zero inversions)
        final term = ModedCompound.listCons(
          Mode.consume,
          ModedVariable.reader('X', structuralMode: Mode.consume),
          ModedVariable.reader('Xs', structuralMode: Mode.consume),
        );
        expect(isIO(term), isTrue);
      });

      test('returns false when root is produce', () {
        // ↑merge(...) - root is ↑, not valid I/O
        final term = ModedCompound(Mode.produce, 'merge', 2, [
          ModedVariable.writer('X', structuralMode: Mode.produce),
          ModedVariable.writer('Y', structuralMode: Mode.produce),
        ]);
        expect(isIO(term), isFalse);
      });

      test('returns false when mode flips back from produce to consume', () {
        // ↓f(↑g(↓h(X?))) - path goes ↓ → ↑ → ↓, invalid
        final term = ModedCompound(Mode.consume, 'f', 1, [
          ModedCompound(Mode.produce, 'g', 1, [
            ModedCompound(Mode.consume, 'h', 1, [ // Invalid: ↑ → ↓
              ModedVariable.reader('X', structuralMode: Mode.consume),
            ]),
          ]),
        ]);
        expect(isIO(term), isFalse);
      });

      test('returns false for single produced constant', () {
        // ↑42 - root is produce
        final term = ModedConstant(Mode.produce, 42);
        expect(isIO(term), isFalse);
      });

      test('returns true for single consumed constant', () {
        // ↓42 - root is consume, valid I/O
        final term = ModedConstant(Mode.consume, 42);
        expect(isIO(term), isTrue);
      });

      test('returns false for writer variable (implicit produce mode)', () {
        // X (writer) has implicit produce mode - not valid I/O root
        final term = ModedVariable.writer('X', structuralMode: Mode.produce);
        expect(isIO(term), isFalse);
      });

      test('returns true for reader variable (implicit consume mode)', () {
        // X? (reader) has implicit consume mode - valid I/O root
        final term = ModedVariable.reader('X', structuralMode: Mode.consume);
        expect(isIO(term), isTrue);
      });
    });
  });
}
