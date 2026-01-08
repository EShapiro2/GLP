// test/analysis/type_checker/moded_head_test.dart
//
// Tests for moded_head.dart
// Specification: docs/modules/moded-head.md v0.5
// Paper Reference: Definition 4.6

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/moded_head.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  group('modedHead', () {
    // =========================================================================
    // Basic Cases
    // =========================================================================

    group('Basic Cases', () {
      test('simple unary input procedure', () {
        // procedure test(Nat?).
        // test(X)
        final decl = ProcDecl('test', [
          TypeRef('Nat', 1, 1, isInput: true),
        ], 1, 1);

        final head = ast.Goal('test', [
          ast.VarTerm('X', false, 1, 1),  // writer
        ], 1, 1);

        final result = modedHead(head, decl);

        expect(result, isA<ModedCompound>());
        final compound = result as ModedCompound;
        expect(compound.mode, equals(Mode.consume)); // Root is ↓
        expect(compound.functor, equals('test'));
        expect(compound.arity, equals(1));

        // Argument: input → mode ↓, variable flipped (X → X?)
        final arg = compound.args[0] as ModedVariable;
        expect(arg.name, equals('X'));
        expect(arg.isReader, isTrue); // X flipped to X?
      });

      test('simple unary output procedure', () {
        // procedure test(Nat).
        // test(Y?)
        final decl = ProcDecl('test', [
          TypeRef('Nat', 1, 1, isInput: false),
        ], 1, 1);

        final head = ast.Goal('test', [
          ast.VarTerm('Y', true, 1, 1),  // reader (isReader=true)
        ], 1, 1);

        final result = modedHead(head, decl);

        expect(result, isA<ModedCompound>());
        final compound = result as ModedCompound;
        expect(compound.mode, equals(Mode.consume)); // Root is ↓

        // Argument: output → mode ↑, variable flipped (Y? → Y)
        final arg = compound.args[0] as ModedVariable;
        expect(arg.name, equals('Y'));
        expect(arg.isWriter, isTrue); // Y? flipped to Y
      });

      test('binary procedure with mixed modes', () {
        // procedure copy(List?, List).
        // copy(Xs, Ys?)
        final decl = ProcDecl('copy', [
          TypeRef('List', 1, 1, isInput: true),
          TypeRef('List', 1, 1, isInput: false),
        ], 1, 1);

        final head = ast.Goal('copy', [
          ast.VarTerm('Xs', false, 1, 1),  // writer
          ast.VarTerm('Ys', true, 1, 1),   // reader
        ], 1, 1);

        final result = modedHead(head, decl) as ModedCompound;

        // Arg 1: input → mode ↓, Xs flipped to Xs?
        final arg1 = result.args[0] as ModedVariable;
        expect(arg1.isReader, isTrue);

        // Arg 2: output → mode ↑, Ys? flipped to Ys
        final arg2 = result.args[1] as ModedVariable;
        expect(arg2.isWriter, isTrue);
      });
    });

    // =========================================================================
    // Paper Example: merge
    // =========================================================================

    group('Paper Example: merge', () {
      test('merge clause 2 head', () {
        // procedure merge(Stream?, Stream?, Stream).
        // merge([X|Xs], Ys, [X?|Zs?])
        //
        // Expected moded head:
        // H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])

        final decl = ProcDecl('merge', [
          TypeRef('Stream', 1, 1, isInput: true),
          TypeRef('Stream', 1, 1, isInput: true),
          TypeRef('Stream', 1, 1, isInput: false),
        ], 1, 1);

        // merge([X|Xs], Ys, [X?|Zs?])
        final head = ast.Goal('merge', [
          // [X|Xs] - list with head X (writer), tail Xs (writer)
          ast.ListTerm(
            ast.VarTerm('X', false, 1, 1),   // head: X (writer)
            ast.VarTerm('Xs', false, 1, 1),  // tail: Xs (writer)
            1,
            1,
          ),
          // Ys - writer variable
          ast.VarTerm('Ys', false, 1, 1),
          // [X?|Zs?] - list with head X? (reader), tail Zs? (reader)
          ast.ListTerm(
            ast.VarTerm('X', true, 1, 1),   // head: X? (reader)
            ast.VarTerm('Zs', true, 1, 1),  // tail: Zs? (reader)
            1,
            1,
          ),
        ], 1, 1);

        final result = modedHead(head, decl) as ModedCompound;

        // Root: ↓merge
        expect(result.mode, equals(Mode.consume));
        expect(result.functor, equals('merge'));
        expect(result.arity, equals(3));

        // Arg 1: ↓[↓X?|Xs?] (input mode, variables flipped)
        final arg1 = result.args[0] as ModedCompound;
        expect(arg1.mode, equals(Mode.consume));
        expect(arg1.isListCons, isTrue);

        final arg1Head = arg1.args[0] as ModedVariable;
        expect(arg1Head.name, equals('X'));
        expect(arg1Head.isReader, isTrue); // X → X?

        final arg1Tail = arg1.args[1] as ModedVariable;
        expect(arg1Tail.name, equals('Xs'));
        expect(arg1Tail.isReader, isTrue); // Xs → Xs?

        // Arg 2: Ys? (input mode, variable flipped)
        final arg2 = result.args[1] as ModedVariable;
        expect(arg2.name, equals('Ys'));
        expect(arg2.isReader, isTrue); // Ys → Ys?

        // Arg 3: ↑[↑X|Zs] (output mode, variables flipped)
        final arg3 = result.args[2] as ModedCompound;
        expect(arg3.mode, equals(Mode.produce));
        expect(arg3.isListCons, isTrue);

        final arg3Head = arg3.args[0] as ModedVariable;
        expect(arg3Head.name, equals('X'));
        expect(arg3Head.isWriter, isTrue); // X? → X

        final arg3Tail = arg3.args[1] as ModedVariable;
        expect(arg3Tail.name, equals('Zs'));
        expect(arg3Tail.isWriter, isTrue); // Zs? → Zs
      });

      test('merge clause 1 head (nil case)', () {
        // merge([], Ys, Ys?)
        final decl = ProcDecl('merge', [
          TypeRef('Stream', 1, 1, isInput: true),
          TypeRef('Stream', 1, 1, isInput: true),
          TypeRef('Stream', 1, 1, isInput: false),
        ], 1, 1);

        final head = ast.Goal('merge', [
          ast.ListTerm(null, null, 1, 1),  // nil
          ast.VarTerm('Ys', false, 1, 1),  // writer
          ast.VarTerm('Ys', true, 1, 1),   // reader
        ], 1, 1);

        final result = modedHead(head, decl) as ModedCompound;

        // Arg 1: ↓[] (nil constant)
        final arg1 = result.args[0] as ModedConstant;
        expect(arg1.mode, equals(Mode.consume));
        expect(arg1.isNil, isTrue);

        // Arg 2: Ys? (flipped from Ys)
        final arg2 = result.args[1] as ModedVariable;
        expect(arg2.isReader, isTrue);

        // Arg 3: ↑Ys (flipped from Ys?, output mode)
        final arg3 = result.args[2] as ModedVariable;
        expect(arg3.isWriter, isTrue);
      });
    });

    // =========================================================================
    // Nested Structures
    // =========================================================================

    group('Nested Structures', () {
      test('nested structure with functor', () {
        // procedure test(Tree?).
        // test(node(L, X, R))
        final decl = ProcDecl('test', [
          TypeRef('Tree', 1, 1, isInput: true),
        ], 1, 1);

        final head = ast.Goal('test', [
          ast.StructTerm('node', [
            ast.VarTerm('L', false, 1, 1),
            ast.VarTerm('X', false, 1, 1),
            ast.VarTerm('R', false, 1, 1),
          ], 1, 1),
        ], 1, 1);

        final result = modedHead(head, decl) as ModedCompound;

        final arg = result.args[0] as ModedCompound;
        expect(arg.mode, equals(Mode.consume));
        expect(arg.functor, equals('node'));
        expect(arg.arity, equals(3));

        // All variables flipped
        for (final subArg in arg.args) {
          expect((subArg as ModedVariable).isReader, isTrue);
        }
      });

      test('constant in structure', () {
        // procedure test(Nat?).
        // test(s(0))
        final decl = ProcDecl('test', [
          TypeRef('Nat', 1, 1, isInput: true),
        ], 1, 1);

        final head = ast.Goal('test', [
          ast.StructTerm('s', [
            ast.ConstTerm(0, 1, 1),
          ], 1, 1),
        ], 1, 1);

        final result = modedHead(head, decl) as ModedCompound;

        final arg = result.args[0] as ModedCompound;
        expect(arg.functor, equals('s'));

        final inner = arg.args[0] as ModedConstant;
        expect(inner.value, equals(0));
        expect(inner.mode, equals(Mode.consume));
      });
    });

    // =========================================================================
    // Error Cases
    // =========================================================================

    group('Error Cases', () {
      test('arity mismatch throws ArityMismatchError', () {
        final decl = ProcDecl('test', [
          TypeRef('Nat', 1, 1, isInput: true),
          TypeRef('Nat', 1, 1, isInput: false),
        ], 1, 1);

        final head = ast.Goal('test', [
          ast.VarTerm('X', false, 1, 1),
        ], 1, 1); // Only 1 arg, but decl expects 2

        expect(
          () => modedHead(head, decl),
          throwsA(isA<ArityMismatchError>()),
        );
      });
    });
  });

  // ===========================================================================
  // producedTerm Tests
  // ===========================================================================

  group('producedTerm', () {
    group('Basic Cases', () {
      test('body atom has produce mode at root', () {
        // procedure test(Nat?).
        // body atom: test(X?)
        final decl = ProcDecl('test', [
          TypeRef('Nat', 1, 1, isInput: true),
        ], 1, 1);

        final atom = ast.Goal('test', [
          ast.VarTerm('X', true, 1, 1),  // reader
        ], 1, 1);

        final result = producedTerm(atom, decl) as ModedCompound;

        expect(result.mode, equals(Mode.produce)); // Root is ↑
        expect(result.functor, equals('test'));
      });

      test('body atom does NOT flip variables', () {
        // procedure copy(List?, List).
        // body atom: copy(Xs?, Ys)
        final decl = ProcDecl('copy', [
          TypeRef('List', 1, 1, isInput: true),
          TypeRef('List', 1, 1, isInput: false),
        ], 1, 1);

        final atom = ast.Goal('copy', [
          ast.VarTerm('Xs', true, 1, 1),   // reader
          ast.VarTerm('Ys', false, 1, 1),  // writer
        ], 1, 1);

        final result = producedTerm(atom, decl) as ModedCompound;

        // Variables NOT flipped (unlike modedHead)
        final arg1 = result.args[0] as ModedVariable;
        expect(arg1.name, equals('Xs'));
        expect(arg1.isReader, isTrue); // Still reader

        final arg2 = result.args[1] as ModedVariable;
        expect(arg2.name, equals('Ys'));
        expect(arg2.isWriter, isTrue); // Still writer
      });
    });

    group('Paper Example: merge body atom', () {
      test('merge body atom', () {
        // Body atom: merge(Ys?, Xs?, Zs)
        // Expected: ↑merge(Ys?, Xs?, Zs)
        final decl = ProcDecl('merge', [
          TypeRef('Stream', 1, 1, isInput: true),
          TypeRef('Stream', 1, 1, isInput: true),
          TypeRef('Stream', 1, 1, isInput: false),
        ], 1, 1);

        final atom = ast.Goal('merge', [
          ast.VarTerm('Ys', true, 1, 1),   // reader
          ast.VarTerm('Xs', true, 1, 1),   // reader
          ast.VarTerm('Zs', false, 1, 1),  // writer
        ], 1, 1);

        final result = producedTerm(atom, decl) as ModedCompound;

        expect(result.mode, equals(Mode.produce));

        // Arg 1: Ys? (NOT flipped)
        final arg1 = result.args[0] as ModedVariable;
        expect(arg1.name, equals('Ys'));
        expect(arg1.isReader, isTrue);

        // Arg 2: Xs? (NOT flipped)
        final arg2 = result.args[1] as ModedVariable;
        expect(arg2.name, equals('Xs'));
        expect(arg2.isReader, isTrue);

        // Arg 3: Zs (NOT flipped)
        final arg3 = result.args[2] as ModedVariable;
        expect(arg3.name, equals('Zs'));
        expect(arg3.isWriter, isTrue);
      });
    });

    group('Error Cases', () {
      test('arity mismatch throws ArityMismatchError', () {
        final decl = ProcDecl('test', [
          TypeRef('Nat', 1, 1, isInput: true),
        ], 1, 1);

        final atom = ast.Goal('test', [
          ast.VarTerm('X', false, 1, 1),
          ast.VarTerm('Y', false, 1, 1),
        ], 1, 1); // 2 args, but decl expects 1

        expect(
          () => producedTerm(atom, decl),
          throwsA(isA<ArityMismatchError>()),
        );
      });
    });
  });

  // ===========================================================================
  // Integration with paths()
  // ===========================================================================

  group('Integration: modedHead + paths', () {
    test('merge moded head produces correct paths', () {
      // H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
      final decl = ProcDecl('merge', [
        TypeRef('Stream', 1, 1, isInput: true),
        TypeRef('Stream', 1, 1, isInput: true),
        TypeRef('Stream', 1, 1, isInput: false),
      ], 1, 1);

      final head = ast.Goal('merge', [
        ast.ListTerm(
          ast.VarTerm('X', false, 1, 1),
          ast.VarTerm('Xs', false, 1, 1),
          1,
          1,
        ),
        ast.VarTerm('Ys', false, 1, 1),
        ast.ListTerm(
          ast.VarTerm('X', true, 1, 1),
          ast.VarTerm('Zs', true, 1, 1),
          1,
          1,
        ),
      ], 1, 1);

      final moded = modedHead(head, decl);
      final ps = paths(moded);

      // Should have 5 paths
      expect(ps.length, equals(5));

      // All paths start with consume mode (root ↓)
      for (final p in ps) {
        expect(p.root.mode, equals(Mode.consume));
        expect(p.root.symbol, equals('merge/3'));
      }

      // Verify path to first X? (input argument)
      final pathToX1 = ps.firstWhere(
        (p) => p.leaf.symbol == 'X?' && p.steps[1].argIndex == 1,
      );
      expect(pathToX1.steps[1].mode, equals(Mode.consume)); // input arg

      // Verify path to X (output argument)
      final pathToX2 = ps.firstWhere(
        (p) => p.leaf.symbol == 'X' && p.steps[1].argIndex == 3,
      );
      expect(pathToX2.steps[1].mode, equals(Mode.produce)); // output arg
    });
  });
}
