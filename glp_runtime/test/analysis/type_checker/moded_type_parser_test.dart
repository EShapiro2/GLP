// test/analysis/type_checker/moded_type_parser_test.dart
//
// Tests for moded type parser - input/output mode annotations

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';

void main() {
  group('Moded Type Parser', () {
    test('Parse procedure with input modes - merge(List?, List?, List)', () {
      final source = 'procedure merge(List?, List?, List).';
      final env = parseTypes(source);

      // Check that merge/3 was parsed (prelude may contain other procedures)
      expect(env.procedures.containsKey('merge/3'), isTrue);
      final proc = env.procedures['merge/3'];
      expect(proc, isNotNull);
      expect(proc!.name, equals('merge'));
      expect(proc.arity, equals(3));

      // First two args are input mode (List?)
      expect(proc.argTypes[0].typeName, equals('List'));
      expect(proc.argTypes[0].isInputMode, isTrue);
      expect(proc.argTypes[1].typeName, equals('List'));
      expect(proc.argTypes[1].isInputMode, isTrue);

      // Third arg is output mode (List)
      expect(proc.argTypes[2].typeName, equals('List'));
      expect(proc.argTypes[2].isInputMode, isFalse);
    });

    test('Parse procedure with all output modes - foo(A, B, C)', () {
      final source = 'procedure foo(A, B, C).';
      final env = parseTypes(source);

      final proc = env.procedures['foo/3'];
      expect(proc, isNotNull);

      // All args are output mode (no ?)
      expect(proc!.argTypes[0].isInputMode, isFalse);
      expect(proc.argTypes[1].isInputMode, isFalse);
      expect(proc.argTypes[2].isInputMode, isFalse);
    });

    test('Parse procedure with mixed modes - bar(X?, Y, Z?)', () {
      final source = 'procedure bar(X?, Y, Z?).';
      final env = parseTypes(source);

      final proc = env.procedures['bar/3'];
      expect(proc, isNotNull);

      expect(proc!.argTypes[0].isInputMode, isTrue);   // X?
      expect(proc!.argTypes[1].isInputMode, isFalse);  // Y
      expect(proc!.argTypes[2].isInputMode, isTrue);   // Z?
    });

    test('Parse type definition with embedded modes - embedded not yet supported', () {
      // Future: TreeMsg ::= leaf(Number?) ; node(TreeMsg?, TreeMsg?, TreeMsg).
      // For Phase 1, we only parse modes in procedure declarations
      // Type definitions don't yet have mode annotations in alternatives
    });

    test('TypeRef complement operation - mode inversion', () {
      final inputRef = TypeRef('List', 1, 1, isInput: true);
      final outputRef = TypeRef('List', 1, 1, isInput: false);

      // Complement inverts mode
      expect(inputRef.complement().isInput, isFalse);
      expect(outputRef.complement().isInput, isTrue);

      // Complement is involution: (T?)? = T
      expect(inputRef.complement().complement().isInput, equals(inputRef.isInput));
      expect(outputRef.complement().complement().isInput, equals(outputRef.isInput));
    });

    test('TypeRef toString includes ? for input mode', () {
      final inputRef = TypeRef('Nat', 1, 1, isInput: true);
      final outputRef = TypeRef('Nat', 1, 1, isInput: false);

      expect(inputRef.toString(), equals('Nat?'));
      expect(outputRef.toString(), equals('Nat'));
    });

    test('TypeRef equality includes mode', () {
      final input1 = TypeRef('List', 1, 1, isInput: true);
      final input2 = TypeRef('List', 1, 1, isInput: true);
      final output = TypeRef('List', 1, 1, isInput: false);

      expect(input1, equals(input2));
      expect(input1, isNot(equals(output)));
    });

    test('ProcDecl isInputArg helper', () {
      final source = 'procedure test(A?, B, C?).';
      final env = parseTypes(source);
      final proc = env.procedures['test/3'];

      expect(proc!.isInputArg(0), isTrue);   // A?
      expect(proc.isInputArg(1), isFalse);   // B
      expect(proc.isInputArg(2), isTrue);    // C?
    });

    // Note: calleeView is no longer part of ProcDecl - mode complement
    // is handled during type checking, not at the declaration level.

    test('Parse counter.glp - real example from typed_book', () {
      final source = '''
CounterMsg ::= clear ; up ; down ; show(Number).
CounterStream ::= [] ; [CounterMsg | CounterStream].

procedure counter(CounterStream, Number).
''';

      final env = parseTypes(source);

      // Check types are parsed (prelude contains additional types)
      expect(env.types.containsKey('CounterMsg'), isTrue);
      expect(env.types.containsKey('CounterStream'), isTrue);
      expect(env.types['CounterMsg'], isNotNull);
      expect(env.types['CounterStream'], isNotNull);

      // Check procedure
      final proc = env.procedures['counter/2'];
      expect(proc, isNotNull);
      expect(proc!.argTypes[0].typeName, equals('CounterStream'));
      expect(proc.argTypes[0].isInputMode, isFalse);  // Output mode (no ?)
      expect(proc.argTypes[1].typeName, equals('Number'));
      expect(proc.argTypes[1].isInputMode, isFalse);  // Output mode (no ?)
    });
  });
}
