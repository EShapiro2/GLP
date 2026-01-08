// test/analysis/type_checker/prelude_test.dart
//
// Tests for predefined types prelude

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/prelude.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'test_helpers.dart';

/// Parse only the prelude (without user code)
TypeEnvironment parsePreludeOnly() {
  final lexer = TypeLexer(typePrelude);
  final tokens = lexer.tokenize();
  final parser = TypeParser(tokens);
  return parser.parse();
}

void main() {
  group('Predefined Types Prelude', () {

    group('Prelude Parsing', () {

      test('prelude parses without errors', () {
        // Parse just the prelude
        expect(() => parsePreludeOnly(), returnsNormally);
      });

      test('Output type is defined', () {
        final env = parsePreludeOnly();
        expect(env.hasType('Output'), isTrue);
        final output = env.getType('Output')!;
        expect(output.alternatives, hasLength(1));
        expect(output.alternatives[0], isA<PrimitiveModeAlt>());
        expect((output.alternatives[0] as PrimitiveModeAlt).isInput, isFalse);
      });

      test('Input type is defined', () {
        final env = parsePreludeOnly();
        expect(env.hasType('Input'), isTrue);
        final input = env.getType('Input')!;
        expect(input.alternatives, hasLength(1));
        expect(input.alternatives[0], isA<PrimitiveModeAlt>());
        expect((input.alternatives[0] as PrimitiveModeAlt).isInput, isTrue);
      });

      test('List type is defined with primitive elements', () {
        final env = parsePreludeOnly();
        expect(env.hasType('List'), isTrue);
        final list = env.getType('List')!;
        expect(list.alternatives, hasLength(2));
      });

      test('Stream type is defined', () {
        final env = parsePreludeOnly();
        expect(env.hasType('Stream'), isTrue);
        final stream = env.getType('Stream')!;
        expect(stream.alternatives, hasLength(2));
      });

      test('DiffList type is defined', () {
        final env = parsePreludeOnly();
        expect(env.hasType('DiffList'), isTrue);
      });

      test('Channel type is defined', () {
        final env = parsePreludeOnly();
        expect(env.hasType('Channel'), isTrue);
      });

    });

    group('Predefined Procedures', () {

      test('= procedure is declared', () {
        final env = parsePreludeOnly();
        expect(env.hasProcedure('=', 2), isTrue);
      });

      test('ground procedure is declared', () {
        final env = parsePreludeOnly();
        expect(env.hasProcedure('ground', 1), isTrue);
      });

      test('dl_append procedure is declared', () {
        final env = parsePreludeOnly();
        expect(env.hasProcedure('dl_append', 3), isTrue);
      });

      test('dl_to_list procedure is declared', () {
        final env = parsePreludeOnly();
        expect(env.hasProcedure('dl_to_list', 2), isTrue);
      });

      test('new_channel procedure is declared', () {
        final env = parsePreludeOnly();
        expect(env.hasProcedure('new_channel', 2), isTrue);
      });

      test('send procedure is declared', () {
        final env = parsePreludeOnly();
        expect(env.hasProcedure('send', 3), isTrue);
      });

      test('receive procedure is declared', () {
        final env = parsePreludeOnly();
        expect(env.hasProcedure('receive', 3), isTrue);
      });

    });

    group('Redefinition Prevention', () {

      test('cannot redefine Output', () {
        expect(
          () => checkTypes('Output ::= foo ; bar.'),
          throwsA(predicate((e) => e.toString().contains('redefine'))),
        );
      });

      test('cannot redefine Input', () {
        expect(
          () => checkTypes('Input ::= foo ; bar.'),
          throwsA(predicate((e) => e.toString().contains('redefine'))),
        );
      });

      test('cannot redefine List', () {
        expect(
          () => checkTypes('List ::= mylist.'),
          throwsA(predicate((e) => e.toString().contains('redefine'))),
        );
      });

      test('cannot redefine DiffList', () {
        expect(
          () => checkTypes('DiffList ::= mydiff.'),
          throwsA(predicate((e) => e.toString().contains('redefine'))),
        );
      });

      test('cannot redefine Channel', () {
        expect(
          () => checkTypes('Channel ::= mychan.'),
          throwsA(predicate((e) => e.toString().contains('redefine'))),
        );
      });

      test('cannot redefine dl_append', () {
        expect(
          () => checkTypes('''
            procedure dl_append(Output, Output, Output).
            dl_append(_, _, _).
          '''),
          throwsA(predicate((e) => e.toString().contains('redefine'))),
        );
      });

      test('cannot redefine send', () {
        expect(
          () => checkTypes('''
            procedure send(Output, Output, Output).
            send(_, _, _).
          '''),
          throwsA(predicate((e) => e.toString().contains('redefine'))),
        );
      });

      test('can define new types', () {
        final result = checkTypes('''
          MyType ::= foo ; bar.
          procedure myProc(MyType).
          myProc(foo).
          myProc(bar).
        ''');
        expect(result.errors, isEmpty);
      });

    });

    group('Usage in Programs', () {

      test('can use List without declaring it', () {
        final result = checkTypes('''
          Atom ::= a ; b ; c.
          procedure myLength(List?, Atom).
          myLength([], a).
          myLength([X | Xs], b) :- myLength(Xs?, a).
        ''');
        // Check that List is recognized as predefined type
        expect(result.errors.where((e) =>
          e.message.contains('List') && e.message.contains('not') && e.message.contains('defined')),
          isEmpty,
          reason: 'List should be recognized as predefined type');
      });

      test('can use Output without declaring it', () {
        final result = checkTypes('''
          procedure identity(Output, Output).
          identity(X, Y).
        ''');
        expect(result.errors.where((e) =>
          e.message.contains('Output') && e.message.contains('not') && e.message.contains('defined')),
          isEmpty,
          reason: 'Output should be recognized as predefined type');
      });

      test('can use Channel in program', () {
        final result = checkTypes('''
          Atom ::= a ; b.
          procedure sender(Channel?, Atom).
          sender(Ch, a).
        ''');
        expect(result.errors.where((e) =>
          e.message.contains('Channel') && e.message.contains('not') && e.message.contains('defined')),
          isEmpty,
          reason: 'Channel should be recognized as predefined type');
      });

      test('can use Stream in program', () {
        final result = checkTypes('''
          Atom ::= a.
          procedure process(Stream?, Atom).
          process([X | Xs], a).
        ''');
        expect(result.errors.where((e) =>
          e.message.contains('Stream') && e.message.contains('not') && e.message.contains('defined')),
          isEmpty,
          reason: 'Stream should be recognized as predefined type');
      });

    });

  });
}
