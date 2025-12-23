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

      test('Every type is defined', () {
        final env = parsePreludeOnly();
        expect(env.hasType('Every'), isTrue);
        final every = env.getType('Every')!;
        expect(every.isExact, isTrue); // ::= semantics
      });

      test('Any type is subtype of Every', () {
        final env = parsePreludeOnly();
        expect(env.hasType('Any'), isTrue);
        final any = env.getType('Any')!;
        expect(any.isExact, isFalse); // ::< semantics
      });

      test('List type is defined with Any elements', () {
        final env = parsePreludeOnly();
        expect(env.hasType('List'), isTrue);
        final list = env.getType('List')!;
        expect(list.isExact, isTrue);
      });

      test('Stream is subtype of List', () {
        final env = parsePreludeOnly();
        expect(env.hasType('Stream'), isTrue);
        final stream = env.getType('Stream')!;
        expect(stream.isExact, isFalse); // ::< semantics
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

      test('cannot redefine Every', () {
        expect(
          () => checkTypes('Every ::= foo ; bar.'),
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
            procedure dl_append(Any, Any, Any).
            dl_append(_, _, _).
          '''),
          throwsA(predicate((e) => e.toString().contains('redefine'))),
        );
      });

      test('cannot redefine send', () {
        expect(
          () => checkTypes('''
            procedure send(Any, Any, Any).
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
        // May have mode coverage errors for Any inside List, but should recognize List type
        expect(result.errors.where((e) =>
          e.message.contains('List') && e.message.contains('not') && e.message.contains('defined')),
          isEmpty,
          reason: 'List should be recognized as predefined type');
      });

      test('can use Any without declaring it', () {
        final result = checkTypes('''
          procedure identity(Any?, Any).
          identity(X, X?).
          identity(X?, X).
        ''');
        expect(result.errors.where((e) =>
          e.message.contains('Any') && e.message.contains('not') && e.message.contains('defined')),
          isEmpty,
          reason: 'Any should be recognized as predefined type');
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
