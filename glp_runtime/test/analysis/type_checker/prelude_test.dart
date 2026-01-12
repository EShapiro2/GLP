// test/analysis/type_checker/prelude_test.dart
//
// Tests for predefined types prelude

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/prelude.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'test_helpers.dart';

void main() {
  group('Predefined Types Prelude', () {

    group('Prelude Parsing', () {

      test('prelude parses without errors', () {
        expect(() => buildPreludeEnvironment(), returnsNormally);
      });

      test('List type is defined with primitive elements', () {
        final env = buildPreludeEnvironment();
        expect(env.hasType('List'), isTrue);
      });

      test('Stream type is defined', () {
        final env = buildPreludeEnvironment();
        expect(env.hasType('Stream'), isTrue);
      });

      test('DiffList type is defined', () {
        final env = buildPreludeEnvironment();
        expect(env.hasType('DiffList'), isTrue);
      });

      test('Channel type is defined', () {
        final env = buildPreludeEnvironment();
        expect(env.hasType('Channel'), isTrue);
      });

    });

    group('Predefined Procedures', () {

      test('= procedure is declared', () {
        final env = buildPreludeEnvironment();
        expect(env.hasProcedure('=', 2), isTrue);
      });

      test('ground procedure is declared', () {
        final env = buildPreludeEnvironment();
        expect(env.hasProcedure('ground', 1), isTrue);
      });

      test('dl_append procedure is declared', () {
        final env = buildPreludeEnvironment();
        expect(env.hasProcedure('dl_append', 3), isTrue);
      });

      test('dl_to_list procedure is declared', () {
        final env = buildPreludeEnvironment();
        expect(env.hasProcedure('dl_to_list', 2), isTrue);
      });

      test('new_channel procedure is declared', () {
        final env = buildPreludeEnvironment();
        expect(env.hasProcedure('new_channel', 2), isTrue);
      });

      test('send procedure is declared', () {
        final env = buildPreludeEnvironment();
        expect(env.hasProcedure('send', 3), isTrue);
      });

      test('receive procedure is declared', () {
        final env = buildPreludeEnvironment();
        expect(env.hasProcedure('receive', 3), isTrue);
      });

    });

    group('Redefinition Prevention', () {

      test('cannot redefine List', () {
        expect(
          () => checkTypes('List ::= mylist.'),
          throwsA(predicate((e) => e.toString().contains('redefine') || e.toString().contains('Redefin'))),
        );
      });

      test('cannot redefine DiffList', () {
        expect(
          () => checkTypes('DiffList ::= mydiff.'),
          throwsA(predicate((e) => e.toString().contains('redefine') || e.toString().contains('Redefin'))),
        );
      });

      test('cannot redefine Channel', () {
        expect(
          () => checkTypes('Channel ::= mychan.'),
          throwsA(predicate((e) => e.toString().contains('redefine') || e.toString().contains('Redefin'))),
        );
      });

      test('cannot redefine dl_append', () {
        expect(
          () => checkTypes('''
            procedure dl_append(_, _, _).
            dl_append(_, _, _).
          '''),
          throwsA(predicate((e) => e.toString().contains('redefine') || e.toString().contains('Redefin'))),
        );
      });

      test('cannot redefine send', () {
        expect(
          () => checkTypes('''
            procedure send(_, _, _).
            send(_, _, _).
          '''),
          throwsA(predicate((e) => e.toString().contains('redefine') || e.toString().contains('Redefin'))),
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
          procedure myLength(List?, _).
          myLength([], zero).
          myLength([X | Xs], s(N?)) :- myLength(Xs?, N).
        ''');
        expect(result.errors.where((e) =>
          e.message.contains('List') && e.message.contains('not') && e.message.contains('defined')),
          isEmpty,
          reason: 'List should be recognized as predefined type');
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
