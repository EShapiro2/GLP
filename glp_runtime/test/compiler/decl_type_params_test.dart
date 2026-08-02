// glp_runtime/test/compiler/decl_type_params_test.dart
//
// The named type-parameter list on a procedure declaration.
// Spec: Moded-Types (TGLP), sections/parameterized-types.tex, subsection
// "Parameterised Procedure Declarations" and the paragraph "Declaration
// parameters".

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/error.dart';

void main() {
  List<dynamic> declsOf(String source) =>
      Parser(Lexer(source).tokenize()).parseModule().procDeclarations;

  group('declaration parameter list', () {
    test('procedure(X) names one parameter', () {
      final d = declsOf('procedure(X) merge(Stream(X)?, Stream(X)?, Stream(X)).\n'
              'merge([], [], []).')
          .single;
      expect(d.name, 'merge');
      expect(d.arity, 3);
      expect(d.typeParams, ['X']);
      expect(d.isParameterized, isTrue);
    });

    test('procedure(X, Y) names both, in order', () {
      final d = declsOf('procedure(X, Y) new_channel(Channel(X, Y), Channel(Y, X)).\n'
              'new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).')
          .single;
      expect(d.typeParams, ['X', 'Y']);
    });

    test('a declaration with no list names no parameters', () {
      final d = declsOf('procedure counter(Stream(CounterCall)?).\n'
              'counter([]).')
          .single;
      expect(d.typeParams, isEmpty);
      expect(d.isParameterized, isFalse);
    });

    test('the list follows exported', () {
      final d = declsOf('exported procedure(X) relay(Stream(X)?, Stream(X)).\n'
              'relay([], []).')
          .single;
      expect(d.exported, isTrue);
      expect(d.typeParams, ['X']);
    });

    test('the list follows imported, with a module path', () {
      final d = declsOf(
              'imported procedure(X) streams#merge(Stream(X)?, Stream(X)?, Stream(X)).')
          .single;
      expect(d.imported, isTrue);
      expect(d.modulePath, 'streams');
      expect(d.name, 'merge');
      expect(d.typeParams, ['X']);
    });

    test('a parameter named on a bare argument is accepted', () {
      // X ties the two arguments to one type and lies within no template
      // instantiation, which is what inference could not read.
      final d = declsOf('procedure(X) =(X, X?).\n'
              'A? = A.')
          .single;
      expect(d.typeParams, ['X']);
    });

    test('a nullary declaration is unaffected', () {
      expect(
          declsOf('procedure play_introduction.\nplay_introduction.').single.typeParams,
          isEmpty);
      expect(
          declsOf('procedure play_introduction().\nplay_introduction.').single.typeParams,
          isEmpty);
    });

    test('an empty list is rejected', () {
      expect(() => declsOf('procedure() p(Stream(X)?).'), throwsA(isA<CompileError>()));
    });

    test('a repeated parameter is rejected', () {
      expect(() => declsOf('procedure(X, X) p(Stream(X)?, Stream(X)).'),
          throwsA(isA<CompileError>()));
    });

    test('a lowercase parameter name is rejected', () {
      expect(() => declsOf('procedure(x) p(Stream(x)?).'),
          throwsA(isA<CompileError>()));
    });
  });
}
