// glp_runtime/test/compiler/decl_type_params_test.dart
//
// The named type-parameter list on a procedure declaration.
// Spec: Moded-Types (TGLP), sections/parameterized-types.tex, subsection
// "Parameterised Procedure Declarations" and the paragraph "Declaration
// parameters".

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart'
    show setRootScopeEnvironmentSource;

void main() {
  List<dynamic> declsOf(String source) =>
      Parser(Lexer(source).tokenize()).parseModule().procDeclarations;

  // The checker groups below read the root scope, for Stream and the other
  // templates every declaration names.  buildRootScopeEnvironment has no
  // source of its own, so a test that does not set one sees no root type.
  setUpAll(() {
    setRootScopeEnvironmentSource(
        File('../programs/self.glp').readAsStringSync());
  });

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

  // The rule the list makes possible: "The parameters of a procedure
  // declaration are exactly those its parameter list names.  An undefined type
  // name occurring in a declaration and not in its parameter list is an error,
  // so a misspelt type name is rejected rather than read as a parameter"
  // (parameterized-types.tex, "Declaration parameters").  Until 2026-09-23 a
  // declaration naming no parameters fell back to inferring them from its
  // undefined names, so `pass(Strem?, Strem)` declared a procedure over an
  // unconstrained type instead of being rejected.
  group('an undefined type name not in the parameter list', () {
    dynamic check(String source) =>
        checkModule(Parser(Lexer(source).tokenize()).parseModule());

    Matcher namesUndefined(String type, String proc) => throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('undefined type "$type"') &&
              s.contains('in the declaration of $proc') &&
              s.contains('Declaration parameters');
        }, 'names $type in the declaration of $proc'));

    test('is refused when the declaration names no parameters', () {
      expect(() => check('procedure pass(Strem?, Strem).\npass(A?, A).'),
          namesUndefined('Strem', 'pass/2'));
    });

    test('is refused when the declaration names other parameters', () {
      expect(() => check('procedure(X) pass(Strem?, Strem).\npass(A?, A).'),
          namesUndefined('Strem', 'pass/2'));
    });

    test('is refused inside a template instantiation', () {
      expect(
          () => check('procedure copy(Stream(Strem)?, Stream(Strem)).\n'
              'copy([], []).'),
          namesUndefined('Strem', 'copy/2'));
    });

    test('is refused on an exported declaration', () {
      expect(
          () => check('exported procedure pass(Strem?, Strem).\npass(A?, A).'),
          namesUndefined('Strem', 'pass/2'));
    });

    test('the message says the declaration names none, and how to name it', () {
      expect(
          () => check('procedure pass(Strem?, Strem).\npass(A?, A).'),
          throwsA(predicate((e) {
            final s = e.toString();
            return s.contains('the declaration names no type parameters') &&
                s.contains('procedure(Strem) pass(...)');
          }, 'says no list is named and gives the remedy')));
    });

    test('the message names the list when there is one', () {
      expect(
          () => check('procedure(X) pass(Strem?, Strem).\npass(A?, A).'),
          throwsA(predicate((e) =>
              e.toString().contains('its type parameters are X'),
              'names the declared parameters')));
    });
  });

  group('a type name the parameter list names', () {
    dynamic check(String source) =>
        checkModule(Parser(Lexer(source).tokenize()).parseModule());

    test('is accepted bare, tying two arguments to one type', () {
      expect(check('procedure(X) ptie(X, X?).\nptie(A?, A).').isWellTyped, isTrue);
    });

    test('is accepted within a template instantiation', () {
      expect(
          check('procedure(X) pcopy(Stream(X)?, Stream(X)).\n'
                  'pcopy([], []).\n'
                  'pcopy([A|As], [A?|Bs?]) :- pcopy(As?, Bs).')
              .isWellTyped,
          isTrue);
    });

    test('a defined type needs no list', () {
      expect(
          check('Msg ::= text(String) ; stop.\n'
                  'procedure pmsgs(Stream(Msg)?, Stream(Msg)).\n'
                  'pmsgs([], []).\n'
                  'pmsgs([A|As], [A?|Bs?]) :- pmsgs(As?, Bs).')
              .isWellTyped,
          isTrue);
    });
  });
}
