/// A constant is typed by its kind, never by its text.  GLP-Spec
/// appendix-lp.tex, Definition "Logic Programs Syntax": "a constant (numbers,
/// strings, or the empty list `[]`)"; TGLP well-typing.tex, the table of
/// Definition "Consistent Paths": an integer matches `Integer` or `Number`, a
/// real `Real` or `Number`, a string `String`, a constant c the alternative c;
/// TGLP appendix-root-self.tex: "The empty list is a String, hence a
/// Constant".  So `'0000'` is a string and not a number, `'[]'` a string and
/// not the empty list, and `[]` the empty list and not the string `'[]'`: a
/// path step carries the constant's value, and the leaf and a constant
/// alternative are classified by its kind (GLP #3 Cowork, 2026-10-09 18:38
/// UTC, "12:19", FAULT 2 (b) (ii); 2026-10-10 19:04 UTC, the 64-zeros key).
///
/// Until 2026-10-10 the checker read a constant's kind off its text: a path
/// step carried the text alone and `int.tryParse` made the string
/// `'0000...0'` the integer 0, refused where `Key ::= String.` admits it
/// ("No alternative of Key? matches the constant 0"); the empty list was held
/// as the string '[]', one with it; and a type's alternatives, its labels and
/// its identity were keyed by text, in which `0` and `'0'` were one.
library;

import 'dart:io';

import 'package:glp_runtime/analysis/type_checker/meet.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/subtyping.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart'
    show TypeCheckResult, checkSource;
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart' show rootScope;
import 'package:glp_runtime/runtime/terms.dart' show nil;
import 'package:test/test.dart';

const _zeros =
    '0000000000000000000000000000000000000000000000000000000000000000';

void main() {
  final scope = rootScope(File('../programs/self.glp').absolute.path);

  TypeCheckResult check(String source) =>
      checkSource(source, ancestorScope: scope);
  String errorsOf(TypeCheckResult r) =>
      r.errors.map((e) => e.message).join('\n');

  group('a path step carries the constant, of its kind', () {
    test("the string '0' and the integer 0, the string '[]' and []", () {
      ModedPath only(ModedTerm t) => paths(t).single;
      expect(only(ModedConstant(Mode.consume, '0')).leaf.value, '0');
      expect(only(ModedConstant(Mode.consume, 0)).leaf.value, 0);
      expect(only(ModedConstant(Mode.consume, '[]')).leaf.value, '[]');
      expect(only(ModedConstant.nil(Mode.consume)).leaf.value, same(nil));
      expect(ModedConstant(Mode.consume, '[]').isNil, isFalse);
      expect(ModedConstant.nil(Mode.consume).isNil, isTrue);
      expect(ModedConstant(Mode.consume, '0') == ModedConstant(Mode.consume, 0),
          isFalse);
      expect(ModedConstant(Mode.consume, 1) == ModedConstant(Mode.consume, 1.0),
          isFalse);
    });
  });

  group('the 64-zeros key (harness SK3, sign/3 under Key?)', () {
    test("'0...0' is a String where Key admits a String", () {
      final r = check('''
procedure k(Key?).
k('$_zeros').
''');
      expect(r.isWellTyped, isTrue, reason: errorsOf(r));
    });

    test('"0...0", the same string, is too', () {
      final r = check('''
procedure k(Key?).
k("$_zeros").
''');
      expect(r.isWellTyped, isTrue, reason: errorsOf(r));
    });

    test('and is passed where a goal calls sign/3 with it', () {
      final r = check('''
procedure s(SignedTerm).
s(S?) :- sign(hello, "$_zeros", S).
''');
      expect(r.isWellTyped, isTrue, reason: errorsOf(r));
    });

    test('0, a Number, is refused where Key admits a String alone', () {
      final r = check('''
procedure k(Key?).
k(0).
''');
      expect(r.isWellTyped, isFalse);
      expect(errorsOf(r), contains('No alternative of Key? matches the constant 0'));
    });

    test("'0' is refused where an Integer is read, 0 is not", () {
      final refused = check('''
procedure n(Integer?).
n('0').
''');
      expect(refused.isWellTyped, isFalse);
      expect(errorsOf(refused), contains('Integer type requires integer literal'));
      final r = check('''
procedure n(Integer?).
n(0).
''');
      expect(r.isWellTyped, isTrue, reason: errorsOf(r));
    });
  });

  group("'[]' against [], both ways", () {
    test("[] is an Ends, '[]' is not", () {
      final r = check('''
Ends ::= [] ; [Integer | Ends].
procedure e(Ends?).
e([]).
e([_ | _]).
''');
      expect(r.isWellTyped, isTrue, reason: errorsOf(r));
      final refused = check('''
Ends ::= [] ; [Integer | Ends].
procedure e(Ends?).
e('[]').
e([_ | _]).
''');
      expect(refused.isWellTyped, isFalse);
      expect(errorsOf(refused),
          contains("No alternative of Ends? matches the constant '[]'"));
    });

    test("'[]' is a Bracket, [] is not", () {
      final r = check('''
Bracket ::= '[]' ; other.
procedure b(Bracket?).
b('[]').
b(other).
''');
      expect(r.isWellTyped, isTrue, reason: errorsOf(r));
      final refused = check('''
Bracket ::= '[]' ; other.
procedure b(Bracket?).
b([]).
b(other).
''');
      expect(refused.isWellTyped, isFalse);
      expect(errorsOf(refused),
          contains('No alternative of Bracket? matches the constant []'));
    });

    test("both are Strings (the empty list is a String)", () {
      final r = check('''
procedure s(String?).
s([]).
s('[]').
''');
      expect(r.isWellTyped, isTrue, reason: errorsOf(r));
    });

    test("a head '[]' does not cover an input's [], nor [] its '[]'", () {
      final onlyString = check('''
Both ::= '[]' ; [].
procedure c(Both?).
c('[]').
''');
      expect(onlyString.isWellTyped, isFalse);
      expect(errorsOf(onlyString), contains('uncovered alternative "[]"'));
      final onlyNil = check('''
Both ::= '[]' ; [].
procedure c(Both?).
c([]).
''');
      expect(onlyNil.isWellTyped, isFalse);
      expect(errorsOf(onlyNil), contains('uncovered alternative "\'[]\'"'));
      final both = check('''
Both ::= '[]' ; [].
procedure c(Both?).
c([]).
c('[]').
''');
      expect(both.isWellTyped, isTrue, reason: errorsOf(both));
    });
  });

  group('a type keeps its constants apart by kind', () {
    final module = Parser(Lexer('''
Mixed ::= 0 ; '0'.
Zero ::= '0'.
Num ::= 0.
Ends ::= [] ; [Integer | Ends].
Bracket ::= '[]'.
procedure p(Mixed?, Zero?, Num?, Ends?, Bracket?).
p(_, _, _, _, _).
''').tokenize()).parseModule();
    final env = buildTypeEnvironment(module, ancestorScope: scope);
    final dfa = buildProgramDFA(env);
    bool sub(String a, String b) =>
        isSubtype(dfa.getState(a), dfa.getState(b), dfa);

    test("0 ; '0' is two alternatives, not a duplicate", () {
      final r = check('''
Mixed ::= 0 ; '0'.
procedure m(Mixed?).
m(0).
m('0').
''');
      expect(r.isWellTyped, isTrue, reason: errorsOf(r));
    });

    test('subtyping reads a constant alternative by its kind', () {
      expect(sub('Zero', 'String'), isTrue);
      expect(sub('Zero', 'Integer'), isFalse);
      expect(sub('Num', 'Integer'), isTrue);
      expect(sub('Num', 'String'), isFalse);
      expect(sub('Zero', 'Num'), isFalse);
      expect(sub('Num', 'Zero'), isFalse);
      expect(sub('Bracket', 'String'), isTrue);
      expect(sub('Bracket', 'Ends'), isFalse);
    });

    test('the meet reads a constant alternative by its kind', () {
      final withString =
          meetOfTypes(dfa.getState('Mixed'), dfa.getState('String'), dfa, env)!;
      expect(isSubtype(withString, dfa.getState('Zero'), dfa), isTrue);
      expect(isSubtype(dfa.getState('Zero'), withString, dfa), isTrue);
      final withInteger = meetOfTypes(
          dfa.getState('Mixed'), dfa.getState('Integer'), dfa, env)!;
      expect(isSubtype(withInteger, dfa.getState('Num'), dfa), isTrue);
      expect(isSubtype(dfa.getState('Num'), withInteger, dfa), isTrue);
    });

    test('a transition label tells the kinds apart', () {
      expect(TransitionLabel.constant(0) == TransitionLabel.constant('0'),
          isFalse);
      expect(TransitionLabel.constant(nil) == TransitionLabel.constant('[]'),
          isFalse);
      expect(TransitionLabel.constant(nil).symbol, '[]');
      expect(TransitionLabel.constant('[]').symbol, "'[]'");
      expect(primitiveOfConstant('0'), 'String');
      expect(primitiveOfConstant(0), 'Integer');
      expect(primitiveOfConstant(0.5), 'Real');
      expect(primitiveOfConstant(nil), 'String');
    });

    test('the type identity tells the kinds apart', () {
      String idOf(String types) => typeIdentityTablesForModule(
              Parser(Lexer('$types\nprocedure p(T?).\np(_).\n').tokenize())
                  .parseModule(),
              ancestorScope: scope)
          .identityOf('p/1')!;
      expect(idOf("T ::= 0 ; 1."), isNot(idOf("T ::= '0' ; '1'.")));
      expect(idOf("T ::= [] ; [Integer | T]."),
          isNot(idOf("T ::= '[]' ; [Integer | T].")));
      expect(idOf('T ::= 0 ; 1.'), idOf('T ::= 1 ; 0.'));
    });
  });
}
