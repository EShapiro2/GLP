// glp_runtime/test/analysis/type_checker/term_type_test.dart
//
// Term, the built-in type of every produced term (Udi, 2026-11-11, to GLP #15;
// TGLP d7ecf4c).
//
// TGLP typed-glp.tex, "Type Declarations": "The built-in type Term is the
// type of every produced term: it is defined as by a rule Term ::= Integer ;
// Real ; String ; Module ; f(Term, ..., Term), with an alternative for every
// functor f of every arity n >= 1.  Its dual Term? is the type of every
// consumed term.  ...  Below a wildcard a term is not examined; below Term
// every position has the mode of Term, and the term is checked in full."
//
// TGLP well-typing.tex, after Definition "Well-Typed GLP Program": "A
// procedure declared with Term? at a position has an input path for every
// functor there, so in a well-typed program one of its clauses has a variable
// at that position or above it"; and in "Subtyping": "A type whose paths are
// produced throughout and end at a constant, at Integer, Real, String or
// Module, or at Term is a subtype of Term".
//
// TGLP appendix-type-automaton.tex, "The type Term": "The state Term has the
// transitions of Integer, Real, String and Module, and delta(Term, (f, n, i,
// up)) = Term for every functor f, arity n >= 1 and position i.  Term? has
// the same transitions with mode down, to Term?."
//
// Fixtures: programs/tests/typechecker/positive/term_type.glp and
// programs/tests/typechecker/negative/term/.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/meet.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/subtyping.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/typechecker/$name').absolute.path;

String _refusal(String name) {
  try {
    GlpEngine(rootSelfGlpPath: _root).loadFile(_fixture(name));
  } catch (e) {
    return e.toString();
  }
  return '';
}

TypeRef _ref(String name, {bool input = false}) =>
    TypeRef(name, 0, 0, isInput: input);

void main() {
  // IntStream ::= [] ; [Integer | IntStream].     produced throughout
  // Pair      ::= pair(String, Term).             over Term
  // Head      ::= Term.                           Term by a bare alternative
  // Ch        ::= ch(IntStream?, IntStream).      a mode inversion
  // Any       ::= any(_).                         a wildcard below
  // Ref       ::= ref(MutualRef).                 a mutual reference below
  // Colour    ::= red ; green.                    constants
  // Item      ::= Integer ; item(Ch) ; leaf(Colour).
  final env = TypeEnvironment({
    'IntStream': TypeDef('IntStream', [
      ListNilAlt(0, 0),
      ListConsAlt(_ref('Integer'), _ref('IntStream'), 0, 0),
    ], 0, 0),
    'Pair': TypeDef('Pair', [
      StructAlt('pair', [_ref('String'), _ref('Term')], 0, 0),
    ], 0, 0),
    'Head': TypeDef('Head', [_ref('Term')], 0, 0),
    'Ch': TypeDef('Ch', [
      StructAlt('ch', [_ref('IntStream', input: true), _ref('IntStream')], 0, 0),
    ], 0, 0),
    'Any': TypeDef('Any', [
      StructAlt('any', [PrimitiveModeAlt(false, 0, 0)], 0, 0),
    ], 0, 0),
    'Ref': TypeDef('Ref', [
      StructAlt('ref', [_ref('MutualRef')], 0, 0),
    ], 0, 0),
    'Colour': TypeDef('Colour', [
      ConstantAlt('red', 0, 0),
      ConstantAlt('green', 0, 0),
    ], 0, 0),
    'Item': TypeDef('Item', [
      _ref('Integer'),
      StructAlt('item', [_ref('Ch')], 0, 0),
      StructAlt('leaf', [_ref('Colour')], 0, 0),
    ], 0, 0),
    r'$abstract_X': TypeDef(r'$abstract_X', const [], 0, 0),
  }, {});
  final dfa = buildProgramDFA(env);
  DFAState s(String n) => dfa.getState(n);

  group('the automaton of Term', () {
    test('a state pair of its own, no primitive and no constant type', () {
      expect(TypeRef.builtins, contains('Term'));
      expect(s('Term').isTermType, isTrue);
      expect(s('Term?').isTermType, isTrue);
      expect(s('Term').isPrimitiveType, isFalse);
      expect(isConstantType(s('Term'), env.types), isFalse);
      expect(licensesRepeatedReader(s('Term?'), env.types), isFalse);
    });

    test('the transitions of Integer, Real, String and Module', () {
      expect(dfa.getAutomaton('Term').acceptedPrimitives,
          {'Integer', 'Real', 'String', 'Module'});
      expect(dfa.getAutomaton('Term?').acceptedPrimitives,
          {'Integer', 'Real', 'String', 'Module'});
    });

    test('on every functor, arity and position, to itself at its own mode',
        () {
      final term = dfa.getAutomaton('Term');
      final dual = dfa.getAutomaton('Term?');
      for (final (f, n, i) in const [('f', 1, 1), ('g', 3, 2), ('[|]', 2, 2)]) {
        expect(
            term.transition(s('Term'),
                TransitionLabel.functor(f, n, i, mode: Mode.produce)),
            s('Term'));
        expect(
            term.transition(s('Term'),
                TransitionLabel.functor(f, n, i, mode: Mode.consume)),
            isNull);
        expect(
            dual.transition(s('Term?'),
                TransitionLabel.functor(f, n, i, mode: Mode.consume)),
            s('Term?'));
        expect(
            dual.transition(s('Term?'),
                TransitionLabel.functor(f, n, i, mode: Mode.produce)),
            isNull);
      }
    });

    test('a type that is Term by a bare alternative has its transitions', () {
      final head = dfa.getAutomaton('Head');
      expect(head.acceptedPrimitives, {'Integer', 'Real', 'String', 'Module'});
      expect(
          head.transition(
              s('Head'), TransitionLabel.functor('f', 2, 1, mode: Mode.produce)),
          s('Term'));
      expect(
          dfa.getAutomaton('Head?').transition(s('Head?'),
              TransitionLabel.functor('f', 2, 1, mode: Mode.consume)),
          s('Term?'));
    });
  });

  group('Term in the subtyping relation', () {
    test('a type produced throughout, ending at constants, is below Term', () {
      for (final n in const [
        'Integer', 'Real', 'String', 'Module', 'IntStream', 'Pair', 'Colour',
        'Head', 'Term',
      ]) {
        expect(isSubtype(s(n), s('Term'), dfa), isTrue, reason: n);
      }
    });

    test('a mode inversion, a wildcard, a mutual reference or an opaque type '
        'below a type keeps it from Term', () {
      for (final n in const [
        'Ch', 'Any', 'Ref', 'Item', 'MutualRef', '_', 'Opaque',
        r'$abstract_X',
      ]) {
        expect(isSubtype(s(n), s('Term'), dfa), isFalse, reason: n);
      }
    });

    test('Term is below itself, a type that is Term, and the wildcard', () {
      expect(isSubtype(s('Term'), s('Head'), dfa), isTrue);
      expect(isSubtype(s('Term'), s('_'), dfa), isTrue);
      for (final n in const [
        'Integer', 'String', 'IntStream', 'Pair', 'Colour', 'Opaque',
        r'$abstract_X',
      ]) {
        expect(isSubtype(s('Term'), s(n), dfa), isFalse, reason: n);
      }
    });
  });

  group('the meet of Term and a type', () {
    DFAState? meet(String a, String b) =>
        meetOfTypes(s(a), s(b), buildProgramDFA(env), env);

    test('a type below Term is its own meet with Term, at either mode', () {
      expect(meet('Term', 'String'), s('String'));
      expect(meet('Term?', 'String?'), s('String?'));
      expect(meet('IntStream?', 'Term?'), s('IntStream?'));
      expect(meet('Term', 'Head'), s('Term'));
      expect(meet('Term', '_'), s('Term'));
    });

    test('nothing of a mutual reference, an opaque type or the other mode',
        () {
      expect(meet('Term', 'MutualRef'), isNull);
      expect(meet('Term', 'Opaque'), isNull);
      expect(meet('Term', 'String?'), isNull);
      expect(meet('Term', 'Ch'), isNull);
    });

    test('the alternatives of a type that lie on Term\'s paths', () {
      // Item ::= Integer ; item(Ch) ; leaf(Colour): item(Ch) has a mode
      // inversion below it and is dropped.
      final m = meet('Term', 'Item')!;
      final local = buildProgramDFA(env);
      expect(isSubtype(m, local.getState('Term'), local), isTrue);
      expect(isSubtype(local.getState('Integer'), m, local), isTrue);
      expect(isSubtype(local.getState('Item'), m, local), isFalse);
    });
  });

  group('the type identity of Term', () {
    test('Term has an identity of its own, the one type at either mode', () {
      final ids = {
        for (final n in const ['Term', 'String', 'Integer', 'Pair'])
          typeIdentityOfTypeHex(_ref(n), env.types)
      };
      expect(ids.length, 4);
      // "T? names the same type, consumed" (type_identity.dart,
      // canonicalTypePrint).
      expect(typeIdentityOfTypeHex(_ref('Term', input: true), env.types),
          typeIdentityOfTypeHex(_ref('Term'), env.types));
    });

    test('a type that is Term by a bare alternative has Term\'s identity', () {
      expect(typeIdentityOfTypeHex(_ref('Head'), env.types),
          typeIdentityOfTypeHex(_ref('Term'), env.types));
    });

    test('the print writes Term\'s functor transitions as one reserved label',
        () {
      final print = canonicalTypePrint(_ref('Pair'), env.types);
      expect(print, contains('#term/n@i:up'));
    });
  });

  group('programs over Term', () {
    test('the positive fixture loads', () {
      expect(_refusal('positive/term_type.glp'), isEmpty);
    });

    test('each refusal, for its reason', () {
      expect(_refusal('negative/term/term_no_variable.glp'),
          contains('kind argument 1: uncovered alternative "every term'));
      expect(_refusal('negative/term/term_not_a_string.glp'),
          contains('Term is not a subtype of String'));
      expect(_refusal('negative/term/term_channel.glp'),
          contains('not within what the body occurrence accepts (Term)'));
      expect(_refusal('negative/term/term_writer_below.glp'),
          contains('writer requires ↑ (produce), got ↓ (consume)'));
      expect(_refusal('negative/term/term_union.glp'),
          contains('overlaps with the other alternatives of Item'));
      expect(_refusal('negative/term/term_wildcard.glp'),
          contains('not within what the body occurrence accepts (Term)'));
    });

    test('a type a program defines under the name Term is that definition',
        () {
      final own = TypeEnvironment({
        'Term': TypeDef('Term', [
          StructAlt('t', [_ref('Integer')], 0, 0),
        ], 0, 0),
        'Alias': TypeDef('Alias', [_ref('Term')], 0, 0),
      }, {});
      final d = buildProgramDFA(own);
      expect(d.getAutomaton('Term').anyFunctorTarget, isNull);
      expect(d.getAutomaton('Alias').anyFunctorTarget, isNull);
      expect(isSubtype(d.getState('String'), d.getState('Term'), d), isFalse);
    });
  });
}
