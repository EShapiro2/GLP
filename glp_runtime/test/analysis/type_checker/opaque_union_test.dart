// glp_runtime/test/analysis/type_checker/opaque_union_test.dart
//
// Opaque, the union of all opaque types (GLP, 2026-10-13 12:52 UTC, O1, Udi's
// ruling; TGLP 34ddb4b, GLP-Spec c30ba39).
//
// TGLP parameterized-types.tex, Definition "Opaque Type": "The built-in type
// Opaque is the union of all opaque types: a state with no outgoing
// transitions, distinct from the wildcard and from every opaque type,
// matched by itself and by every opaque type, and by nothing else."
//
// TGLP modules.tex, "Opaque types": "A host that holds the ends of several
// guests in one structure, as a stream of invitations does, types them by the
// union of all opaque types, Opaque, and can only pass them on.  The type
// checker takes a term of type Opaque back to one opaque type in one place:
// in a goal that run activates at an opaque instance at h, a variable of type
// Opaque may stand at a position typed by the opaque type labelled h."
//
// TGLP parameterized-types.tex, Lemma "Parametricity": "for every
// instantiation---by opaque types, where a term of a parameter's type may
// stand at a position of type Opaque---... the clauses of the instantiated
// procedure are well-typed with subtyping".
//
// Opaque inside a type (GLP, 2026-10-13 20:04 UTC, point 3, Udi's ruling;
// TGLP 5a0950f, GLP-Spec 851cd34): "T° is T with every opaque type replaced by
// Opaque, and the subtyping relation of Definition Subtyping is extended by
// T <: T° for every type T"; the step back, "in a goal that run activates at
// an opaque instance at h, a variable of type T° may stand at a position typed
// T, for a type T whose opaque types are labelled h".
//
// Fixtures: programs/tests/opaque_union/.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/subtyping.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/opaque_union/$name').absolute.path;

String _refusal(String name) {
  try {
    GlpEngine(rootSelfGlpPath: _root).loadFile(_fixture(name));
  } catch (e) {
    return e.toString();
  }
  return '';
}

void main() {
  group('Opaque in the subtyping relation', () {
    final env = TypeEnvironment({
      r'$abstract_X': TypeDef(r'$abstract_X', const [], 0, 0),
      r'$abstract_@H': TypeDef(r'$abstract_@H', const [], 0, 0),
      'Key': TypeDef('Key', [TypeRef('String', 0, 0)], 0, 0),
    }, {});
    final dfa = buildProgramDFA(env);
    DFAState s(String n) => dfa.getState(n);

    test('every opaque type is below Opaque, a parameter\'s and one labelled h',
        () {
      expect(isSubtype(s(r'$abstract_X'), s('Opaque'), dfa), isTrue);
      expect(isSubtype(s(r'$abstract_@H'), s('Opaque'), dfa), isTrue);
      expect(isSubtype(s('Opaque'), s('Opaque'), dfa), isTrue);
    });

    test('no other type is below Opaque', () {
      expect(isSubtype(s('Integer'), s('Opaque'), dfa), isFalse);
      expect(isSubtype(s('Key'), s('Opaque'), dfa), isFalse);
      expect(isSubtype(s('_'), s('Opaque'), dfa), isFalse);
    });

    test('Opaque is below itself alone: not the wildcard, not an opaque type',
        () {
      expect(isSubtype(s('Opaque'), s('_'), dfa), isFalse);
      expect(isSubtype(s('Opaque'), s(r'$abstract_X'), dfa), isFalse);
      expect(isSubtype(s('Opaque'), s(r'$abstract_@H'), dfa), isFalse);
      expect(isSubtype(s('Opaque'), s('Key'), dfa), isFalse);
    });

    test('Opaque is no constant type and licenses no repeated reader', () {
      expect(isConstantType(s('Opaque'), env.types), isFalse);
      expect(licensesRepeatedReader(s('Opaque?'), env.types), isFalse);
    });
  });

  group('T <: T°, Opaque at any position of a type (GLP, 2026-10-13 20:04 '
      'UTC, point 3; TGLP 5a0950f, Definition "Opaque Type")', () {
    TypeRef r(String n, {bool input = false}) =>
        TypeRef(n, 0, 0, isInput: input);
    // Hs ::= [] ; [h | Hs].   Os ::= [] ; [Opaque | Os].   Xs, at a
    // parameter's abstract type.   Is ::= [] ; [Integer | Is].
    // RC ::= rc(Hs, Hs?).   RO ::= rc(Os, Os?).   RX ::= rc(Xs, Xs?).
    // RI ::= rc(Is, Is?).   RM ::= rc(Hs, Os?), half closed.
    TypeDef list(String n, String elem) => TypeDef(
        n, [ListNilAlt(0, 0), ListConsAlt(r(elem), r(n), 0, 0)], 0, 0);
    TypeDef rc(String n, String out, String inp) => TypeDef(
        n, [StructAlt('rc', [r(out), r(inp, input: true)], 0, 0)], 0, 0);
    final env = TypeEnvironment({
      r'$abstract_X': TypeDef(r'$abstract_X', const [], 0, 0),
      r'$abstract_@H': TypeDef(r'$abstract_@H', const [], 0, 0),
      'Hs': list('Hs', r'$abstract_@H'),
      'Os': list('Os', 'Opaque'),
      'Xs': list('Xs', r'$abstract_X'),
      'Is': list('Is', 'Integer'),
      'RC': rc('RC', 'Hs', 'Hs'),
      'RO': rc('RO', 'Os', 'Os'),
      'RX': rc('RX', 'Xs', 'Xs'),
      'RI': rc('RI', 'Is', 'Is'),
      'RM': rc('RM', 'Hs', 'Os'),
    }, {});
    final dfa = buildProgramDFA(env);
    DFAState s(String n) => dfa.getState(n);

    test('RootChannel(h) <: RootChannel(Opaque), at both modes of the channel',
        () {
      expect(isSubtype(s('RC'), s('RO'), dfa), isTrue);
      expect(isSubtype(s('RX'), s('RO'), dfa), isTrue);
      expect(isSubtype(s('Hs'), s('Os'), dfa), isTrue);
    });

    test('Opaque <: h does not follow: RootChannel(Opaque) is not below '
        'RootChannel(h)', () {
      expect(isSubtype(s('RO'), s('RC'), dfa), isFalse);
      expect(isSubtype(s('Os'), s('Hs'), dfa), isFalse);
    });

    test('a type with no opaque type is its own T°, below no other', () {
      expect(isSubtype(s('RI'), s('RO'), dfa), isFalse);
      expect(isSubtype(s('Is'), s('Os'), dfa), isFalse);
    });

    test('T° replaces every opaque type: RootChannel(h) is not below a type '
        'that replaces some', () {
      expect(isSubtype(s('RC'), s('RM'), dfa), isFalse);
    });

    test('the step back: RootChannel(Opaque) with Opaque as h is below '
        'RootChannel(h), and nothing else is', () {
      expect(isSubtypeWithOpaqueAs(s('RO'), s('RC'), {s(r'$abstract_@H')}, dfa),
          isTrue);
      expect(isSubtypeWithOpaqueAs(s('RI'), s('RC'), {s(r'$abstract_@H')}, dfa),
          isFalse);
      expect(isSubtypeWithOpaqueAs(s('RO'), s('RX'), {s(r'$abstract_@H')}, dfa),
          isFalse);
    });

    test('a parameter\'s abstract type is below Opaque inside a type only '
        'where it is at a bare position', () {
      expect(withParametersNotBelowOpaque(
              () => isSubtype(s('RX'), s('RO'), dfa)),
          isFalse);
      expect(withParametersNotBelowOpaque(
              () => isSubtype(s('RC'), s('RO'), dfa)),
          isTrue);
    });
  });

  group('a clause holds only a variable at an Opaque position', () {
    test('a variable of type Opaque passed on at Opaque loads', () {
      expect(_refusal('okpass.glp'), '');
    });

    test('a host clause putting an Integer constant at Opaque is refused', () {
      expect(_refusal('badint.glp'),
          contains('Opaque, the union of all opaque types, holds only a variable'));
    });

    test('a host clause putting an Integer variable at Opaque is refused', () {
      expect(_refusal('badintvar.glp'),
          contains('Opaque vs Integer?'));
    });

    test('a term of type Opaque is not passed on at the wildcard', () {
      expect(_refusal('badwild.glp'), contains('_ vs Opaque?'));
    });
  });

  group('the one step back, in the goal run/3 activates', () {
    test('a host storing two guests\' ends at different types in one '
        'Stream(Opaque) certifies, the activating goals holding Opaque '
        'variables at the template\'s parameter', () {
      expect(_refusal('activate.glp'), '');
    });

    test('in the activating goal, an Opaque variable at a position the '
        'parameter does not type is refused', () {
      final r = _refusal('activate_key.glp');
      expect(r, contains('the head occurrence receives Opaque, which is not '
          'within what the body occurrence accepts (Key)'));
      expect(r, contains('in the goal run/3 activates'));
      expect(r, isNot(contains('(F, F?)')));
    });

    test('the same variable in any other goal, at a position an opaque type '
        'types, is refused', () {
      expect(_refusal('elsewhere.glp'),
          contains('the body occurrence produces Opaque, which is not within '
              r'what the head occurrence hands out ($abstract_X)'));
    });
  });

  group('Opaque inside a type, the whole end (GLP, 2026-10-13 20:04 UTC, '
      'point 3)', () {
    test('a host storing a RootChannel(h) end in a '
        'Stream(RootChannel(Opaque)) certifies, its activating goal holding a '
        'RootChannel(Opaque) end at RootChannel(X)?', () {
      expect(_refusal('rootchannel.glp'), '');
    });

    test('a clause putting a term with a constant at an Opaque position '
        'inside RootChannel(Opaque) is refused', () {
      expect(_refusal('rootconst.glp'),
          contains('Opaque, the union of all opaque types, holds only a variable'));
    });

    test('a RootChannel(Opaque) variable at a position typed by an opaque '
        'type inside RootChannel, outside the goal run/3 activates, is refused',
        () {
      expect(_refusal('rootelsewhere.glp'),
          contains('the body occurrence produces RootChannel<Opaque>, which is '
              r'not within what the head occurrence hands out (RootChannel<$abstract_X>)'));
    });

    test('in the activating goal, a RootChannel(Opaque) end at a position '
        'typed T whose T° it is not is refused', () {
      final r = _refusal('rootwrongpos.glp');
      expect(r, contains('the head occurrence receives RootChannel<Opaque>, '
          r'which is not within what the body occurrence accepts (Stream<$abstract_@H>)'));
      expect(r, contains('in the goal run/3 activates'));
    });

    test('a host putting its own RootChannel(A) end at RootChannel(Opaque) '
        'through a procedure certified there is refused: certification at S° '
        'against S, S of opaque types', () {
      expect(_refusal('rootstore.glp'),
          contains('the head occurrence receives RootChannel<A>, which is not '
              'within what the body occurrence accepts (RootChannel<Opaque>)'));
    });
  });

  group('certification at opaque instantiations only, where a parameter\'s '
      'term stands at Opaque', () {
    test('a host storing its own terms of two concrete types at Opaque '
        'through a certified procedure is refused', () {
      final r = _refusal('store.glp');
      expect(r, contains('store(X?, [], S1) conflict'));
      expect(r, contains('store(Y?, S1?, S) conflict'));
    });

    test('nor does a certified procedure calling it carry the concrete type '
        'there', () {
      expect(_refusal('wrap.glp'),
          contains('the head occurrence receives A, which is not within what '
              'the body occurrence accepts (Opaque)'));
    });
  });

  group('the type identity of a declaration naming Opaque', () {
    String id(String source, String key) => typeIdentityTablesForModule(
            Parser(Lexer(source).tokenize()).parseInterface())
        .identityOf(key)!;

    test('Opaque has an identity distinct from the wildcard\'s', () {
      expect(id('procedure p(Opaque?).', 'p/1'),
          isNot(id('procedure p(_?).', 'p/1')));
    });

    test('the mode of an Opaque position is its transition\'s, and the name is abstracted', () {
      expect(id('procedure p(Opaque?).', 'p/1'),
          isNot(id('procedure p(Opaque).', 'p/1')));
      expect(id('procedure p(Opaque?, Opaque).', 'p/2'),
          id('procedure q(Opaque?, Opaque).', 'q/2'));
    });

    test('Opaque prints as its own label, no type identity\'s', () {
      expect(opaqueUnionLabel, 'Opaque');
      expect(RegExp(r'^[0-9a-f]+$').hasMatch(opaqueUnionLabel), isFalse);
    });
  });
}
