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
