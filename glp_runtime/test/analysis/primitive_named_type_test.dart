// glp_runtime/test/analysis/primitive_named_type_test.dart
//
// TGLP appendix "Type Aliases": a simple alias is "a single alternative that
// is a type reference: an alias for a defined type, or for the dual of one",
// and "the referenced types must be defined---not aliases themselves, and not
// primitives".  A definition naming a primitive --- `Key ::= String.` --- is
// therefore an ordinary type definition, not an alias, and is not erased.
//
// Regression guard: `_isSimpleAlias` (type_environment_builder.dart) counted a
// single type reference as an alias whatever it named, so the root self.glp's
// `Key ::= String.` was erased from every environment and a descendant
// module's `NetMsg ::= msg(Key, _)` failed with "Unresolved type: Key" in
// every linked program (IGLP, 2026-09-18).
//
// Fixture: programs/tests/primitive_named_type/ --- a self.glp defining
// `Key ::= String.` and a module naming Key in a type definition of its own.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/subtyping.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart'
    show checkSource;
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart' show rootScope;

void main() {
  final rootSelfGlp = File('../programs/self.glp');
  final rootSelfPath = rootSelfGlp.absolute.path;
  // The scope a module directly under the root is checked in, passed in.
  final scope = rootScope(rootSelfPath);
  final fixtureDir =
      Directory('../programs/tests/primitive_named_type').absolute.path;

  group('a type definition naming a primitive (TGLP "Type Aliases")', () {
    test('survives into the environment as a definition, not an alias', () {
      final module = Parser(Lexer('Tag ::= String.\n'
              'Msg ::= msg(Tag, _).\n'
              'procedure p(Msg?).\n'
              'p(_).\n')
          .tokenize())
          .parseModule();
      final env = buildTypeEnvironment(module, ancestorScope: scope);
      expect(env.types, contains('Tag'));
      final alt = env.types['Tag']!.alternatives.single;
      expect(alt, isA<TypeRef>().having((t) => t.name, 'name', 'String'));
      // Msg still names Tag: nothing was replaced by String.
      final msgAlt = env.types['Msg']!.alternatives.single as StructAlt;
      expect(msgAlt.args.first,
          isA<TypeRef>().having((t) => t.name, 'name', 'Tag'));
    });

    test('the dual of a primitive is a definition too', () {
      final module = Parser(Lexer('In ::= String?.\n'
              'procedure p(In).\n'
              'p(_).\n')
          .tokenize())
          .parseModule();
      final env = buildTypeEnvironment(module, ancestorScope: scope);
      expect(env.types, contains('In'));
      expect(env.procedures['p/1']!.argTypes.single,
          isA<TypeRef>().having((t) => t.name, 'name', 'In'));
    });

    test('an alias for a defined type is still an alias, and is erased', () {
      final module = Parser(Lexer('Agent ::= Constant.\n'
              'procedure p(Agent?).\n'
              'p(_).\n')
          .tokenize())
          .parseModule();
      final env = buildTypeEnvironment(module, ancestorScope: scope);
      expect(env.types, isNot(contains('Agent')));
      expect(env.procedures['p/1']!.argTypes.single,
          isA<TypeRef>().having((t) => t.name, 'name', 'Constant'));
    });

    test('a definition inheriting a primitive is below it and it below the '
        'definition; a type is below a primitive where its automaton is, '
        'whatever the supertype is named', () {
      final module = Parser(Lexer('Tag ::= String.\n'
              'Ack ::= ok ; error.\n'
              'Mixed ::= ok ; String.\n'
              'Wrapped ::= w(String).\n'
              'OkOrInt ::= ok ; Integer.\n'
              'procedure p(Tag?, Ack?, Mixed?, Wrapped?, OkOrInt?).\n'
              'p(_, _, _, _, _).\n')
          .tokenize())
          .parseModule();
      final dfa = buildProgramDFA(buildTypeEnvironment(module, ancestorScope: scope));
      bool sub(String a, String b) =>
          isSubtype(dfa.getState(a), dfa.getState(b), dfa);
      expect(sub('Tag', 'String'), isTrue);
      expect(sub('String', 'Tag'), isTrue);
      expect(sameBaseType('Tag', 'String', dfa), isTrue);
      expect(sub('Tag', 'Integer'), isFalse);
      expect(sub('Constant', 'String'), isFalse);
      // TGLP well-typing.tex, Definition "Prefix Acceptance": "a constant
      // matches String".  Tag and String have one automaton, so a type below
      // the one is below the other: until 2026-10-02 Ack was below Tag and
      // Key and not below String.
      expect(sub('Ack', 'Tag'), isTrue);
      expect(sub('Ack', 'Key'), isTrue);
      expect(sub('Ack', 'String'), isTrue);
      expect(sub('Mixed', 'String'), isTrue);
      expect(sub('String', 'Ack'), isFalse);
      expect(sub('String', 'Mixed'), isTrue);
      expect(sub('Ack', 'Integer'), isFalse);
      expect(sub('Wrapped', 'String'), isFalse);
      expect(sub('OkOrInt', 'String'), isFalse);
      expect(sub('OkOrInt', 'Integer'), isFalse);
    });

    test('a writer of a constant type is read where a String is', () {
      final result = checkSource('''
Ack ::= ok ; error.

procedure pass(String?, String).
pass(X, X?).

procedure q(Ack?, String).
q(A, B?) :- pass(A?, B).
''', ancestorScope: scope);
      expect(result.isWellTyped, isTrue,
          reason: result.errors.map((e) => e.message).join('\n'));
      final refused = checkSource('''
Ack ::= ok ; error.

procedure pass(Ack?, Ack).
pass(X, X?).

procedure q(String?, Ack).
q(A, B?) :- pass(A?, B).
''', ancestorScope: scope);
      expect(refused.isWellTyped, isFalse,
          reason: 'a String is not an Ack');
    });

    test("the root self.glp's Key, SignedTerm and Hash are in the root scope",
        () {
      expect(scope.types.keys, containsAll(['Key', 'SignedTerm', 'Hash']));
    });

    test('a module naming a scope-level Key in its own type definition links',
        () {
      final modules =
          discoverProgram(fixtureDir, rootSelfGlpPath: rootSelfPath);
      final linked = checkedLinkedProgram(modules, rootDir: fixtureDir);
      // Renamed by its module's path from the root (TGLP modules.tex,
      // Compilation, third step).
      expect(linked.program.procedures.map((p) => p.name),
          contains('tests/primitive_named_type/code:keys'));
    });

    test('and the program loads and runs', () async {
      final engine = GlpEngine(rootSelfGlpPath: rootSelfPath);
      expect(engine.loadProgram(fixtureDir), isTrue);
      final result = await engine.runGoal('go(Ks)');
      expect(result.succeeded, isTrue, reason: result.error ?? '');
      expect(result.bindings, contains('Ks'));
    });
  });
}
