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
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart'
    show setRootScopeUnitClauseSource;
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  final rootSelfGlp = File('../programs/self.glp');
  final rootSource = rootSelfGlp.readAsStringSync();
  setRootScopeUnitClauseSource(rootSource);
  setRootScopeEnvironmentSource(rootSource);
  final rootSelfPath = rootSelfGlp.absolute.path;
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
      final env = buildTypeEnvironment(module);
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
      final env = buildTypeEnvironment(module);
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
      final env = buildTypeEnvironment(module);
      expect(env.types, isNot(contains('Agent')));
      expect(env.procedures['p/1']!.argTypes.single,
          isA<TypeRef>().having((t) => t.name, 'name', 'Constant'));
    });

    test('a definition inheriting a primitive is below it and it below the '
        'definition; a type with alternatives of its own is below no primitive',
        () {
      final module = Parser(Lexer('Tag ::= String.\n'
              'Ack ::= ok ; error.\n'
              'procedure p(Tag?, Ack?).\n'
              'p(_, _).\n')
          .tokenize())
          .parseModule();
      final dfa = buildProgramDFA(buildTypeEnvironment(module));
      expect(isSubtype(dfa.getState('Tag'), dfa.getState('String'), dfa),
          isTrue);
      expect(isSubtype(dfa.getState('String'), dfa.getState('Tag'), dfa),
          isTrue);
      expect(sameBaseType('Tag', 'String', dfa), isTrue);
      expect(isSubtype(dfa.getState('Tag'), dfa.getState('Integer'), dfa),
          isFalse);
      expect(isSubtype(dfa.getState('Constant'), dfa.getState('String'), dfa),
          isFalse);
      expect(isSubtype(dfa.getState('Ack'), dfa.getState('String'), dfa),
          isFalse);
    });

    test("the root self.glp's Key, SignedTerm and Hash are in the root scope",
        () {
      final env = buildRootScopeEnvironment();
      expect(env.types.keys, containsAll(['Key', 'SignedTerm', 'Hash']));
    });

    test('a module naming a scope-level Key in its own type definition links',
        () {
      final modules =
          discoverProgram(fixtureDir, rootSelfGlpPath: rootSelfPath);
      final linked = checkedLinkedProgram(modules, rootDir: fixtureDir);
      expect(linked.program.procedures.map((p) => p.name),
          contains('code:keys'));
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
