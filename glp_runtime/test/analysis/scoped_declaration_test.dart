// glp_runtime/test/analysis/scoped_declaration_test.dart
//
// A declaration's types are the declaring scope's.
//
// TGLP "Compilation", third step: every type T of every .glp file, self.glp
// files included, is renamed to M:T --- "two sibling modules have distinct
// scopes, so two same-named types defined in them are two types, which one
// flat namespace would otherwise make one, checking a module against a
// definition it cannot see" --- and fourth step: every type reference resolves
// "to the renamed type of the nearest scope defining it".  The root self.glp's
// `procedure authorise_link(GlobalName?, Answer?)` therefore means the root's
// Answer in every module, whatever Answer a module or its directory defines.
//
// Regression guard: the per-module check (step 2) merged the scope chain into
// one flat name map, so a descendant's Answer replaced the one the root's
// declaration named, and programs/social/graph/core/agent.glp:925's
// `authorise_link(L, authorise)` was rejected against social/graph/self.glp's
// Answer (IGLP, 2026-09-18).  The fix is TypeEnvironment.merge: a shadowed
// definition survives under `<origin>:T` and its scope's references to it are
// rewritten, in both directions of shadowing.
//
// Fixtures: programs/tests/scoped_declaration/ (loads and runs) and
// programs/tests/scoped_declaration_reject/ (rejected).

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart'
    show setRootScopeUnitClauseSource;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart';

void main() {
  final rootSelfGlp = File('../programs/self.glp');
  final rootSource = rootSelfGlp.readAsStringSync();
  setRootScopeUnitClauseSource(rootSource);
  setRootScopeEnvironmentSource(rootSource);
  final rootSelfPath = rootSelfGlp.absolute.path;

  TypeEnvironment layer(String source) => buildScopeFromModule(
      Parser(Lexer(source).tokenize()).parseModule());

  group('TypeEnvironment.merge keeps a shadowed definition and its references',
      () {
    final outer = TypeEnvironment.empty().merge(
        layer('Answer ::= authorise ; refuse.\n'
            'Report ::= report(Answer).\n'
            'procedure decide(Answer?, Report).\n'
            'decide(_, _).\n'),
        label: 'root');
    final inner = layer('Answer ::= yes ; no.\n'
        'procedure judge(Answer?).\n'
        'judge(_).\n');
    final merged = outer.merge(inner, label: 'inner');

    test('the nearer definition takes the bare name', () {
      final alts = merged.types['Answer']!.alternatives;
      expect(alts.map((a) => a.toString()), ['yes', 'no']);
      expect(merged.typeOrigins['Answer'], 'inner');
    });

    test('the shadowed definition survives under its origin', () {
      final alts = merged.types['root:Answer']!.alternatives;
      expect(alts.map((a) => a.toString()), ['authorise', 'refuse']);
      expect(merged.typeOrigins['root:Answer'], 'root');
    });

    test("the outer declaration's types are the outer scope's", () {
      final decide = merged.procedures['decide/2']!;
      expect((decide.argTypes[0] as TypeRef).name, 'root:Answer');
      expect((decide.argTypes[0] as TypeRef).isInput, isTrue);
    });

    test("the outer type definitions' references are the outer scope's", () {
      final report = merged.types['Report']!.alternatives.single as StructAlt;
      expect((report.args.single as TypeRef).name, 'root:Answer');
    });

    test("the inner declaration's types are the inner scope's", () {
      final judge = merged.procedures['judge/1']!;
      expect((judge.argTypes.single as TypeRef).name, 'Answer');
    });

    test('an equal definition is one type and is not kept apart', () {
      final same = outer.merge(
          layer('Answer ::= authorise ; refuse.\n'
              'procedure judge(Answer?).\n'
              'judge(_).\n'),
          label: 'inner');
      expect(same.types.keys, isNot(contains('root:Answer')));
      expect((same.procedures['decide/2']!.argTypes[0] as TypeRef).name,
          'Answer');
    });

    test('a definition shadowed twice is kept apart from both', () {
      final third = merged.merge(
          layer('Answer ::= maybe.\n'
              'procedure weigh(Answer?).\n'
              'weigh(_).\n'),
          label: 'third');
      expect(third.types['root:Answer']!.alternatives.map((a) => '$a'),
          ['authorise', 'refuse']);
      expect(third.types['inner:Answer']!.alternatives.map((a) => '$a'),
          ['yes', 'no']);
      expect(third.types['Answer']!.alternatives.map((a) => '$a'), ['maybe']);
      expect((third.procedures['decide/2']!.argTypes[0] as TypeRef).name,
          'root:Answer');
      expect((third.procedures['judge/1']!.argTypes[0] as TypeRef).name,
          'inner:Answer');
      expect((third.procedures['weigh/1']!.argTypes[0] as TypeRef).name,
          'Answer');
    });
  });

  group('the merge under a scope (types filling gaps) is the same rule', () {
    test('the module\'s declaration means the module\'s own type', () {
      final scope = TypeEnvironment.empty().merge(
          layer('Answer ::= authorise ; refuse.\n'
              'procedure decide(Answer?).\n'
              'decide(_).\n'),
          label: 'root');
      final module = Parser(Lexer('Answer ::= yes ; no.\n'
              'procedure judge(Answer?).\n'
              'judge(_).\n')
          .tokenize())
          .parseModule();
      final env = mergeModuleIntoScope(scope, module,
          typesFillGapsOnly: true, label: 'deep');
      expect(env.types['Answer']!.alternatives.map((a) => '$a'),
          ['authorise', 'refuse']);
      expect(env.types['deep:Answer']!.alternatives.map((a) => '$a'),
          ['yes', 'no']);
      expect((env.procedures['decide/1']!.argTypes.single as TypeRef).name,
          'Answer');
      expect((env.procedures['judge/1']!.argTypes.single as TypeRef).name,
          'deep:Answer');
    });
  });

  group('a directory program whose module shadows an ancestor\'s type', () {
    final okDir =
        Directory('../programs/tests/scoped_declaration').absolute.path;
    final rejectDir =
        Directory('../programs/tests/scoped_declaration_reject').absolute.path;

    test("the ancestor declarations keep their own types: it loads and runs",
        () async {
      final engine = GlpEngine(rootSelfGlpPath: rootSelfPath);
      expect(engine.loadProgram(okDir), isTrue);
      final result = await engine.runGoal('go(Ts)');
      expect(result.succeeded, isTrue, reason: result.error ?? '');
    });

    test('the module\'s own constant is rejected by the ancestor declaration',
        () {
      final engine = GlpEngine(rootSelfGlpPath: rootSelfPath);
      String err = '';
      try {
        engine.loadProgram(rejectDir);
      } catch (e) {
        err = e.toString();
      }
      expect(err, contains('Type checking failed'));
      expect(err, contains('caller.glp'));
      expect(err, contains('matches the constant yes'));
      // The type the declaration means is the self.glp's, named by its scope.
      expect(err, contains('scoped_declaration_reject:Answer'));
    });

    test("the root's Key produced by self_key/1 is not the module's Key", () {
      final engine = GlpEngine(rootSelfGlpPath: rootSelfPath);
      String err = '';
      try {
        engine.loadProgram(rejectDir);
      } catch (e) {
        err = e.toString();
      }
      expect(err, contains('keyed.glp'));
      expect(err, contains('root:Key'));
    });
  });
}
