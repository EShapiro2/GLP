// glp_runtime/test/analysis/mutual_ref_test.dart
//
// `MutualRef` is a primitive type.
// Spec: TGLP (Moded-Types) 99332fc, sections/typed-glp.tex sec:type-declarations
// --- "The primitive types are Integer, Real, String, Module and MutualRef, the
// last an opaque handle on the tail of a stream, through which mwm/2 appends in
// constant time ... MutualRef is not among them [Number, Constant, Exp]: a
// mutual reference holds the writer of a stream tail, so it is neither ground
// nor a constant type" --- and sections/well-typing.tex, row 9 of the
// consistency table: "mutual reference term | MutualRef".
// GLP-Spec 556efc9, sections/appendix-guards.tex, carries the guard row, the
// three MWM kernels and the three wrappers as the catalogue types them.
//
// Until 2026-09-23 the whole family was declared with wildcards, and
// `'_allocate_mutual_reference'(_, _)` was the one root `_`-declaration that
// condition 3(b) reached: its produced `_` met the `Stream(X)` a head handed
// out, and `_` is top and below nothing, so `mwm/2` and the seven p99
// directories under it were refused.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
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

  group('MutualRef is a primitive type', () {
    test('it is a primitive leaf, outside Constant and no constant type', () {
      final env = buildRootScopeEnvironment();
      final dfa = buildProgramDFA(env);

      // A state of its own, in both modes, with no outgoing transition ---
      // a leaf, exactly as `Module` is.
      expect(dfa.getState('MutualRef').isPrimitiveType, isTrue);
      expect(dfa.getState('MutualRef?').isPrimitiveType, isTrue);
      expect(dfa.getAutomaton('MutualRef').transitions, isEmpty);
      expect(TypeRef.builtins, contains('MutualRef'));

      // Outside `Constant ::= Number ; String ; Module.`
      expect(dfa.getAutomaton('Constant').acceptedPrimitives,
          isNot(contains('MutualRef')));

      // And it FAILS isConstantType, where `Module` passes: "a mutual reference
      // holds the writer of a stream tail, so it is neither ground nor a
      // constant type".
      expect(isConstantType(dfa.getState('MutualRef'), env.types), isFalse);
      expect(isConstantType(dfa.getState('MutualRef?'), env.types), isFalse);
      expect(isConstantType(dfa.getState('Module'), env.types), isTrue);
      expect(isConstantType(dfa.getState('Constant'), env.types), isTrue);
    });

    test("p99/self.glp's mwm/2 type-checks, and the seven p99 directories with it",
        () {
      for (final name in const [
        'arithmetic',
        'lists',
        'btrees',
        'graphs',
        'misc',
        'codes',
        'mtrees',
      ]) {
        final dir = Directory('../programs/p99/$name').absolute.path;
        final modules = discoverProgram(dir, rootSelfGlpPath: rootSelfPath);
        expect(() => checkedLinkedProgram(modules, rootDir: dir), returnsNormally,
            reason: 'p99/$name');
      }
    });

    test('the two single-file mwm programs load', () {
      for (final path in const [
        '../programs/book/streams/producers_consumers/mwm.glp',
        '../programs/tests/test_p99_probe.glp',
      ]) {
        final engine = GlpEngine(rootSelfGlpPath: rootSelfPath);
        expect(engine.loadFile(File(path).absolute.path), isTrue, reason: path);
      }
    });

    test('a term that is not a mutual reference is refused at a MutualRef position',
        () {
      // Row 9 admits a mutual reference term and nothing else; a mutual
      // reference is produced by the runtime and never written as a literal, so
      // every literal at the position is refused.
      final result = checkSource('''
procedure hold(MutualRef?).
hold(3).
''');
      expect(result.errors, isNotEmpty);
      expect(
          result.errors
              .any((e) => e.message.contains('requires a mutual reference term')),
          isTrue,
          reason: result.errors.map((e) => e.message).join('\n'));
    });

    test('is_mutual_ref narrows a union by the meet, as module/1 does', () {
      // `Held ::= String ; MutualRef.` is the `Content ::= String ; Module.`
      // case: the guard tests the alternative its clause is for, and it is the
      // MEET --- `MutualRef?` --- that condition 3(b) compares against the body.
      // Asking `Held <: MutualRef` instead refuses the clause.
      final result = checkSource('''
Held ::= String ; MutualRef.

procedure take_ref(MutualRef?, MutualRef).
take_ref(R, R?).

procedure use_held(Held?, MutualRef).
use_held(H, R?) :- is_mutual_ref(H?) | take_ref(H?, R).
''');
      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });
  });
}
