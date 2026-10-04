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
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart' show rootScope;
import 'package:glp_runtime/runtime/terms.dart' as rt;

void main() {
  final rootSelfGlp = File('../programs/self.glp');
  final rootSource = rootSelfGlp.readAsStringSync();
  final rootSelfPath = rootSelfGlp.absolute.path;
  // The scope a module directly under the root is checked in, passed in.
  final scope = rootScope(rootSelfPath);

  group('MutualRef is a primitive type', () {
    test('it is a primitive leaf, outside Constant and no constant type', () {
      final env = scope;
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
''', ancestorScope: scope);
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
''', ancestorScope: scope);
      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });
  });

  // TGLP typed-glp.tex (350eb7d): "A reader of type MutualRef may also occur
  // more than once: the writer a mutual reference holds is the runtime's, no
  // program can reach it, and every write through it is a kernel's, so no
  // occurrence of the reader gives a program a second writer."  The licence is
  // the occurrence's TYPE, and is the reader's alone.  `is_mutual_ref/1` is
  // "Ground: no" in the catalogue (GLP-Spec appendix-guards.tex) and marks
  // nothing; until 2026-10-01 it marked its argument grounded, which licensed a
  // repeated writer too.
  group('a reader of MutualRef may occur more than once', () {
    String refusal(String source) {
      try {
        GlpEngine(rootSelfGlpPath: rootSelfPath).loadSource(source);
      } catch (e) {
        return e.toString();
      }
      return '';
    }

    test('a reader declared MutualRef? read twice in the body loads, no guard',
        () {
      expect(refusal('''
procedure take(MutualRef?).
take(_).
procedure twice(MutualRef?).
twice(R) :- take(R?), take(R?).
'''), isEmpty);
    });

    test('the same clause at `_?` is refused: the licence is the type', () {
      expect(refusal('''
procedure take(_?).
take(_).
procedure twice(_?).
twice(R) :- take(R?), take(R?).
'''), contains('Reader variable "R?" occurs 2 times'));
    });

    test('is_mutual_ref narrows `_?` to MutualRef?, and the type licenses it',
        () {
      // A guard atom that tests a head occurrence narrows it to the meet, and
      // the occurrence has that type in the body (TGLP typed-glp.tex, "Type
      // checking of guards"); the meet of `_?` and `MutualRef?` is `MutualRef?`.
      // This is the shape of p99/self.glp's mm_start/2 and mm_subs/4.  The
      // source names no kernel and declares no -mode(system): a source with
      // no file behind it may not (GLP's round six, item 4), and it did until
      // 2026-10-04 for nothing it called.
      expect(refusal('''
procedure take(_?).
take(_).
procedure twice(_?).
twice(R) :- is_mutual_ref(R?) | take(R?), take(R?).
'''), isEmpty);
    });

    test('no guard licenses a repeated WRITER, is_mutual_ref nor ground', () {
      // "If the success of a guard implies that X? is bound to a ground term,
      // then X? may occur multiple times in the clause; X occurs once, as
      // ever" (GLP-Spec glp.tex, Remark "Guards and SRSW", bbff21d), and the
      // type licence is the reader's alone.  So a writer in the head and again
      // at a produced position of the body is refused under is_mutual_ref,
      // "Ground: no", and under ground/1, "Ground: yes", alike.  Until
      // 2026-10-02 the grounding mark licensed both X and X?, and the clause
      // under ground/1 loaded.  A writer twice in the HEAD is refused whatever
      // the guards (GLP-Spec glp.tex, Definition "GLP Program"; GLP #3 Cowork,
      // 2026-10-02 08:40 UTC, G).
      expect(refusal('''
procedure sink_w(_).
sink_w(X?) :- X = done.
procedure two(MutualRef?).
two(R) :- is_mutual_ref(R?) | sink_w(R).
'''), contains('Writer variable "R" occurs 2 times; a writer occurs once, whatever the guards'));
      expect(refusal('''
procedure sink_w(_).
sink_w(X?) :- X = done.
procedure two(_?).
two(R) :- ground(R?) | sink_w(R).
'''), contains('Writer variable "R" occurs 2 times; a writer occurs once, whatever the guards'));
      // Twice in the head, under either guard: refused.
      expect(refusal('''
procedure two(MutualRef?, MutualRef?).
two(R, R) :- is_mutual_ref(R?) | true.
'''), contains('Writer variable "R" occurs 2 times in the head'));
      expect(refusal('''
procedure two(_?, _?).
two(R, R) :- ground(R?) | true.
'''), contains('Writer variable "R" occurs 2 times in the head'));
    });

    test("the root's mwm/2 runs, its Ref declared MutualRef?", () async {
      final engine = GlpEngine(rootSelfGlpPath: rootSelfPath);
      for (final decl in const [
        'procedure(X) mwm_main(MwmInput(X)?, MutualRef?).',
        'procedure(X) mwm1(MwmInput(X)?, MutualRef?, Done?, Done).',
        'procedure(X) mwm_copy(Stream(X)?, MutualRef?, Done?, Done).',
        'procedure close_when_done(Done?, MutualRef?).',
      ]) {
        expect(rootSource, contains(decl));
      }
      final r = await engine.runGoal('mwm([merge([1,2,3]), merge([a,b])], Out)');
      expect(r.succeeded, isTrue, reason: '${r.error}');
      // Each input stream's order is kept and the two are interleaved.
      final heap = engine.runtime.heap;
      final out = <Object?>[];
      var cur = heap.dereference(r.bindings['Out']!);
      while (cur is rt.StructTerm && cur.functor == '.') {
        final x = heap.dereference(cur.args[0]);
        out.add(x is rt.ConstTerm ? x.value : x);
        cur = heap.dereference(cur.args[1]);
      }
      expect(out.whereType<int>(), [1, 2, 3]);
      expect(out.whereType<String>(), ['a', 'b']);
    });
  });
}
