// glp_runtime/test/compiler/co_load_reject_test.dart
//
// A loose source that cross-calls is not a program (TGLP modules.tex,
// def:program): a program is a self-contained module or a directory with a
// self.glp. Co-loading a hierarchy by successive `loadSource` calls is
// therefore rejected at load, naming the cause and the directory-program
// remedy.
//
// Regression guard: before this check the second load fell through to the
// direct compile path left from the retired dynamic-dispatch mechanism, which
// emits the retired Distribute instruction. The load appeared to succeed and
// the program died later and unreadably with
// "WireFormatException: instruction not in the wire ISA: Distribute".
//
// Fixture: programs/tests/co_load_neg/{self.glp, wallet.glp, top.glp}. It lives
// under programs/ so the self.glp ancestor chain resolves Coin — the chain is
// anchored at the hierarchy root, so a fixture outside programs/ has no
// ancestor scope and Coin would read as a free type parameter.
//
// The fixture carries a second rejection: its self.glp declares a type and
// exports no procedure, so the directory has no entry points and is not a
// program either (modules.tex §Static Linking, "Entry and the absence of a boot
// module"). Both rejections are asserted below.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;
  final fixtureDir = Directory('../programs/tests/co_load_neg').absolute.path;

  late GlpEngine engine;

  setUp(() => engine = GlpEngine(rootSelfGlpPath: rootSelf));

  String path(String name) => '$fixtureDir${Platform.pathSeparator}$name';

  Matcher throwsContaining(String needle) => throwsA(predicate(
      (e) => e.toString().contains(needle), 'message contains "$needle"'));

  group('Co-loaded source that cross-calls', () {
    test('wallet.glp alone is self-contained and loads', () {
      expect(engine.loadFile(path('wallet.glp')), isTrue);
    });

    test('top.glp is rejected at load, not at run time', () {
      engine.loadFile(path('wallet.glp'));
      expect(() => engine.loadFile(path('top.glp')),
          throwsContaining('is not a program'));
    });

    test('the rejection names def:program and the directory remedy', () {
      expect(() => engine.loadFile(path('top.glp')),
          throwsContaining('def:program'));
      expect(() => engine.loadFile(path('top.glp')),
          throwsContaining('directory program'));
    });

    test('the rejection names the imported declaration as the cause', () {
      expect(() => engine.loadFile(path('top.glp')),
          throwsContaining('imported procedure'));
    });

    test('no Distribute instruction reaches the wire format', () {
      // The old path compiled and failed only when the program was shipped.
      expect(() => engine.loadFile(path('top.glp')),
          throwsA(predicate((e) => !e.toString().contains('Distribute'),
              'not a WireFormatException about Distribute')));
    });

    // Reclassified 2026-08-02: this directory used to be the positive control
    // ("links cleanly"). Its self.glp exports no procedure, so the program has
    // no entry points and the loader now rejects it (modules.tex §Static
    // Linking, "Entry and the absence of a boot module"). The fixture keeps its
    // first purpose — top.glp is not a program on its own — and gains this one.
    // The positive control for a directory program that links cleanly is
    // programs/tests/module_self_procs, used in per_module_check_test.dart.
    test('the same fixture as a directory program is rejected: no entry points',
        () {
      expect(() => engine.loadProgram(fixtureDir),
          throwsContaining('has no entry points'));
    });

    test('the rejection names the directory and the root self.glp', () {
      expect(() => engine.loadProgram(fixtureDir),
          throwsContaining(fixtureDir));
      expect(() => engine.loadProgram(fixtureDir),
          throwsContaining('self.glp exports no procedure'));
    });
  });

  // The per-isolate loaders (multiagent/agent_runtime.dart,
  // multiagent/isolate_manager.dart) hand boot sources to loadSource under a
  // synthetic name, so the source never exists on disk. The rejection covers
  // that arrival too — otherwise a `#` call in a boot source still reaches the
  // direct compile path and dies at run time on Distribute.
  group('Co-loaded source text that cross-calls', () {
    final crossCalling = File(path('top.glp')).readAsStringSync();
    // Source text has no place in the hierarchy, so its types must be its own
    // or the root scope's — wallet.glp's Coin comes from the fixture's self.glp
    // and would read as a free type parameter here.
    const selfContained = 'procedure twice(Integer?, Integer).\n'
        'twice(X, Y?) :- Y := X? * 2.\n';

    test('is rejected under a synthetic name, as a real file is', () {
      expect(() => engine.loadSource(crossCalling, filename: 'program'),
          throwsContaining('is not a program'));
    });

    test('the rejection names the cause and the directory remedy', () {
      expect(() => engine.loadSource(crossCalling, filename: 'source_0'),
          throwsContaining('imported procedure'));
      expect(() => engine.loadSource(crossCalling, filename: 'source_0'),
          throwsContaining('directory program'));
    });

    test('self-contained source text still loads', () {
      expect(engine.loadSource(selfContained, filename: 'program'), isTrue);
    });
  });
}
