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

    test('the same fixture loaded as a directory program links cleanly', () {
      expect(engine.loadProgram(fixtureDir), isTrue);
    });
  });
}
