/// Tests for '_output'/1 kernel and send_to_user/1 GLP predicate.
///
/// A module that calls the '_output' kernel declares -mode(system), and
/// -mode(system) is admitted only for the root self.glp and modules under
/// programs/system/ (TGLP appendix-root-self.tex, app:system-mode); a source
/// with no file behind it is neither (GLP's round six, item 4).  So the kernel's
/// tests load their module from a file under programs/system/, written there
/// for the test's length as primitive_layer_test does, and the send_to_user
/// tests are application modules calling the root self.glp's send_to_user/1.
/// Until 2026-10-04 each loaded its source as text under -mode(system), Rule A
/// skipping a source with no file, and the send_to_user tests defined their
/// own send_to_user/1 over '_output'.
import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  group('_output kernel', () {
    late GlpEngine engine;
    late List<String> outputLines;
    late Directory systemDir;
    late Directory dir;

    setUp(() {
      engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      outputLines = [];
      engine.runtime.outputCallback = (line) => outputLines.add(line);
      systemDir = Directory('../programs/system');
      // Made again where the other test removed it between the two calls.
      for (var attempt = 0;; attempt++) {
        try {
          systemDir.createSync(recursive: true);
          dir = systemDir.createTempSync('output_kernel_');
          break;
        } on FileSystemException {
          if (attempt >= 3) rethrow;
        }
      }
    });

    tearDown(() {
      dir.deleteSync(recursive: true);
      // Another test file may be writing its own module there at the same
      // time (primitive_layer_test); the directory, which git does not hold,
      // goes once empty, whichever test leaves it so.
      try {
        if (systemDir.listSync().isEmpty) systemDir.deleteSync();
      } on FileSystemException {
        // not empty, or removed already: the other test removes it
      }
    });

    /// Load [source] as the system module `out.glp` under programs/system/.
    void loadSystemModule(String source) {
      final f = File('${dir.path}${Platform.pathSeparator}out.glp')
        ..writeAsStringSync(source);
      expect(engine.loadFile(f.path), isTrue);
    }

    test('prints a constant', () async {
      loadSystemModule('''
-mode(system).
procedure test.
test :- '_output'(hello).
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['hello']);
    });

    test('prints a struct', () async {
      loadSystemModule('''
-mode(system).
procedure test.
test :- '_output'(msg(alice, bob, text(hi))).
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['msg(alice, bob, text(hi))']);
    });

    test('prints a list', () async {
      loadSystemModule('''
-mode(system).
procedure test.
test :- '_output'([a, b, c]).
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['[a, b, c]']);
    });

    test('a source with no file behind it may not declare -mode(system)', () {
      expect(
          () => engine.loadSource('''
-mode(system).
procedure test.
test :- '_output'(hello).
'''),
          throwsA(predicate((e) =>
              '$e'.contains('confined to the primitive layer') &&
              '$e'.contains('no file behind it'))));
    });
  });

  group('send_to_user', () {
    late GlpEngine engine;
    late List<String> outputLines;

    setUp(() {
      engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      outputLines = [];
      engine.runtime.outputCallback = (line) => outputLines.add(line);
    });

    test('consumes a ground stream and prints each term', () async {
      // send_to_user/1 is the root self.glp's (GLP-Spec appendix-guards, the
      // seam-predicate rows).
      engine.loadSource('''
procedure test.
test :- send_to_user([hello, world, msg(a, b)]).
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['hello', 'world', 'msg(a, b)']);
    });

    test('waits for stream elements to become ground', () async {
      // Test that send_to_user suspends on non-ground and resumes when bound
      engine.loadSource('''
procedure test.
test :- send_to_user([hello | Tail?]), Tail = [world].
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['hello', 'world']);
    });
  });
}
