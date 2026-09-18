/// The load path refuses what does not typecheck.
///
/// The object typechecked is the object compiled, and no diagnostic on a load
/// path is a warning: a program that does not check does not run. Until
/// 2026-09-18 `GlpEngine.loadSource` printed `[TYPE WARNING] Type errors found`
/// and carried on whenever `strictTypes` was off, and both multi-isolate loaders
/// — `multiagent/isolate_manager.dart` and `multiagent/agent_runtime.dart` — set
/// it off, so every agent isolate ran its program unchecked. The flag is gone;
/// these tests hold the refusal on that path.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// A module whose one body atom passes an atom where the callee declares
/// Integer. This is what the checker sees on the boot path: the boot clause is
/// stripped there, so what remains of the file is this ordinary module.
const _illTyped = '''
procedure agent_init(_?, _?).
agent_init(_, _) :- takes_integer(hello).

procedure takes_integer(Integer?).
takes_integer(_).
''';

/// The same module with that body atom corrected.
const _wellTyped = '''
procedure agent_init(_?, _?).
agent_init(_, _) :- takes_integer(7).

procedure takes_integer(Integer?).
takes_integer(_).
''';

/// The boot clause the multi-isolate path spawns one agent from.
const _bootClause = '''
procedure boot.
boot :- agent_init(alice, _)@alice.

''';

String get _rootSelf => File('../programs/self.glp').absolute.path;

void main() {
  group('loadSource refuses a module that does not typecheck', () {
    test('the load throws and names the errors', () {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);

      expect(
        () => engine.loadSource(_illTyped, filename: 'refused'),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('Type checking failed') &&
              s.contains('takes_integer');
        }, 'names the type error')),
      );
    });

    test('a refused load leaves no compiled program behind', () {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);

      try {
        engine.loadSource(_illTyped, filename: 'refused');
      } catch (_) {
        // expected — the assertion is on what the engine holds afterwards
      }
      expect(engine.loadedPrograms.containsKey('refused'), isFalse);
    });

    test('the corrected module loads', () {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);

      expect(engine.loadSource(_wellTyped, filename: 'accepted'), isTrue);
    });
  });

  group('the multi-isolate path refuses it too', () {
    late IsolateManager manager;

    setUp(() => manager = IsolateManager());
    tearDown(() async => manager.shutdown());

    test('boot fails on an agent program that does not typecheck', () async {
      final config = BootLoader().load(_bootClause + _illTyped);
      config.rootSelfGlpPath = _rootSelf;

      await expectLater(
        manager.boot(config),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('failed to initialize') &&
              s.contains('Type checking failed');
        }, 'the agent reports the type-check failure, so boot fails fast')),
      );
    }, timeout: Timeout(Duration(seconds: 30)));

    test('boot succeeds on the corrected program', () async {
      final config = BootLoader().load(_bootClause + _wellTyped);
      config.rootSelfGlpPath = _rootSelf;

      await manager.boot(config);
      manager.start();
      await manager.settle();
    }, timeout: Timeout(Duration(seconds: 30)));
  });
}
