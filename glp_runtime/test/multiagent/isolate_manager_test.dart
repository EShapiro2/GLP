import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// The agent cold-call round-trip fixture (repo-relative from glp_runtime/).
const _fixtureDir = '../programs/tests/agent_roundtrip';

/// Read a GLP file from the fixture directory, or skip the test.
String? _readGlpFile(String filename) {
  final file = File('$_fixtureDir/$filename');
  if (!file.existsSync()) {
    print('Skipping: $filename not found at ${file.path}');
    return null;
  }
  return file.readAsStringSync();
}

void main() {
  group('IsolateManager', () {
    late IsolateManager manager;

    setUp(() {
      manager = IsolateManager();
    });

    tearDown(() async {
      await manager.shutdown();
    });

    test('boots three agents from boot config', () async {
      // Minimal program that completes immediately
      final source = '''
procedure boot.
boot :-
    agent(ch(_?, _), ch(_?, _))@alice,
    agent(ch(_?, _), ch(_?, _))@bob,
    agent(ch(_?, _), ch(_?, _))@charlie.

exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(_, []), ch(_, [])).
''';

      final loader = BootLoader();
      final config = loader.load(source);
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;

      await manager.boot(config);

      // Start and let event-driven execution run
      manager.start();
      await manager.settle();

      // Test verifies boot + start don't crash with trivial goals
    }, timeout: Timeout(Duration(seconds: 10)));

    test("each agent's key pair is made at its spawn, and self_key/1 assigns "
        "it", () async {
      // IGLP, Implementation Notes, "Boot": "When the runtime spawns an
      // agent, before anything is compiled or sent, it makes the agent's key
      // pair ... it is the key the networking layer holds for the agent";
      // GLP-Spec appendix-guards.tex, "Identity and signature": "self_key(Key)
      // assigns Key the agent's public key".  Each agent tells its person the
      // key self_key/1 assigns it.
      final source = '''
procedure boot.
boot :-
    agent(ch(_?, _), ch(_?, _))@alice,
    agent(ch(_?, _), ch(_?, _))@bob.

KeyReport ::= self(Key).

exported procedure agent(Channel(Stream(Constant), Stream(KeyReport))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(_, [self(K?)]), ch(_, [])) :- self_key(K).
''';
      final config = BootLoader().load(source);
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;

      await manager.boot(config);
      // The keys exist before the agents run, and are the agents' own.
      final alice = manager.keyOf('alice');
      final bob = manager.keyOf('bob');
      expect(alice, isNotNull);
      expect(bob, isNotNull);
      expect(alice, isNot(equals(bob)));

      manager.start();
      await manager.settle();
      expect(manager.faults, isEmpty);
      for (final (agent, key) in [('alice', alice!), ('bob', bob!)]) {
        // A key prints bare where it reads as an atom, and quoted where it
        // begins with a digit (formatTerm).
        expect(manager.outputOf(agent),
            [matches(RegExp("^self\\('?${key.hex}'?\\)\$"))],
            reason: '$agent: self_key/1 assigns the key made at its spawn');
      }
    }, timeout: Timeout(Duration(seconds: 30)));

    test('runs full play with actor scripts (no UI)', () async {
      final source = _readGlpFile('play_madglp_boot.glp');
      if (source == null) return;

      final loader = BootLoader();
      final config = loader.load(source);
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
      // Converted 2026-08-03 to a program directory, so this play loads through
      // the linker rather than as co-loaded sources. Its code is not in the
      // directory: agent/4 and the three actor scripts are reached through
      // -expose in the parent self.glp, because the fixture's ten alternative
      // plays cannot be one program.
      config.programDir = File('$_fixtureDir/play_madglp').absolute.path;

      // Every agent's goal is agent/2, its person say/2 telling it its name
      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));
      expect(config.directives.every((d) => d.person?.functor == 'say'),
          isTrue);

      await manager.boot(config,
          traceConfig: TraceConfig(glp: true, mad: true));

      // Start and let event-driven execution run the protocol
      manager.start();
      await manager.settle();

      // Termination is external — we shut down in tearDown
    }, timeout: Timeout(Duration(seconds: 30)));

    test('runs full play with UI mediator and UI actors', () async {
      final source = _readGlpFile('play_ui_madglp_boot.glp');
      if (source == null) return;

      final loader = BootLoader();
      final config = loader.load(source);
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
      // Converted 2026-08-03 to a program directory, so this play loads through
      // the linker rather than as co-loaded sources. Its code is not in the
      // directory: agent/4, ui_mediator/5 and ui_actor/2 are reached through
      // -expose in the parent self.glp, because the fixture's ten alternative
      // plays cannot be one program.
      config.programDir =
          File('$_fixtureDir/play_ui_madglp').absolute.path;

      // Every agent's goal is agent/2, its person say/2 telling it its name
      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));
      expect(config.directives.every((d) => d.person?.functor == 'say'),
          isTrue);

      await manager.boot(config,
          traceConfig: TraceConfig(glp: true, mad: true));

      // Start and let event-driven execution run the protocol
      manager.start();
      await manager.settle();

      // Termination is external — we shut down in tearDown
    }, timeout: Timeout(Duration(seconds: 30)));
  });
}
