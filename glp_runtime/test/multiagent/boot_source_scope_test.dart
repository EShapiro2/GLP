/// The scope a boot source is checked in.
///
/// IGLP, Implementation Notes, "The scope a boot source is checked in": a boot
/// source is loaded on top of a program already in the engine, so it is checked
/// in the scope the engine holds when it is handed over --- the linked program,
/// the kernels the runtime has loaded, and the boot file's own ancestor chain
/// of self.glp declarations. A check that sees the ancestor chain alone refuses
/// calls the engine resolves, a kernel loaded a moment earlier among them.
///
/// Until 2026-09-18 the multi-isolate loaders handed a boot source to
/// `loadSource` under a synthetic name, which has no ancestor chain, so the
/// check saw the bare root scope and refused `send_to_net/1` --- the kernel
/// `enableMadGLP` had loaded a moment earlier. The loaders now pass the
/// engine's scope ([GlpEngine.scope], [GlpEngine.scopeFor]); these tests hold
/// that a boot source calling what the engine holds loads, and that one
/// calling nothing that exists is still refused.  Since 2026-10-04
/// `send_to_net/1` is the root self.glp's (GLP-Spec appendix-guards, "Output
/// to the network") and `enableMadGLP` loads nothing, so the procedure the
/// engine holds and the bare root scope does not is the loaded program's
/// export, `double/2` of programs/tests/cert_ok.  AgentRuntime co-loads no boot
/// source since 2026-10-02: it runs one program, and a module file given as
/// that program is checked in the same scope, which its two tests below hold.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// A boot source whose one body atom is the seam predicate `send_to_net/1`,
/// the root self.glp's.
const _callsSendToNet = '''
procedure agent_init(_?, _?).
agent_init(_, _) :- send_to_net([]).
''';

/// A boot source calling `double/2`, the export of programs/tests/cert_ok:
/// held by an engine that has loaded that program, declared in no self.glp
/// chain of the boot source and not in the root scope.
const _callsProgram = '''
procedure agent_init(_?, _?).
agent_init(_, _) :- double(1, _).
''';

/// The program [_callsProgram] calls.
const _certOk = '../programs/tests/cert_ok';

/// A boot source calling a procedure that exists nowhere.
const _callsNothing = '''
procedure agent_init(_?, _?).
agent_init(_, _) :- no_such_procedure(1).
''';

/// A source calling `shared_double/2`, declared only in
/// programs/tests/module_self_procs/self.glp --- a directory's own chain.
const _callsChainProcedure = '''
procedure agent_init(_?, _?).
agent_init(_, _) :- shared_double(1, _).
''';

const _bootClause = '''
procedure boot.
boot :- agent_init(alice, _)@alice.

''';

String get _rootSelf => File('../programs/self.glp').absolute.path;

/// [source] written as the module file [name].glp in [dir]: the one program an
/// AgentRuntime runs (TGLP, def:program).
String _moduleFile(Directory dir, String name, String source) {
  final path = '${dir.path}/$name.glp';
  File(path).writeAsStringSync(source);
  return path;
}

/// A file under a directory whose self.glp the engine's scope does not carry.
String get _chainFile =>
    File('../programs/tests/module_self_procs/worker.glp').absolute.path;

void main() {
  group('the scope a boot source is checked in (engine)', () {
    test('a source calling the loaded program loads in the engine\'s scope',
        () {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      engine.enableMadGLP(agentId: 'alice');
      expect(engine.loadProgram(_certOk), isTrue);

      expect(
          engine.loadSource(_callsProgram,
              filename: 'program', scope: engine.scope),
          isTrue);
    });

    test('the same source under a synthetic name alone is what was refused',
        () {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      engine.enableMadGLP(agentId: 'alice');
      expect(engine.loadProgram(_certOk), isTrue);

      expect(
        () => engine.loadSource(_callsProgram, filename: 'program'),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('Type checking failed') &&
              s.contains('double/2');
        }, 'the bare chain scope does not carry the program\'s export')),
      );
    });

    test('a source calling send_to_net/1 loads in either scope: it is the root\'s',
        () {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      engine.enableMadGLP(agentId: 'alice');

      expect(
          engine.loadSource(_callsSendToNet,
              filename: 'program', scope: engine.scope),
          isTrue);
      expect(engine.loadSource(_callsSendToNet, filename: 'program'), isTrue);
    });

    test('a source calling nothing that exists is refused in the engine\'s scope',
        () {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      engine.enableMadGLP(agentId: 'alice');

      expect(
        () => engine.loadSource(_callsNothing,
            filename: 'program', scope: engine.scope),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('Type checking failed') &&
              s.contains('no_such_procedure/1');
        }, 'names the undefined procedure')),
      );
      expect(engine.loadedPrograms.containsKey('program'), isFalse);
    });

    test('scopeFor layers the boot file\'s own ancestor chain on the scope',
        () {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      engine.enableMadGLP(agentId: 'alice');

      // Without the chain the procedure is undefined.
      expect(
        () => engine.loadSource(_callsChainProcedure,
            filename: 'source_0', scope: engine.scope),
        throwsA(predicate(
            (e) => e.toString().contains('shared_double/2'),
            'shared_double/2 undefined')),
      );
      // With the boot file's chain, module_self_procs/self.glp declares it.
      expect(
          engine.loadSource(_callsChainProcedure,
              filename: 'source_0', scope: engine.scopeFor(_chainFile)),
          isTrue);
    });
  });

  group('the multi-isolate loaders hand the boot source that scope', () {
    late IsolateManager manager;
    late Directory dir;

    // The program lies under the root, programs/ (TGLP modules.tex, "Scope
    // construction": "A program lies at or below the root"): its scope then
    // holds the root self.glp, and send_to_net/1 with it, in the check of the
    // linked program as in the module's.  Until
    // 2026-10-03 it was written to the system's temporary directory, outside the
    // root, and the single-file path checked the module alone.
    setUpAll(() =>
        dir = Directory('../programs/tests').createTempSync('glp_boot_scope_'));
    tearDownAll(() => dir.deleteSync(recursive: true));
    setUp(() => manager = IsolateManager());
    tearDown(() async => manager.shutdown());

    test('IsolateManager boots an agent whose program calls send_to_net/1',
        () async {
      final config = BootLoader().load(_bootClause + _callsSendToNet);
      config.rootSelfGlpPath = _rootSelf;

      await manager.boot(config);
      manager.start();
      await manager.settle();
    }, timeout: Timeout(Duration(seconds: 30)));

    test('IsolateManager still refuses a program calling nothing that exists',
        () async {
      final config = BootLoader().load(_bootClause + _callsNothing);
      config.rootSelfGlpPath = _rootSelf;

      await expectLater(
        manager.boot(config),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('failed to initialize') &&
              s.contains('no_such_procedure/1');
        }, 'the agent reports the undefined procedure, so boot fails fast')),
      );
    }, timeout: Timeout(Duration(seconds: 30)));

    test('AgentRuntime initialises an agent whose program calls send_to_net/1',
        () async {
      final agent = AgentRuntime(
        agentId: 'alice',
        program: _moduleFile(dir, 'calls_send_to_net', _callsSendToNet),
        rootSelfGlpPath: _rootSelf,
        goalLabel: 'agent_init/2',
      );
      await agent.initialize();
      expect(agent.initialized, isTrue);
    });

    test('AgentRuntime still refuses a program calling nothing that exists',
        () async {
      final agent = AgentRuntime(
        agentId: 'alice',
        program: _moduleFile(dir, 'calls_nothing', _callsNothing),
        rootSelfGlpPath: _rootSelf,
        goalLabel: 'agent_init/2',
      );
      await expectLater(
        agent.initialize(),
        throwsA(predicate(
            (e) => e.toString().contains('no_such_procedure/1'),
            'names the undefined procedure')),
      );
    });
  });
}
