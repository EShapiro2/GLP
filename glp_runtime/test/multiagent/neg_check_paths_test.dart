/// One negative per check on the multi-isolate load paths only Dart reaches
/// (Integration 2026-10-11 15:53 UTC, item 5: a negative fixture per check on
/// every load path, so that no check passes by never running).
///
/// IsolateManager with programDir: the agent loads a program directory and the
/// fault is in a module of that directory, not in the boot source --- the
/// path ":boot <f>_boot.glp" takes, which test/run_all_tests.sh holds through
/// the REPL as well.  Fixtures: programs/tests/neg_boot_programdir/{types,
/// srsw,guard}/ and their boot files.
///
/// AgentRuntime: the one program it runs is a module file carrying the fault.
/// Fixtures: programs/tests/neg_srsw_boot/agent.glp and
/// programs/tests/neg_guard_boot/agent.glp.  The types counterpart is
/// boot_source_scope_test.dart's.
///
/// The faults, each a fault the paper names: an ill-typed body atom (TGLP
/// well-typing.tex, Definition "Well-Typed Clause", condition 2); a reader
/// occurring twice (GLP-Spec glp.tex, Definitions "Single-Occurrence (SO)
/// Invariant" and "GLP Program, Goals"); true in guard position (GLP-Spec
/// glp.tex, Definition "Guarded Clause": "G is a conjunction of guard
/// predicates").  Each test asserts the refusal's own words.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

String get _rootSelf => File('../programs/self.glp').absolute.path;

const _bootDir = '../programs/tests/neg_boot_programdir';

void main() {
  group('IsolateManager with programDir refuses a fault in the program', () {
    late IsolateManager manager;
    setUp(() => manager = IsolateManager());
    tearDown(() async => manager.shutdown());

    /// Boot `<kind>_boot.glp` with `<kind>/` as the program directory, as
    /// the REPL's `:boot` does.
    Future<void> boot(String kind) {
      final path = File('$_bootDir/${kind}_boot.glp').absolute.path;
      final config = BootLoader().load(File(path).readAsStringSync());
      config.programDir = Directory('$_bootDir/$kind').absolute.path;
      config.rootSelfGlpPath = _rootSelf;
      config.bootPath = path;
      return manager.boot(config);
    }

    test('types: the module\'s ill-typed atom', () async {
      await expectLater(
        boot('types'),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('Agent alice failed to initialize') &&
              s.contains('Type checking failed for module(s) of the program') &&
              s.contains('types/agents.glp:6: Body atom 0 (takes_integer) '
                  'is not well-typed');
        }, 'the type check names the module and the atom')),
      );
    }, timeout: Timeout(Duration(seconds: 30)));

    test('SRSW: the module\'s reader occurring twice', () async {
      await expectLater(
        boot('srsw'),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('Agent alice failed to initialize') &&
              s.contains('SRSW violations found') &&
              s.contains('neg_boot_programdir/srsw/agents:agent_init/2: '
                  'Line 5: Reader variable "Id?" occurs 2 times');
        }, 'the SRSW pass names the module\'s clause and the reader')),
      );
    }, timeout: Timeout(Duration(seconds: 30)));

    test('guards: true in the module\'s guard', () async {
      await expectLater(
        boot('guard'),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('Agent alice failed to initialize') &&
              s.contains('"true" is not a guard - it can only appear in body '
                  'position at Line 5');
        }, 'the analyzer refuses the guard')),
      );
    }, timeout: Timeout(Duration(seconds: 30)));
  });

  group('AgentRuntime refuses a fault in the program it runs', () {
    Future<void> initialize(String file) => AgentRuntime(
          agentId: 'alice',
          program: File(file).absolute.path,
          rootSelfGlpPath: _rootSelf,
          goalLabel: 'agent_init/2',
        ).initialize();

    test('SRSW: a reader occurring twice', () async {
      await expectLater(
        initialize('../programs/tests/neg_srsw_boot/agent.glp'),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('SRSW violations found') &&
              s.contains('Reader variable "Id?" occurs 2 times');
        }, 'the SRSW pass names the reader')),
      );
    });

    test('guards: true in guard position', () async {
      await expectLater(
        initialize('../programs/tests/neg_guard_boot/agent.glp'),
        throwsA(predicate(
            (e) => e.toString().contains(
                '"true" is not a guard - it can only appear in body position'),
            'the analyzer refuses the guard')),
      );
    });
  });
}
