/// A boot ends on an event in every case, never on the clock.
///
/// IGLP, Implementation Notes, "Event-driven execution": an agent "reduces
/// until quiescent ... and then performs its Sends ... there is no clock and
/// no polling".  [IsolateManager.boot] returns when every agent reports that
/// it is ready, and fails when one cannot be: when it reports that it could
/// not initialise, and when its isolate ends before it is ready, by a throw
/// outside the steps that report their own failure or by an exit, which reach
/// the isolate's onError and onExit and not the manager's port.
///
/// Until 2026-11-02 the second ended no wait: boot waited for a Ready that
/// would never come, and only a test's wall-clock timeout ended it.  And a
/// failure reported while a later agent's isolate was still being spawned
/// failed the boot's completer before boot awaited it, so it reached the zone
/// as an uncaught error and not boot's caller.  The multiagent plays wait on
/// boot and on quiescence and on no clock (multiagent_glp_test.dart), which
/// is sound only when every way a boot can end is an event (IGLP-coop).
library;

import 'dart:io';
import 'dart:isolate';
import 'dart:typed_data';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';

String get _rootSelf => File('../programs/self.glp').absolute.path;

/// A layer whose opening ends the agent's isolate before the agent is ready.
AgentLayer _exitingLayer(({PubKey pub, Uint8List priv}) keyPair) => AgentLayer(
    keyPair: keyPair,
    open: () async {
      Isolate.exit();
    });

void main() {
  test('an agent isolate that exits before it is ready fails the boot, '
      'naming the agent', () async {
    final kp = generateKeyPair();
    final name = kp.pub.hex;
    final manager = IsolateManager(layers: [_exitingLayer(kp)]);
    addTearDown(manager.shutdown);
    final config = BootLoader().load('''
procedure boot.
boot :- agent1_init('$name', _)@'$name'.
''');
    config.rootSelfGlpPath = _rootSelf;
    config.programDir = File('../programs/tests/multiagent/coop_stream')
        .absolute
        .path;
    await expectLater(
        manager.boot(config),
        throwsA(predicate(
            (e) => e is StateError && '$e'.contains(name) &&
                '$e'.contains('before it was ready'),
            'a StateError naming the agent and that it exited before it was '
            'ready')));
  }, timeout: Timeout.none);

  test('agents that cannot initialise fail the boot, through boot, while '
      'their isolates are being spawned', () async {
    final manager = IsolateManager();
    addTearDown(manager.shutdown);
    final source = File('../programs/tests/multiagent/coop_stream_boot.glp')
        .readAsStringSync();
    final config = BootLoader().load(source);
    // The program lies outside this root, so every agent's load is refused
    // (TGLP modules.tex, "Scope construction"), and each reports it.
    config.rootSelfGlpPath = '/nonexistent/programs/self.glp';
    config.programDir = File('../programs/tests/multiagent/coop_stream')
        .absolute
        .path;
    await expectLater(
        manager.boot(config),
        throwsA(predicate(
            (e) => e is StateError && '$e'.contains('failed to initialize'),
            'a StateError that an agent failed to initialise')));
  }, timeout: Timeout.none);
}
