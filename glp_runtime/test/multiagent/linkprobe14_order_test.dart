/// linkprobe14 in another order: bob's messages to carol held in the router
/// and then released, so that bob's greeting has reached carol before she
/// forwards the end of bob's channel to alice, and alice sees it, from carol.
/// The check is Section MB's on linkprobe14 (test/run_all_tests.sh, MB5),
/// which holds in every order.
///
/// GLP #11 Cowork, 2026-11-03 16:06 UTC, item 3: "Once Bob's value has
/// reached Carol it is Carol's to send, as madGLP's Send Reader and Globalize
/// say, so Alice seeing it from Carol is a correct run ... Alice may see the
/// value only from Carol, never on Bob's held link."  IGLP, Implementation
/// Notes, "Held links": a link end whose other end is at an agent the runtime
/// did not exchange the name with is held; the probe calls no
/// authorise_link/2, so nothing is released.  So in every order the router
/// delivers no message from bob to alice --- no value of bob's on the held
/// link, and no answer to a request of alice's --- and alice may see bob's
/// greeting, from carol.
///
/// The waits are on events only: boot and settle end when the agents are
/// ready and quiescent, or on a fault, and the test has no clock.
@Timeout.none
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

const _probes = '../programs/tests/linkprobes';

void main() {
  late IsolateManager manager;

  setUp(() => manager = IsolateManager());
  tearDown(() async => manager.shutdown());

  test(
      'bob\'s messages to carol held and then released: no message from bob '
      'reaches alice, and alice sees bob\'s greeting, from carol', () async {
    final config = BootLoader()
        .load(File('$_probes/linkprobe14_boot.glp').readAsStringSync());
    config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
    config.programDir = File('$_probes/linkprobe14').absolute.path;
    await manager.boot(config);

    manager.holdDelivery('bob', 'carol');
    manager.start();
    await manager.settle();
    expect(manager.delivered('bob', 'carol'), 0,
        reason: 'bob\'s messages to carol are held');

    manager.releaseDelivery('bob', 'carol');
    await manager.settle();

    expect(manager.faults, isEmpty);
    expect(manager.delivered('bob', 'carol'), greaterThan(0));
    expect(manager.delivered('carol', 'alice'), greaterThan(0),
        reason: 'carol forwards the end to alice');
    expect(manager.delivered('bob', 'alice'), 0,
        reason: 'nothing of bob\'s reaches alice on the held link');
    for (final line in manager.outputOf('alice')) {
      expect(line, contains('saw(hello_from(bob))'),
          reason: 'what alice sees is bob\'s greeting, which came from carol');
    }
    // In this order bob's greeting reached carol before she forwarded the
    // end, and alice sees it.
    expect(manager.outputOf('alice'), ['saw(hello_from(bob))']);
  });
}
