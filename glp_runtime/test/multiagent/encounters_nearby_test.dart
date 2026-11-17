/// Encounters on two phones under the isolate simulation: alice and bob, each
/// an agent of the boot harness running Encounters' canonical platform and
/// its dispatcher with a scripted person, come to know each other by the
/// standing encounter once both have opened their place, and the program
/// declares no trust and no peer of its own.
///
/// The contract, cascade/Encounters/Encounters.txt and schemas.txt:
/// "Encounters:1 Open place: open_place(Alice) <- ~open_place(Alice) guarded
/// by Alice"; "Encounters:2 Encounter: knows(Alice,Bob), knows(Bob,Alice) <-
/// ~knows(Alice,Bob), ~knows(Bob,Alice), nearby(Alice,Bob), nearby(Bob,Alice),
/// open_place(Alice), open_place(Bob) guarded by Alice, Bob must Alice, Bob";
/// its Implementation pragmas: "Open place: while the place is open Alice's
/// app sets the trust of Bluetooth and of the local network open, and closed
/// otherwise (GNA)", "Nearby: an environment predicate, nearby(Alice, Bob),
/// the persons the networking layer reports nearby", "Encounter: Alice stands
/// on [Open place] until [Close place]".
///
/// GLP-Spec, appendix-guards, "Networking seam": "nearby(U, S) assigns S a
/// stream of the agents nearby over the proximity underlay U: seen(K) when the
/// layer discovers on U an agent of key K that it recognises or, under open
/// trust, contacts".  IGLP, Implementation Notes, "Simulation realisation":
/// "The adjacency relation is total by default"; "A nearby declaration on the
/// PAN is given seen ... for any other adjacent agent once the PAN trust
/// levels of both are open, at the moment the second opens"; "The harness sets
/// no trust level at spawn."
///
/// The fixture is programs/tests/encounters_nearby: boot.glp, the boot clause
/// and the scripted persons, and program/self.glp, the agents' program, which
/// runs the platform's agent and dispatcher over the agent's own channels.
/// The program directory is made here from program/self.glp and the platform
/// as the cascade prints it, cascade/Encounters/platform.vglp, the one copy,
/// which the canonical compilation compiles when each agent loads the
/// directory.  The boot settles on no clock.
@Timeout.none
library;

import 'dart:io';

import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';
import 'package:test/test.dart';

import 'boot_program.dart';

const _fixture = '../programs/tests/encounters_nearby';
const _platform = '../cascade/Encounters/platform.vglp';

/// The boot of the fixture over a program directory made of its program and
/// Encounters' platform, removed when the test ends.
BootConfig _encountersBoot() {
  final dir =
      Directory('../programs/tests').createTempSync('glp_encounters_prog_');
  addTearDown(() {
    if (dir.existsSync()) dir.deleteSync(recursive: true);
  });
  File('$_fixture/program/self.glp').copySync('${dir.path}/self.glp');
  File(_platform).copySync('${dir.path}/platform.vglp');
  final bootFile = File('$_fixture/boot.glp');
  return BootLoader().load(bootFile.readAsStringSync())
    ..rootSelfGlpPath = rootSelfGlp
    ..programDir = dir.absolute.path
    ..bootPath = bootFile.absolute.path;
}

void main() {
  late IsolateManager manager;

  setUp(() => manager = IsolateManager());
  tearDown(() async => manager.shutdown());

  test('alice and bob, each opening their place, stand on the encounter, '
      'which opens their trust; each is then given the other nearby and the '
      'encounter commits, each knowing the other, and neither knew the other '
      'before', () async {
    await manager.boot(_encountersBoot());
    manager.start();
    await manager.settle();

    expect(manager.faults, isEmpty);
    for (final (me, other) in [('alice', 'bob'), ('bob', 'alice')]) {
      final out = manager.outputOf(me);
      final stood = out.where((l) => l.startsWith('stood(')).toList();
      expect(stood, hasLength(1),
          reason: '$me opens their place once: $out');
      expect(stood.single, contains('identity'),
          reason: '$me established their identity first: $out');
      expect(stood.single, isNot(contains('knows($other)')),
          reason: '$me does not know $other before the stand: $out');
      expect(stood.single, isNot(contains('nearby(')),
          reason: 'before the stand $me\'s trust is closed, so no one is '
              'nearby: $out');
      expect(out, contains('knows($other)'),
          reason: '$me knows $other once the encounter commits: $out');
      expect(out.indexOf(stood.single), lessThan(out.indexOf('knows($other)')),
          reason: '$me stands before knowing $other: $out');
    }
  }, timeout: Timeout.none);
}
