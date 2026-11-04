// Persons far apart: their layers never meet, and every message goes through
// the relay (gasrelay), encrypted end to end.  Ann adds Ben from afar, Ben's
// phone learns of Ann from the relay, and GKK's acquaint runs between them.
//
// The first test runs the relay on the loopback from its source,
// GASRELAY=<path to gasrelay/bin/relay.dart> (the talmonlab repo,
// server/gasrelay); the second uses a running relay, GASRELAY_URL (e.g.
// https://talmonlab.bgu.ac.il/gasrelay).  Each self-skips without its
// variable.
import 'dart:convert';
import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:gas_core/person_host.dart';
import 'package:gas_core/relay_network.dart';

import 'people.dart';

/// Ann adds Ben from afar, and they are acquainted, through the relay.
Future<void> far(PersonHost ann, PersonHost ben) async {
  final ra = ann.network as RelayGlpNetwork, rb = ben.network as RelayGlpNetwork;
  final addedBy = <String>[];
  rb.onAddedBy = (pk) => addedBy.add(pk.hex);
  final all = [ann, ben];
  for (final p in all) {
    await propose(p, S('establish', [p.me]));
  }
  await ra.addPerson(ben.keyPair.pub);
  await until(all, () => addedBy.contains(ann.me), 'Ben told that Ann added him',
      within: const Duration(seconds: 60));
  expect(rb.isPeerReachable(ann.keyPair.pub), isTrue);
  await propose(ann, S('acquaint', [ann.me, ben.me]));
  await until(all, () => hasAtom(ann, 'knows', [ben.me]) && hasAtom(ben, 'knows', [ann.me]),
      'Ann and Ben acquainted through the relay');
  expect(ra.state, 'online');
  expect(rb.state, 'online');
}

void main() {
  test('GKK between two persons far apart, through a relay on the loopback', () async {
    final why = await cannotRun('gkk');
    if (why != null) return markTestSkipped(why);
    final src = Platform.environment['GASRELAY'];
    if (src == null || !File(src).existsSync()) return markTestSkipped('GASRELAY not set');
    final s = await ServerSocket.bind('127.0.0.1', 0);
    final port = s.port;
    await s.close();
    final relay = await Process.start('dart', [src, '$port'],
        workingDirectory: File(src).parent.parent.path);
    addTearDown(relay.kill);
    await relay.stdout.transform(utf8.decoder).firstWhere((l) => l.contains('gasrelay on'));

    final [ann, ben] = await people('gkk', 2, relay: 'http://127.0.0.1:$port');
    await far(ann, ben);
    // Everything was acknowledged: the relay holds nothing.
    await Future<void>.delayed(const Duration(seconds: 2));
    final st = await (await HttpClient().getUrl(Uri.parse('http://127.0.0.1:$port/'))).close();
    expect((jsonDecode(await utf8.decodeStream(st)) as Map)['queued'], 0);
  }, timeout: const Timeout(Duration(minutes: 6)));

  test('GKK between two persons far apart, through a running relay', () async {
    final why = await cannotRun('gkk');
    if (why != null) return markTestSkipped(why);
    final url = Platform.environment['GASRELAY_URL'];
    if (url == null) return markTestSkipped('GASRELAY_URL not set');
    final [ann, ben] = await people('gkk', 2, relay: url);
    await far(ann, ben);
  }, timeout: const Timeout(Duration(minutes: 6)));
}
