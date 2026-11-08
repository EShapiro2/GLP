/// The services the Grassroots App is configured with (GSA paper, Appendix
/// "The Operating System", "Boot", 1d430b6: "the services the app is
/// configured with, its rendezvous servers and its directory, each by key and
/// address ... the configuration is a file of the repository the app reads
/// when it is built"): `programs/social/graph/core/services.json`, read by
/// [readServices], the rendezvous servers in their order and then the
/// directory.
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/superapp_host.dart';

void main() {
  test('the repository\'s configuration names the live rendezvous server and '
      'the live directory', () {
    final services = readServices('../programs/social/graph/core');
    expect([for (final s in services) (s.kind, s.address)], [
      (ServiceKind.rendezvous, '162.35.126.126:9516'),
      (ServiceKind.directory, '162.35.126.126:9518'),
    ]);
    for (final s in services) {
      expect(s.key, matches(RegExp(r'^[0-9a-f]{64}$')));
    }
  });

  test('a configuration with a directory names it after the rendezvous '
      'servers; no file, no service', () {
    final dir = Directory.systemTemp.createTempSync('services_');
    addTearDown(() => dir.deleteSync(recursive: true));
    expect(readServices(dir.path), isEmpty);
    File('${dir.path}/services.json').writeAsStringSync(
        '{"directory": {"key": "dd", "address": "10.0.0.1:9518"}, '
        '"rendezvous": [{"key": "aa", "address": "10.0.0.1:9516"}, '
        '{"key": "bb", "address": "10.0.0.2:9516"}]}');
    final services = readServices(dir.path);
    expect([for (final s in services) (s.kind, s.key, s.address)], [
      (ServiceKind.rendezvous, 'aa', '10.0.0.1:9516'),
      (ServiceKind.rendezvous, 'bb', '10.0.0.2:9516'),
      (ServiceKind.directory, 'dd', '10.0.0.1:9518'),
    ]);
  });
}
