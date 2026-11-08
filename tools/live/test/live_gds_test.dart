// bin/live_gds.dart, part 2's flow of the live suite, run against a local
// directory and rendezvous server over the loopback, each built as the VPS
// builds it and run as a process of its own: the directory under a wrapper
// that starts it again when it stops, so that --restart-cmd restarts it as
// systemd restarts the unit, and its output in a log file that --log-cmd
// prints, as journalctl prints the unit's.  Every check of 2.2 to 2.10 is to
// pass.
//
// Needs a native libsodium, as the glue's tests do.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:test/test.dart';

/// The Flutter SDK's Dart, which resolves the packages that depend on
/// Flutter; under `dart test`, the Dart running the test.
String _dart() {
  final root = Platform.environment['FLUTTER_ROOT'];
  if (root != null && File('$root/bin/dart').existsSync()) return '$root/bin/dart';
  return Platform.resolvedExecutable;
}

Future<int> _freeUdp() async {
  final s = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
  final p = s.port;
  s.close();
  return p;
}

Future<int> _freeTcp() async {
  final s = await ServerSocket.bind(InternetAddress.loopbackIPv4, 0);
  final p = s.port;
  await s.close();
  return p;
}

Future<String> _build(String package, String target, String out) async {
  final r = await Process.run(_dart(), ['build', 'cli', '-t', target, '-o', out],
      workingDirectory: package);
  expect(r.exitCode, 0, reason: 'dart build cli in $package: ${r.stdout}\n${r.stderr}');
  return '$out/bundle/bin/${target.split('/').last.replaceAll('.dart', '')}';
}

/// The first line of [file] matching [re], once written.
Future<RegExpMatch> _lineIn(File file, RegExp re) async {
  while (true) {
    if (file.existsSync()) {
      for (final l in file.readAsLinesSync()) {
        final m = re.firstMatch(l);
        if (m != null) return m;
      }
    }
    await Future<void>.delayed(const Duration(milliseconds: 200));
  }
}

void main() {
  test('part 2: every check of the directory\'s flow passes against a local directory and rendezvous server',
      () async {
    final glp = Directory('../..').absolute.path;
    final tmp = Directory.systemTemp.createTempSync('live_gds_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final t = tmp.path;

    final rvExe = await _build('$glp/networking/glue', 'bin/rendezvous_server.dart', '$t/rv');
    final dirExe = await _build('$glp/servers/directory', 'bin/directory_server.dart', '$t/dir');

    // The rendezvous server.
    final rvPort = await _freeUdp();
    final rv = await Process.start(rvExe, [
      '--identity', '$t/rv-identity.json', '--port', '$rvPort',
      '--public-address', '127.0.0.1:$rvPort', '--outbound-queue-file', '$t/rv-queue.json',
      '--state-file', '$t/rv-state', '--program', '$glp/programs/examples/rendezvous/rv_agent.glp',
      '--root-self', '$glp/programs/self.glp',
    ]);
    addTearDown(() async {
      rv.kill(ProcessSignal.sigterm);
      await rv.exitCode;
    });
    final rvLines = rv.stdout.transform(utf8.decoder).transform(const LineSplitter()).asBroadcastStream();
    rv.stderr.drain<void>();
    final rvReady = (await rvLines.firstWhere((l) => l.startsWith('READY '))).split(' ');

    // The directory, under a wrapper that starts it again when it stops.
    final dirPort = await _freeUdp();
    final web = await _freeTcp();
    final base = 'http://127.0.0.1:$web';
    final log = File('$t/directory.log');
    final wrapper = File('$t/run_directory.sh')
      ..writeAsStringSync('''#!/bin/bash
while [ ! -f "$t/stop" ]; do
  "$dirExe" "\$@" >> "$t/directory.log" 2>&1 &
  echo \$! > "$t/directory.pid"
  wait \$!
done
''');
    final dir = await Process.start('bash', [
      wrapper.path,
      '--identity', '$t/gds-identity.json', '--port', '$dirPort',
      '--public-address', '127.0.0.1:$dirPort', '--outbound-queue-file', '$t/gds-queue.json',
      '--state-file', '$t/gds-state', '--program', '$glp/programs/examples/rendezvous/gds_agent.glp',
      '--root-self', '$glp/programs/self.glp',
    ], environment: {'HOST': '127.0.0.1', 'PORT': '$web', 'BASE_URL': base, 'DATA': '$t/directory.json'});
    addTearDown(() async {
      File('$t/stop').writeAsStringSync('');
      final pid = File('$t/directory.pid');
      if (pid.existsSync()) Process.killPid(int.parse(pid.readAsStringSync().trim()));
      await dir.exitCode;
    });
    final dirKey = (await _lineIn(log, RegExp(r'^READY \d+ ([0-9a-f]{64}) '))).group(1)!;

    final out = <String>[];
    final live = await Process.start(_dart(), [
      'run', 'bin/live_gds.dart',
      '--base', base,
      '--dir-key', dirKey, '--dir-address', '127.0.0.1:$dirPort',
      '--rv-key', rvReady[2], '--rv-address', '127.0.0.1:$rvPort',
      '--log-cmd', 'cat "${log.path}"',
      '--restart-cmd', 'kill -TERM \$(cat "$t/directory.pid")',
      '--glp-root', '$glp/programs',
    ]);
    live.stdout.transform(utf8.decoder).transform(const LineSplitter()).listen(out.add);
    final err = StringBuffer();
    live.stderr.transform(utf8.decoder).listen(err.write);
    final code = await live.exitCode;
    final results = out.where((l) => l.startsWith('RESULT ')).toList();
    printOnFailure(results.join('\n'));
    printOnFailure('stderr: $err');
    printOnFailure('the directory said:\n${log.readAsStringSync()}');
    for (final id in [
      '2.2-list-verify', '2.3-listing-kept', '2.4-supply', '2.5-lookup-name-email', '2.6-unlisted',
      '2.7-link-redeemed', '2.4-supply-after-lookup', '2.8-unlist', '2.9-restart', '2.10-mail-rate',
    ]) {
      expect(results.where((l) => l.startsWith('RESULT $id PASS ')), hasLength(1), reason: id);
    }
    expect(results.where((l) => l.contains(' FAIL ')), isEmpty);
    expect(code, 0);
  }, timeout: Timeout.none);
}
