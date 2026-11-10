/// The reporter of a debuggable build (lib/debuglog/; GLP/phones/docs/
/// debugging.md, "The phone side", items 1 to 5), against a log service in
/// this test that takes what the service at peoplesweb.org/log takes: `POST
/// /log/<phone>/<run>`, the header `Authorization: Bearer <password>`, the
/// events as JSON lines, answered 204.
///
/// The launch's first events; a runtime line, a layer callback and an act of
/// the person, from the Grassroots App's agent on the app's own host over a
/// network in this process; the batch posted every two seconds; the batch
/// posted at once on an `[ERROR]` line; the buffer kept across a restart of
/// the reporter and posted under its own run; the layer's diagnostics, the
/// debugPrint lines of networking/core and of networking/lib; and, in a build without
/// `GLP_DEBUGLOG`, no reporter and nothing sent.  Each wait is on the post it
/// waits for, with no clock.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter/foundation.dart' as flutter show debugPrint;
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/debuglog/app_reporting.dart';
import 'package:glp_multiagent/debuglog/reporter.dart';
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_multiagent/superapp_host.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:grassroots_networking_core/grassroots_networking_core.dart'
    as core show debugPrint;
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show generateKeyPair;

import 'programs_dir.dart';

const String _password = 'test-password';

/// One post the service took.
class _Post {
  final String path;
  final String? authorization;
  final List<Map<String, dynamic>> events;
  _Post(this.path, this.authorization, this.events);

  List<String> get kinds => [for (final e in events) '${e['source']}/${e['kind']}'];
}

/// The log service: every post taken, answered [status].
class _Service {
  final HttpServer server;
  final List<_Post> posts = [];
  int status = HttpStatus.noContent;
  final List<(int, Completer<_Post>)> _waits = [];

  _Service._(this.server) {
    server.listen((request) async {
      final body = await utf8.decoder.bind(request).join();
      final post = _Post(
        request.uri.path,
        request.headers.value(HttpHeaders.authorizationHeader),
        [
          for (final l in const LineSplitter().convert(body))
            if (l.isNotEmpty) jsonDecode(l) as Map<String, dynamic>,
        ],
      );
      request.response.statusCode = status;
      await request.response.close();
      if (status != HttpStatus.noContent) return;
      posts.add(post);
      _waits.removeWhere((w) {
        if (posts.length <= w.$1) return false;
        w.$2.complete(posts[w.$1]);
        return true;
      });
    });
  }

  static Future<_Service> start() async =>
      _Service._(await HttpServer.bind(InternetAddress.loopbackIPv4, 0));

  String get base => 'http://127.0.0.1:${server.port}/log';

  /// The [n]th post taken, from 0, once it is taken.
  Future<_Post> post(int n) {
    if (posts.length > n) return Future.value(posts[n]);
    final c = Completer<_Post>();
    _waits.add((n, c));
    return c.future;
  }

  Future<void> close() => server.close(force: true);
}

typedef _KeyPair = ({PubKey pub, Uint8List priv});

/// A layer with no peer, whose callbacks a test fires as the layer would.
class _Alone extends GlpNetwork {
  final _KeyPair keyPair;
  _Alone(this.keyPair);

  @override
  PubKey getIdentity() => keyPair.pub;
  @override
  Future<String> send(PubKey pk, Uint8List payload) => Future.value('sent');
  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) {}
  @override
  void acknowledge(String messageId) {}
  @override
  void putKnownPeer(PubKey pk) {}
  @override
  void removeKnownPeer(PubKey pk) {}
  @override
  bool isPeerReachable(PubKey pk) => false;
  @override
  List<Underlay> peerUnderlays(PubKey pk) => const [];
  @override
  // The layer holds the private key and signs: the runtime signs by it, for
  // the certificate its compiler writes and sign/3 (the GLP paper f265fec,
  // "Identity and signature").
  Uint8List sign(Uint8List message) =>
      PersonIdentity(keyPair.pub, keyPair.priv).sign(message);
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      PersonIdentity.verify(signer, message, signature);
  @override
  List<DiscoveredPeer> getPeers() => const [];
  @override
  String? getPublicAddress() => null;
  @override
  String? networkIdentity() => null;
  @override
  void putPeerAddress(PubKey pk, String address) {}
  @override
  String? observedPeerAddress(PubKey pk) => null;
  @override
  void punchUdp(String address) {}
  @override
  void putRendezvousServer(PubKey pk, String address) {}
  @override
  void removeRendezvousServer(PubKey pk) {}
  @override
  Future<PlaceCentre?> declarePlace(
          String place, PlaceCentre? centre, double radiusMetres) =>
      Future.value(null);
  @override
  Future<void> removePlace(String place) => Future.value();
}

void main() {
  late Directory tmp;
  late _Service service;
  final key = 'ab' * 32;

  setUp(() async {
    tmp = Directory.systemTemp.createTempSync('glp_debuglog_');
    service = await _Service.start();
  });
  tearDown(() async {
    await service.close();
    tmp.deleteSync(recursive: true);
  });

  Future<DebugReporter> open(
          {Duration period = const Duration(hours: 1),
          void Function(String)? syslog}) =>
      DebugReporter.open(
        dir: Directory('${tmp.path}/debuglog'),
        password: _password,
        base: service.base,
        period: period,
        syslog: syslog,
      );

  test('a launch reports its run, the build, the device and the agent\'s key '
      'first, posted to /log/<phone>/<run> with the password', () async {
    final r = await open();
    addTearDown(r.close);
    reportLaunch(r, '05462f39', {'model': 'iPhone12,3', 'os': 'ios', 'osVersion': '18.1'});
    r.setPhone(key);
    final p = await service.post(0);
    expect(p.path, '/log/$key/${r.run}');
    expect(p.authorization, 'Bearer $_password');
    expect(p.kinds, ['app/launch', 'app/build', 'app/device', 'app/key']);
    expect(p.events[0]['run'], r.run);
    expect(p.events[1]['commit'], '05462f39');
    expect(p.events[2]['model'], 'iPhone12,3');
    expect(p.events[3]['key'], key);
    for (final e in p.events) {
      expect(e['time'], isA<int>());
      expect(e['time'] as int, greaterThanOrEqualTo(r.run));
    }
  });

  test('the agent on the app\'s host: its transcript, its trace and what it '
      'sends its person (runtime), a callback of the glue and the trust levels '
      'it sets (layer), and the person\'s act (screen)', () async {
    final r = await open();
    addTearDown(r.close);
    r.setPhone(key);
    await service.post(0);

    final kp = generateKeyPair();
    final alone = _Alone(kp);
    final programs = programsDir();
    final host = SuperAppHost(
      agentId: 'alice',
      program: '$programs/social/graph/core',
      rootSelfGlpPath: '$programs/self.glp',
      network: ReportingGlpNetwork(alone, r),
      manifest: superappManifest,
    );
    addTearDown(host.dispose);
    final screen = ScreenReporter(r);
    reportHost(r, host, screen);
    final errors = <String>[];
    host.onOutput = (line) {
      r.runtimeOutput(line);
      if (line.startsWith('[ERROR]')) errors.add(line);
    };
    final drawn = Completer<OpenConstruct>();
    OpenConstruct? form() {
      for (final c in host.ui.bridge.constructs) {
        if (c.id != 0 && formatTerm(c.widget).startsWith('input_box(menu(')) {
          return c;
        }
      }
      return null;
    }

    host.ui.onChange = () {
      final f = form();
      if (f != null && !drawn.isCompleted) drawn.complete(f);
    };
    await host.start();
    final f = await drawn.future;
    host.ui.bridge.grant(f.id, questionAt(f.view)!, const GAtom('open_place'));
    await host.idle;

    // The layer discovers a peer on the PAN, as the glue would report it.
    final peer = generateKeyPair().pub;
    alone.onPeerDiscovered!(peer, ProximityUnderlay.pan);

    await r.flush();
    final events = [for (final p in service.posts.skip(1)) ...p.events];
    final kinds = [for (final e in events) '${e['source']}/${e['kind']}'];
    expect(errors, isEmpty);
    expect(kinds, contains('runtime/output'));
    expect(kinds, contains('runtime/log'));
    expect(kinds, contains('runtime/userOutput'));
    expect(
        events.where((e) => e['kind'] == 'setTrustLevel').map((e) =>
            '${e['underlay']} ${e['level']}'),
        containsAllInOrder(['pan open', 'lan open']));
    final discovered = events.singleWhere((e) => e['kind'] == 'onPeerDiscovered');
    expect(discovered['source'], 'layer');
    expect(discovered['peer'], peer.hex);
    expect(discovered['underlay'], 'pan');
    final act = events.singleWhere((e) => e['kind'] == 'act');
    expect(act['source'], 'screen');
    expect(act['construct'], f.id);
    expect(act['act'], contains('open_place'));
  }, timeout: const Timeout(Duration(minutes: 3)));

  test('the events are posted as one batch every two seconds', () async {
    final r = await open(period: const Duration(seconds: 2));
    addTearDown(r.close);
    expect(r.period, const Duration(seconds: 2));
    r.setPhone(key);
    await service.post(0);
    r
      ..event(Source.runtime, 'output', {'line': 'one'})
      ..event(Source.layer, 'onPeerLost', {'peer': key, 'underlay': 'lan'})
      ..event(Source.app, 'status', {'line': 'Ready'});
    expect(service.posts, hasLength(1), reason: 'nothing is posted before the batch');
    final batch = await service.post(1);
    expect(batch.kinds, ['runtime/output', 'layer/onPeerLost', 'app/status']);
    expect(batch.path, '/log/$key/${r.run}');
  });

  test('an [ERROR] line of the runtime is posted at once', () async {
    final r = await open();
    addTearDown(r.close);
    r.setPhone(key);
    await service.post(0);
    r.runtimeOutput('[ERROR] the place is not opened');
    final p = await service.post(1);
    expect(p.kinds, ['runtime/output']);
    expect(p.events.single['line'], '[ERROR] the place is not opened');
  });

  test('the buffer is kept across a restart and posted at the next launch '
      'under its own run', () async {
    final failures = <String>[];
    service.status = HttpStatus.serviceUnavailable;
    final first = await open(syslog: failures.add);
    first.setPhone(key);
    first.runtimeOutput('before the restart');
    await first.close();
    expect(service.posts, isEmpty);
    expect(failures, isNotEmpty, reason: 'the refusals go to the syslog');

    service.status = HttpStatus.noContent;
    final second = await open();
    addTearDown(second.close);
    expect(second.run, isNot(first.run));
    second.setPhone(key);
    final earlier = await service.post(0);
    final now = await service.post(1);
    expect(earlier.path, '/log/$key/${first.run}');
    expect(earlier.kinds, ['app/key', 'runtime/output']);
    expect(earlier.events[1]['line'], 'before the restart');
    expect(now.path, '/log/$key/${second.run}');
    expect(now.kinds, ['app/key']);
    expect(File('${tmp.path}/debuglog/${first.run}.jsonl').existsSync(), isFalse,
        reason: 'a run posted whole is deleted');
  });

  test('the layer\'s diagnostics: a debugPrint line of networking/core and '
      'of the Flutter package networking/lib, [lan], [ble] and [local-host], '
      'each reaching the service; the reporter\'s own failures are not '
      'reported', () async {
    // The syslog is Flutter's debugPrint as it was before the redirect, as
    // startDebugReporter takes it.
    final printed = flutter.debugPrint;
    final failures = <String>[];
    final r = await open(syslog: (m) {
      failures.add(m);
      printed(m);
    });
    addTearDown(r.close);
    final restore = reportLayerDiagnostics(r);
    addTearDown(restore);
    r.setPhone(key);
    await service.post(0);
    core.debugPrint('[transport] the core\'s line');
    flutter.debugPrint('[lan] discovery started (port 4242)');
    flutter.debugPrint('[ble] scanning');
    flutter.debugPrint('[local-host] 192.168.1.7');
    await r.flush();
    final p = await service.post(1);
    expect(
        [for (final e in p.events) '${e['source']}/${e['kind']} ${e['printer']} ${e['line']}'],
        [
          'layer/diagnostic core [transport] the core\'s line',
          'layer/diagnostic flutter [lan] discovery started (port 4242)',
          'layer/diagnostic flutter [ble] scanning',
          'layer/diagnostic flutter [local-host] 192.168.1.7',
        ]);
    expect(p.path, '/log/$key/${r.run}');

    // A post the service refuses goes to the syslog, and is not reported.
    service.status = HttpStatus.serviceUnavailable;
    r.event(Source.app, 'status', {'line': 'Ready'});
    await r.flush();
    expect(failures, isNotEmpty);
    final buffered = File('${tmp.path}/debuglog/${r.run}.jsonl').readAsStringSync();
    expect(buffered.contains('debuglog:'), isFalse);
  });

  test('a build without GLP_DEBUGLOG has no reporter and sends nothing',
      () async {
    expect(debugLogOn, isFalse);
    expect(await startDebugReporter(), isNull);
    expect(service.posts, isEmpty);
  });
}
