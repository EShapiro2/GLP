/// An update of the Grassroots App, headless, on the app's own host
/// ([SuperAppHost], lib/superapp_host.dart): a restart under a new version of
/// the agent's program (IGLP 61f3283, madglp-spec.tex, Termination and
/// Restart: "A restart may load a new version of the agent's program, as when
/// the application is updated.  The local state is then restored under the
/// new version when every procedure called by a goal of the resolvent is
/// defined in the new version with the same type identity; otherwise the
/// update is refused, and the agent restarts under the version it terminated
/// under"; Implementation Notes, "Persistence": the store keeps "the compiled
/// program it runs").
///
/// The app's program, `programs/social/graph/core` with the root, is copied
/// into a tree of the test's own, whose `self.glp` is the root, so that an
/// update replaces files of the program where they are, the procedures
/// keeping the names the linking gives them.  The first launch starts the app
/// and the person opens the place.  The second launches a version whose code
/// differs and whose procedures keep their types: the state is restored under
/// it, and the runtime's refresh draws the person's screen as it was.  The
/// third launches a version that no longer defines serve_1/7, which home's
/// goal serves the person's constructs with: the update is refused, the host
/// holds the refusal, and the agent goes on under the version it terminated
/// under.
library;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_multiagent/superapp_host.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show generateKeyPair;

import 'programs_dir.dart';

typedef _KeyPair = ({PubKey pub, Uint8List priv});

/// A layer with no peer: the agent alone, instantiated with its key pair
/// (superapp_second_start_test.dart).
class _Alone extends GlpNetwork {
  final _KeyPair keyPair;
  final List<String> declared = [];

  _Alone(this.keyPair);

  @override
  PubKey getIdentity() => keyPair.pub;
  @override
  Future<String> send(PubKey pk, Uint8List payload) => Future.value('sent');
  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) =>
      declared.add('${underlay.name} ${level.name}');
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
  Uint8List sign(Uint8List message) => throw UnimplementedError();
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      throw UnimplementedError();
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

/// One launch of the app: its host over a fresh layer, the agent's
/// transcript, and a wait on what the person sees.
class _Launch {
  final SuperAppHost host;
  final List<String> lines = [];
  final List<String> errors = [];
  final List<(bool Function(), Completer<void>)> _waits = [];

  _Launch(this.host) {
    host.ui.onChange = () => _waits.removeWhere((w) {
          if (!w.$1()) return false;
          w.$2.complete();
          return true;
        });
    host.onOutput = (line) {
      lines.add(line);
      if (line.startsWith('[ERROR]')) {
        errors.add(line);
        for (final (_, c) in _waits) {
          c.completeError(StateError(line));
        }
        _waits.clear();
      }
    };
  }

  Future<void> until(bool Function() test) {
    if (test()) return Future.value();
    final c = Completer<void>();
    _waits.add((test, c));
    return c.future;
  }

  /// What the person sees: each open construct, its identifier, the widget
  /// its last draw names and the view that draw carries.
  Map<int, String> screen() => {
        for (final c in host.ui.bridge.constructs)
          c.id: '${formatTerm(c.widget)} ${formatTerm(c.view)}',
      };

  /// Home's request form: the stream of the person's requests.
  OpenConstruct? form() {
    for (final c in host.ui.bridge.constructs) {
      if (c.id != 0 && formatTerm(c.widget).startsWith('input_box(menu(')) {
        return c;
      }
    }
    return null;
  }

  void request(GTerm r) {
    final f = form()!;
    host.ui.bridge.grant(f.id, questionAt(f.view)!, r);
  }
}

/// Copy the directory [from] into [to], file by file.
void _copy(Directory from, Directory to) {
  to.createSync(recursive: true);
  for (final e in from.listSync()) {
    final name = e.uri.pathSegments.lastWhere((s) => s.isNotEmpty);
    if (e is Directory) {
      _copy(e, Directory('${to.path}/$name'));
    } else if (e is File) {
      e.copySync('${to.path}/$name');
    }
  }
}

void main() {
  final programs = programsDir();
  late Directory dir;
  late String tree;

  setUpAll(() {
    dir = Directory('$programs/tests').createTempSync('glp_update_');
    tree = '${dir.path}/programs';
    Directory(tree).createSync();
    File('$programs/self.glp').copySync('$tree/self.glp');
    _copy(Directory('$programs/social'), Directory('$tree/social'));
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  test(
      'an update of the app restores the agent\'s state under the new version '
      'where the procedures its goals call keep their types, and refresh draws '
      'the person\'s screen as it was; an update that no longer defines one '
      'of them is refused, and the agent goes on under the version it '
      'terminated under', () async {
    final kp = generateKeyPair();
    final path = '${dir.path}/agents/alice.state';
    final home = File('$tree/social/graph/core/home.glp');

    _Launch launch() => _Launch(SuperAppHost(
          agentId: 'alice',
          program: '$tree/social/graph/core',
          rootSelfGlpPath: '$tree/self.glp',
          network: _Alone(kp),
          keyPair: kp,
          manifest: superappManifest,
          stateStore: FileAgentStateStore(path),
        ));

    // The first start, and the person opens the place.
    final first = launch();
    await first.host.start();
    expect(first.host.agent.firstStart, isTrue);
    await first.until(() => first.form() != null);
    first.request(const GAtom('open_place'));
    await first.host.idle;
    final seen = first.screen();
    expect(seen.length, greaterThan(1));
    expect(first.errors, isEmpty);
    expect(File('$path.program').existsSync(), isTrue,
        reason: 'the store keeps the compiled program the agent runs');
    first.host.dispose();

    // A new version: dispatch/6, which ran at the start and is called by no
    // goal of the resolvent, spawns its body's goals in another order.  Every
    // procedure keeps its type.
    const before = 'split_grants(PIn?, Inputs, MIn), '
        'merge_draws(ODs?, DOut?, POut), ';
    const after = 'merge_draws(ODs?, DOut?, POut), '
        'split_grants(PIn?, Inputs, MIn), ';
    final v1 = home.readAsStringSync();
    expect(v1, contains(before));
    home.writeAsStringSync(v1.replaceFirst(before, after));

    final second = launch();
    await second.host.start();
    expect(second.host.agent.firstStart, isFalse);
    expect(second.host.agent.updated, isTrue);
    expect(second.host.updateRefusal, isNull);
    await second.until(() => second.screen().length == seen.length);
    expect(second.screen(), seen,
        reason: 'the refresh draws the person\'s screen from the state '
            'restored under the new version');
    second.request(const GAtom('close_place'));
    await second.host.idle;
    // The layer starts empty at every launch, and at the restart the
    // runtime made the trust levels standing, open, to it again before any
    // goal was reduced (IGLP 0c77e47, Implementation Notes, "Persistence").
    expect((second.host.network as _Alone).declared,
        ['pan open', 'lan open', 'pan closed', 'lan closed']);
    final seen2 = second.screen();
    expect(second.errors, isEmpty);
    second.host.dispose();

    // A version that calls home's serve_1/7 serve_2/7: the goal that serves
    // the person's constructs calls a procedure the new version does not
    // define, and the update is refused.
    home.writeAsStringSync(
        home.readAsStringSync().replaceAll('serve_1(', 'serve_2('));

    final third = launch();
    await third.host.start();
    expect(third.host.agent.updated, isFalse);
    expect(third.host.updateRefusal, isNotNull);
    expect(third.host.updateRefusal!.reasons.join('; '),
        contains('serve_1/7 is not defined in the new version'));
    expect(third.lines.where((l) => l.startsWith('[INIT] The update is '
        'refused')), hasLength(1));
    await third.until(() => third.screen().length == seen2.length);
    expect(third.screen(), seen2,
        reason: 'the version the agent terminated under draws the screen');
    // The person goes on under that version: opening the place is taken
    // from the restored state.
    third.request(const GAtom('open_place'));
    await third.host.idle;
    // The trust levels standing at the termination, closed, are made to the
    // new layer at the restart, before the person's act.
    expect((third.host.network as _Alone).declared,
        ['pan closed', 'lan closed', 'pan open', 'lan open']);
    expect(third.errors, isEmpty);
    third.host.dispose();
  }, timeout: const Timeout(Duration(minutes: 5)));
}
