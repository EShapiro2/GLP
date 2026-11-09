/// A deployed agent's second start, headless, on the app's own host
/// ([SuperAppHost], lib/superapp_host.dart): the Grassroots App's agent over a
/// network in this process, with a store that keeps its local state across the
/// termination of the app (IGLP, Definition "Restart"; Implementation Notes,
/// "Persistence": "the runtime keeps it by a store the host gives it ... and a
/// restart resumes from the state the store holds").
///
/// At the first start the app sends the act that starts the Grassroots App,
/// `superapp(Me, [], [])`, and the person acts on home; the app terminates.  At
/// the second start, from the same store and under the same key, the app sends
/// no first act: the agent goes on from the state the store holds, and the
/// runtime grants refresh in the person's place, on which the dispatcher has
/// every construct, and construct 0, draw again what it last drew, so the
/// screen is drawn from the restored state (IGLP 7a25ff8, madglp-spec.tex, the
/// paragraph after Definition "Restart"; vGLP d433f71, Section 6.3).  The
/// person's screen after the refresh is the screen the person saw before the
/// termination: the same constructs, each with the same widget and the same
/// view.  Until 2026-11-06 the old branch GLP-rung4 tested a second start on
/// the agent in an isolate of its own (agent_entry_test.dart), which main's
/// entry replaced; this is its test on main's.
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

/// A layer with no peer: the agent alone, instantiated with its key pair,
/// both proximity underlays Closed until the program declares them open.
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
  Future<bool> declarePlace(String place, double radiusMetres) =>
      Future.value(false);
  @override
  Future<void> removePlace(String place) => Future.value();
}

/// One launch of the app: its host over a fresh layer, the agent's transcript,
/// and a wait on what the person sees.
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

void main() {
  final programs = programsDir();
  late Directory dir;

  setUpAll(() {
    dir = Directory('$programs/tests').createTempSync('glp_second_start_');
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  test(
      'a deployed agent\'s second start goes on from the state its store '
      'holds, the app sending no first act, and the runtime\'s refresh draws '
      'the person\'s screen as it was', () async {
    final kp = generateKeyPair();
    final path = '${dir.path}/agents/alice.state';

    _Launch launch() => _Launch(SuperAppHost(
          agentId: 'alice',
          program: '$programs/social/graph/core',
          rootSelfGlpPath: '$programs/self.glp',
          network: _Alone(kp),
          keyPair: kp,
          manifest: superappManifest,
          stateStore: FileAgentStateStore(path),
        ));

    // The first start: the app's first act starts the Grassroots App, home
    // is drawn, and the person opens the place (GPE:1).
    final first = launch();
    await first.host.start();
    expect(first.host.agent.firstStart, isTrue);
    await first.until(() => first.form() != null);
    first.request(const GAtom('open_place'));
    await first.host.idle;
    expect((first.host.network as _Alone).declared,
        ['pan closed', 'lan closed', 'pan open', 'lan open']);
    final seen = first.screen();
    expect(seen.keys, contains(0));
    expect(seen.length, greaterThan(1));
    expect(first.errors, isEmpty);
    first.host.dispose();

    // The second start, from the same store: no first act, and the screen,
    // which is not part of the local state, drawn again by the refresh the
    // runtime grants.
    final second = launch();
    expect(second.screen(), isEmpty);
    await second.host.start();
    expect(second.host.agent.firstStart, isFalse);
    expect(second.lines, contains('[INIT] Restored the agent\'s saved state'));
    expect(second.lines.where((l) => l.startsWith('> ')), isEmpty,
        reason: 'the app sends no act at a restart; the refresh is granted '
            'in the person\'s place');
    await second.until(() => second.screen().length == seen.length);
    expect(second.screen(), seen);
    expect(second.errors, isEmpty);

    // The person goes on where they were: the place is open, and closing it
    // is taken from the restored state.
    second.request(const GAtom('close_place'));
    await second.host.idle;
    // The layer starts empty at every launch, and at the restart the
    // runtime made the trust levels standing, open, to it again before any
    // goal was reduced (IGLP 0c77e47, Implementation Notes, "Persistence").
    expect((second.host.network as _Alone).declared,
        ['pan open', 'lan open', 'pan closed', 'lan closed']);
    expect(second.errors, isEmpty);
    second.host.dispose();
  }, timeout: const Timeout(Duration(minutes: 3)));
}
