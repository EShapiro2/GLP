/// Characterization + migration tests for AgentRuntime's madGLP message path
/// (Issue 8). Two AgentRuntimes are booted headless; one sends, the other
/// receives, and the surfaced callbacks (onSendMadMessage, onMadMessageReceived,
/// onOutput) are asserted. These pin the behavior across the GlpNetwork
/// migration: the external (to/from + opaque payload) contract is unchanged.

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';

/// A minimal 2-agent play: agent `a` cold-calls `b` with `ping`; `b` receives
/// it on its network input and emits `got(ping)` to its user output.
const String _play = r'''
-mode(system).

procedure a_init(_?, _?).
a_init(_, _) :- send_to_net([msg(b, ping)]).

procedure b_init(_?, _?).
b_init(_, [msg(_, M) | _]) :- send_to_user([got(M?)]).
''';

void main() {
  group('AgentRuntime madGLP message path (Issue 8)', () {
    late String root;
    setUp(() {
      root = File('../programs/self.glp').absolute.path;
    });

    /// Boot agents `a` and `b`, route messages between them until quiescent,
    /// and return the captured user-output lines per agent.
    Future<Map<String, List<String>>> runColdCall() async {
      final out = <String, List<String>>{'a': [], 'b': []};
      final pending = <String, List<(String, Uint8List)>>{};

      AgentRuntime mk(String id, String goal) {
        final a = AgentRuntime(
          agentId: id,
          glpSources: const [_play],
          rootSelfGlpPath: root,
          goalLabel: goal,
        );
        a.onOutput = (line) => out[id]!.add(line);
        a.onSendMadMessage = (to, payload) async => pending
            .putIfAbsent(to, () => [])
            .add((id, Uint8List.fromList(payload)));
        return a;
      }

      final a = mk('a', 'a_init/2');
      final b = mk('b', 'b_init/2');
      final agents = {'a': a, 'b': b};

      await a.initialize();
      await b.initialize();

      var rounds = 0;
      while (pending.isNotEmpty && rounds < 20) {
        rounds++;
        final snapshot =
            Map<String, List<(String, Uint8List)>>.from(pending);
        pending.clear();
        for (final entry in snapshot.entries) {
          final dest = agents[entry.key];
          if (dest == null) continue;
          for (final (from, payload) in entry.value) {
            await dest.onMadMessageReceived(from, payload);
          }
        }
      }
      return out;
    }

    test('a cold-calls b; b receives and surfaces the value', () async {
      final out = await runColdCall();
      // The user output line is the `< ` prefixed form from _output.
      expect(out['b']!.any((l) => l.contains('got(ping)')), isTrue,
          reason: 'b should receive ping and emit got(ping); got ${out['b']}');
    });

    test('router connectivity events are forwarded to client callbacks',
        () async {
      final agent = AgentRuntime(
        agentId: 'a',
        glpSources: const [_play],
        rootSelfGlpPath: root,
        goalLabel: 'a_init/2',
      );
      final events = <String>[];
      final pk = generateKeyPair().pub;
      agent.onPeerDiscovered = (p) => events.add('discovered');
      agent.onPeerConnected = (p, t) => events.add('connected:$t');
      agent.onPeerDisconnected = (p, t) => events.add('disconnected');

      agent.onConnectivityEvent(pk, Transport.ble, ConnectivityEvent.discovered);
      agent.onConnectivityEvent(pk, Transport.ble, ConnectivityEvent.connected);
      agent.onConnectivityEvent(
          pk, Transport.ble, ConnectivityEvent.disconnected);

      expect(events,
          ['discovered', 'connected:Transport.ble', 'disconnected']);
    });
  });
}
