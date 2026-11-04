// One person of a GAS app on this device: the person's agent of madGLP,
// running the contract's live harness (live_person/3, told me(Key) first)
// over the network it is given, and what the person's dispatcher draws, in
// the shape the GAS app page reads (servers/gas/web/app.html): the state of
// one person, and the person's acts.
//
// The phone gives it the real networking layer (GrassrootsGlpNetwork); the
// test gives it the headless one (HeadlessGlpNetwork).  The person is named by
// the key of the layer it runs over, as every agent over a real network is.
import 'dart:async';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

import 'terms_json.dart';

class PersonHost {
  /// The harness's directory, programs/vglp/cva/gas_worlds/live_CONTRACT.
  final String program;

  /// programs/self.glp of the tree the harness is in.
  final String rootSelfGlp;
  final ({PubKey pub, Uint8List priv}) keyPair;
  final GlpNetwork network;

  PersonHost({
    required this.program,
    required this.rootSelfGlp,
    required this.keyPair,
    required this.network,
  });

  /// The person's name: the key, as 64 lowercase hex characters.
  String get me => keyPair.pub.hex;

  late final AgentRuntime agent;
  final Map<int, Object?> draws = {};
  final List<Object?> net = [];
  final List<String> log = [];
  int version = 0;
  int? day;
  String status = 'starting';
  bool calendar = false;

  /// Called on every change of the state.
  void Function()? onChange;

  void _changed() {
    version++;
    onChange?.call();
  }

  void _note(String line) {
    log.add(line);
    if (log.length > 300) log.removeAt(0);
  }

  Future<void> start({bool calendar = false}) async {
    this.calendar = calendar;
    agent = AgentRuntime(
      agentId: me,
      program: program,
      rootSelfGlpPath: rootSelfGlp,
      keyPair: keyPair,
      network: network,
    );
    agent.maxQuiescenceCycles = 20000000;
    agent.onOutput = _note;
    agent.onUserOutput = (t) {
      final j = toJson(t);
      if (j is Map && j['f'] == 'd') {
        final draw = (j['a'] as List)[1];
        if (draw is Map && draw['f'] == 'draw') {
          final d = draw['a'] as List;
          draws[d[0] as int] = d[2];
        } else if (draw is Map && draw['f'] == 'withdraw') {
          draws.remove((draw['a'] as List)[0] as int);
        }
      } else if (j is Map && j['f'] == 'net') {
        net.add((j['a'] as List)[0]);
        if (net.length > 300) net.removeAt(0);
      }
      _changed();
    };
    try {
      await agent.initialize();
      // The host's first act says which agent this is: this person alone.
      await agent.injectUserInput(rt.StructTerm('me', [rt.ConstTerm(me)]));
      if (calendar) await nextDay();
      status = 'ready';
    } catch (e) {
      status = 'failed: $e';
    }
    _changed();
  }

  /// The person's act on construct [id] at [path]: the grant [value].
  Future<void> act(int id, List<int> path, Object? value) async {
    status = 'working';
    _changed();
    await agent.injectUserInput(rt.StructTerm('g', [
      rt.StructTerm('input', [
        rt.ConstTerm(id),
        listOf([for (final x in path) rt.ConstTerm(x)]),
        fromJson(value),
      ]),
    ]));
    status = 'ready';
    _changed();
  }

  /// The next day of the calendar, where the contract reads one.
  Future<void> nextDay() async {
    if (!calendar) return;
    day = (day ?? 0) + 1;
    await agent.injectUserInput(rt.StructTerm('cal', [rt.ConstTerm(day!)]));
    _changed();
  }

  /// The state, in the shape the app page reads: one person, this one.
  Map<String, Object?> state() => {
        'version': version,
        'status': status,
        if (day != null) 'day': day,
        'me': me,
        'persons': {me: draws.map((k, v) => MapEntry('$k', v))},
        'net': net.length > 60 ? net.sublist(net.length - 60) : net,
      };
}
