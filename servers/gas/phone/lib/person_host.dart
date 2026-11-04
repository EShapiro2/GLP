// One person of a GAS app on this device: the person's agent of madGLP,
// running the contract's live harness (live_person/3, told me(Key) first)
// over the network it is given, and what the person's dispatcher draws, in
// the shape the GAS app page reads (servers/gas/web/app.html): the state of
// one person, and the person's acts.
//
// The phone gives it the real networking layer (GrassrootsGlpNetwork); the
// test gives it the headless one (HeadlessGlpNetwork).  The person is named by
// the key of the layer it runs over, as every agent over a real network is.
//
// Given a state file, the person goes on where they left off: the agent saves
// its local state in each event cycle (AgentStateStore), and the file holds,
// with it, what the dispatcher had drawn by then --- the agent draws nothing
// again at restart --- so the two are always of the same moment.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
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

  /// Where the person's state is kept between runs; none, a fresh start each run.
  final String? stateFile;

  PersonHost({
    required this.program,
    required this.rootSelfGlp,
    required this.keyPair,
    required this.network,
    this.stateFile,
  });

  /// Whether this run went on from a saved state.
  bool resumed = false;
  _Store? _store;

  /// This run is over: nothing more of it is saved, so the next run of the
  /// same person, which may start before a cycle of this one ends, is not
  /// overwritten by it.
  void stop() => _store?.closed = true;

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
    final store = _store = stateFile == null ? null : _Store(stateFile!, this);
    agent = AgentRuntime(
      agentId: me,
      program: program,
      rootSelfGlpPath: rootSelfGlp,
      keyPair: keyPair,
      network: network,
      stateStore: store,
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
      resumed = store?.loaded ?? false;
      if (!resumed) {
        // The host's first act says which agent this is: this person alone.
        await agent.injectUserInput(rt.StructTerm('me', [rt.ConstTerm(me)]));
        if (calendar) await nextDay();
      }
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

/// The agent's saved state and the drawing of the same moment, in one file:
/// "GAS1", the drawing's length (4 bytes, big-endian), the drawing as JSON,
/// then the agent's state.  A save writes a file beside it and renames it
/// over the file, so a termination during the save leaves the one before.
class _Store implements AgentStateStore {
  final String path;
  final PersonHost host;
  bool loaded = false, closed = false;
  _Store(this.path, this.host);

  @override
  Future<Uint8List?> load() async {
    final f = File(path);
    if (!f.existsSync()) return null;
    try {
      final b = await f.readAsBytes();
      if (b.length < 8 || ascii.decode(b.sublist(0, 4)) != 'GAS1') return null;
      final n = ByteData.sublistView(b, 4, 8).getUint32(0);
      final view = jsonDecode(utf8.decode(b.sublist(8, 8 + n))) as Map<String, dynamic>;
      host.draws
        ..clear()
        ..addAll({for (final e in (view['draws'] as Map).entries) int.parse(e.key as String): e.value});
      host.day = view['day'] as int?;
      host.net
        ..clear()
        ..addAll(view['net'] as List);
      loaded = true;
      return Uint8List.sublistView(b, 8 + n);
    } on Object {
      return null; // unreadable: a fresh start
    }
  }

  @override
  Future<void> save(Uint8List state) async {
    if (closed) return;
    final view = utf8.encode(jsonEncode({
      'draws': host.draws.map((k, v) => MapEntry('$k', v)),
      'day': host.day,
      'net': host.net.length > 60 ? host.net.sublist(host.net.length - 60) : host.net,
    }));
    final head = ByteData(8)
      ..setUint8(0, 0x47)
      ..setUint8(1, 0x41)
      ..setUint8(2, 0x53)
      ..setUint8(3, 0x31)
      ..setUint32(4, view.length);
    final f = File(path);
    f.parent.createSync(recursive: true);
    final tmp = File('$path.new');
    final out = BytesBuilder(copy: false)
      ..add(head.buffer.asUint8List())
      ..add(view)
      ..add(state);
    await tmp.writeAsBytes(out.takeBytes(), flush: true);
    await tmp.rename(path);
  }
}
