// The app's core, under its page (www/phone.html): the person's key, their
// contracts (contracts.dart), the names they give people, and the session of
// the contract open now --- the networking layer, the relay around it
// (relay_network.dart), and the person's agent (person_host.dart).  One
// contract runs at a time; opening another ends the session before.
//
// The layer is made by a factory, the phone's (Bluetooth and the local
// network, main.dart) or a test's (the headless layer on the loopback).
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'contracts.dart';
import 'person_host.dart';
import 'relay_network.dart';

/// A networking layer for a session: the network the agent runs over, and
/// starting and ending the layer.
abstract class Layer {
  glp.GlpNetwork get net;

  /// After the agent is bound to [net]: discovery and connections begin.
  Future<void> start();
  Future<void> dispose();
}

typedef LayerFactory = Future<Layer> Function();

/// A person the layer or the relay reports.
class Peer {
  final String key;
  final Set<String> discovered = {}, connected = {};
  Peer(this.key);

  String get state => connected.any((u) => u != 'wan')
      ? 'near'
      : connected.contains('wan')
          ? 'far'
          : 'found';
}

class Session {
  final Contract contract;
  final Core core;
  late final Layer layer;
  late final RelayGlpNetwork relay;
  late final PersonHost host;
  final Map<String, Peer> peers = {};
  final List<String> events = [];
  String phase = 'starting';

  Session(this.core, this.contract);

  void event(String s) {
    events.add('${DateTime.now().toIso8601String().substring(11, 19)} $s');
    if (events.length > 200) events.removeAt(0);
    core.changed();
  }

  Peer _peer(glp.PubKey pk) => peers.putIfAbsent(pk.hex, () => Peer(pk.hex));

  Future<void> start({String? invite}) async {
    try {
      layer = await core.layers();
      relay = RelayGlpNetwork(
        inner: layer.net,
        sodium: core.sodium,
        privateKey: core.privateKey,
        app: contract.name,
        dir: '${core.mine}/relay',
        relayUrl: core.relayUrl,
      );
      host = PersonHost(
        program: core.store.program(contract),
        rootSelfGlp: '${core.base}/programs/self.glp',
        keyPair: (pub: core.me, priv: core.privateKey),
        network: relay,
        stateFile: '${core.mine}/state/${contract.name}.bin',
      );
      final calendar = File('${host.program}/self.glp').readAsStringSync().contains('cal(alice, N)');
      host.onChange = core.changed;
      await host.start(calendar: calendar);
      event('agent: ${host.status}');
      host.agent.onPeerDiscovered = (pk, u) {
        _peer(pk).discovered.add(u.name);
        relay.putKnownPeer(pk);
        event('found ${pk.hex.substring(0, 8)}… over ${u.name}');
      };
      host.agent.onPeerConnected = (pk, u, _) {
        _peer(pk).connected.add(u.name);
        relay.putKnownPeer(pk);
        core.met(pk.hex, contract.title);
        event('connected ${pk.hex.substring(0, 8)}… over ${u.name}');
      };
      host.agent.onPeerDisconnected = (pk, u) {
        _peer(pk).connected.remove(u.name);
        event('lost ${pk.hex.substring(0, 8)}… over ${u.name}');
      };
      await layer.start();
      relay.onState = () => event('relay: ${relay.state}');
      relay.onAddedBy = (pk) {
        event('added from afar by ${pk.hex.substring(0, 8)}…');
        core.met(pk.hex, contract.title);
        core.notice('${core.nameOf(pk.hex)} added you');
      };
      relay.setOn(core.relayOn);
      phase = 'running';
      if (invite != null && invite != core.me.hex) await addFar(glp.PubKey.fromHex(invite));
    } catch (e) {
      phase = 'failed: $e';
      event('FAILED: $e');
    }
    core.changed();
  }

  Future<void> addFar(glp.PubKey pk) async {
    await relay.addPerson(pk);
    core.met(pk.hex, contract.title);
    event('added ${pk.hex.substring(0, 8)}… from afar');
    core.notice(relay.on
        ? 'Added ${core.nameOf(pk.hex)}. They will see that you added them.'
        : 'Turn on Internet to reach ${core.nameOf(pk.hex)}.');
  }

  List<Map<String, String>> people() => [
        for (final p in peers.values)
          if (p.discovered.isNotEmpty || p.connected.isNotEmpty)
            {'key': p.key, 'name': core.nameOf(p.key), 'state': p.state}
      ];

  Future<void> dispose() async {
    try {
      host.stop();
    } catch (_) {}
    try {
      relay.dispose();
    } catch (_) {}
    try {
      await layer.dispose();
    } catch (_) {}
  }
}

class Core {
  final String base;
  final ContractStore store;
  final SodiumSumo sodium;
  glp.PubKey me;
  Uint8List privateKey;
  final LayerFactory layers;

  /// A new key pair, made and kept as the device's identity; [renew] asks it.
  final Future<({glp.PubKey pub, Uint8List priv})> Function()? fresh;
  final String relayUrl;
  Session? session;

  Core({
    required this.base,
    required this.store,
    required this.sodium,
    required this.me,
    required this.privateKey,
    required this.layers,
    this.fresh,
    this.relayUrl = defaultRelay,
  }) {
    final f = File('$base/names.json');
    if (f.existsSync()) {
      try {
        names.addAll((jsonDecode(f.readAsStringSync()) as Map).cast<String, String>());
      } catch (_) {}
    }
  }

  // --- what changes, for the page's long poll ---

  int version = 0;
  final _waiters = <Completer<void>>[];

  void changed() {
    version++;
    for (final c in _waiters) {
      if (!c.isCompleted) c.complete();
    }
    _waiters.clear();
  }

  /// Until something changes after [since], or [max].
  Future<void> wait(int since, Duration max) async {
    if (version != since) return;
    final c = Completer<void>();
    _waiters.add(c);
    await c.future.timeout(max, onTimeout: () {});
  }

  /// Lines for the page to show for a moment, each with its number: the
  /// page shows those after the last it showed.
  final List<Map<String, Object>> notices = [];
  int _notice = 0;
  void notice(String s) {
    notices.add({'id': ++_notice, 'text': s});
    if (notices.length > 10) notices.removeAt(0);
    changed();
  }

  /// What is kept for the present identity alone: the people met over the
  /// relay, and each app's saved state.  A new identity starts with none.
  String get mine => '$base/people/${me.hex.substring(0, 16)}';

  /// Start over as a new person: a new key pair, and nothing of the old
  /// one's in use (it stays on the device, unused).
  Future<void> renew() async {
    final f = fresh;
    if (f == null) throw StateError('no way to make a key pair here');
    await close();
    final k = await f();
    me = k.pub;
    privateKey = k.priv;
    _apps = null;
    _met = null;
    changed();
  }

  // --- the person's apps: of the apps on the device, the ones this person
  // keeps (people/<key>/apps.json, by name, in the order added); a new
  // person has GKK alone, the rest a tap away in Discover ---

  List<String>? _apps;
  File get _appsFile => File('$mine/apps.json');

  List<String> get _names {
    if (_apps != null) return _apps!;
    try {
      _apps = (jsonDecode(_appsFile.readAsStringSync()) as List).cast<String>();
    } on Object {
      _apps = [for (final c in store.installed()) if (c.id == 'GKK') c.name].take(1).toList();
    }
    return _apps!;
  }

  void _saveApps() {
    _appsFile.parent.createSync(recursive: true);
    _appsFile.writeAsStringSync(jsonEncode(_names));
  }

  /// The person's apps, in the order added.
  List<Contract> apps() {
    final all = {for (final c in store.installed()) c.name: c};
    return [for (final n in _names) if (all[n] != null) all[n]!];
  }

  /// The apps on the device the person does not keep.
  List<Contract> others() {
    final mine = _names.toSet();
    return [for (final c in store.installed()) if (!mine.contains(c.name)) c];
  }

  void addApp(Contract c) {
    if (_names.contains(c.name)) return;
    _names.add(c.name);
    _saveApps();
    changed();
  }

  /// The app gone from the person's apps, and what they did in it with it.
  Future<void> removeApp(String name) async {
    if (session?.contract.name == name) await close();
    _names.remove(name);
    _saveApps();
    final f = File('$mine/state/$name.bin');
    if (f.existsSync()) f.deleteSync();
    changed();
  }

  // --- the people this person has met, in any app (people/<key>/people.json:
  // each key, when last met, and in which apps) ---

  Map<String, Map<String, Object?>>? _met;
  File get _metFile => File('$mine/people.json');

  Map<String, Map<String, Object?>> get _people {
    if (_met != null) return _met!;
    try {
      _met = (jsonDecode(_metFile.readAsStringSync()) as Map)
          .map((k, v) => MapEntry(k as String, (v as Map).cast<String, Object?>()));
    } on Object {
      _met = {};
    }
    return _met!;
  }

  void met(String key, String app) {
    if (key == me.hex) return;
    final p = _people.putIfAbsent(key, () => {'apps': <String>[]});
    p['seen'] = DateTime.now().toUtc().toIso8601String();
    final apps = (p['apps'] as List).cast<String>();
    if (!apps.contains(app)) p['apps'] = [...apps, app];
    _metFile.parent.createSync(recursive: true);
    _metFile.writeAsStringSync(jsonEncode(_people));
  }

  /// Everyone met, with the name given them here: the named first, by name.
  List<Map<String, Object?>> known() {
    // Those named before the list was kept are on it too.
    final keys = {..._people.keys, ...names.keys.where((k) => k != me.hex && RegExp(r'^[0-9a-f]{64}$').hasMatch(k))};
    final out = [
      for (final k in keys)
        {'key': k, 'name': nameOf(k), 'named': names.containsKey(k), 'seen': _people[k]?['seen'], 'apps': _people[k]?['apps'] ?? const []}
    ];
    out.sort((a, b) => a['named'] != b['named']
        ? (a['named'] == true ? -1 : 1)
        : (a['name'] as String).toLowerCase().compareTo((b['name'] as String).toLowerCase()));
    return out;
  }

  /// Forget a person met: off the list, and their name here.
  Future<void> forget(String key) async {
    _people.remove(key);
    _metFile.parent.createSync(recursive: true);
    _metFile.writeAsStringSync(jsonEncode(_people));
    await setName(key, '');
  }

  // --- names ---

  final Map<String, String> names = {};
  String nameOf(String key) => names[key] ?? (key == me.hex ? 'Me' : 'Person ${key.substring(0, 4)}');

  Future<void> setName(String key, String name) async {
    name = name.trim();
    if (name.isEmpty) {
      names.remove(key);
    } else {
      names[key] = name;
    }
    await File('$base/names.json').writeAsString(jsonEncode(names));
    changed();
  }

  // --- the relay's switch, kept across sessions ---

  File get _relayOff => File('$base/relay/off');
  bool get relayOn => !_relayOff.existsSync();

  Future<void> setRelay(bool on) async {
    if (on) {
      if (_relayOff.existsSync()) _relayOff.deleteSync();
    } else {
      _relayOff.parent.createSync(recursive: true);
      _relayOff.writeAsStringSync('off');
    }
    session?.relay.setOn(on);
    changed();
  }

  // --- contracts and sessions ---

  Future<Session> open(Contract c, {String? invite}) async {
    addApp(c);
    final s = session;
    if (s != null && s.contract.hash == c.hash) {
      if (invite != null && invite != me.hex && s.phase == 'running') {
        await s.addFar(glp.PubKey.fromHex(invite));
      }
      return s;
    }
    if (s != null) {
      session = null;
      await s.dispose();
    }
    final n = Session(this, c);
    session = n;
    changed();
    unawaited(n.start(invite: invite));
    return n;
  }

  Future<void> close() async {
    final s = session;
    session = null;
    await s?.dispose();
    changed();
  }

  /// What a pasted or opened link asks, done as far as it can be here: its
  /// contract got; the page then opens it with the person to add.
  Future<Map<String, Object?>> link(String text) async {
    final l = GasLink.parse(text);
    if (l == null) return {'error': 'That is not a GAS link.'};
    if (l.contract == null) return {'person': l.person};
    try {
      final c = await store.get(l.contract!);
      changed();
      return {'name': c.name, 'person': l.person};
    } on Object catch (e) {
      return {'error': 'Could not get the app: $e'};
    }
  }

  String invite() =>
      session == null ? 'https://talmonlab.bgu.ac.il/gas/add.html#${me.hex}' : inviteLink(me, session!.contract.hash);
}
