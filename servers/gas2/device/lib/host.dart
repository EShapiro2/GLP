// GAS2 on a device: the person's super-app.
//
// One agent runs the Grassroots Super-App of GLP's programs/social/graph/core with
// GAS2's device harness (glp/gas2_device.glp), started by the person's first act
// gas2_device(Key).  The person's acts are the grants of the super-app's person
// channel, as the harness reads them; what it shows the person is kept here, in the
// shape the device's page reads (web/index.html).
//
// The mini-apps come with the device, as the directories GAS2 makes from the
// contracts (servers/gas2/tools/miniapp.py).  At first start the device certifies
// each one under the person's own key, as the REPL's :artefact does, so the person
// trusts their own compiler.  Two devices built from one tree give each mini-app one
// source identity, which the handshake compares.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart' show GlpEngine;
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/wire/artefact.dart' show Artefact;

import 'terms_json.dart';

/// A mini-app the device carries: its number once installed, its draws.
class MiniApp {
  final String app; // gas2_<id>
  final String id; // the contract
  final String title;
  Map<String, Object?> shapes;
  String version = ''; // its files, as a short digest: two who play must have the same
  int? n; // its number in the harness, once installed
  bool preparing = false; // being certified, for its install
  final Map<int, Object?> draws = {}; // construct id -> draw(Id, W, V) as JSON
  MiniApp(this.app, this.id, this.title, this.shapes);
}

class SuperHost {
  final String tree; // a copy of GLP's programs/, prepared by GAS2
  final ({PubKey pub, Uint8List priv}) keyPair;
  final GlpNetwork network;
  final String dataDir;

  /// The GAS2 site, whose live build's mini-apps are installed where it answers
  /// (/apps/<app>.json): two devices installing a mini-app then run one source,
  /// whichever build their app came with.
  final String? site;

  SuperHost({required this.tree, required this.keyPair, required this.network, required this.dataDir, this.site});

  String get me => keyPair.pub.hex;
  String get core => '$tree/social/graph/core';
  String get rootSelf => '$tree/self.glp';

  late final AgentRuntime agent;
  _Store? _store;
  bool get resumed => _store?.loaded ?? false;
  final List<MiniApp> apps = [];
  final List<String> friends = [];
  final List<String> friendRequests = [];
  final List<List<String>> introRequests = [];
  final List<String> log = [];

  /// The people near, over Bluetooth or the local network, where the device has
  /// those (the app on a phone): key -> the underlays over which they are connected.
  final Map<String, Set<String>> nearby = {};

  /// Told of each person found or connected near, by key (the relay is told, so
  /// that it reaches them when they go away).
  void Function(PubKey pk)? onNear;
  final List<String> notes = [];
  bool trusted = false;
  String status = 'starting';
  int version = 0;
  void Function()? onChange;
  Future<void> _queue = Future.value();

  final List<Completer<void>> _waiting = [];

  void _changed() {
    version++;
    onChange?.call();
    for (final c in _waiting) {
      if (!c.isCompleted) c.complete();
    }
    _waiting.clear();
  }

  /// Completes at the next change of what the page shows.
  Future<void> nextChange() {
    final c = Completer<void>();
    _waiting.add(c);
    return c.future;
  }

  void _note(String s) {
    notes.add(s);
    if (notes.length > 200) notes.removeAt(0);
  }

  /// The mini-apps the tree carries (apps.json, written by GAS2), and those the person
  /// added from the app maker (dataDir/made), each certified under the person's key
  /// where not already.
  Future<void> _prepareApps() async {
    final idx = [
      ...(jsonDecode(File('$tree/gas2_apps.json').readAsStringSync()) as List).cast<Map>(),
      for (final f in _madeFiles()) _unpack(jsonDecode(f.readAsStringSync()) as Map),
    ];
    for (final m in idx) {
      apps.add(MiniApp(m['app'] as String, m['id'] as String, m['title'] as String, (m['shapes'] as Map).cast<String, Object?>()));
      // certified at its first install (_certify); one certified before is put in place
      _kept(m['app'] as String);
      apps.last.version = _digest('$tree/apps/${m['app']}').padLeft(6, '0').substring(0, 6);
    }
  }

  /// A mini-app certified before, for this version of its files, put where the
  /// harness loads it: true where there was one.
  bool _kept(String app) {
    final kept = File('$dataDir/certified/$app.glpw');
    final stamp = File('$dataDir/certified/$app.stamp');
    if (kept.existsSync() && stamp.existsSync() && stamp.readAsStringSync() == '$me ${_digest('$tree/apps/$app')}') {
      kept.copySync('$core/$app.glpw');
      return true;
    }
    return false;
  }

  List<File> _madeFiles() {
    final d = Directory('$dataDir/made');
    if (!d.existsSync()) return [];
    return d.listSync().whereType<File>().where((f) => f.path.endsWith('.json')).toList()..sort((a, b) => a.path.compareTo(b.path));
  }

  /// A mini-app from the maker (server.py's /made/<key>.json) written into the tree; its index entry.
  Map _unpack(Map j) {
    final app = j['app'] as String;
    if (!RegExp(r'^gas2m_[a-z0-9_]+$').hasMatch(app)) throw ArgumentError('not a made mini-app: $app');
    final files = (j['files'] as Map).cast<String, String>();
    final d = Directory('$tree/apps/$app')..createSync(recursive: true);
    for (final e in files.entries) {
      if (!RegExp(r'^[a-z0-9_]+\.(glp|vglp|json)$').hasMatch(e.key)) throw ArgumentError('not a mini-app file: ${e.key}');
      File('${d.path}/${e.key}').writeAsStringSync(e.value);
    }
    return {'app': app, 'id': j['id'], 'title': j['title'], 'shapes': j['shapes']};
  }

  /// Certify [app] under the person's key, once per version of its files, and keep it.
  Future<void> _certify(String app) async {
    if (_kept(app)) return;
    final out = File('$core/$app.glpw');
    // in an isolate of its own: compiling a mini-app takes seconds (a minute, on a
    // phone), and the page and the network are served meanwhile
    final rs = rootSelf, src = '$tree/apps/$app', pub = keyPair.pub, priv = keyPair.priv;
    final bytes = await Isolate.run(() {
      final engine = GlpEngine(rootSelfGlpPath: rs, identity: PersonIdentity(pub, priv));
      engine.loadProgram(src);
      return (engine.appModule!.artefact as Artefact).toBytes();
    });
    out.writeAsBytesSync(bytes);
    final kept = File('$dataDir/certified/$app.glpw')..parent.createSync(recursive: true);
    out.copySync(kept.path);
    File('$dataDir/certified/$app.stamp').writeAsStringSync('$me ${_digest(src)}');
  }

  /// The site's current version of [m], where the site answers and it differs from
  /// the one here; the one here otherwise.
  Future<void> _fromSite(MiniApp m) async {
    final base = site;
    if (base == null || !m.app.startsWith('gas2_')) return;
    final c = HttpClient()..connectionTimeout = const Duration(seconds: 10);
    try {
      Map? j;
      for (var i = 0; j == null; i++) {
        try {
          final r = await (await c.getUrl(Uri.parse('$base/apps/${m.app}.json'))).close().timeout(const Duration(seconds: 20));
          if (r.statusCode != 200) return;
          j = jsonDecode(await utf8.decoder.bind(r).join()) as Map;
        } on Object {
          if (i == 2) rethrow; // a network's blip, twice more
          await Future<void>.delayed(const Duration(seconds: 1));
        }
      }
      final files = (j['files'] as Map).cast<String, String>();
      final d = Directory('$tree/apps/${m.app}');
      final same = files.entries.every((e) {
        final f = File('${d.path}/${e.key}');
        return f.existsSync() && f.readAsStringSync() == e.value;
      });
      if (same) return;
      for (final f in d.listSync().whereType<File>()) {
        f.deleteSync();
      }
      for (final e in files.entries) {
        if (RegExp(r'^[a-z0-9_]+\.(glp|vglp|json)$').hasMatch(e.key)) File('${d.path}/${e.key}').writeAsStringSync(e.value);
      }
      m.shapes = (j['shapes'] as Map).cast<String, Object?>();
      m.version = _digest(d.path).padLeft(6, '0').substring(0, 6);
      _note('${m.title}: the site\'s current version, ${m.version}');
    } on Object catch (e) {
      log.add('site: ${m.app}: $e'); // offline: the version the app came with
    } finally {
      c.close();
    }
  }

  /// Add a mini-app made by GAS2's app maker, from its link: the maker's page
  /// (…/maker.html?k=<key>) or its file (…/made/<key>.json).
  Future<void> addApp(String link) async {
    var u = Uri.parse(link.trim());
    final k = u.queryParameters['k'];
    if (k != null) u = u.resolve('made/$k.json');
    final c = HttpClient();
    try {
      final r = await (await c.getUrl(u)).close();
      if (r.statusCode != 200) throw StateError('the link answered ${r.statusCode}');
      final body = await utf8.decoder.bind(r).join();
      final j = jsonDecode(body) as Map;
      final m = _unpack(j);
      final app = m['app'] as String;
      if (apps.any((x) => x.app == app)) {
        _note('${m['id']} is here already');
        return;
      }
      final f = File('$dataDir/made/$app.json');
      f.parent.createSync(recursive: true);
      f.writeAsStringSync(body);
      apps.add(MiniApp(app, m['id'] as String, m['title'] as String, (m['shapes'] as Map).cast<String, Object?>())
        ..version = _digest('$tree/apps/$app').padLeft(6, '0').substring(0, 6));
      _note('added ${m['id']}, ${m['title']}, from the app maker');
    } catch (e) {
      _note('could not add the mini-app: $e');
    } finally {
      c.close();
      _changed();
    }
  }

  /// The files of a directory, as one number: their names and lengths and contents.
  static String _digest(String dir) {
    var h = 0;
    final fs = Directory(dir).listSync(recursive: true).whereType<File>().toList()
      ..sort((a, b) => a.path.compareTo(b.path));
    for (final f in fs) {
      // the names with '/' on every system, so that one mini-app has one digest everywhere
      for (final b in [...utf8.encode(f.path.substring(dir.length).replaceAll('\\', '/')), ...f.readAsBytesSync()]) {
        h = (h * 31 + b) & 0x3fffffff;
      }
    }
    return h.toRadixString(16);
  }

  Future<void> start() async {
    try {
      await _prepareApps();
      final store = _store = _Store('$dataDir/state.bin', this);
      agent = AgentRuntime(
        agentId: me,
        program: core,
        rootSelfGlpPath: rootSelf,
        keyPair: keyPair,
        network: network,
        stateStore: store,
      );
      agent.maxQuiescenceCycles = 20000000;
      agent.onOutput = (l) {
        log.add(l);
        if (log.length > 400) log.removeAt(0);
      };
      agent.onUserOutput = _shown;
      agent.onPeerDiscovered = (pk, u) {
        if (pk.hex == me) return;
        nearby.putIfAbsent(pk.hex, () => {});
        onNear?.call(pk);
        _changed();
      };
      agent.onPeerConnected = (pk, u, _) {
        if (pk.hex == me) return;
        nearby.putIfAbsent(pk.hex, () => {}).add(u.name);
        onNear?.call(pk);
        _changed();
      };
      agent.onPeerDisconnected = (pk, u) {
        nearby[pk.hex]?.remove(u.name);
        _changed();
      };
      status = 'starting the super-app';
      _changed();
      await agent.initialize();
      // A person who was here before goes on where they left off: the agent's state
      // and what the page shows were saved together (_Store).
      if (!store.loaded) await _send(rt.StructTerm('gas2_device', [rt.ConstTerm(me)]));
      status = 'ready';
      _up.complete();
    } catch (e, st) {
      status = 'failed: $e';
      stderr.writeln('$e\n$st');
      if (!_up.isCompleted) _up.completeError(e);
    }
    _changed();
  }

  // --- What the harness shows ---------------------------------------------------

  void _shown(rt.Term t) {
    final j = toJson(t);
    if (j is! Map) return;
    final f = j['f'], a = j['a'] as List;
    switch (f) {
      case 'me':
        break;
      case 'compiler':
        break;
      case 'installing':
        final n = a[0] as int, app = a[1] as String;
        for (final m in apps) {
          if (m.app == app) m.n = n;
        }
        _note('Installed ${_title(app)}');
        break;
      case 'not_a_module':
        _note('Not a mini-app: ${a[0]}');
        break;
      case 'friend_request':
        if (!friendRequests.contains(a[0])) friendRequests.add(a[0] as String);
        break;
      case 'intro_request':
        introRequests.add([a[0] as String, a[1] as String]);
        break;
      case 'answered':
        friendRequests.remove(a[0]);
        break;
      case 'told':
        final c = a[0];
        if (c is Map && c['f'] == 'connected') {
          final q = (c['a'] as List)[0] as String;
          if (!friends.contains(q)) friends.add(q);
          friendRequests.remove(q);
          _note('Friends with ${_short(q)}');
        } else if (c is Map && c['f'] == 'unfriended') {
          friends.remove((c['a'] as List)[0]);
        } else if (c is Map) {
          _note('${c['f']} ${(c['a'] as List).join(' ')}');
        } else {
          _note('$c');
        }
        break;
      case 'app':
        final n = a[0] as int;
        final m = apps.where((x) => x.n == n).firstOrNull;
        final s = a[2];
        if (m != null && s is Map && s['f'] == 'drawn') {
          final d = (s['a'] as List)[0] as Map;
          final da = d['a'] as List;
          if (d['f'] == 'draw') {
            m.draws[da[0] as int] = {'w': da[1], 'v': da[2]};
          } else if (d['f'] == 'withdraw') {
            m.draws.remove(da[0] as int);
          }
        }
        break;
    }
    _changed();
  }

  String _title(String app) => apps.where((m) => m.app == app).map((m) => m.title).firstOrNull ?? app;
  static String _short(String k) => k.length > 10 ? '${k.substring(0, 6)}…' : k;

  // --- The person's acts ------------------------------------------------------------

  /// The person's acts wait until the super-app is up (start): one injected before
  /// its agent is initialised would be lost.
  final Completer<void> _up = Completer<void>();
  final List<DateTime> _pending = [];

  Future<void> _inject(rt.Term t) async {
    final at = DateTime.now();
    _pending.add(at);
    try {
      await _up.future;
      await _send(t);
    } finally {
      _pending.remove(at);
    }
  }

  Future<void> _send(rt.Term t) {
    final f = _queue.then((_) => agent.injectUserInput(t));
    _queue = f.catchError((_) {});
    return f;
  }

  rt.Term _in(int code, List<int> path, rt.Term v) => rt.StructTerm('input', [
        rt.ConstTerm(code),
        listOf([for (final x in path) rt.ConstTerm(x)]),
        v,
      ]);

  rt.Term _c(String f, List<Object> args) =>
      rt.StructTerm(f, [for (final x in args) x is rt.Term ? x : rt.ConstTerm(x)]);

  Future<void> trustMyCompiler() async {
    await _inject(_in(0, [], _c('trust', [me])));
    trusted = true;
    _changed();
  }

  /// Install [app]: the person's compiler trusted first, where not yet (the mini-apps
  /// here are certified under the person's own key), and the mini-app certified.
  Future<void> install(String app) async {
    final m = apps.firstWhere((x) => x.app == app);
    if (m.preparing || m.n != null) return;
    if (!trusted) await trustMyCompiler();
    m.preparing = true;
    _changed();
    try {
      await _fromSite(m);
      await _certify(app);
    } catch (e) {
      _note('could not prepare ${m.title}: $e');
      return;
    } finally {
      m.preparing = false;
      _changed();
    }
    await _inject(_in(0, [], _c('install', [app, '$app.glpw'])));
  }
  Future<void> connect(String key) => _inject(_in(0, [], _c('connect', [key])));
  Future<void> invite(String app, String key) => _inject(_in(0, [], _c('invite', [app, key])));
  Future<void> accept(String app, String key) => _inject(_in(1, [], _c('invite', [app, key])));
  Future<void> answerFriend(String key, bool yes) async {
    friendRequests.remove(key);
    await _inject(_in(yes ? 2 : 3, [], _c('connect', [key])));
  }

  Future<void> answerIntro(String other, bool yes) async {
    introRequests.removeWhere((r) => r[1] == other);
    await _inject(_in(yes ? 4 : 5, [], _c('connect', [other])));
  }

  /// A grant to mini-app [app]: input(Id, Path, Value), written as keys the
  /// mini-app's own live person reads back (miniapp.py, live_glp).
  Future<void> grant(String app, int id, List<int> path, Object? value) async {
    final m = apps.firstWhere((x) => x.app == app);
    final n = m.n;
    if (n == null) throw StateError('$app is not installed');
    final toks = <String>['g', ..._int(id), for (final p in path) ..._int(p), '/', ..._value(value)];
    for (final t in toks) {
      await _inject(_in(7, [n], _c('connect', [t])));
    }
  }

  static List<String> _int(int n) => [...n.toString().split(''), '.'];

  /// A value as keys: yes, no, refresh; a proposal {"propose": {"f": act, "a": [...]}};
  /// an argument a string, an integer, or {"f": constructor, "a": [...]}.
  static List<String> _value(Object? v) {
    if (v is String) return [v];
    if (v is Map && v.containsKey('propose')) return ['propose', ..._arg(v['propose'])];
    throw ArgumentError('not a grant value: $v');
  }

  static List<String> _arg(Object? v) {
    if (v is int) return _int(v);
    if (v is String) return [v];
    if (v is Map) return [v['f'] as String, for (final a in (v['a'] as List? ?? [])) ..._arg(a)];
    throw ArgumentError('not an argument: $v');
  }

  // --- For the page -------------------------------------------------------------------

  Map<String, Object?> state({bool debug = false}) => {
        'version': version,
        'status': status,
        'me': me,
        'trusted': trusted,
        'friends': friends,
        'friendRequests': friendRequests,
        'introRequests': introRequests,
        'nearby': [
          for (final e in nearby.entries) {'key': e.key, 'over': e.value.toList()}
        ],
        'notes': notes.reversed.take(30).toList(),
        if (debug || Platform.environment['GAS2_DEBUG'] != null) ...{
          'log': log.reversed.take(80).toList(),
          'pending': [for (final d in _pending) DateTime.now().difference(d).inSeconds],
        },
        'apps': [
          for (final m in apps)
            {
              'app': m.app,
              'id': m.id,
              'title': m.title,
              'installed': m.n != null,
              'preparing': m.preparing,
              'version': m.version,
              'draws': {for (final e in m.draws.entries) '${e.key}': e.value},
              'shapes': m.shapes,
            }
        ],
      };

  /// One act of the page: {"op": ..., ...}.
  /// One of the page's acts; what goes wrong is told the person (Lately).
  Future<void> act(Map j) async {
    try {
      await _act(j);
    } catch (e) {
      _note('${j['op']} did not go through: $e');
      _changed();
      rethrow;
    }
  }

  Future<void> _act(Map j) async {
    switch (j['op']) {
      case 'trust':
        return trustMyCompiler();
      case 'install':
        return install(j['app'] as String);
      case 'connect':
        return connect(j['key'] as String);
      case 'invite':
        return invite(j['app'] as String, j['key'] as String);
      case 'accept':
        return accept(j['app'] as String, j['key'] as String);
      case 'friend':
        return answerFriend(j['key'] as String, j['yes'] == true);
      case 'intro':
        return answerIntro(j['key'] as String, j['yes'] == true);
      case 'add':
        return addApp(j['link'] as String);
      case 'grant':
        return grant(j['app'] as String, j['id'] as int, (j['path'] as List).cast<int>(), j['value']);
    }
    throw ArgumentError('no such act: ${j['op']}');
  }
}

/// The agent's state, saved in each event cycle (AgentStateStore), with what the
/// page shows at that moment, so that the two are always of one moment, as GAS's
/// phone keeps them (servers/gas/core/lib/person_host.dart).
class _Store implements AgentStateStore {
  final String path;
  final SuperHost host;
  bool loaded = false;
  _Store(this.path, this.host);

  @override
  Future<Uint8List?> load() async {
    final f = File(path);
    if (!f.existsSync()) return null;
    try {
      final b = await f.readAsBytes();
      if (b.length < 8 || ascii.decode(b.sublist(0, 4)) != 'GAS2') return null;
      final n = ByteData.sublistView(b, 4, 8).getUint32(0);
      final v = jsonDecode(utf8.decode(b.sublist(8, 8 + n))) as Map<String, dynamic>;
      host.trusted = v['trusted'] == true;
      host.friends..clear()..addAll((v['friends'] as List).cast<String>());
      host.friendRequests..clear()..addAll((v['friendRequests'] as List).cast<String>());
      host.introRequests..clear()..addAll([for (final r in v['introRequests'] as List) (r as List).cast<String>()]);
      host.notes..clear()..addAll((v['notes'] as List).cast<String>());
      final apps = v['apps'] as Map<String, dynamic>;
      for (final m in host.apps) {
        final a = apps[m.app] as Map<String, dynamic>?;
        if (a == null) continue;
        m.n = a['n'] as int?;
        m.draws..clear()..addAll({for (final e in (a['draws'] as Map).entries) int.parse(e.key as String): e.value});
      }
      loaded = true;
      return Uint8List.sublistView(b, 8 + n);
    } on Object {
      return null; // unreadable: a fresh start
    }
  }

  @override
  Future<void> save(Uint8List state) async {
    final view = utf8.encode(jsonEncode({
      'trusted': host.trusted,
      'friends': host.friends,
      'friendRequests': host.friendRequests,
      'introRequests': host.introRequests,
      'notes': host.notes,
      'apps': {
        for (final m in host.apps)
          m.app: {'n': m.n, 'draws': m.draws.map((k, v) => MapEntry('$k', v))}
      },
    }));
    final head = ByteData(8)
      ..setUint8(0, 0x47)
      ..setUint8(1, 0x41)
      ..setUint8(2, 0x53)
      ..setUint8(3, 0x32)
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
