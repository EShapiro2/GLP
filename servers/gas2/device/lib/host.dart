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
  final Map<String, Object?> shapes;
  int? n; // its number in the harness, once installed
  final Map<int, Object?> draws = {}; // construct id -> draw(Id, W, V) as JSON
  MiniApp(this.app, this.id, this.title, this.shapes);
}

class SuperHost {
  final String tree; // a copy of GLP's programs/, prepared by GAS2
  final ({PubKey pub, Uint8List priv}) keyPair;
  final GlpNetwork network;
  final String dataDir;

  SuperHost({required this.tree, required this.keyPair, required this.network, required this.dataDir});

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
  final List<String> notes = [];
  bool trusted = false;
  String status = 'starting';
  int version = 0;
  void Function()? onChange;
  Future<void> _queue = Future.value();

  void _changed() {
    version++;
    onChange?.call();
  }

  void _note(String s) {
    notes.add(s);
    if (notes.length > 200) notes.removeAt(0);
  }

  /// The mini-apps the tree carries (apps.json, written by GAS2), each certified
  /// under the person's key where not already.
  Future<void> _prepareApps() async {
    final idx = jsonDecode(File('$tree/gas2_apps.json').readAsStringSync()) as List;
    final identity = PersonIdentity(keyPair.pub, keyPair.priv);
    for (final a in idx) {
      final m = a as Map;
      final app = m['app'] as String;
      apps.add(MiniApp(app, m['id'] as String, m['title'] as String, (m['shapes'] as Map).cast<String, Object?>()));
      // certified once per person and per version of the mini-app's files, and kept
      final out = File('$core/$app.glpw');
      final kept = File('$dataDir/certified/$app.glpw');
      final stamp = File('$dataDir/certified/$app.stamp');
      final version = '$me ${_digest('$tree/apps/$app')}';
      if (kept.existsSync() && stamp.existsSync() && stamp.readAsStringSync() == version) {
        kept.copySync(out.path);
        continue;
      }
      status = 'certifying ${m['title']}';
      _changed();
      final engine = GlpEngine(rootSelfGlpPath: rootSelf, identity: identity);
      engine.loadProgram('$tree/apps/$app');
      final art = engine.appModule!.artefact as Artefact;
      out.writeAsBytesSync(art.toBytes());
      kept.parent.createSync(recursive: true);
      out.copySync(kept.path);
      stamp.writeAsStringSync(version);
    }
  }

  /// The files of a directory, as one number: their names and lengths and contents.
  static String _digest(String dir) {
    var h = 0;
    final fs = Directory(dir).listSync(recursive: true).whereType<File>().toList()
      ..sort((a, b) => a.path.compareTo(b.path));
    for (final f in fs) {
      for (final b in [...utf8.encode(f.path.substring(dir.length)), ...f.readAsBytesSync()]) {
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
      status = 'starting the super-app';
      _changed();
      await agent.initialize();
      // A person who was here before goes on where they left off: the agent's state
      // and what the page shows were saved together (_Store).
      if (!store.loaded) await _inject(rt.StructTerm('gas2_device', [rt.ConstTerm(me)]));
      status = 'ready';
    } catch (e, st) {
      status = 'failed: $e';
      stderr.writeln('$e\n$st');
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

  Future<void> _inject(rt.Term t) {
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

  Future<void> install(String app) => _inject(_in(0, [], _c('install', [app, '$app.glpw'])));
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

  Map<String, Object?> state() => {
        'version': version,
        'status': status,
        'me': me,
        'trusted': trusted,
        'friends': friends,
        'friendRequests': friendRequests,
        'introRequests': introRequests,
        'notes': notes.reversed.take(30).toList(),
        if (Platform.environment['GAS2_DEBUG'] != null) 'log': log.reversed.take(60).toList(),
        'apps': [
          for (final m in apps)
            {
              'app': m.app,
              'id': m.id,
              'title': m.title,
              'installed': m.n != null,
              'draws': {for (final e in m.draws.entries) '${e.key}': e.value},
              'shapes': m.shapes,
            }
        ],
      };

  /// One act of the page: {"op": ..., ...}.
  Future<void> act(Map j) async {
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
