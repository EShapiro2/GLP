// GAS on a phone: one person, the phone's owner, running one grassroots
// contract's canonical platform as an agent of madGLP over GNA's networking
// layer --- Bluetooth (PAN) and the local network (LAN) --- named by the
// layer's key.  The screen is the GAS app page for one person, served by the
// app's own server on the phone's loopback; the people the layer finds near
// the phone are the persons the page offers.  A debug panel shows what the
// layer sees.
//
// The first test bed of madGLP on phones: no rendezvous server yet (WAN), no
// attestation (every peer reported unattested), first contact Open on both
// proximity underlays.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart' show rootBundle;
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking/grassroots_networking.dart';
import 'package:grassroots_networking_glue/grassroots_glp_network.dart';
import 'package:path_provider/path_provider.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';
import 'package:webview_flutter/webview_flutter.dart';

import 'local_server.dart';
import 'person_host.dart';

Future<void> main() async {
  WidgetsFlutterBinding.ensureInitialized();
  runApp(const MaterialApp(
    title: 'GAS',
    debugShowCheckedModeBanner: false,
    home: Boot(),
  ));
}

/// The bundle (tool/sync_assets.sh) copied into the app's documents
/// directory, a real path the GLP engine reads: programs/ and www/.
Future<String> unbundle() async {
  final docs = await getApplicationDocumentsDirectory();
  final base = '${docs.path}/gas';
  final manifest = await rootBundle.loadString('assets/manifest.txt', cache: false);
  for (final line in manifest.split('\n')) {
    final tab = line.indexOf('\t');
    if (tab < 0) continue;
    final data = await rootBundle.load('assets/bundle/${line.substring(0, tab)}');
    final out = File('$base/${line.substring(tab + 1)}');
    await out.parent.create(recursive: true);
    await out.writeAsBytes(data.buffer.asUint8List(data.offsetInBytes, data.lengthInBytes));
  }
  return base;
}

/// The contracts this build carries: id (lower case) and title, from the
/// site's specs.
Future<List<(String, String)>> contracts(String base) async {
  final out = <(String, String)>[];
  final dir = Directory('$base/www/specs');
  for (final f in dir.listSync().whereType<File>()) {
    final id = f.uri.pathSegments.last.replaceAll('.json', '');
    if (!Directory('$base/programs/vglp/cva/gas_worlds/live_$id').existsSync()) continue;
    final spec = jsonDecode(f.readAsStringSync()) as Map;
    out.add((id, '${spec['id']}: ${spec['title']}'));
  }
  out.sort((a, b) => a.$1.compareTo(b.$1));
  return out;
}

class Boot extends StatefulWidget {
  const Boot({super.key});
  @override
  State<Boot> createState() => _BootState();
}

class _BootState extends State<Boot> {
  String? base;
  List<(String, String)> list = [];
  String? error;

  @override
  void initState() {
    super.initState();
    () async {
      try {
        final b = await unbundle();
        final l = await contracts(b);
        setState(() {
          base = b;
          list = l;
        });
      } catch (e) {
        setState(() => error = '$e');
      }
    }();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('GAS · pick a contract')),
      body: error != null
          ? Padding(padding: const EdgeInsets.all(16), child: Text('Could not unpack: $error'))
          : base == null
              ? const Center(child: CircularProgressIndicator())
              : ListView(children: [
                  for (final (id, title) in list)
                    ListTile(
                      title: Text(title),
                      trailing: const Icon(Icons.chevron_right),
                      onTap: () => Navigator.of(context).pushReplacement(MaterialPageRoute(
                          builder: (_) => Running(base: base!, contract: id, title: title))),
                    ),
                ]),
    );
  }
}

/// A person the layer reports: when it was found or connected, and over which
/// underlays it is reachable.
class Peer {
  final String key;
  final Set<String> discovered = {};
  final Set<String> connected = {};
  Peer(this.key);
}

class Running extends StatefulWidget {
  final String base, contract, title;
  const Running({super.key, required this.base, required this.contract, required this.title});
  @override
  State<Running> createState() => _RunningState();
}

class _RunningState extends State<Running> {
  PersonHost? host;
  GrassrootsNetwork? layer;
  LocalServer? server;
  WebViewController? web;
  final Map<String, Peer> peers = {};
  final List<String> events = [];
  String phase = 'starting';
  bool layerUp = false;

  void event(String s) {
    final t = DateTime.now().toIso8601String().substring(11, 19);
    events.add('$t $s');
    if (events.length > 200) events.removeAt(0);
    if (mounted) setState(() {});
  }

  Peer peer(Uint8List pk) {
    final key = glp.PubKey.fromList(pk).hex;
    return peers.putIfAbsent(key, () => Peer(key));
  }

  @override
  void initState() {
    super.initState();
    unawaited(_start());
  }

  Future<void> _start() async {
    try {
      phase = 'loading the network layer';
      event(phase);
      final sodium = await SodiumSumoInit.init();
      final identity = await IdentityStore.getIdentity() ??
          await () async {
            final id = await GrassrootsIdentity.generate();
            await IdentityStore.putIdentity(id);
            return id;
          }();
      final store = Store<AppState>(appReducer, initialState: AppState());
      final l = GrassrootsNetwork(
        identity: identity,
        underlays: {Underlay.pan, Underlay.lan},
        store: store,
        sodium: sodium,
      );
      layer = l;
      final net = GrassrootsGlpNetwork(l);
      final h = PersonHost(
        program: '${widget.base}/programs/vglp/cva/gas_worlds/live_${widget.contract}',
        rootSelfGlp: '${widget.base}/programs/self.glp',
        keyPair: (pub: glp.PubKey.fromList(identity.publicKey), priv: Uint8List.fromList(identity.privateKey)),
        network: net,
      );
      host = h;
      event('my key ${h.me.substring(0, 8)}…');
      phase = 'starting the agent (loading the platform)';
      event(phase);
      final calendar = File('${h.program}/self.glp').readAsStringSync().contains('cal(alice, N)');
      // The agent is bound to its layer before the layer starts, so that no
      // message the layer delivers finds no agent.
      await h.start(calendar: calendar);
      event('agent: ${h.status}');
      h.agent.onPeerDiscovered = (pk, underlay) {
        peer(pk.bytes).discovered.add(underlay.name);
        net.putKnownPeer(pk);
        event('found ${pk.hex.substring(0, 8)}… over ${underlay.name}');
      };
      h.agent.onPeerConnected = (pk, underlay, attested) {
        peer(pk.bytes).connected.add(underlay.name);
        net.putKnownPeer(pk);
        event('connected ${pk.hex.substring(0, 8)}… over ${underlay.name}');
      };
      h.agent.onPeerDisconnected = (pk, underlay) {
        peer(pk.bytes).connected.remove(underlay.name);
        event('lost ${pk.hex.substring(0, 8)}… over ${underlay.name}');
      };
      h.onChange = () {
        if (mounted) setState(() {});
      };

      phase = 'starting Bluetooth and the local network';
      event(phase);
      final ok = await l.initialize();
      event('layer initialized: $ok');
      await l.pan?.setTrustLevel(ColdCallTrustLevel.open);
      await l.lan?.setTrustLevel(ColdCallTrustLevel.open);
      await l.start();
      layerUp = true;
      event('layer started');

      final s = LocalServer(
        host: h,
        contract: widget.contract,
        www: '${widget.base}/www',
        peers: () => [
          for (final p in peers.values)
            if (p.discovered.isNotEmpty || p.connected.isNotEmpty)
              {'key': p.key, 'state': p.connected.isNotEmpty ? 'connected' : 'found'}
        ],
        namesFile: '${widget.base}/names.json',
      );
      await s.start();
      server = s;
      final w = WebViewController()
        ..setJavaScriptMode(JavaScriptMode.unrestricted)
        ..loadRequest(Uri.parse(s.url));
      setState(() {
        web = w;
        phase = 'running';
      });
    } catch (e, st) {
      event('FAILED: $e');
      debugPrint('$e\n$st');
      setState(() => phase = 'failed: $e');
    }
  }

  @override
  void dispose() {
    unawaited(server?.stop());
    unawaited(layer?.dispose());
    super.dispose();
  }

  void _debug() {
    showModalBottomSheet(
      context: context,
      isScrollControlled: true,
      builder: (_) => StatefulBuilder(
        builder: (context, set) => DraggableScrollableSheet(
          expand: false,
          initialChildSize: .7,
          builder: (context, scroll) => ListView(
            controller: scroll,
            padding: const EdgeInsets.all(16),
            children: [
              Text('What the network layer sees', style: Theme.of(context).textTheme.titleMedium),
              const SizedBox(height: 8),
              SelectableText('My key: ${host?.me ?? '—'}'),
              Text('Layer: ${layerUp ? 'running' : 'not running'} · underlays: PAN (Bluetooth), LAN · first contact: Open'),
              Text('Agent: ${host?.status ?? '—'} · $phase'),
              const Divider(),
              Text('People (${peers.length})', style: Theme.of(context).textTheme.titleSmall),
              for (final p in peers.values)
                Text('${p.key.substring(0, 8)}…  found: ${p.discovered.join(', ')}  connected: ${p.connected.join(', ')}'),
              const Divider(),
              Text('Events', style: Theme.of(context).textTheme.titleSmall),
              for (final e in events.reversed.take(80)) Text(e, style: const TextStyle(fontFamily: 'monospace', fontSize: 11)),
              const Divider(),
              Text('Agent log', style: Theme.of(context).textTheme.titleSmall),
              for (final e in (host?.log ?? const <String>[]).reversed.take(60))
                Text(e, style: const TextStyle(fontFamily: 'monospace', fontSize: 11)),
            ],
          ),
        ),
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    final connected = peers.values.where((p) => p.connected.isNotEmpty).length;
    return Scaffold(
      appBar: AppBar(
        title: Text(widget.title, overflow: TextOverflow.ellipsis),
        actions: [
          TextButton.icon(
            onPressed: _debug,
            icon: const Icon(Icons.wifi_tethering),
            label: Text('$connected/${peers.length}'),
          ),
        ],
      ),
      body: web == null
          ? Center(
              child: Padding(
                padding: const EdgeInsets.all(24),
                child: Column(mainAxisSize: MainAxisSize.min, children: [
                  if (!phase.startsWith('failed')) const CircularProgressIndicator(),
                  const SizedBox(height: 16),
                  Text(phase, textAlign: TextAlign.center),
                  TextButton(onPressed: _debug, child: const Text('Details')),
                ]),
              ),
            )
          : WebViewWidget(controller: web!),
    );
  }
}
