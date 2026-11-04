// GAS on a phone: one person, the phone's owner, running one grassroots
// contract's canonical platform as an agent of madGLP over GNA's networking
// layer --- Bluetooth (PAN) and the local network (LAN) --- named by the
// layer's key.  The screen is the GAS app page for one person, served by the
// app's own server on the phone's loopback; the people the layer finds near
// the phone are the persons the page offers.  A debug panel shows what the
// layer sees.
//
// People who are not near each other meet through talmonlab's relay
// (relay_network.dart): one shares their invite link or QR code, the other
// adds it, and their messages go through the relay, encrypted end to end.
//
// The first test bed of madGLP on phones: no rendezvous server (WAN), no
// attestation (every peer reported unattested), first contact Open on both
// proximity underlays.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart' show Clipboard, ClipboardData, rootBundle;
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking/grassroots_networking.dart';
import 'package:grassroots_networking_glue/grassroots_glp_network.dart';
import 'package:mobile_scanner/mobile_scanner.dart';
import 'package:path_provider/path_provider.dart';
import 'package:qr_flutter/qr_flutter.dart';
import 'package:redux/redux.dart';
import 'package:share_plus/share_plus.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';
import 'package:webview_flutter/webview_flutter.dart';

import 'local_server.dart';
import 'person_host.dart';
import 'relay_network.dart';

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
Future<String> unbundle([String? into]) async {
  final base = into ?? '${(await getApplicationDocumentsDirectory()).path}/gas';
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
  RelayGlpNetwork? relay;
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
      // The key pair, made once and kept in a file of the app's own (the
      // platform keychain needs a signing entitlement this test bed lacks).
      final idFile = File('${widget.base}/identity.json');
      final identity = idFile.existsSync()
          ? GrassrootsIdentity.fromMap(jsonDecode(idFile.readAsStringSync()) as Map<String, dynamic>)
          : await () async {
              final id = await GrassrootsIdentity.generate();
              idFile.writeAsStringSync(jsonEncode(id.toJson()));
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
      final net = RelayGlpNetwork(
        inner: GrassrootsGlpNetwork(l),
        sodium: sodium,
        privateKey: Uint8List.fromList(identity.privateKey),
        app: widget.contract,
        dir: '${widget.base}/relay',
      );
      relay = net;
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

      net.onState = () {
        event('relay: ${net.state}');
      };
      net.onAddedBy = (pk) {
        event('added from afar by ${pk.hex.substring(0, 8)}…');
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
              SnackBar(content: Text('${_name(pk.hex)} added you, over the Internet')));
        }
      };
      net.setOn(!File(_relayOff).existsSync());

      final s = LocalServer(
        host: h,
        contract: widget.contract,
        www: '${widget.base}/www',
        peers: () => [
          for (final p in peers.values)
            if (p.discovered.isNotEmpty || p.connected.isNotEmpty)
              {
                'key': p.key,
                'state': p.connected.any((u) => u != 'wan')
                    ? 'connected'
                    : p.connected.contains('wan')
                        ? 'internet'
                        : 'found'
              }
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

  String get _relayOff => '${widget.base}/relay/off';

  /// The name this phone gave the person of [key], or 'Person xxxx'.
  String _name(String key) {
    try {
      final names = jsonDecode(File('${widget.base}/names.json').readAsStringSync()) as Map;
      if (names[key] is String && (names[key] as String).isNotEmpty) return names[key] as String;
    } on Object {
      // no names yet
    }
    return 'Person ${key.substring(0, 4)}';
  }

  Future<void> _add(String text, void Function(String) say) async {
    final pk = keyOfInvite(text);
    if (pk == null) return say('That is not a GAS invite link.');
    if (pk.hex == host?.me) return say('That is your own link.');
    await relay!.addPerson(pk);
    event('added ${pk.hex.substring(0, 8)}… from afar');
    say('Added ${_name(pk.hex)}. They will see that you added them.');
  }

  /// You (your invite QR code and link), adding someone from afar, and the
  /// relay switch.
  void _people() async {
    final r = relay, me = host?.me;
    if (r == null || me == null) return;
    final link = inviteLink(glp.PubKey.fromHex(me));
    final field = TextEditingController();
    final clip = await Clipboard.getData('text/plain');
    if (clip?.text != null && keyOfInvite(clip!.text!) != null && !clip.text!.contains(me)) {
      field.text = clip.text!.trim();
    }
    String note = '';
    final phone = Platform.isAndroid || Platform.isIOS;
    if (!mounted) return;
    await showModalBottomSheet(
      context: context,
      isScrollControlled: true,
      builder: (_) => StatefulBuilder(builder: (context, set) {
        void say(String s) => set(() => note = s);
        return DraggableScrollableSheet(
          expand: false,
          initialChildSize: .85,
          builder: (context, scroll) => ListView(
            controller: scroll,
            padding: const EdgeInsets.all(20),
            children: [
              Text('People far away', style: Theme.of(context).textTheme.titleLarge),
              const SizedBox(height: 4),
              const Text('People near you appear by themselves. To meet someone far away, '
                  'one of you shares their link or QR code and the other adds it.'),
              const SizedBox(height: 16),
              Text('Your invite', style: Theme.of(context).textTheme.titleMedium),
              const SizedBox(height: 8),
              Center(child: QrImageView(data: link, size: 200, backgroundColor: Colors.white)),
              const SizedBox(height: 8),
              Row(mainAxisAlignment: MainAxisAlignment.center, children: [
                OutlinedButton.icon(
                  icon: const Icon(Icons.copy),
                  label: const Text('Copy link'),
                  onPressed: () async {
                    await Clipboard.setData(ClipboardData(text: link));
                    say('Your link is copied.');
                  },
                ),
                const SizedBox(width: 8),
                FilledButton.icon(
                  icon: const Icon(Icons.share),
                  label: const Text('Share'),
                  onPressed: () => SharePlus.instance
                      .share(ShareParams(text: 'Add me on GAS: $link', subject: 'GAS invite')),
                ),
              ]),
              const Divider(height: 32),
              Text('Add a person', style: Theme.of(context).textTheme.titleMedium),
              const SizedBox(height: 8),
              TextField(
                controller: field,
                decoration: const InputDecoration(
                    hintText: 'Paste their invite link', border: OutlineInputBorder()),
              ),
              const SizedBox(height: 8),
              Row(children: [
                FilledButton(
                  onPressed: r.on ? () => _add(field.text, say) : null,
                  child: const Text('Add'),
                ),
                const SizedBox(width: 8),
                if (phone)
                  OutlinedButton.icon(
                    icon: const Icon(Icons.qr_code_scanner),
                    label: const Text('Scan their QR'),
                    onPressed: !r.on
                        ? null
                        : () async {
                            final got = await Navigator.of(context).push<String>(
                                MaterialPageRoute(builder: (_) => const _Scan()));
                            if (got != null) await _add(got, say);
                          },
                  ),
              ]),
              if (note.isNotEmpty) Padding(padding: const EdgeInsets.only(top: 8), child: Text(note)),
              const Divider(height: 32),
              SwitchListTile(
                contentPadding: EdgeInsets.zero,
                title: const Text('Internet (talmonlab relay)'),
                subtitle: Text(r.on
                    ? 'Messages to people not near you go through talmonlab, encrypted: '
                        'it sees who writes to whom, not what. Now: ${r.state}'
                    : 'Off: only people near you (Bluetooth, the same Wi-Fi).'),
                value: r.on,
                onChanged: (v) {
                  r.setOn(v);
                  final off = File(_relayOff);
                  if (v) {
                    if (off.existsSync()) off.deleteSync();
                  } else {
                    off.parent.createSync(recursive: true);
                    off.writeAsStringSync('off');
                  }
                  set(() {});
                },
              ),
              if (r.relayPeers.isNotEmpty) ...[
                const SizedBox(height: 8),
                Text('Met from afar: ${r.relayPeers.map((p) => _name(p.hex)).join(', ')}'),
              ],
            ],
          ),
        );
      }),
    );
  }

  @override
  void dispose() {
    relay?.dispose();
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
              Text('Relay: ${relay?.state ?? '—'} · met from afar: ${relay?.relayPeers.length ?? 0}'),
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
          IconButton(
            tooltip: 'People far away',
            onPressed: _people,
            icon: Icon(Icons.person_add, color: relay?.state == 'online' ? Colors.green : null),
          ),
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

/// The camera, until it reads a QR code; pops with its text.
class _Scan extends StatefulWidget {
  const _Scan();
  @override
  State<_Scan> createState() => _ScanState();
}

class _ScanState extends State<_Scan> {
  bool done = false;
  @override
  Widget build(BuildContext context) => Scaffold(
        appBar: AppBar(title: const Text('Scan their QR code')),
        body: MobileScanner(onDetect: (capture) {
          final v = capture.barcodes.isEmpty ? null : capture.barcodes.first.rawValue;
          if (done || v == null || keyOfInvite(v) == null) return;
          done = true;
          Navigator.of(context).pop(v);
        }),
      );
}
