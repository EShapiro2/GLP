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

import 'package:app_links/app_links.dart';
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

import 'contracts.dart';
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

/// The home screen: the person's apps (the starters the app carries and
/// those got since), more from GAS's catalog, and links (pasted, or opened
/// from a browser as gas://) to get an app or to add a person in one.
class Boot extends StatefulWidget {
  const Boot({super.key});
  @override
  State<Boot> createState() => _BootState();
}

class _BootState extends State<Boot> {
  ContractStore? store;
  List<Contract> mine = [];
  List<Contract>? more;
  String? moreError, error, busy;
  StreamSubscription<Uri>? _links;

  @override
  void initState() {
    super.initState();
    () async {
      try {
        final b = await unbundle();
        final st = ContractStore(b);
        await st.installStarters();
        setState(() {
          store = st;
          mine = st.installed();
        });
        unawaited(_catalog());
        try {
          _links = AppLinks().uriLinkStream.listen((u) => _handle(u.toString()));
        } on Object {
          // No links on this platform.
        }
      } catch (e) {
        setState(() => error = '$e');
      }
    }();
  }

  @override
  void dispose() {
    _links?.cancel();
    super.dispose();
  }

  Future<void> _catalog() async {
    setState(() => moreError = null);
    try {
      final c = await store!.catalog();
      if (mounted) setState(() => more = c);
    } on Object catch (e) {
      if (mounted) setState(() => moreError = '$e');
    }
  }

  void _say(String s) {
    if (mounted) ScaffoldMessenger.of(context).showSnackBar(SnackBar(content: Text(s)));
  }

  Future<Contract?> _get(String hash, String what) async {
    setState(() => busy = what);
    try {
      final c = await store!.get(hash);
      setState(() => mine = store!.installed());
      return c;
    } on Object catch (e) {
      _say('Could not get it: $e');
      return null;
    } finally {
      if (mounted) setState(() => busy = null);
    }
  }

  Future<void> _open(Contract c, {String? person}) async {
    await Navigator.of(context).push(MaterialPageRoute(
        builder: (_) => Running(base: store!.base, contract: c, addPerson: person)));
    if (mounted) setState(() => mine = store!.installed());
  }

  /// A link: get its app if it is not here, and open it, adding its person.
  Future<void> _handle(String text) async {
    final l = GasLink.parse(text);
    if (store == null) return;
    if (l == null) return _say('That is not a GAS link.');
    Navigator.of(context).popUntil((r) => r.isFirst);
    Contract? c;
    if (l.contract != null) {
      c = await _get(l.contract!, 'Getting the app…');
      if (c == null) return;
    } else {
      // An invite without its app: the person picks the app.
      c = await showDialog<Contract>(
          context: context,
          builder: (_) => SimpleDialog(title: const Text('Add them in which app?'), children: [
                for (final m in mine)
                  SimpleDialogOption(onPressed: () => Navigator.pop(context, m), child: Text('${m.icon}  ${m.title}')),
              ]));
      if (c == null) return;
    }
    if (!mounted) return;
    await _open(c, person: l.person);
  }

  Future<void> _paste() async {
    final field = TextEditingController();
    final clip = await Clipboard.getData('text/plain');
    if (clip?.text != null && GasLink.parse(clip!.text!) != null) field.text = clip.text!.trim();
    if (!mounted) return;
    final go = await showDialog<bool>(
      context: context,
      builder: (_) => AlertDialog(
        title: const Text('Open a GAS link'),
        content: Column(mainAxisSize: MainAxisSize.min, crossAxisAlignment: CrossAxisAlignment.start, children: [
          const Text('A link to an app, or someone\'s invite.'),
          const SizedBox(height: 10),
          TextField(controller: field, decoration: const InputDecoration(hintText: 'Paste the link', border: OutlineInputBorder())),
        ]),
        actions: [
          TextButton(onPressed: () => Navigator.pop(context, false), child: const Text('Cancel')),
          FilledButton(onPressed: () => Navigator.pop(context, true), child: const Text('Open')),
        ],
      ),
    );
    if (go == true) await _handle(field.text);
  }

  Widget _tile(Contract c, {Widget? trailing, VoidCallback? onTap}) => ListTile(
        leading: CircleAvatar(backgroundColor: const Color(0xFFF0F0F5), child: Text(c.icon, style: const TextStyle(fontSize: 20))),
        title: Text(c.title),
        subtitle: Text(c.about.isEmpty ? c.id : c.about, maxLines: 2, overflow: TextOverflow.ellipsis),
        trailing: trailing,
        onTap: onTap,
      );

  @override
  Widget build(BuildContext context) {
    final have = {for (final c in mine) c.hash};
    final others = [for (final c in more ?? const <Contract>[]) if (!have.contains(c.hash)) c];
    final head = Theme.of(context).textTheme.titleSmall?.copyWith(color: Colors.black54);
    return Scaffold(
      appBar: AppBar(
        title: const Text('GAS'),
        actions: [IconButton(tooltip: 'Open a link', icon: const Icon(Icons.link), onPressed: store == null ? null : _paste)],
      ),
      body: error != null
          ? Padding(padding: const EdgeInsets.all(16), child: Text('Could not unpack: $error'))
          : store == null
              ? const Center(child: CircularProgressIndicator())
              : Stack(children: [
                  RefreshIndicator(
                    onRefresh: _catalog,
                    child: ListView(children: [
                      Padding(padding: const EdgeInsets.fromLTRB(16, 16, 16, 4), child: Text('MY APPS', style: head)),
                      for (final c in mine) _tile(c, trailing: const Icon(Icons.chevron_right), onTap: () => _open(c)),
                      Padding(padding: const EdgeInsets.fromLTRB(16, 20, 16, 4), child: Text('MORE FROM GAS', style: head)),
                      if (more == null && moreError == null)
                        const Padding(padding: EdgeInsets.all(16), child: Text('Loading the catalog…')),
                      if (moreError != null)
                        ListTile(
                          title: const Text('Could not reach GAS'),
                          subtitle: const Text('Check the Internet; your apps work without it.'),
                          trailing: TextButton(onPressed: _catalog, child: const Text('Retry')),
                        ),
                      if (more != null && others.isEmpty)
                        const Padding(padding: EdgeInsets.all(16), child: Text('You have them all.')),
                      for (final c in others)
                        _tile(c,
                            trailing: FilledButton.tonal(
                              onPressed: busy != null
                                  ? null
                                  : () async {
                                      final got = await _get(c.hash, 'Getting ${c.title}…');
                                      if (got != null) _say('${got.title} is in your apps.');
                                    },
                              child: const Text('Get'),
                            )),
                      const Padding(
                        padding: EdgeInsets.fromLTRB(16, 24, 16, 32),
                        child: Text('Got a link to an app or an invite? Tap 🔗 at the top.', style: TextStyle(color: Colors.black54)),
                      ),
                    ]),
                  ),
                  if (busy != null)
                    Container(
                      color: Colors.black26,
                      child: Center(
                        child: Card(
                            child: Padding(
                                padding: const EdgeInsets.all(20),
                                child: Row(mainAxisSize: MainAxisSize.min, children: [
                                  const CircularProgressIndicator(),
                                  const SizedBox(width: 16),
                                  Text(busy!),
                                ]))),
                      ),
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
  final String base;
  final Contract contract;

  /// A person to add from afar once the agent runs (an invite opened).
  final String? addPerson;
  const Running({super.key, required this.base, required this.contract, this.addPerson});
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
        app: widget.contract.name,
        dir: '${widget.base}/relay',
      );
      relay = net;
      final h = PersonHost(
        program: '${widget.base}/programs/vglp/cva/gas_worlds/live_${widget.contract.name}',
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
      final invited = widget.addPerson;
      if (invited != null && invited != h.me) {
        await net.addPerson(glp.PubKey.fromHex(invited));
        event('added ${invited.substring(0, 8)}… from an invite');
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(SnackBar(
              content: Text(net.on
                  ? 'Added ${_name(invited)}. They will see that you added them.'
                  : 'Turn on Internet (person+ button) to reach ${_name(invited)}.')));
        }
      }

      final s = LocalServer(
        host: h,
        contract: widget.contract.name,
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
    final l = GasLink.parse(text);
    if (l?.contract != null && l!.contract != widget.contract.hash) {
      return say('That invite is for another app: go back to your apps and open it there (🔗).');
    }
    final pk = l?.person == null ? null : glp.PubKey.fromHex(l!.person!);
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
    final link = inviteLink(glp.PubKey.fromHex(me), widget.contract.hash);
    final field = TextEditingController();
    final clip = await Clipboard.getData('text/plain');
    if (clip?.text != null && GasLink.parse(clip!.text!)?.person != null && !clip.text!.contains(me)) {
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
                      .share(ShareParams(text: 'Join me in ${widget.contract.title} on GAS: $link', subject: 'GAS invite')),
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
              if (relay != null)
                SwitchListTile(
                  contentPadding: EdgeInsets.zero,
                  dense: true,
                  title: const Text('Relay only (for testing)'),
                  subtitle: const Text('Every message through talmonlab, even to people near you'),
                  value: relay!.relayOnly,
                  onChanged: (v) => set(() => relay!.relayOnly = v),
                ),
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
        title: Text(widget.contract.title, overflow: TextOverflow.ellipsis),
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
          if (done || v == null || GasLink.parse(v)?.person == null) return;
          done = true;
          Navigator.of(context).pop(v);
        }),
      );
}
