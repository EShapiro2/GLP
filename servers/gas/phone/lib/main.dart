// GAS on a phone or a computer: the person's grassroots apps, each a
// contract's canonical platform run by the person's agent of madGLP over GNA's
// networking layer (Bluetooth and the local network), and talmonlab's relay
// for people who are not near (relay_network.dart).
//
// The app is one page, www/phone.html, drawn by a web view and answered by the
// app's own server on the loopback (local_server.dart) from its core
// (core.dart).  This file is the shell: it unpacks the bundle, makes the
// person's key, starts the core and the server, shows the page, and does for
// the page what only the device can --- share, copy, paste, scan a QR code,
// open a web page ---
// over the JavaScript channel GASNative, and hands it the gas:// links the
// device opens.
//
// The first test bed of madGLP on phones: no rendezvous server (WAN), no
// attestation (every peer reported unattested), first contact Open on both
// proximity underlays.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:app_links/app_links.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking/grassroots_networking.dart';
import 'package:grassroots_networking_glue/grassroots_glp_network.dart';
import 'package:mobile_scanner/mobile_scanner.dart';
import 'package:path_provider/path_provider.dart';
import 'package:redux/redux.dart';
import 'package:share_plus/share_plus.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';
import 'package:url_launcher/url_launcher.dart';
import 'package:webview_flutter/webview_flutter.dart';

import 'contracts.dart';
import 'core.dart';
import 'local_server.dart';

const _bg = Color(0xFFF5F5F7);

Future<void> main() async {
  WidgetsFlutterBinding.ensureInitialized();
  SystemChrome.setSystemUIOverlayStyle(const SystemUiOverlayStyle(
    statusBarColor: _bg,
    statusBarIconBrightness: Brightness.dark,
    statusBarBrightness: Brightness.light,
    systemNavigationBarColor: _bg,
    systemNavigationBarIconBrightness: Brightness.dark,
  ));
  runApp(const MaterialApp(title: 'GAS', debugShowCheckedModeBanner: false, home: Shell()));
}

/// The bundle (tool/sync_assets.sh) copied into the app's documents
/// directory, a real path the GLP engine reads: programs/, www/ and the
/// starter packages.
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

/// The phone's layer: Bluetooth (PAN) and the local network (LAN), first
/// contact Open on both.
class PhoneLayer implements Layer {
  final GrassrootsNetwork layer;
  @override
  final glp.GlpNetwork net;
  PhoneLayer._(this.layer) : net = GrassrootsGlpNetwork(layer);

  factory PhoneLayer(GrassrootsIdentity id, SodiumSumo sodium) => PhoneLayer._(GrassrootsNetwork(
        identity: id,
        underlays: {Underlay.pan, Underlay.lan},
        store: Store<AppState>(appReducer, initialState: AppState()),
        sodium: sodium,
      ));

  @override
  Future<void> start() async {
    await layer.initialize();
    await layer.pan?.setTrustLevel(ColdCallTrustLevel.open);
    await layer.lan?.setTrustLevel(ColdCallTrustLevel.open);
    await layer.start();
  }

  @override
  Future<void> dispose() => layer.dispose();
}

class Shell extends StatefulWidget {
  const Shell({super.key});
  @override
  State<Shell> createState() => _ShellState();
}

class _ShellState extends State<Shell> {
  WebViewController? web;
  LocalServer? server;
  String? error;
  StreamSubscription<Uri>? _links;
  final List<String> _early = [];
  bool _ready = false;

  @override
  void initState() {
    super.initState();
    unawaited(_start());
  }

  Future<void> _start() async {
    try {
      final base = await unbundle();
      final store = ContractStore(base);
      await store.installStarters();
      final sodium = await SodiumSumoInit.init();
      // The key pair, made once and kept in a file of the app's own (the
      // platform keychain needs a signing entitlement this test bed lacks).
      final idFile = File('$base/identity.json');
      final identity = idFile.existsSync()
          ? GrassrootsIdentity.fromMap(jsonDecode(idFile.readAsStringSync()) as Map<String, dynamic>)
          : await () async {
              final id = await GrassrootsIdentity.generate();
              idFile.writeAsStringSync(jsonEncode(id.toJson()));
              return id;
            }();
      final core = Core(
        base: base,
        store: store,
        sodium: sodium,
        me: glp.PubKey.fromList(identity.publicKey),
        privateKey: Uint8List.fromList(identity.privateKey),
        layers: () async => PhoneLayer(identity, sodium),
      );
      final s = LocalServer(core: core, www: '$base/www');
      await s.start();
      server = s;
      final w = WebViewController()
        ..setJavaScriptMode(JavaScriptMode.unrestricted)
        ..setBackgroundColor(_bg)
        ..addJavaScriptChannel('GASNative', onMessageReceived: (m) => unawaited(_native(m.message)))
        ..setNavigationDelegate(NavigationDelegate(onPageFinished: (_) {
          _ready = true;
          for (final l in _early) {
            _send('link', l);
          }
          _early.clear();
        }))
        ..loadRequest(Uri.parse(s.url));
      setState(() => web = w);
      try {
        _links = AppLinks().uriLinkStream.listen((u) {
          if (_ready) {
            _send('link', '$u');
          } else {
            _early.add('$u');
          }
        });
      } on Object {
        // No links on this platform.
      }
    } catch (e, st) {
      debugPrint('$e\n$st');
      setState(() => error = '$e');
    }
  }

  /// Tell the page: window.gasNative[kind](value).
  void _send(String kind, String value) =>
      unawaited(web?.runJavaScript('window.gasNative && gasNative.$kind && gasNative.$kind(${jsonEncode(value)})'));

  Future<void> _native(String message) async {
    final m = jsonDecode(message) as Map;
    switch (m['op']) {
      case 'share':
        await SharePlus.instance.share(ShareParams(text: m['text'] as String, subject: 'GAS'));
      case 'copy':
        await Clipboard.setData(ClipboardData(text: m['text'] as String));
      case 'paste':
        _send('pasted', (await Clipboard.getData('text/plain'))?.text ?? '');
      case 'scan':
        if (!mounted) return;
        final got = await Navigator.of(context).push<String>(MaterialPageRoute(builder: (_) => const _Scan()));
        if (got != null) _send('scanned', got);
      case 'haptic':
        await HapticFeedback.selectionClick();
      case 'open':
        await launchUrl(Uri.parse(m['url'] as String), mode: LaunchMode.externalApplication);
    }
  }

  @override
  void dispose() {
    _links?.cancel();
    unawaited(server?.stop());
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final w = web;
    return PopScope(
      canPop: false,
      onPopInvokedWithResult: (popped, _) async {
        if (popped || w == null) return;
        if (await w.canGoBack()) {
          await w.goBack();
        } else {
          await SystemNavigator.pop();
        }
      },
      child: Scaffold(
        backgroundColor: _bg,
        body: SafeArea(
          child: w != null
              ? WebViewWidget(controller: w)
              : Center(
                  child: Column(mainAxisSize: MainAxisSize.min, children: [
                    Container(
                      width: 72,
                      height: 72,
                      decoration: BoxDecoration(
                        borderRadius: BorderRadius.circular(20),
                        gradient: const SweepGradient(colors: [
                          Color(0xFF4F7BD9), Color(0xFF2BB3A0), Color(0xFF9B7BD4), Color(0xFFF0B54A), Color(0xFF4F7BD9),
                        ]),
                      ),
                    ),
                    const SizedBox(height: 18),
                    Text(error == null ? 'GAS' : 'GAS could not start',
                        style: const TextStyle(fontSize: 22, fontWeight: FontWeight.w700, color: Color(0xFF1D1D1F))),
                    if (error != null)
                      Padding(
                        padding: const EdgeInsets.all(24),
                        child: Text(error!, textAlign: TextAlign.center, style: const TextStyle(color: Color(0xFF6E6E73))),
                      ),
                  ]),
                ),
        ),
      ),
    );
  }
}

/// The camera, until it reads a GAS link; pops with its text.
class _Scan extends StatefulWidget {
  const _Scan();
  @override
  State<_Scan> createState() => _ScanState();
}

class _ScanState extends State<_Scan> {
  bool done = false;
  @override
  Widget build(BuildContext context) => Scaffold(
        backgroundColor: Colors.black,
        appBar: AppBar(
          backgroundColor: Colors.black,
          foregroundColor: Colors.white,
          title: const Text('Scan a GAS code'),
        ),
        body: MobileScanner(onDetect: (capture) {
          final v = capture.barcodes.isEmpty ? null : capture.barcodes.first.rawValue;
          if (done || v == null || GasLink.parse(v) == null) return;
          done = true;
          Navigator.of(context).pop(v);
        }),
      );
}
