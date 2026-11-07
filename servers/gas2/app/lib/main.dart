// GAS2 on a phone or a computer: the person's Grassroots Super-App.
//
// The app carries the GLP tree GAS2 prepared (the super-app of
// programs/social/graph/core with GAS2's device harness, and the mini-apps made
// from the contracts) and the page, laid out by tool/sync_assets.sh.  At each start
// it unpacks them, runs the person's super-app (gas2_device's SuperHost) over the
// relay, and shows the page, served on the loopback, in a web view.  The person's
// key pair is made at first start and kept with the app's files.
//
// On a phone the super-app's network is GNA's layer, Bluetooth and the local network,
// first contact Open on both, as GAS's phone has it, under talmonlab's relay for people
// not near; where the layer does not start (no Bluetooth, permissions refused), the
// relay alone.
//
// The page's "Copy" posts the key to the app, which puts it on the clipboard (a web
// view's own clipboard is not always the system's).
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:gas2_device/host.dart';
import 'package:gas2_device/offline_network.dart';
import 'package:gas2_device/page_server.dart';
import 'package:gas2_device/relay_network.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:grassroots_networking/grassroots_networking.dart' as gn;
import 'package:grassroots_networking_glue/grassroots_glp_network.dart';
import 'package:path_provider/path_provider.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'view.dart';

const _bg = Color(0xFFF2F2F7);

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

/// The bundle, unpacked into [base]: the tree and the page, afresh at each start.
Future<void> unbundle(String base) async {
  final d = Directory(base);
  if (d.existsSync()) d.deleteSync(recursive: true);
  final manifest = await rootBundle.loadString('assets/manifest.txt', cache: false);
  for (final line in manifest.split('\n')) {
    final tab = line.indexOf('\t');
    if (tab < 0) continue;
    final data = await rootBundle.load('assets/bundle/${line.substring(0, tab)}');
    final out = File('$base/${line.substring(tab + 1)}');
    await out.parent.create(recursive: true);
    await out.writeAsBytes(data.buffer.asUint8List(data.offsetInBytes, data.lengthInBytes));
  }
}

class Shell extends StatefulWidget {
  const Shell({super.key});
  @override
  State<Shell> createState() => _ShellState();
}

class _ShellState extends State<Shell> {
  GasView? web;
  String status = 'Starting…';
  SodiumSumo? _sodium;

  @override
  void initState() {
    super.initState();
    unawaited(_start());
  }

  Future<void> _start() async {
    try {
      final home = '${(await getApplicationSupportDirectory()).path}/gas2';
      await unbundle('$home/bundle');
      final sodium = _sodium ??= await SodiumSumoInit.init();
      final keyFile = File('$home/key.json');
      late final ({PubKey pub, Uint8List priv}) kp;
      if (keyFile.existsSync()) {
        final j = jsonDecode(keyFile.readAsStringSync()) as Map;
        kp = (pub: PubKey.fromHex(j['publicKey'] as String), priv: Uint8List.fromList(base64Decode(j['privateKey'] as String)));
      } else {
        final k = sodium.crypto.sign.keyPair();
        kp = (pub: PubKey.fromList(k.publicKey), priv: k.secretKey.extractBytes());
        keyFile.writeAsStringSync(jsonEncode({'publicKey': kp.pub.hex, 'privateKey': base64Encode(kp.priv)}));
      }
      final near = await _near(kp, sodium);
      final relay = RelayGlpNetwork(
        inner: near == null ? OfflineGlpNetwork(sodium: sodium, me: kp.pub, privateKey: kp.priv) : GrassrootsGlpNetwork(near),
        sodium: sodium,
        privateKey: kp.priv,
        app: 'gas2',
        dir: '$home/relay',
      )..relayOnly = near == null;
      final host = SuperHost(tree: '$home/bundle/programs', keyPair: kp, network: relay, dataDir: home, site: 'https://talmonlab.bgu.ac.il/gas')
        ..onNear = relay.putKnownPeer;
      final server = PageServer(host: host, www: '$home/bundle/web', addPerson: (k) => relay.addPerson(PubKey.fromHex(k)));
      // Start over: the network stopped, everything the app keeps deleted, and the
      // app started again, which makes a new key pair
      server.reset = () async {
        relay.dispose();
        try {
          await near?.dispose();
        } on Object {
          // gone already
        }
        await server.stop();
        for (final e in Directory(home).listSync()) {
          try {
            e.deleteSync(recursive: true);
          } on Object {
            // the next start writes over it
          }
        }
        final old = web;
        setState(() {
          web = null;
          status = 'Starting over…';
        });
        await old?.dispose();
        await _start();
      };
      final port = await server.start(0);
      relay.onCarrier = host.carried;
      relay.setOn(true);
      final w = await GasView.open('http://127.0.0.1:$port/',
          background: _bg, onMessage: (m) => unawaited(_native(m)), onLoaded: () {});
      setState(() => web = w);
      await host.start();
      // discovery and connections begin once the agent is bound to the layer
      if (near != null) {
        try {
          await near.start();
        } catch (e) {
          host.notes.add('Bluetooth and the local network did not start: $e');
        }
      }
    } catch (e) {
      setState(() => status = 'Could not start: $e');
    }
  }

  /// GNA's layer over Bluetooth and the local network, under the person's key; null
  /// where it does not come up.
  Future<gn.GrassrootsNetwork?> _near(({PubKey pub, Uint8List priv}) kp, SodiumSumo sodium) async {
    if (!(Platform.isAndroid || Platform.isIOS || Platform.isMacOS)) return null;
    try {
      final id = await gn.GrassrootsIdentity.create(keyPair: await Ed25519().newKeyPairFromSeed(kp.priv.sublist(0, 32)));
      final layer = gn.GrassrootsNetwork(
        identity: id,
        underlays: {gn.Underlay.pan, gn.Underlay.lan},
        store: Store<gn.AppState>(gn.appReducer, initialState: gn.AppState()),
        sodium: sodium,
      );
      await layer.initialize();
      await layer.pan?.setTrustLevel(gn.ColdCallTrustLevel.open);
      await layer.lan?.setTrustLevel(gn.ColdCallTrustLevel.open);
      return layer;
    } catch (e) {
      debugPrint('GAS2: no Bluetooth or local network: $e');
      return null;
    }
  }

  Future<void> _native(String m) async {
    try {
      final j = jsonDecode(m) as Map;
      if (j['copy'] is String) await Clipboard.setData(ClipboardData(text: j['copy'] as String));
    } on Object {
      // not ours
    }
  }

  @override
  Widget build(BuildContext context) {
    final w = web;
    return Scaffold(
      backgroundColor: _bg,
      body: SafeArea(
        child: w == null
            ? Center(child: Text(status, style: const TextStyle(color: Color(0xFF6E6E73))))
            : PopScope(
                canPop: false,
                onPopInvokedWithResult: (didPop, _) async {
                  if (!didPop) await w.run('window.gas2Back && window.gas2Back()');
                },
                child: w.widget(),
              ),
      ),
    );
  }
}
