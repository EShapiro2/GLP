// GAS2 on a phone or a computer: the person's Grassroots Super-App.
//
// The app carries the GLP tree GAS2 prepared (the super-app of
// programs/social/graph/core with GAS2's device harness, and the mini-apps made
// from the contracts) and the page, laid out by tool/sync_assets.sh.  At each start
// it unpacks them, runs the person's super-app (gas2_device's SuperHost) over the
// relay, and shows the page, served on the loopback, in a web view.  The person's
// key pair is made at first start and kept with the app's files.
//
// The page's "Copy" posts the key to the app, which puts it on the clipboard (a web
// view's own clipboard is not always the system's).
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:gas2_device/host.dart';
import 'package:gas2_device/offline_network.dart';
import 'package:gas2_device/page_server.dart';
import 'package:gas2_device/relay_network.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:path_provider/path_provider.dart';
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
  runApp(const MaterialApp(title: 'GAS2', debugShowCheckedModeBanner: false, home: Shell()));
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

  @override
  void initState() {
    super.initState();
    unawaited(_start());
  }

  Future<void> _start() async {
    try {
      final home = '${(await getApplicationSupportDirectory()).path}/gas2';
      await unbundle('$home/bundle');
      final sodium = await SodiumSumoInit.init();
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
      final relay = RelayGlpNetwork(
        inner: OfflineGlpNetwork(sodium: sodium, me: kp.pub, privateKey: kp.priv),
        sodium: sodium,
        privateKey: kp.priv,
        app: 'gas2',
        dir: '$home/relay',
      )..relayOnly = true;
      final host = SuperHost(tree: '$home/bundle/programs', keyPair: kp, network: relay, dataDir: home);
      final server = PageServer(host: host, www: '$home/bundle/web', addPerson: (k) => relay.addPerson(PubKey.fromHex(k)));
      final port = await server.start(0);
      relay.setOn(true);
      final w = await GasView.open('http://127.0.0.1:$port/',
          background: _bg, onMessage: (m) => unawaited(_native(m)), onLoaded: () {});
      setState(() => web = w);
      await host.start();
    } catch (e) {
      setState(() => status = 'Could not start: $e');
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
