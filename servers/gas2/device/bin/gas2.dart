// GAS2 on a computer: the person's super-app, its page on the loopback.
//
//   dart run bin/gas2.dart --tree <programs> [--data <dir>] [--port 8301]
//                          [--relay https://talmonlab.bgu.ac.il/gasrelay]
//                          [--site https://talmonlab.bgu.ac.il/gas2 | none]
//
// The tree is a copy of GLP's programs/ that GAS2 prepared (tools/device_tree.py).
// The person's key pair is made at first start and kept in <data>/key.json.  The
// agent runs over the relay alone: a computer has no Bluetooth layer here, and the
// people met are added by their keys (the page's "Add a person").  Open
// http://127.0.0.1:<port>/ in a browser.
import 'dart:async';
import 'dart:io';

import 'package:gas2_device/host.dart';
import 'package:gas2_device/offline_network.dart';
import 'package:gas2_device/relay_network.dart';
import 'package:gas2_device/page_server.dart';
import 'package:gas2_device/setup.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';

String? _opt(List<String> args, String name) {
  final i = args.indexOf('--$name');
  return i >= 0 && i + 1 < args.length ? args[i + 1] : null;
}

Future<void> main(List<String> args) async {
  final tree = _opt(args, 'tree') ?? Platform.environment['GAS2_TREE'];
  if (tree == null) {
    stderr.writeln('usage: gas2 --tree <programs> [--data <dir>] [--port 8301] [--relay <url>] [--www <dir>]');
    exit(2);
  }
  final data = _opt(args, 'data') ?? 'gas2-data';
  final port = int.parse(_opt(args, 'port') ?? '8301');
  final relayUrl = _opt(args, 'relay') ?? defaultRelay;
  final site = _opt(args, 'site') ?? 'https://talmonlab.bgu.ac.il/gas2';
  final www = _opt(args, 'www') ?? '${File(Platform.script.toFilePath()).parent.parent.path}/web';
  final sodium = await loadSodium();
  final kp = keyPairIn(sodium, data);
  final relay = RelayGlpNetwork(
    inner: OfflineGlpNetwork(sodium: sodium, me: kp.pub, privateKey: kp.priv),
    sodium: sodium,
    privateKey: kp.priv,
    app: 'gas2',
    dir: '$data/relay',
    relayUrl: relayUrl,
  )..relayOnly = true;
  // The device writes into its tree (the mini-apps it certifies), so it runs from a
  // copy of its own, made afresh at each start.
  final own = Directory('$data/tree');
  if (own.existsSync()) own.deleteSync(recursive: true);
  await copyTree(Directory(tree), own);
  final host = SuperHost(tree: own.path, keyPair: kp, network: relay, dataDir: data, site: site == 'none' ? null : site);
  final server = PageServer(host: host, www: www, addPerson: (k) => relay.addPerson(PubKey.fromHex(k)));
  // Start over: everything in the data directory deleted, and GAS2 ended with code 3,
  // on which run.sh starts it again (a new key pair is made at the start)
  server.reset = () async {
    relay.dispose();
    await Future<void>.delayed(const Duration(milliseconds: 300));
    await server.stop();
    for (final e in Directory(data).listSync()) {
      e.deleteSync(recursive: true);
    }
    stdout.writeln('GAS2: started over; run it again for a new person');
    exit(3);
  };
  await server.start(port);
  stdout.writeln('GAS2: http://127.0.0.1:$port/  (you are ${kp.pub.hex})');
  relay.setOn(true);
  await host.start();
  stdout.writeln('GAS2: ${host.status}');
}
