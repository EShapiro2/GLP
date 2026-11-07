// GAS2 on a computer: the person's super-app, its page on the loopback.
//
//   dart run bin/gas2.dart --tree <programs> [--data <dir>] [--port 8301]
//                          [--relay https://talmonlab.bgu.ac.il/gasrelay]
//
// The tree is a copy of GLP's programs/ that GAS2 prepared (tools/device_tree.py).
// The person's key pair is made at first start and kept in <data>/key.json.  The
// agent runs over the relay alone: a computer has no Bluetooth layer here, and the
// people met are added by their keys (the page's "Add a person").  Open
// http://127.0.0.1:<port>/ in a browser.
import 'dart:async';
import 'dart:convert';
import 'dart:ffi';
import 'dart:io';
import 'dart:typed_data';

import 'package:gas2_device/host.dart';
import 'package:gas2_device/offline_network.dart';
import 'package:gas2_device/relay_network.dart';
import 'package:gas2_device/page_server.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:sodium/sodium_sumo.dart';

String? _opt(List<String> args, String name) {
  final i = args.indexOf('--$name');
  return i >= 0 && i + 1 < args.length ? args[i + 1] : null;
}

Future<SodiumSumo> loadSodium() => SodiumSumoInit.init(() {
      final path = Platform.environment['LIBSODIUM_PATH'];
      if (path != null && path.isNotEmpty) return DynamicLibrary.open(path);
      for (final n in [
        if (Platform.isLinux) ...['libsodium.so.23', 'libsodium.so'],
        if (Platform.isMacOS) ...['/opt/homebrew/lib/libsodium.dylib', '/usr/local/lib/libsodium.dylib'],
        if (Platform.isWindows) ...['libsodium.dll', '${File(Platform.resolvedExecutable).parent.path}\\libsodium.dll'],
      ]) {
        try {
          return DynamicLibrary.open(n);
        } on Object {
          continue;
        }
      }
      throw StateError('no libsodium: set LIBSODIUM_PATH');
    });

({PubKey pub, Uint8List priv}) keyPairIn(SodiumSumo sodium, String dir) {
  final f = File('$dir/key.json');
  if (f.existsSync()) {
    final j = jsonDecode(f.readAsStringSync()) as Map;
    return (pub: PubKey.fromHex(j['publicKey'] as String), priv: Uint8List.fromList(base64Decode(j['privateKey'] as String)));
  }
  final kp = sodium.crypto.sign.keyPair();
  final pair = (pub: PubKey.fromList(kp.publicKey), priv: kp.secretKey.extractBytes());
  f.parent.createSync(recursive: true);
  f.writeAsStringSync(jsonEncode({'publicKey': pair.pub.hex, 'privateKey': base64Encode(pair.priv)}));
  return pair;
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
  await _copy(Directory(tree), own);
  final host = SuperHost(tree: own.path, keyPair: kp, network: relay, dataDir: data);
  final server = PageServer(host: host, www: www, addPerson: (k) => relay.addPerson(PubKey.fromHex(k)));
  await server.start(port);
  stdout.writeln('GAS2: http://127.0.0.1:$port/  (you are ${kp.pub.hex})');
  relay.setOn(true);
  await host.start();
  stdout.writeln('GAS2: ${host.status}');
}

Future<void> _copy(Directory from, Directory to) async {
  to.createSync(recursive: true);
  await for (final e in from.list(recursive: false)) {
    final name = e.uri.pathSegments.where((x) => x.isNotEmpty).last;
    if (e is Directory) {
      await _copy(e, Directory('${to.path}/$name'));
    } else if (e is File) {
      e.copySync('${to.path}/$name');
    }
  }
}
