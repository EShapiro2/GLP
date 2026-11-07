// What a GAS2 host needs at its start, on a device or on the web: libsodium, the
// person's key pair, a copy of the tree.
import 'dart:convert';
import 'dart:ffi';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:sodium/sodium_sumo.dart';

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

Future<void> copyTree(Directory from, Directory to) async {
  to.createSync(recursive: true);
  await for (final e in from.list(recursive: false)) {
    final name = e.uri.pathSegments.where((x) => x.isNotEmpty).last;
    if (e is Directory) {
      await copyTree(e, Directory('${to.path}/$name'));
    } else if (e is File) {
      e.copySync('${to.path}/$name');
    }
  }
}
