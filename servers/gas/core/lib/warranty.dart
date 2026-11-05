// A package's warranties (the GSC paper, Section "Signing the Contract", The
// code): the code that realises a contract is warranted by its creator and by
// its compiler, each warranty signed and carried with the code.
//
// The code a package warrants is its contract unit, contract/: the contract's
// canonical platform and one person of it, a GLP program of its own that the
// harness gives its network and person streams (tools/gen_harness.py).
//
//  - The compiler's warranty is GLP's own module certificate (the IGLP code
//    format, "Program Artefact"): the compiler's key, the source identity of
//    the unit, the identity of its compiled body, and the compiler's
//    signature over the two.  GLP's compiler refuses a certificate to a
//    program that reaches the network or the person other than through
//    variables its host gives it, so a certified unit reaches them only
//    through the harness.
//  - The creator's warranty is a GLP signed term (GLP-Spec, "Identity and
//    signature", the form sign/3 writes and signature/2 reads), over the
//    unit's source identity: creator_cert(Src, Schemas, Verdict), saying that
//    the unit Src realises the contract whose speech-act schemas hash to
//    Schemas, which the checker found Verdict.  The package carries the
//    contract's text, its schemas and the verdict.
//
// The source identity of a program depends on where it is compiled, its
// modules being named by their paths, so the unit is certified, here and on
// every device that checks it, as the directory
// programs/vglp/cva/gas_worlds/certified/<ID>, made for the purpose and
// removed after.  A device installs a package only after compiling the unit
// itself: a unit refused a certificate is not installed, and neither is one
// whose identities differ from those its warranties name.
import 'dart:convert';
import 'dart:io';
import 'dart:math' show Random;
import 'dart:typed_data';

import 'package:crypto/crypto.dart' as crypto;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show PubKey;
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/codec.dart';

/// The files of a package's contract unit, as the package names them.
const contractFiles = ['contract/self.glp', 'contract/platform.vglp'];

String hexOf(List<int> b) => b.map((x) => x.toRadixString(16).padLeft(2, '0')).join();

Uint8List bytesOfHex(String h) {
  if (h.length.isOdd || !RegExp(r'^[0-9a-f]*$').hasMatch(h)) {
    throw const FormatException('not lowercase hex');
  }
  return Uint8List.fromList([for (var i = 0; i < h.length; i += 2) int.parse(h.substring(i, i + 2), radix: 16)]);
}

String sha256Hex(String s) => crypto.sha256.convert(utf8.encode(s)).toString();

/// The directory a contract unit is certified in, under [programs].
String certifiedDir(String programs, String id) => '$programs/vglp/cva/gas_worlds/certified/$id';

/// Compile the contract unit [files] (contract/self.glp, contract/platform.vglp)
/// of contract [id] in the tree [programs], and give its certificate:
/// [compiler]'s, or a fresh key's where none is given; refused where the
/// unit reaches the network or the person.  Throws where it does not compile.
Certificate compileContract(String programs, String id, Map<String, Object?> files,
    {PersonIdentity? compiler}) {
  if (!RegExp(r'^[A-Za-z][A-Za-z0-9]{0,30}$').hasMatch(id)) {
    throw const FormatException('a contract id of letters and digits');
  }
  final dir = Directory(certifiedDir(programs, id));
  if (dir.existsSync()) dir.deleteSync(recursive: true);
  dir.createSync(recursive: true);
  try {
    for (final f in contractFiles) {
      final text = files[f];
      if (text is! String) throw FormatException('the package has no $f');
      File('${dir.path}/${f.substring('contract/'.length)}').writeAsStringSync(text);
    }
    final engine = GlpEngine(rootSelfGlpPath: '$programs/self.glp', identity: compiler);
    engine.loadProgram(dir.path);
    final art = engine.appModule?.artefact;
    if (art is! Artefact) throw const FormatException('the contract unit compiled to no module');
    return art.certificate;
  } finally {
    if (dir.existsSync()) dir.deleteSync(recursive: true);
  }
}

/// A GLP signed term, as sign/3 writes it: the signer's key, the signature,
/// then e(sig(HSrc, T)), the signature over those encoded bytes.
String signTerm(PersonIdentity who, Uint8List hSrc, WireTerm term) {
  final content = encodeTermToBytes(WStruct('sig', [WConst(WBlob(hSrc)), term]));
  final w = WireWriter()
    ..bytes(who.pub.bytes)
    ..bytes(who.sign(content));
  return hexOf([...w.toBytes(), ...content]);
}

/// The signed term [hex], where its signature verifies under the key it
/// carries; null otherwise.
({PubKey key, Uint8List hSrc, WireTerm term})? readSigned(String hex) {
  try {
    final bytes = bytesOfHex(hex);
    final r = WireReader(bytes);
    final key = PubKey(Uint8List.fromList(r.bytes()));
    final signature = Uint8List.fromList(r.bytes());
    final content = Uint8List.fromList(Uint8List.sublistView(bytes, r.offset));
    final sig = decodeTermFromBytes(content);
    if (sig is! WStruct || sig.functor != 'sig' || sig.args.length != 2) return null;
    final h = sig.args[0];
    if (h is! WConst || h.constant is! WBlob) return null;
    if (!PersonIdentity.verify(key, content, signature)) return null;
    return (key: key, hSrc: (h.constant as WBlob).value, term: sig.args[1]);
  } on Object {
    return null;
  }
}

/// The string arguments of [t], where it is the structure [functor] of
/// strings only; null otherwise.
List<String>? stringArgs(WireTerm t, String functor) {
  if (t is! WStruct || t.functor != functor) return null;
  final out = <String>[];
  for (final a in t.args) {
    if (a is! WConst || a.constant is! WString) return null;
    out.add((a.constant as WString).value);
  }
  return out;
}

WireTerm strings(String functor, List<String> args) =>
    WStruct(functor, [for (final a in args) WConst(WString(a))]);

/// A key pair kept in [path], a file holding its Ed25519 seed as hex; made
/// there where there is none.
PersonIdentity keyAt(String path) {
  final f = File(path);
  if (!f.existsSync()) {
    final seed = Uint8List.fromList(List.generate(32, (_) => _random.nextInt(256)));
    f.parent.createSync(recursive: true);
    f.writeAsStringSync('${hexOf(seed)}\n');
  }
  return PersonIdentity.fromSeed(bytesOfHex(f.readAsStringSync().trim()));
}

final _random = Random.secure();

/// The verdict the checker gives a syntactically grassroots contract begins so.
const grassroots = 'syntactically_grassroots';

/// The warranties of the contract unit [files] of contract [id]: compiled
/// under [compiler], its creator [creator]; [text], [schemas] and [verdict]
/// the contract's text, its speech-act schemas in GSC's notation, and the
/// checker's verdict on them.  Throws where the unit is refused a
/// certificate or the verdict is not grassroots.
Map<String, Object?> warrant({
  required String programs,
  required String id,
  required Map<String, Object?> files,
  required String text,
  required String schemas,
  required String verdict,
  required PersonIdentity compiler,
  required PersonIdentity creator,
}) {
  if (!verdict.startsWith(grassroots)) {
    throw FormatException('the checker did not find $id grassroots: $verdict');
  }
  final cert = compileContract(programs, id, files, compiler: compiler);
  if (cert.isRefused) {
    throw FormatException('$id\'s code reaches the network or the person');
  }
  return {
    'contract': {'text': text, 'schemas': schemas, 'verdict': verdict},
    'compiler': {
      'key': hexOf(cert.agent),
      'hsrc': hexOf(cert.hSrc),
      'hbin': hexOf(cert.hBin),
      'sig': hexOf(cert.signature),
    },
    'creator': signTerm(creator, cert.hSrc,
        strings('creator_cert', [hexOf(cert.hSrc), sha256Hex(schemas), verdict])),
  };
}

/// What a device found of a package's warranties: who compiled and who
/// created its contract unit, and the unit's source identity.
class Warranted {
  final String compiler, creator, hSrc, verdict;
  const Warranted(this.compiler, this.creator, this.hSrc, this.verdict);
  Map<String, String> toJson() => {'compiler': compiler, 'creator': creator, 'hsrc': hSrc, 'verdict': verdict};
}

/// Check the warranties [w] of the contract unit [files] of contract [id] in
/// the tree [programs], by compiling the unit here: it must be given a
/// certificate, and its two identities must be those the compiler's warranty
/// names; that warranty's signature must verify, and the creator's must
/// verify over the same unit, the package's schemas, and a grassroots
/// verdict.  Throws a FormatException saying what fails.
Warranted checkWarranty(String programs, String id, Map<String, Object?> files, Object? w) {
  if (w is! Map) throw const FormatException('the contract carries no warranties');
  final contract = w['contract'], comp = w['compiler'], creator = w['creator'];
  if (contract is! Map || comp is! Map || creator is! String) {
    throw const FormatException('the contract\'s warranties are incomplete');
  }
  final schemas = contract['schemas'], verdict = contract['verdict'];
  if (schemas is! String || verdict is! String || contract['text'] is! String) {
    throw const FormatException('the warranties carry no contract');
  }
  if (!verdict.startsWith(grassroots)) {
    throw const FormatException('the contract is not warranted grassroots');
  }
  final Certificate local;
  try {
    local = compileContract(programs, id, files);
  } on FormatException {
    rethrow;
  } on Object catch (e) {
    throw FormatException('the contract\'s code does not compile here: ${'$e'.split('\n').first}');
  }
  if (local.isRefused) {
    throw const FormatException('the contract\'s code reaches the network or the person');
  }
  final Certificate theirs;
  try {
    theirs = Certificate(
      agent: bytesOfHex('${comp['key']}'),
      hSrc: bytesOfHex('${comp['hsrc']}'),
      hBin: bytesOfHex('${comp['hbin']}'),
      signature: bytesOfHex('${comp['sig']}'),
    );
  } on Object {
    throw const FormatException('the compiler\'s warranty is malformed');
  }
  if (hexOf(theirs.hSrc) != hexOf(local.hSrc) || hexOf(theirs.hBin) != hexOf(local.hBin)) {
    throw const FormatException('the compiler\'s warranty is for other code (or another version of GAS)');
  }
  if (!theirs.verifies()) throw const FormatException('the compiler\'s signature does not verify');
  final signed = readSigned(creator);
  if (signed == null) throw const FormatException('the creator\'s signature does not verify');
  final args = stringArgs(signed.term, 'creator_cert');
  if (args == null ||
      args.length != 3 ||
      hexOf(signed.hSrc) != hexOf(local.hSrc) ||
      args[0] != hexOf(local.hSrc) ||
      args[1] != sha256Hex(schemas) ||
      args[2] != verdict) {
    throw const FormatException('the creator\'s warranty is for another contract or other code');
  }
  return Warranted(hexOf(theirs.agent), signed.key.hex, hexOf(local.hSrc), verdict);
}
