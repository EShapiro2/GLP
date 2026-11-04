// The contracts on this device, as packages (tool/packages.py): each named by
// the SHA-256 of its bytes, installed under the canonical platforms' worlds
// as live_<id>_<first 8 hex of the hash>/, its spec where the app page reads
// it.  The starters come in the app; the others from GAS's catalog on
// talmonlab, each checked against its hash before it is installed.
//
// Two people's agents work together only when they run the same package, so
// a contract here is named by its directory, id and hash together: that name
// is the page's contract, and the relay keeps each one's messages apart.
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Sha256;

/// Where the catalog and the packages are, and where the apps made with
/// GAS's app maker are (unlisted: in no catalog, got by their hash).
const downloads = 'https://talmonlab.bgu.ac.il/downloads';
const made = 'https://talmonlab.bgu.ac.il/gas/made';

/// A contract's link: GAS's page for getting it, its hash after the '#'.
String contractLink(String hash) => 'https://talmonlab.bgu.ac.il/gas/get.html#$hash';

final _hex64 = RegExp(r'[0-9a-f]{64}');

/// What a pasted or opened link asks: a contract to get (get.html#H,
/// gas://get/H), a person to add, with the contract they are in
/// (add.html#K&c=H, gas://add/K?c=H), or nothing.
class GasLink {
  final String? person, contract;
  const GasLink({this.person, this.contract});

  static GasLink? parse(String text) {
    final t = text.trim().toLowerCase();
    final add = RegExp(r'(?:add\.html#|gas://add/)([0-9a-f]{64})(?:[&?]c=([0-9a-f]{64}))?').firstMatch(t);
    if (add != null) return GasLink(person: add.group(1), contract: add.group(2));
    final get = RegExp(r'(?:get\.html#|gas://get/)([0-9a-f]{64})').firstMatch(t);
    if (get != null) return GasLink(contract: get.group(1));
    if (RegExp(r'^[0-9a-f]{64}$').hasMatch(t)) return GasLink(person: t);
    return null;
  }
}

/// An entry of the catalog, or a contract installed here.
class Contract {
  final String id, title, hash, icon, about, family;
  final int bytes;
  const Contract({
    required this.id,
    required this.title,
    required this.hash,
    this.icon = '📄',
    this.about = '',
    this.family = 'other',
    this.bytes = 0,
  });

  /// Its name on this device, the page's contract and the relay's app.
  String get name => '${id.toLowerCase()}_${hash.substring(0, 8)}';

  factory Contract.fromJson(Map j) => Contract(
        id: j['id'] as String,
        title: j['title'] as String,
        hash: j['hash'] as String,
        icon: (j['icon'] as String?) ?? '📄',
        about: (j['about'] as String?) ?? '',
        family: (j['family'] as String?) ?? 'other',
        bytes: (j['bytes'] as int?) ?? 0,
      );

  Map<String, Object> toJson() =>
      {'id': id, 'title': title, 'hash': hash, 'icon': icon, 'about': about, 'family': family, 'bytes': bytes};
}

class ContractStore {
  /// The app's unpacked tree: programs/ and www/.
  final String base;
  final String server, madeServer;
  ContractStore(this.base, {this.server = downloads, this.madeServer = made});

  String get _worlds => '$base/programs/vglp/cva/gas_worlds';

  /// The directory a contract runs from.
  String program(Contract c) => '$_worlds/live_${c.name}';

  /// The contracts installed here, by title.
  List<Contract> installed() {
    final dir = Directory(_worlds);
    if (!dir.existsSync()) return [];
    final out = <Contract>[];
    for (final d in dir.listSync().whereType<Directory>()) {
      final f = File('${d.path}/package.json');
      if (!f.existsSync()) continue;
      try {
        out.add(Contract.fromJson(jsonDecode(f.readAsStringSync()) as Map));
      } on Object {
        // A damaged record: not listed.
      }
    }
    out.sort((a, b) => a.title.compareTo(b.title));
    return out;
  }

  Contract? byHash(String hash) {
    for (final c in installed()) {
      if (c.hash == hash) return c;
    }
    return null;
  }

  static Future<String> sha256(List<int> bytes) async =>
      (await Sha256().hash(bytes)).bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  /// Install the package [bytes]; when [expect] is given, only if they hash
  /// to it.  Only the harness's two files are written, under the contract's
  /// own directory.
  Future<Contract> install(Uint8List bytes, {String? expect}) async {
    final hash = await sha256(bytes);
    if (expect != null && hash != expect) {
      throw const FormatException('the download does not match its hash');
    }
    final j = jsonDecode(utf8.decode(bytes));
    if (j is! Map || j['gas'] != 1 || j['id'] is! String || j['title'] is! String) {
      throw const FormatException('not a GAS contract');
    }
    final id = j['id'] as String;
    if (!RegExp(r'^[A-Za-z][A-Za-z0-9]{0,30}$').hasMatch(id)) {
      throw const FormatException('a contract id of letters and digits');
    }
    final files = j['files'], spec = j['spec'], meta = (j['meta'] as Map?) ?? {};
    if (files is! Map || files['self.glp'] is! String || files['platform.vglp'] is! String || spec is! Map) {
      throw const FormatException('an incomplete contract');
    }
    final c = Contract(
      id: id,
      title: j['title'] as String,
      hash: hash,
      icon: (meta['icon'] as String?) ?? '📄',
      about: (meta['about'] as String?) ?? '',
      family: (meta['family'] as String?) ?? 'other',
      bytes: bytes.length,
    );
    final dir = Directory(program(c));
    await dir.create(recursive: true);
    await File('${dir.path}/self.glp').writeAsString(files['self.glp'] as String);
    await File('${dir.path}/platform.vglp').writeAsString(files['platform.vglp'] as String);
    final specs = Directory('$base/www/specs');
    await specs.create(recursive: true);
    await File('${specs.path}/${c.name}.json').writeAsString(jsonEncode({...spec, 'meta': meta}));
    await File('${dir.path}/package.json').writeAsString(jsonEncode(c.toJson()));
    return c;
  }

  /// Install every package the app carries (packages/pkg-HASH.json) not
  /// installed yet.
  Future<void> installStarters() async {
    final dir = Directory('$base/packages');
    if (!dir.existsSync()) return;
    final have = {for (final c in installed()) c.hash};
    for (final f in dir.listSync().whereType<File>()) {
      final m = RegExp(r'pkg-([0-9a-f]{64})\.json$').firstMatch(f.path);
      if (m == null || have.contains(m.group(1))) continue;
      await install(await f.readAsBytes(), expect: m.group(1));
    }
  }

  HttpClient _client() => HttpClient()
    ..connectionTimeout = const Duration(seconds: 15)
    ..findProxy = HttpClient.findProxyFromEnvironment;

  /// [path] from GAS, tried three times (a mobile network drops connections).
  Future<List<int>> _get(String path, [String? from]) async {
    for (var i = 1;; i++) {
      try {
        return await _get1(path, from);
      } on SocketException {
        if (i == 3) rethrow;
        await Future<void>.delayed(Duration(milliseconds: 500 * i));
      }
    }
  }

  Future<List<int>> _get1(String path, [String? from]) async {
    final c = _client();
    try {
      final res = await (await c.getUrl(Uri.parse('${from ?? server}/$path'))).close();
      if (res.statusCode != 200) throw HttpException('GAS answered ${res.statusCode}');
      final out = <int>[];
      await for (final chunk in res) {
        out.addAll(chunk);
      }
      return out;
    } finally {
      c.close();
    }
  }

  /// GAS's catalog.
  Future<List<Contract>> catalog() async {
    final j = jsonDecode(utf8.decode(await _get('catalog.json'))) as Map;
    return [for (final c in j['contracts'] as List) Contract.fromJson(c as Map)];
  }

  /// The contract of [hash], installed (from GAS when not here yet).
  Future<Contract> get(String hash) async {
    if (!_hex64.hasMatch(hash)) throw const FormatException('not a contract hash');
    final have = byHash(hash);
    if (have != null) return have;
    List<int> bytes;
    try {
      bytes = await _get('pkg-$hash.json');
    } on HttpException {
      bytes = await _get('pkg-$hash.json', madeServer); // made with the app maker
    }
    return install(Uint8List.fromList(bytes), expect: hash);
  }
}
