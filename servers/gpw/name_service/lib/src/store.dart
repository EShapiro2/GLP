/// Per-web-name state of the name server, kept on disk (one JSON file per
/// web-name) so the zone's bindings survive restarts and the server can be
/// rebuilt.  Retired names keep their final state as a tombstone in an
/// archive file; the label itself returns to the zone's pool.
library;

import 'dart:convert';
import 'dart:io';

/// The stored state of one web-name.
class NameState {
  NameState({
    required this.manifestBody,
    required this.manifestSignature,
    required this.counterSignature,
    required this.manifestEpoch,
    this.repointEpoch = -1,
    this.mirror,
    this.retirement,
  });

  /// The name-manifest body and the person's signature, as deposited.
  Map<String, Object?> manifestBody;
  String manifestSignature;

  /// The server's counter-signature block.
  Map<String, Object?> counterSignature;

  int manifestEpoch;

  /// Highest repoint epoch seen; -1 before the first repoint.
  int repointEpoch;

  /// Current mirroring-server target, if repointed.
  String? mirror;

  /// The retirement envelope, once retired (the tombstone).
  Map<String, Object?>? retirement;

  bool get retired => retirement != null;

  Map<String, Object?> toJson() => {
        'manifestBody': manifestBody,
        'manifestSignature': manifestSignature,
        'counterSignature': counterSignature,
        'manifestEpoch': manifestEpoch,
        'repointEpoch': repointEpoch,
        'mirror': mirror,
        'retirement': retirement,
      };

  static NameState fromJson(Map<String, Object?> json) => NameState(
        manifestBody:
            (json['manifestBody'] as Map).cast<String, Object?>(),
        manifestSignature: json['manifestSignature'] as String,
        counterSignature:
            (json['counterSignature'] as Map).cast<String, Object?>(),
        manifestEpoch: json['manifestEpoch'] as int,
        repointEpoch: json['repointEpoch'] as int? ?? -1,
        mirror: json['mirror'] as String?,
        retirement:
            (json['retirement'] as Map?)?.cast<String, Object?>(),
      );

  /// The counter-signed manifest as served on GET.
  Map<String, Object?> served() => {
        'body': manifestBody,
        'signature': manifestSignature,
        'counterSignature': counterSignature,
      };
}

/// Disk store: `<dir>/names/<webName>.json`, written atomically.
class NameStore {
  NameStore(String dir) : _names = Directory('$dir/names') {
    _names.createSync(recursive: true);
  }

  final Directory _names;

  File _file(String webName) => File('${_names.path}/$webName.json');

  NameState? load(String webName) {
    final f = _file(webName);
    if (!f.existsSync()) return null;
    return NameState.fromJson(
        (jsonDecode(f.readAsStringSync()) as Map).cast<String, Object?>());
  }

  void save(String webName, NameState state) {
    final f = _file(webName);
    final tmp = File('${f.path}.tmp');
    tmp.writeAsStringSync(jsonEncode(state.toJson()));
    tmp.renameSync(f.path);
  }

  /// The public keys of all currently bound (unretired) web-names.
  Set<String> boundKeys() {
    final keys = <String>{};
    for (final f in _names.listSync().whereType<File>()) {
      final name = f.uri.pathSegments.last;
      if (!name.endsWith('.json') || name.contains('.retired.')) continue;
      final state = NameState.fromJson(
          (jsonDecode(f.readAsStringSync()) as Map).cast<String, Object?>());
      if (!state.retired) {
        keys.add(state.manifestBody['publicKey'] as String);
      }
    }
    return keys;
  }

  /// Archive a retired name's final state (tombstone) aside, freeing the
  /// file for a fresh binding.  Called when a retired label is re-allocated;
  /// until then the tombstone stays in place so the retirement epoch guards
  /// against replay of an older manifest.
  void archiveTombstone(String webName, NameState state) {
    final epoch = (state.retirement?['body'] as Map?)?['epoch'] ?? 'x';
    _file(webName)
        .renameSync('${_names.path}/$webName.retired.$epoch.json');
  }
}
