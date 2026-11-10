import 'dart:typed_data';

/// The kind of an agent's layer (GNA api.tex, "Versions", sec:versions): "The
/// layer of a smartphone and the layer of a server are versioned
/// independently.  Each agent's layer has a kind, smartphone or server".  The
/// Flutter profile is a smartphone's layer and the headless profile a
/// server's ("A headless server profile", GNA, Attestation).
enum LayerKind {
  smartphone(1),
  server(2);

  const LayerKind(this.value);

  /// The byte the kind travels as.
  final int value;

  static LayerKind fromValue(int value) => switch (value) {
        1 => LayerKind.smartphone,
        2 => LayerKind.server,
        _ => throw FormatException('no layer kind $value'),
      };
}

/// What a layer declares of its versions, in the handshake envelope and in an
/// ANNOUNCE that precedes it (GNA api.tex, "Versions"): "each peer declares
/// its kind, its version, its earliest compatible version, and the version
/// of the other's kind that it works with.  Each peer checks that the version
/// the other works with lies between its own earliest compatible version and
/// its own version; where both checks pass the session is established, and
/// each talks to the other as the version the other works with, and
/// otherwise the connection is refused. ... Two peers of one kind do the
/// same, each declaring the version of its own kind that it works with."
///
/// A peer declares the version it works with of each kind, smartphone and
/// server, since an initiator does not know the kind of the peer it dials
/// before the handshake; the receiver reads the one of its own kind.  Its
/// own kind's is its own version unless it is given another.
///
/// On the wire, 9 bytes, big-endian: the kind (1), the version (2), the
/// earliest compatible version (2), the smartphone version it works with (2)
/// and the server version it works with (2).  It replaces, since 2026-11-10,
/// the one protocol version both kinds shared (9 at the last) and the
/// refusal of any other.
class VersionDeclaration {
  /// The layer's kind.
  final LayerKind kind;

  /// Its version: "a whole number that rises with each change of what its
  /// kind sends and accepts".
  final int version;

  /// Its earliest compatible version: "the earliest version of its kind as
  /// which it can still talk".
  final int earliest;

  /// The smartphone version it works with.
  final int worksWithSmartphone;

  /// The server version it works with.
  final int worksWithServer;

  const VersionDeclaration({
    required this.kind,
    required this.version,
    required this.earliest,
    required this.worksWithSmartphone,
    required this.worksWithServer,
  });

  /// The first versions (GSA #3's task of 2026-11-10 18:13 UTC, from GNA
  /// 099b6b5): smartphone 1 and server 1, each compatible back to 1.
  static const int smartphoneVersion = 1;
  static const int smartphoneEarliest = 1;
  static const int serverVersion = 1;
  static const int serverEarliest = 1;

  /// A smartphone's layer at the current versions.
  static const VersionDeclaration smartphone = VersionDeclaration(
    kind: LayerKind.smartphone,
    version: smartphoneVersion,
    earliest: smartphoneEarliest,
    worksWithSmartphone: smartphoneVersion,
    worksWithServer: serverVersion,
  );

  /// A server's layer at the current versions.
  static const VersionDeclaration server = VersionDeclaration(
    kind: LayerKind.server,
    version: serverVersion,
    earliest: serverEarliest,
    worksWithSmartphone: smartphoneVersion,
    worksWithServer: serverVersion,
  );

  /// Its length on the wire.
  static const int length = 9;

  /// The version of [other] kind it works with.
  int worksWith(LayerKind other) => switch (other) {
        LayerKind.smartphone => worksWithSmartphone,
        LayerKind.server => worksWithServer,
      };

  /// The check of [other]'s declaration: the version [other] works with of
  /// this layer's kind, where it lies between this layer's earliest
  /// compatible version and its version, and this layer talks to [other] as
  /// it; null where it does not, and the connection is refused.
  int? accepts(VersionDeclaration other) {
    final w = other.worksWith(kind);
    return w >= earliest && w <= version ? w : null;
  }

  Uint8List encode() {
    final b = ByteData(length)
      ..setUint8(0, kind.value)
      ..setUint16(1, version, Endian.big)
      ..setUint16(3, earliest, Endian.big)
      ..setUint16(5, worksWithSmartphone, Endian.big)
      ..setUint16(7, worksWithServer, Endian.big);
    return b.buffer.asUint8List();
  }

  /// Read the declaration at [offset] of [bytes]; a [FormatException] where
  /// it is short or names no kind.
  static VersionDeclaration decode(Uint8List bytes, [int offset = 0]) {
    if (bytes.length < offset + length) {
      throw const FormatException('the version declaration is truncated');
    }
    final b = ByteData.sublistView(bytes, offset, offset + length);
    return VersionDeclaration(
      kind: LayerKind.fromValue(b.getUint8(0)),
      version: b.getUint16(1, Endian.big),
      earliest: b.getUint16(3, Endian.big),
      worksWithSmartphone: b.getUint16(5, Endian.big),
      worksWithServer: b.getUint16(7, Endian.big),
    );
  }

  /// The [FormatException] of a refusal of [other] by [self].
  static FormatException refusal(
          VersionDeclaration self, VersionDeclaration other) =>
      FormatException('refused: the ${other.kind.name} peer works with '
          '${self.kind.name} version ${other.worksWith(self.kind)}, outside '
          'this ${self.kind.name}\'s ${self.earliest} to ${self.version}');

  @override
  bool operator ==(Object other) =>
      other is VersionDeclaration &&
      other.kind == kind &&
      other.version == version &&
      other.earliest == earliest &&
      other.worksWithSmartphone == worksWithSmartphone &&
      other.worksWithServer == worksWithServer;

  @override
  int get hashCode =>
      Object.hash(kind, version, earliest, worksWithSmartphone, worksWithServer);

  @override
  String toString() => '${kind.name} v$version (from $earliest; works with '
      'smartphone $worksWithSmartphone, server $worksWithServer)';
}
