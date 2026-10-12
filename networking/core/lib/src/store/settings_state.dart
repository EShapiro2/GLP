import '../models/underlay.dart';
import '../platform/compat.dart';

/// Debug knob: which BLE roles this device should run.
///
/// In production both roles are always on (`auto`) so peer discovery is
/// symmetric. The other modes exist so a developer can deterministically
/// produce asymmetric topologies for testing — e.g. force one device to be
/// peripheral-only and confirm the consumer falls back to the peripheral
/// path for outbound sends.
enum BleRoleMode {
  /// Scan AND advertise (default; produces both central and peripheral paths).
  auto,

  /// Scan only — never advertise. Peers won't dial us, so we'll only ever
  /// have `central:*` paths.
  centralOnly,

  /// Advertise only — never scan. We won't dial peers, so we'll only ever
  /// have `peripheral:*` paths.
  peripheralOnly,
}

/// Policy for unsolicited proximity contact from peers that do not already
/// have an accepted relationship with us, held per [ProximityUnderlay]. Spec
/// §Trust levels.
enum ColdCallTrustLevel {
  /// Complete the ANNOUNCE exchange and the session handshake with any
  /// unknown contact on this underlay, revealing our full public key.
  open,

  /// Complete neither with an unknown key: unmatched encounters on this
  /// underlay are ignored.
  closed,
}

/// The layer's persisted settings: what GLP and a developer set on a running
/// layer, as against what it is instantiated with.
///
/// The underlays and the rendezvous servers are not here: they are
/// constructor arguments of the layer (GNA §Instantiation: "The layer is
/// created once per agent, with everything it needs to run; nothing in it
/// changes for the layer's lifetime").  Until 2026-10-12 they were settings,
/// toggled and edited on a running layer.
@immutable
class SettingsState {
  /// Which BLE roles this device should run. Default `auto`.
  final BleRoleMode bleRoleMode;

  /// The cold-call trust level of each proximity underlay (spec §Trust
  /// levels). Both default to Closed, and GLP sets one underlay's level with
  /// `setTrustLevel(underlay, level)`.
  ///
  /// The two levels are independent because the declarations behind them are:
  /// this place is trusted, so PAN contact here is open; this local network is
  /// trusted, so LAN contact on it is open. Neither declaration implies the
  /// other, and neither is global.
  final Map<ProximityUnderlay, ColdCallTrustLevel> coldCallTrustLevels;

  const SettingsState({
    this.bleRoleMode = BleRoleMode.auto,
    this.coldCallTrustLevels = closedOnEveryUnderlay,
  });

  static const SettingsState initial = SettingsState();

  /// The starting level of every proximity underlay: until GLP sets one,
  /// both are Closed.
  static const Map<ProximityUnderlay, ColdCallTrustLevel>
      closedOnEveryUnderlay = {
    ProximityUnderlay.pan: ColdCallTrustLevel.closed,
    ProximityUnderlay.lan: ColdCallTrustLevel.closed,
  };

  /// The cold-call trust level of [underlay]. Closed until GLP sets it.
  ColdCallTrustLevel trustLevelOf(ProximityUnderlay underlay) =>
      coldCallTrustLevels[underlay] ?? ColdCallTrustLevel.closed;

  /// Whether [underlay] answers unsolicited contact from an unknown key.
  bool isOpenOn(ProximityUnderlay underlay) =>
      trustLevelOf(underlay) == ColdCallTrustLevel.open;

  SettingsState copyWith({
    BleRoleMode? bleRoleMode,
    Map<ProximityUnderlay, ColdCallTrustLevel>? coldCallTrustLevels,
  }) {
    return SettingsState(
      bleRoleMode: bleRoleMode ?? this.bleRoleMode,
      coldCallTrustLevels: coldCallTrustLevels ?? this.coldCallTrustLevels,
    );
  }

  /// Set the level of [underlay], leaving every other underlay's untouched.
  SettingsState withTrustLevel(
    ProximityUnderlay underlay,
    ColdCallTrustLevel level,
  ) =>
      copyWith(
        coldCallTrustLevels: {...coldCallTrustLevels, underlay: level},
      );

  Map<String, dynamic> toJson() => {
        'bleRoleMode': bleRoleMode.name,
        'coldCallTrustLevels': {
          for (final entry in coldCallTrustLevels.entries)
            entry.key.name: entry.value.name,
        },
      };

  /// Decode a persisted blob: exactly a BLE role mode and a trust level per
  /// proximity underlay, and nothing else (no tolerant decoder,
  /// `networking/CLAUDE.md`).
  ///
  /// A blob that lacks either, carries any other key, or names no
  /// [BleRoleMode] is malformed: it throws, and the caller discards it whole
  /// and falls back to the default.  A blob written before 2026-10-12, which
  /// carried the transport toggles and the rendezvous servers, is such a blob.
  factory SettingsState.fromJson(Map<String, dynamic> json) {
    const keys = {'bleRoleMode', 'coldCallTrustLevels'};
    final unknown = json.keys.where((key) => !keys.contains(key)).toList();
    if (unknown.isNotEmpty) {
      throw FormatException('settings: unknown keys $unknown');
    }
    final roleModeName = json['bleRoleMode'];
    final bleRoleMode = BleRoleMode.values.firstWhere(
      (mode) => mode.name == roleModeName,
      orElse: () => throw FormatException(
        'settings: bleRoleMode must be one of '
        '${BleRoleMode.values.map((m) => m.name).join(', ')}, '
        'got $roleModeName',
      ),
    );
    return SettingsState(
      bleRoleMode: bleRoleMode,
      coldCallTrustLevels:
          _decodeColdCallTrustLevels(json['coldCallTrustLevels']),
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is SettingsState &&
          runtimeType == other.runtimeType &&
          bleRoleMode == other.bleRoleMode &&
          mapEquals(coldCallTrustLevels, other.coldCallTrustLevels);

  @override
  int get hashCode => Object.hash(
        bleRoleMode,
        Object.hashAll([
          for (final underlay in ProximityUnderlay.values)
            trustLevelOf(underlay),
        ]),
      );

  @override
  String toString() => 'SettingsState(bleRoleMode: ${bleRoleMode.name}, '
      'trust: ${{
        for (final u in ProximityUnderlay.values) u.name: trustLevelOf(u).name,
      }})';
}

/// Decode the persisted trust levels: a level for each [ProximityUnderlay],
/// keyed by its name, and nothing else (no tolerant decoder,
/// `networking/CLAUDE.md`).
///
/// A blob that lacks a level for either underlay, carries any other key, or
/// names no [ColdCallTrustLevel] is malformed: it throws, and the caller falls
/// back to the default — Closed on every underlay, which is what an unset
/// level means.
Map<ProximityUnderlay, ColdCallTrustLevel> _decodeColdCallTrustLevels(
  Object? json,
) {
  if (json is! Map) {
    throw FormatException(
      'settings: coldCallTrustLevels must be a map of '
      'ProximityUnderlay to ColdCallTrustLevel, got $json',
    );
  }
  final names = {for (final underlay in ProximityUnderlay.values) underlay.name};
  final unknown = json.keys.where((key) => !names.contains(key)).toList();
  if (unknown.isNotEmpty) {
    throw FormatException(
      'settings: coldCallTrustLevels names no ProximityUnderlay $unknown',
    );
  }
  return {
    for (final underlay in ProximityUnderlay.values)
      underlay: ColdCallTrustLevel.values.firstWhere(
        (level) => level.name == json[underlay.name],
        orElse: () => throw FormatException(
          'settings: coldCallTrustLevels.${underlay.name} must be one of '
          '${ColdCallTrustLevel.values.map((l) => l.name).join(', ')}, '
          'got ${json[underlay.name]}',
        ),
      ),
  };
}
