import '../platform/compat.dart';

/// Available transport protocols
enum TransportProtocol {
  bluetooth,
  udp,
}

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

/// The medium on which a nearby agent is discovered: BLE radio range, or the
/// local network. Spec `docs/GLP_Networking_API/sections/api.tex` §Networking
/// API.
///
/// Not a `MessageTransport`: a LAN session is an IP session. A trust level is
/// keyed on the medium of discovery, never on the transport carrying the
/// session it leads to — keying on the transport would tie LAN contact to
/// contact from the Internet at large.
enum ProximityMedium {
  /// Discovered by BLE advertisement.
  ble,

  /// Discovered by mDNS on the local network.
  lan,
}

/// Policy for unsolicited proximity contact from peers that do not already
/// have an accepted relationship with us. Spec §Trust levels.
enum ColdCallTrustLevel {
  /// Complete the ANNOUNCE exchange and the session handshake with any
  /// unknown contact on this medium, revealing our full public key.
  open,

  /// Complete neither with an unknown key: unmatched encounters on this
  /// medium are ignored.
  closed,
}

/// Extension for display info
extension TransportProtocolDisplay on TransportProtocol {
  String get displayName {
    switch (this) {
      case TransportProtocol.bluetooth:
        return 'Bluetooth';
      case TransportProtocol.udp:
        return 'Internet (UDP)';
    }
  }

  String get description {
    switch (this) {
      case TransportProtocol.bluetooth:
        return 'Connect to nearby peers via Bluetooth Low Energy';
      case TransportProtocol.udp:
        return 'Connect to peers over the Internet via UDP';
    }
  }
}

/// Immutable transport settings for Redux store
@immutable
class RendezvousServerSettings {
  final String address;
  final String pubkeyHex;

  const RendezvousServerSettings({
    required this.address,
    required this.pubkeyHex,
  });

  Map<String, dynamic> toJson() => {
        'address': address,
        'pubkeyHex': pubkeyHex,
      };

  factory RendezvousServerSettings.fromJson(Map<String, dynamic> json) {
    return RendezvousServerSettings(
      address: json['address'] as String? ?? '',
      pubkeyHex: json['pubkeyHex'] as String? ?? '',
    );
  }

  String get normalizedPubkeyHex => pubkeyHex.toLowerCase();

  String get configKey => '$normalizedPubkeyHex@${address.trim()}';

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is RendezvousServerSettings &&
          runtimeType == other.runtimeType &&
          address == other.address &&
          pubkeyHex == other.pubkeyHex;

  @override
  int get hashCode => Object.hash(address, pubkeyHex);
}

/// Immutable transport settings for Redux store
@immutable
class SettingsState {
  /// Whether Bluetooth transport is enabled
  final bool bluetoothEnabled;

  /// Whether UDP Internet transport is enabled
  final bool udpEnabled;

  /// Priority order for transports (lower index = higher priority)
  /// Default: Bluetooth first, then UDP
  final List<TransportProtocol> transportPriority;

  /// Rendezvous server address (e.g. "[2600:1234::1]:9514").
  /// Null means no rendezvous server configured.
  final String? anchorAddress;

  /// Rendezvous server public key hex (64 chars, 32 bytes).
  /// The server has its own independent Ed25519 keypair — this must
  /// be configured explicitly (not derived from the owner's key).
  final String? anchorPubkeyHex;

  /// Full configured rendezvous server list.
  final List<RendezvousServerSettings> rendezvousServers;

  /// Which BLE roles this device should run. Default `auto`.
  final BleRoleMode bleRoleMode;

  /// The cold-call trust level of each proximity medium (spec §Trust levels).
  /// Both default to Closed, and GLP sets one medium's level with
  /// `setTrustLevel(medium, level)`.
  ///
  /// The two levels are independent because the declarations behind them are:
  /// this place is trusted, so BLE contact here is open; this local network is
  /// trusted, so LAN contact on it is open. Neither declaration implies the
  /// other, and neither is global.
  final Map<ProximityMedium, ColdCallTrustLevel> coldCallTrustLevels;

  const SettingsState({
    this.bluetoothEnabled = true,
    this.udpEnabled = true,
    this.transportPriority = const [
      TransportProtocol.bluetooth,
      TransportProtocol.udp,
    ],
    this.anchorAddress,
    this.anchorPubkeyHex,
    this.rendezvousServers = const [],
    this.bleRoleMode = BleRoleMode.auto,
    this.coldCallTrustLevels = closedOnEveryMedium,
  });

  static const SettingsState initial = SettingsState();

  /// The starting level of every proximity medium: until GLP sets one, both
  /// are Closed.
  static const Map<ProximityMedium, ColdCallTrustLevel> closedOnEveryMedium = {
    ProximityMedium.ble: ColdCallTrustLevel.closed,
    ProximityMedium.lan: ColdCallTrustLevel.closed,
  };

  /// The cold-call trust level of [medium]. Closed until GLP sets it.
  ColdCallTrustLevel trustLevelOf(ProximityMedium medium) =>
      coldCallTrustLevels[medium] ?? ColdCallTrustLevel.closed;

  /// Whether [medium] answers unsolicited contact from an unknown key.
  bool isOpenOn(ProximityMedium medium) =>
      trustLevelOf(medium) == ColdCallTrustLevel.open;

  /// Whether at least one transport is enabled
  bool get hasActiveTransport => bluetoothEnabled || udpEnabled;

  /// Get the preferred transport for sending messages
  TransportProtocol? get preferredTransport {
    for (final transport in transportPriority) {
      if (transport == TransportProtocol.bluetooth && bluetoothEnabled) {
        return TransportProtocol.bluetooth;
      }
      if (transport == TransportProtocol.udp && udpEnabled) {
        return TransportProtocol.udp;
      }
    }
    return null;
  }

  List<RendezvousServerSettings> get configuredRendezvousServers {
    final merged = <RendezvousServerSettings>[];
    final seen = <String>{};

    for (final server in rendezvousServers) {
      final key = server.configKey;
      if (server.address.trim().isEmpty || server.pubkeyHex.trim().isEmpty) {
        continue;
      }
      if (seen.add(key)) {
        merged.add(server);
      }
    }

    if (anchorAddress != null &&
        anchorAddress!.isNotEmpty &&
        anchorPubkeyHex != null &&
        anchorPubkeyHex!.isNotEmpty) {
      final legacy = RendezvousServerSettings(
        address: anchorAddress!,
        pubkeyHex: anchorPubkeyHex!,
      );
      if (seen.add(legacy.configKey)) {
        merged.add(legacy);
      }
    }

    return List<RendezvousServerSettings>.unmodifiable(merged);
  }

  /// Whether a rendezvous server is fully configured (address + pubkey).
  bool get hasAnchor => configuredRendezvousServers.isNotEmpty;

  SettingsState copyWith({
    bool? bluetoothEnabled,
    bool? udpEnabled,
    List<TransportProtocol>? transportPriority,
    List<RendezvousServerSettings>? rendezvousServers,
    BleRoleMode? bleRoleMode,
    Map<ProximityMedium, ColdCallTrustLevel>? coldCallTrustLevels,
    // Use Object? + sentinel so callers can pass null to clear.
    Object? anchorAddress = _sentinel,
    Object? anchorPubkeyHex = _sentinel,
  }) {
    return SettingsState(
      bluetoothEnabled: bluetoothEnabled ?? this.bluetoothEnabled,
      udpEnabled: udpEnabled ?? this.udpEnabled,
      transportPriority: transportPriority ?? this.transportPriority,
      rendezvousServers: rendezvousServers ?? this.rendezvousServers,
      bleRoleMode: bleRoleMode ?? this.bleRoleMode,
      coldCallTrustLevels: coldCallTrustLevels ?? this.coldCallTrustLevels,
      anchorAddress: identical(anchorAddress, _sentinel)
          ? this.anchorAddress
          : anchorAddress as String?,
      anchorPubkeyHex: identical(anchorPubkeyHex, _sentinel)
          ? this.anchorPubkeyHex
          : anchorPubkeyHex as String?,
    );
  }

  /// Set the level of [medium], leaving every other medium's untouched.
  SettingsState withTrustLevel(
    ProximityMedium medium,
    ColdCallTrustLevel level,
  ) =>
      copyWith(
        coldCallTrustLevels: {...coldCallTrustLevels, medium: level},
      );

  Map<String, dynamic> toJson() => {
        'bluetoothEnabled': bluetoothEnabled,
        'udpEnabled': udpEnabled,
        'transportPriority': transportPriority.map((t) => t.name).toList(),
        'anchorAddress': anchorAddress,
        'anchorPubkeyHex': anchorPubkeyHex,
        'rendezvousServers':
            rendezvousServers.map((server) => server.toJson()).toList(),
        'bleRoleMode': bleRoleMode.name,
        'coldCallTrustLevels': {
          for (final entry in coldCallTrustLevels.entries)
            entry.key.name: entry.value.name,
        },
      };

  factory SettingsState.fromJson(Map<String, dynamic> json) {
    final rendezvousServers = (json['rendezvousServers'] as List<dynamic>?)
            ?.map((entry) => RendezvousServerSettings.fromJson(
                entry as Map<String, dynamic>))
            .toList() ??
        const <RendezvousServerSettings>[];

    final roleModeName = json['bleRoleMode'] as String?;
    final bleRoleMode = BleRoleMode.values.firstWhere(
      (m) => m.name == roleModeName,
      orElse: () => BleRoleMode.auto,
    );
    // A level per proximity medium is required. A blob written before the
    // levels were split carries a single `coldCallTrustLevel` scalar and is
    // malformed under this shape; it throws, and the caller falls back to the
    // default — Closed on every medium, which is what an unset level means.
    final trustLevelsJson = json['coldCallTrustLevels'];
    if (trustLevelsJson is! Map) {
      throw FormatException(
        'settings: coldCallTrustLevels must be a map of '
        'ProximityMedium to ColdCallTrustLevel, got $trustLevelsJson',
      );
    }
    final coldCallTrustLevels = <ProximityMedium, ColdCallTrustLevel>{
      for (final medium in ProximityMedium.values)
        medium: ColdCallTrustLevel.values.firstWhere(
          (level) => level.name == trustLevelsJson[medium.name],
          orElse: () => ColdCallTrustLevel.closed,
        ),
    };

    return SettingsState(
      bluetoothEnabled: json['bluetoothEnabled'] as bool? ?? true,
      udpEnabled: json['udpEnabled'] as bool? ?? true,
      transportPriority: (json['transportPriority'] as List<dynamic>?)
              ?.map((e) => TransportProtocol.values.firstWhere(
                    (t) => t.name == e,
                    orElse: () => TransportProtocol.bluetooth,
                  ))
              .toList() ??
          const [TransportProtocol.bluetooth, TransportProtocol.udp],
      anchorAddress: json['anchorAddress'] as String?,
      anchorPubkeyHex: json['anchorPubkeyHex'] as String?,
      rendezvousServers: rendezvousServers,
      bleRoleMode: bleRoleMode,
      coldCallTrustLevels: coldCallTrustLevels,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is SettingsState &&
          runtimeType == other.runtimeType &&
          bluetoothEnabled == other.bluetoothEnabled &&
          udpEnabled == other.udpEnabled &&
          listEquals(transportPriority, other.transportPriority) &&
          anchorAddress == other.anchorAddress &&
          anchorPubkeyHex == other.anchorPubkeyHex &&
          listEquals(rendezvousServers, other.rendezvousServers) &&
          bleRoleMode == other.bleRoleMode &&
          mapEquals(coldCallTrustLevels, other.coldCallTrustLevels);

  @override
  int get hashCode => Object.hash(
        bluetoothEnabled,
        udpEnabled,
        Object.hashAll(transportPriority),
        anchorAddress,
        anchorPubkeyHex,
        Object.hashAll(rendezvousServers),
        bleRoleMode,
        Object.hashAll([
          for (final medium in ProximityMedium.values)
            trustLevelOf(medium),
        ]),
      );

  @override
  String toString() => 'SettingsState(bt: $bluetoothEnabled, udp: $udpEnabled, '
      'rendezvous: ${configuredRendezvousServers.length})';
}

/// Sentinel for copyWith — distinguishes "not passed" from "passed null".
const _sentinel = Object();
