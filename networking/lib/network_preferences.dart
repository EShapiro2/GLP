import 'dart:convert';

import 'package:flutter/foundation.dart';
import 'package:grassroots_networking_core/src/models/rendezvous_server.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:shared_preferences/shared_preferences.dart';

/// The demo app's choice of how to instantiate the layer: the underlays it
/// runs and the rendezvous servers it connects to.
///
/// These are not the layer's settings.  The layer takes both as constructor
/// arguments and holds them for its lifetime (GNA §Instantiation: "The layer
/// is created once per agent, with everything it needs to run; nothing in it
/// changes for the layer's lifetime"), so the app keeps them here and, when
/// the person changes them, disposes the layer and instantiates another.
@immutable
class NetworkPreferences {
  /// The underlays the layer is instantiated with: a non-empty subset of
  /// PAN, LAN and WAN.
  final Set<Underlay> underlays;

  /// The rendezvous servers the layer is instantiated with.
  final List<RendezvousServer> rendezvousServers;

  NetworkPreferences({
    required Set<Underlay> underlays,
    required List<RendezvousServer> rendezvousServers,
  })  : underlays = Set.unmodifiable(underlays),
        rendezvousServers = List.unmodifiable(rendezvousServers);

  /// Every underlay, and no rendezvous server.
  static final NetworkPreferences defaults = NetworkPreferences(
    underlays: Underlay.values.toSet(),
    rendezvousServers: const [],
  );

  NetworkPreferences copyWith({
    Set<Underlay>? underlays,
    List<RendezvousServer>? rendezvousServers,
  }) =>
      NetworkPreferences(
        underlays: underlays ?? this.underlays,
        rendezvousServers: rendezvousServers ?? this.rendezvousServers,
      );

  Map<String, dynamic> toJson() => {
        'underlays': [for (final u in Underlay.values) if (underlays.contains(u)) u.name],
        'rendezvousServers': [
          for (final server in rendezvousServers)
            {'address': server.address, 'publicKey': server.pubkeyHex},
        ],
      };

  /// Decode a persisted blob: exactly the two keys, every underlay named, and
  /// every server an address and a 64-hex-character key, with nothing
  /// tolerated (`networking/CLAUDE.md`).  A malformed blob throws, and the
  /// store discards it whole.
  factory NetworkPreferences.fromJson(Map<String, dynamic> json) {
    _exactKeys(json, const {'underlays', 'rendezvousServers'}, 'preferences');
    final underlayNames = json['underlays'];
    if (underlayNames is! List || underlayNames.isEmpty) {
      throw FormatException(
          'preferences: underlays must be a non-empty list, got $underlayNames');
    }
    final underlays = <Underlay>{
      for (final name in underlayNames)
        Underlay.values.firstWhere(
          (u) => u.name == name,
          orElse: () =>
              throw FormatException('preferences: no underlay named $name'),
        ),
    };
    final servers = json['rendezvousServers'];
    if (servers is! List) {
      throw FormatException(
          'preferences: rendezvousServers must be a list, got $servers');
    }
    return NetworkPreferences(
      underlays: underlays,
      rendezvousServers: [
        for (final entry in servers) _decodeServer(entry),
      ],
    );
  }

  static RendezvousServer _decodeServer(Object? entry) {
    if (entry is! Map<String, dynamic>) {
      throw FormatException('preferences: a server must be a map, got $entry');
    }
    _exactKeys(entry, const {'address', 'publicKey'}, 'preferences server');
    final address = entry['address'];
    final key = entry['publicKey'];
    if (address is! String ||
        key is! String ||
        !RegExp(r'^[0-9a-f]{64}$').hasMatch(key)) {
      throw FormatException('preferences: malformed server $entry');
    }
    try {
      return RendezvousServer(address, hexToKey(key));
    } on ArgumentError catch (e) {
      throw FormatException('preferences: malformed server $entry: $e');
    }
  }

  static void _exactKeys(
      Map<String, dynamic> json, Set<String> keys, String what) {
    final unknown = json.keys.where((k) => !keys.contains(k)).toList();
    final missing = keys.where((k) => !json.containsKey(k)).toList();
    if (unknown.isNotEmpty || missing.isNotEmpty) {
      throw FormatException(
          '$what: unknown keys $unknown, missing keys $missing');
    }
  }

  /// The 32 bytes of a 64-character lowercase hex key.
  static Uint8List hexToKey(String hex) => Uint8List.fromList([
        for (var i = 0; i < hex.length; i += 2)
          int.parse(hex.substring(i, i + 2), radix: 16),
      ]);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is NetworkPreferences &&
          setEquals(underlays, other.underlays) &&
          listEquals(rendezvousServers, other.rendezvousServers);

  @override
  int get hashCode => Object.hash(
        Object.hashAll(Underlay.values.where(underlays.contains)),
        Object.hashAll(rendezvousServers),
      );
}

/// Persists [NetworkPreferences] in SharedPreferences under a key of its own.
class NetworkPreferencesStore {
  static const String _key = 'grassroots_demo_network_v1';

  /// The stored preferences, or [NetworkPreferences.defaults] when none are
  /// stored or the stored blob is malformed (discarded whole).
  Future<NetworkPreferences> load() async {
    final prefs = await SharedPreferences.getInstance();
    final data = prefs.getString(_key);
    if (data == null) return NetworkPreferences.defaults;
    try {
      return NetworkPreferences.fromJson(
          jsonDecode(data) as Map<String, dynamic>);
    } catch (e) {
      debugPrint('Discarding malformed network preferences: $e');
      return NetworkPreferences.defaults;
    }
  }

  Future<void> save(NetworkPreferences preferences) async {
    final prefs = await SharedPreferences.getInstance();
    await prefs.setString(_key, jsonEncode(preferences.toJson()));
  }
}
