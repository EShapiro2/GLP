import 'dart:convert';
import 'dart:io';

import 'package:flutter/foundation.dart';

import 'address_utils.dart';

/// Discovers our public-facing UDP address and combines it with the local
/// UDP port to form the address we advertise to friends.
///
/// Discovers public IPv6 and IPv4 candidates. The discovered addresses are
/// included in ANNOUNCE messages so peers and rendezvous servers know where
/// to reach us.
class PublicAddressDiscovery {
  final Map<InternetAddressType, InternetAddress> _cachedPublicIps = {};
  final Map<InternetAddressType, DateTime> _cacheTimes = {};

  /// Cache duration (public IP doesn't change often).
  static const Duration cacheDuration = Duration(minutes: 5);

  /// The best public IP we know of, preferring IPv6 for display.
  InternetAddress? get bestPublicIp =>
      _cachedPublicIps[InternetAddressType.IPv6] ??
      _cachedPublicIps[InternetAddressType.IPv4];

  /// Discover our public address for [type].
  ///
  /// Result is cached for [cacheDuration] to avoid excessive lookups.
  Future<InternetAddress?> discoverPublicIp({
    InternetAddressType type = InternetAddressType.IPv6,
  }) async {
    final cached = _cachedPublicIps[type];
    final cacheTime = _cacheTimes[type];
    if (cached != null &&
        cacheTime != null &&
        DateTime.now().difference(cacheTime) < cacheDuration) {
      return cached;
    }

    final url = type == InternetAddressType.IPv6
        ? 'https://ipv6.seeip.org'
        : 'https://ipv4.seeip.org';
    final discovered = await _fetchPublicIp(url);
    if (discovered == null || discovered.type != type) {
      debugPrint(
        'No public ${type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} '
        'address available for UDP transport',
      );
      return null;
    }

    _cachedPublicIps[type] = discovered;
    _cacheTimes[type] = DateTime.now();
    debugPrint(
      'Discovered public '
      '${type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"}: '
      '${discovered.address}',
    );
    return discovered;
  }

  /// Fetch our public IP from the given URL.
  Future<InternetAddress?> _fetchPublicIp(String url) async {
    try {
      final client = HttpClient();
      client.connectionTimeout = const Duration(seconds: 5);
      try {
        final request = await client.getUrl(Uri.parse(url));
        final response = await request.close();
        final body = await response.transform(utf8.decoder).join();
        final ip = body.trim();

        if (ip.isEmpty) {
          debugPrint('Empty response from $url');
          return null;
        }

        final parsed = InternetAddress.tryParse(ip);
        if (parsed == null) {
          debugPrint('Failed to parse IP from $url: $ip');
          return null;
        }

        return parsed;
      } finally {
        client.close();
      }
    } catch (e) {
      debugPrint('IP discovery from $url failed: $e');
      return null;
    }
  }

  /// Get our public address string for [type].
  ///
  /// Returns null if no public address is available for the family.
  Future<String?> getPublicAddress(
    int localPort, {
    InternetAddressType type = InternetAddressType.IPv6,
  }) async {
    final ip = await discoverPublicIp(type: type);
    if (ip == null) return null;
    return AddressInfo(ip, localPort).toAddressString();
  }

  /// Discover our link-local IPv6 address from network interfaces.
  ///
  /// Link-local addresses (fe80::) work on the same L2 segment and can
  /// bypass WiFi AP client isolation that blocks global IPv6 traffic.
  /// Returns the address as `[fe80::...%iface]:port` ready for use, or
  /// null if no link-local IPv6 interface is found.
  Future<String?> getLinkLocalAddress(int localPort) async {
    try {
      final interfaces = await NetworkInterface.list(
        type: InternetAddressType.IPv6,
        includeLoopback: false,
      );

      for (final iface in interfaces) {
        for (final addr in iface.addresses) {
          if (addr.isLinkLocal) {
            final llAddr = AddressInfo(addr, localPort).toAddressString();
            debugPrint(
                'Discovered link-local IPv6: ${addr.address} on ${iface.name}');
            return llAddr;
          }
        }
      }
    } catch (e) {
      debugPrint('Link-local discovery failed: $e');
    }
    return null;
  }

  /// Invalidate the cached public IP (e.g. on network change).
  void invalidateCache() {
    _cachedPublicIps.clear();
    _cacheTimes.clear();
  }
}
