import 'dart:io';

/// Parsed IP address and port pair.
///
/// Used throughout the transport layer as a simple, dependency-free
/// address representation.
///
/// String format: `[ip]:port` for IPv6, `ip:port` for IPv4.
class AddressInfo {
  final InternetAddress ip;
  final int port;

  AddressInfo(this.ip, this.port);

  /// Format as string: `[2001:db8::1]:4242` or `192.168.1.5:4242`.
  String toAddressString() {
    if (ip.type == InternetAddressType.IPv6) {
      return '[${ip.address}]:$port';
    }
    return '${ip.address}:$port';
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is AddressInfo &&
          ip.address == other.ip.address &&
          port == other.port;

  @override
  int get hashCode => Object.hash(ip.address, port);

  @override
  String toString() => 'AddressInfo(${toAddressString()})';
}

/// Parse an address string in `[ip]:port` or `ip:port` format.
///
/// Returns null if the string is malformed.
///
/// Examples:
///   `[2001:db8::1]:4242` → AddressInfo(2001:db8::1, 4242)
///   `192.168.1.5:4242`   → AddressInfo(192.168.1.5, 4242)
///   `[::1]:80`           → AddressInfo(::1, 80)
AddressInfo? parseAddressString(String addr) {
  if (addr.isEmpty) return null;

  String ipStr;
  String portStr;

  if (addr.startsWith('[')) {
    // IPv6 format: [ip]:port
    final closeBracket = addr.indexOf(']');
    if (closeBracket < 0) return null;
    ipStr = addr.substring(1, closeBracket);

    // Expect `:port` after the closing bracket
    final afterBracket = addr.substring(closeBracket + 1);
    if (!afterBracket.startsWith(':')) return null;
    portStr = afterBracket.substring(1);
  } else {
    // IPv4 format: ip:port
    // Find the LAST colon (to avoid confusion with IPv6 without brackets,
    // which we don't support — IPv6 must use brackets).
    final lastColon = addr.lastIndexOf(':');
    if (lastColon < 0) return null;

    ipStr = addr.substring(0, lastColon);
    portStr = addr.substring(lastColon + 1);

    // If ipStr contains a colon, it's probably an unbracketed IPv6 — reject.
    if (ipStr.contains(':')) return null;
  }

  if (ipStr.isEmpty || portStr.isEmpty) return null;

  final port = int.tryParse(portStr);
  if (port == null || port < 0 || port > 65535) return null;

  final ip = InternetAddress.tryParse(ipStr);
  if (ip == null) return null;

  return AddressInfo(ip, port);
}

/// Parse an address string and require IPv6.
///
/// Returns null for malformed inputs and for valid IPv4 addresses.
AddressInfo? parseIpv6AddressString(String addr) {
  final parsed = parseAddressString(addr);
  if (parsed == null) return null;
  if (parsed.ip.type != InternetAddressType.IPv6) return null;
  return parsed;
}

/// Whether an address string is a valid IPv6 `ip:port` pair.
bool isIpv6AddressString(String addrString) =>
    parseIpv6AddressString(addrString) != null;

/// Normalize a collection of address strings, dropping malformed entries
/// and preserving first-seen order.
Set<String> normalizeAddressStrings(Iterable<String?> addresses) {
  final normalized = <String>{};
  for (final address in addresses) {
    if (address == null || address.isEmpty) continue;
    final parsed = parseAddressString(address);
    if (parsed == null) continue;
    normalized.add(parsed.toAddressString());
  }
  return normalized;
}

/// Parse a collection of address strings, dropping malformed entries.
Set<AddressInfo> parseAddressCandidates(Iterable<String> addresses) {
  final parsed = <AddressInfo>{};
  for (final address in addresses) {
    final candidate = parseAddressString(address);
    if (candidate != null) {
      parsed.add(candidate);
    }
  }
  return parsed;
}

/// Check if an IP address is a globally routable IPv4 address.
///
/// Excludes:
///   - Unspecified / "this network" (0.0.0.0/8)
///   - Loopback (127.0.0.0/8)
///   - Link-local (169.254.0.0/16)
///   - Private RFC1918 (10/8, 172.16/12, 192.168/16)
///   - Carrier-grade NAT (100.64.0.0/10)
///   - IETF protocol assignments / special use (192.0.0.0/24)
///   - Documentation/test nets (192.0.2/24, 198.51.100/24, 203.0.113/24)
///   - Benchmarking (198.18.0.0/15)
///   - Multicast (224.0.0.0/4)
///   - Reserved / future use (240.0.0.0/4)
bool isGloballyRoutableIPv4(InternetAddress addr) {
  if (addr.type != InternetAddressType.IPv4) return false;

  final bytes = addr.rawAddress;
  if (bytes.length != 4) return false;

  if (addr.isLoopback) return false;

  final a = bytes[0];
  final b = bytes[1];

  if (a == 0) return false;
  if (a == 10) return false;
  if (a == 100 && b >= 64 && b <= 127) return false;
  if (a == 127) return false;
  if (a == 169 && b == 254) return false;
  if (a == 172 && b >= 16 && b <= 31) return false;
  if (a == 192 && b == 0 && bytes[2] == 0) return false;
  if (a == 192 && b == 168) return false;
  if (a == 192 && b == 0 && bytes[2] == 2) return false;
  if (a == 198 && (b == 18 || b == 19)) return false;
  if (a == 198 && b == 51 && bytes[2] == 100) return false;
  if (a == 203 && b == 0 && bytes[2] == 113) return false;
  if (a >= 224) return false;

  return true;
}

/// Check if an IP address is a globally routable IPv6 address.
///
/// Globally routable means it can be reached from the public internet.
/// Excludes:
///   - Loopback (::1)
///   - Link-local (fe80::/10)
///   - Unique local / ULA (fc00::/7 — both fc00:: and fd00::)
///   - Unspecified (::)
///   - IPv4-mapped IPv6 (::ffff:0:0/96)
///   - IPv4-compatible IPv6 (deprecated, ::0:0/96 with IPv4 suffix)
///   - Documentation (2001:db8::/32)
///   - Multicast (ff00::/8)
///   - Teredo (2001:0000::/32)
///   - 6to4 (2002::/16) — often unreliable
///   - IPv4 addresses (not IPv6 at all)
///
/// Note: This is intentionally conservative. Some excluded ranges
/// (like 6to4) might work in practice, but we err on the side of
/// only advertising addresses that are reliably reachable.
bool isGloballyRoutableIPv6(InternetAddress addr) {
  if (addr.type != InternetAddressType.IPv6) return false;

  final bytes = addr.rawAddress;
  if (bytes.length != 16) return false;

  // Unspecified (::) — all zeros
  if (bytes.every((b) => b == 0)) return false;

  // Loopback (::1)
  if (addr.isLoopback) return false;

  // Link-local (fe80::/10)
  if (addr.isLinkLocal) return false;

  // Unique Local Address / ULA (fc00::/7)
  // fc00:: and fd00:: both match: first byte & 0xFE == 0xFC
  if ((bytes[0] & 0xFE) == 0xFC) return false;

  // Multicast (ff00::/8)
  if (bytes[0] == 0xFF) return false;

  // IPv4-mapped IPv6 (::ffff:x.x.x.x)
  // bytes[0..9] == 0, bytes[10..11] == 0xFF
  if (_isIPv4Mapped(bytes)) return false;

  // IPv4-compatible IPv6 (deprecated) (::x.x.x.x)
  // bytes[0..11] == 0, bytes[12..15] has the IPv4 address
  if (_isIPv4Compatible(bytes)) return false;

  // Documentation (2001:0db8::/32)
  if (bytes[0] == 0x20 &&
      bytes[1] == 0x01 &&
      bytes[2] == 0x0D &&
      bytes[3] == 0xB8) {
    return false;
  }

  // Teredo (2001:0000::/32)
  if (bytes[0] == 0x20 &&
      bytes[1] == 0x01 &&
      bytes[2] == 0x00 &&
      bytes[3] == 0x00) {
    return false;
  }

  // 6to4 (2002::/16)
  if (bytes[0] == 0x20 && bytes[1] == 0x02) return false;

  return true;
}

/// Check if an address string (ip:port format) represents a globally routable address.
///
/// A peer with a globally routable address can serve as a well-connected
/// signaling node (GLP server).
bool isGloballyRoutableAddress(String addrString) {
  final parsed = parseAddressString(addrString);
  if (parsed == null) return false;
  if (parsed.ip.type == InternetAddressType.IPv6) {
    return isGloballyRoutableIPv6(parsed.ip);
  }
  return isGloballyRoutableIPv4(parsed.ip);
}

// --- Private helpers ---

bool _isIPv4Mapped(List<int> bytes) {
  // ::ffff:x.x.x.x — first 10 bytes are 0, bytes 10-11 are 0xFF
  for (int i = 0; i < 10; i++) {
    if (bytes[i] != 0) return false;
  }
  return bytes[10] == 0xFF && bytes[11] == 0xFF;
}

bool _isIPv4Compatible(List<int> bytes) {
  // ::x.x.x.x — first 12 bytes are 0, last 4 are the IPv4 address
  // Must not be all zeros (that's unspecified, caught earlier)
  // Must not be ::1 (that's loopback, caught earlier)
  for (int i = 0; i < 12; i++) {
    if (bytes[i] != 0) return false;
  }
  // If we get here, bytes[0..11] are all 0.
  // bytes[12..15] is the IPv4 part — if non-zero, it's IPv4-compatible.
  return bytes[12] != 0 || bytes[13] != 0 || bytes[14] != 0 || bytes[15] != 0;
}
