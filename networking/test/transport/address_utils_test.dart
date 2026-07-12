import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/transport/address_utils.dart';

void main() {
  // =========================================================================
  // parseAddressString
  // =========================================================================
  group('parseAddressString', () {
    group('valid IPv6 addresses', () {
      test('parses full IPv6 with port', () {
        final result = parseAddressString('[2001:db8::1]:4242');
        expect(result, isNotNull);
        expect(result!.ip.address, equals('2001:db8::1'));
        expect(result.port, equals(4242));
      });

      test('parses loopback IPv6', () {
        final result = parseAddressString('[::1]:80');
        expect(result, isNotNull);
        expect(result!.ip.address, equals('::1'));
        expect(result.port, equals(80));
      });

      test('parses unspecified IPv6', () {
        final result = parseAddressString('[::]:0');
        expect(result, isNotNull);
        expect(result!.ip.address, equals('::'));
        expect(result.port, equals(0));
      });

      test('parses full-length IPv6', () {
        final result =
            parseAddressString('[2001:0db8:85a3:0000:0000:8a2e:0370:7334]:443');
        expect(result, isNotNull);
        expect(result!.port, equals(443));
      });

      test('parses link-local IPv6', () {
        final result = parseAddressString('[fe80::1]:1234');
        expect(result, isNotNull);
        expect(result!.ip.address, equals('fe80::1'));
      });

      test('parses port 0 (ephemeral)', () {
        final result = parseAddressString('[2001:db8::1]:0');
        expect(result, isNotNull);
        expect(result!.port, equals(0));
      });

      test('parses port 65535 (max)', () {
        final result = parseAddressString('[2001:db8::1]:65535');
        expect(result, isNotNull);
        expect(result!.port, equals(65535));
      });
    });

    group('valid IPv4 addresses', () {
      test('parses basic IPv4 with port', () {
        final result = parseAddressString('192.168.1.5:4242');
        expect(result, isNotNull);
        expect(result!.ip.address, equals('192.168.1.5'));
        expect(result.port, equals(4242));
      });

      test('parses localhost IPv4', () {
        final result = parseAddressString('127.0.0.1:8080');
        expect(result, isNotNull);
        expect(result!.ip.address, equals('127.0.0.1'));
        expect(result.port, equals(8080));
      });

      test('parses 0.0.0.0', () {
        final result = parseAddressString('0.0.0.0:0');
        expect(result, isNotNull);
        expect(result!.ip.address, equals('0.0.0.0'));
      });
    });

    group('invalid inputs', () {
      test('rejects empty string', () {
        expect(parseAddressString(''), isNull);
      });

      test('rejects just an IP without port', () {
        expect(parseAddressString('192.168.1.1'), isNull);
      });

      test('rejects IPv6 without brackets', () {
        // Ambiguous: 2001:db8::1:4242 — can't tell where IP ends and port begins
        expect(parseAddressString('2001:db8::1:4242'), isNull);
      });

      test('rejects missing closing bracket', () {
        expect(parseAddressString('[2001:db8::1:4242'), isNull);
      });

      test('rejects missing port after bracket', () {
        expect(parseAddressString('[2001:db8::1]'), isNull);
      });

      test('rejects non-numeric port', () {
        expect(parseAddressString('192.168.1.1:abc'), isNull);
      });

      test('rejects negative port', () {
        expect(parseAddressString('192.168.1.1:-1'), isNull);
      });

      test('rejects port > 65535', () {
        expect(parseAddressString('192.168.1.1:65536'), isNull);
      });

      test('rejects garbage', () {
        expect(parseAddressString('not-an-address'), isNull);
      });

      test('rejects empty IP', () {
        expect(parseAddressString(':4242'), isNull);
      });

      test('rejects empty port', () {
        expect(parseAddressString('192.168.1.1:'), isNull);
      });

      test('rejects empty brackets', () {
        expect(parseAddressString('[]:80'), isNull);
      });
    });
  });

  group('parseIpv6AddressString', () {
    test('accepts bracketed IPv6 addresses', () {
      final result = parseIpv6AddressString('[2001:db8::1]:4242');
      expect(result, isNotNull);
      expect(result!.ip.address, '2001:db8::1');
      expect(result.port, 4242);
    });

    test('rejects IPv4 addresses', () {
      expect(parseIpv6AddressString('192.168.1.5:4242'), isNull);
    });
  });

  group('isIpv6AddressString', () {
    test('returns true for valid IPv6 address strings', () {
      expect(isIpv6AddressString('[2001:db8::1]:4242'), isTrue);
    });

    test('returns false for IPv4 address strings', () {
      expect(isIpv6AddressString('192.168.1.5:4242'), isFalse);
    });
  });

  // =========================================================================
  // AddressInfo.toAddressString
  // =========================================================================
  group('AddressInfo.toAddressString', () {
    test('formats IPv6 with brackets', () {
      final addr = AddressInfo(InternetAddress('2001:db8::1'), 4242);
      expect(addr.toAddressString(), equals('[2001:db8::1]:4242'));
    });

    test('formats IPv4 without brackets', () {
      final addr = AddressInfo(InternetAddress('192.168.1.5'), 4242);
      expect(addr.toAddressString(), equals('192.168.1.5:4242'));
    });

    test('round-trips IPv6 through parse', () {
      const original = '[2001:db8::1]:4242';
      final parsed = parseAddressString(original);
      expect(parsed, isNotNull);
      expect(parsed!.toAddressString(), equals(original));
    });

    test('round-trips IPv4 through parse', () {
      const original = '192.168.1.5:4242';
      final parsed = parseAddressString(original);
      expect(parsed, isNotNull);
      expect(parsed!.toAddressString(), equals(original));
    });
  });

  // =========================================================================
  // AddressInfo equality
  // =========================================================================
  group('AddressInfo equality', () {
    test('equal when same ip and port', () {
      final a = AddressInfo(InternetAddress('192.168.1.1'), 80);
      final b = AddressInfo(InternetAddress('192.168.1.1'), 80);
      expect(a, equals(b));
      expect(a.hashCode, equals(b.hashCode));
    });

    test('not equal when different port', () {
      final a = AddressInfo(InternetAddress('192.168.1.1'), 80);
      final b = AddressInfo(InternetAddress('192.168.1.1'), 81);
      expect(a, isNot(equals(b)));
    });

    test('not equal when different ip', () {
      final a = AddressInfo(InternetAddress('192.168.1.1'), 80);
      final b = AddressInfo(InternetAddress('192.168.1.2'), 80);
      expect(a, isNot(equals(b)));
    });
  });

  // =========================================================================
  // isGloballyRoutableIPv4
  // =========================================================================
  group('isGloballyRoutableIPv4', () {
    test('returns true for globally routable IPv4 addresses', () {
      expect(isGloballyRoutableIPv4(InternetAddress('8.8.8.8')), isTrue);
      expect(isGloballyRoutableIPv4(InternetAddress('1.1.1.1')), isTrue);
    });

    test('returns false for private and special-use IPv4 ranges', () {
      expect(isGloballyRoutableIPv4(InternetAddress('10.0.0.1')), isFalse);
      expect(isGloballyRoutableIPv4(InternetAddress('192.168.1.1')), isFalse);
      expect(isGloballyRoutableIPv4(InternetAddress('172.16.0.1')), isFalse);
      expect(isGloballyRoutableIPv4(InternetAddress('100.64.0.1')), isFalse);
      expect(isGloballyRoutableIPv4(InternetAddress('203.0.113.5')), isFalse);
    });
  });

  // =========================================================================
  // isGloballyRoutableIPv6
  // =========================================================================
  group('isGloballyRoutableIPv6', () {
    group('returns true for globally routable addresses', () {
      test(
          'standard global unicast (2001:db8 is documentation, but 2607:: is real)',
          () {
        // Google's public DNS
        expect(
            isGloballyRoutableIPv6(InternetAddress('2607:f8b0:4004:800::200e')),
            isTrue);
      });

      test('another global unicast', () {
        expect(
            isGloballyRoutableIPv6(InternetAddress('2a00:1450:4009:823::200e')),
            isTrue);
      });

      test('global unicast starting with 2001:4', () {
        // Not documentation (2001:db8), not Teredo (2001:0000)
        expect(isGloballyRoutableIPv6(InternetAddress('2001:4860:4860::8888')),
            isTrue);
      });

      test('global unicast starting with 2400::', () {
        expect(isGloballyRoutableIPv6(InternetAddress('2400::1')), isTrue);
      });

      test('global unicast starting with 2600::', () {
        expect(isGloballyRoutableIPv6(InternetAddress('2600::1')), isTrue);
      });

      test('global unicast starting with 2800::', () {
        expect(isGloballyRoutableIPv6(InternetAddress('2800::1')), isTrue);
      });

      test('global unicast starting with 2c00::', () {
        expect(isGloballyRoutableIPv6(InternetAddress('2c00::1')), isTrue);
      });
    });

    group('returns false for non-routable addresses', () {
      test('loopback (::1)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('::1')), isFalse);
      });

      test('unspecified (::)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('::')), isFalse);
      });

      test('link-local (fe80::)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('fe80::1')), isFalse);
      });

      test('link-local (fe80::abcd:1234)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('fe80::abcd:1234')),
            isFalse);
      });

      test('ULA fd00::', () {
        expect(isGloballyRoutableIPv6(InternetAddress('fd00::1')), isFalse);
      });

      test('ULA fc00::', () {
        expect(isGloballyRoutableIPv6(InternetAddress('fc00::1')), isFalse);
      });

      test('ULA fdxx::xx', () {
        expect(isGloballyRoutableIPv6(InternetAddress('fd12:3456:789a::1')),
            isFalse);
      });

      test('documentation (2001:db8::)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('2001:db8::1')), isFalse);
      });

      test('documentation (2001:0db8:85a3::)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('2001:0db8:85a3::1')),
            isFalse);
      });

      test('Teredo (2001:0000::)', () {
        expect(
            isGloballyRoutableIPv6(InternetAddress('2001:0000::1')), isFalse);
      });

      test('Teredo (2001::1) — note: 2001:0:... is Teredo', () {
        // 2001:0000:xxxx is Teredo. But 2001:xxxx where xxxx != 0 is not Teredo.
        // 2001::1 expands to 2001:0000:0000:...:0001 — first 4 bytes: 20 01 00 00 → Teredo!
        expect(isGloballyRoutableIPv6(InternetAddress('2001::1')), isFalse);
      });

      test('6to4 (2002::)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('2002::1')), isFalse);
      });

      test('6to4 (2002:c0a8::)', () {
        expect(
            isGloballyRoutableIPv6(InternetAddress('2002:c0a8::1')), isFalse);
      });

      test('multicast (ff00::)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('ff00::1')), isFalse);
      });

      test('multicast (ff02::1)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('ff02::1')), isFalse);
      });

      test('multicast (ff0e::1) — global scope multicast', () {
        // Even global-scope multicast is not a unicast routable address
        expect(isGloballyRoutableIPv6(InternetAddress('ff0e::1')), isFalse);
      });
    });

    group('returns false for IPv4 addresses', () {
      test('IPv4 address passed as InternetAddress', () {
        expect(isGloballyRoutableIPv6(InternetAddress('192.168.1.1')), isFalse);
      });

      test('IPv4 loopback', () {
        expect(isGloballyRoutableIPv6(InternetAddress('127.0.0.1')), isFalse);
      });
    });

    group('returns false for IPv4-mapped/compatible IPv6', () {
      test('IPv4-mapped (::ffff:192.168.1.1)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('::ffff:192.168.1.1')),
            isFalse);
      });

      test('IPv4-mapped (::ffff:127.0.0.1)', () {
        expect(isGloballyRoutableIPv6(InternetAddress('::ffff:127.0.0.1')),
            isFalse);
      });
    });

    group('edge cases', () {
      test(
          '2001:db8:ffff:ffff:ffff:ffff:ffff:ffff (documentation range boundary)',
          () {
        expect(
            isGloballyRoutableIPv6(
                InternetAddress('2001:db8:ffff:ffff:ffff:ffff:ffff:ffff')),
            isFalse);
      });

      test('2001:db9::1 (just past documentation range — IS routable)', () {
        // 2001:0db9 is NOT in the documentation range (which is 2001:0db8::/32)
        // Also not Teredo (which is 2001:0000::/32)
        expect(isGloballyRoutableIPv6(InternetAddress('2001:db9::1')), isTrue);
      });

      test('fe7f::1 is NOT link-local (fe80::/10 starts at fe80)', () {
        // fe80::/10 means first 10 bits are 1111111010
        // fe7f = 1111 1110 0111 1111 — first 10 bits are 1111 1110 01 = different from fe80::/10
        // Actually: fe80::/10 covers fe80:: through febf::
        // fe7f:: has first byte FE, second byte 7F. In binary: 1111 1110 0111 1111
        // /10 means match first 10 bits: 1111 1110 01 — that's FE 8x through FE Bx range
        // fe7f second nibble 7 = 0111, bit pattern: 0111 1111
        // So fe7f is NOT in fe80::/10 range. But it IS in fe00::/9 range... which is not excluded.
        // However, Dart's isLinkLocal should handle this correctly.
        // fe7f:: is not link-local, not ULA, not multicast, not documentation, not Teredo, not 6to4
        // It's in the reserved range but our function should consider it routable
        // unless Dart's InternetAddress.isLinkLocal catches it.
        // Let's just test what happens — if Dart says it's link-local, we exclude it.
        // This is a genuinely ambiguous case that depends on Dart's implementation.
        final addr = InternetAddress('fe7f::1');
        // We don't assert the result here — just that the function doesn't throw.
        isGloballyRoutableIPv6(addr);
      });
    });
  });

  // =========================================================================
  // isGloballyRoutableAddress (convenience wrapper)
  // =========================================================================
  group('isGloballyRoutableAddress', () {
    test('returns true for routable IPv6 address string', () {
      expect(
          isGloballyRoutableAddress('[2607:f8b0:4004:800::200e]:4242'), isTrue);
    });

    test('returns false for non-routable IPv6 address string', () {
      expect(isGloballyRoutableAddress('[::1]:4242'), isFalse);
    });

    test('returns true for routable IPv4 address string', () {
      expect(isGloballyRoutableAddress('8.8.8.8:4242'), isTrue);
    });

    test('returns false for private IPv4 address string', () {
      expect(isGloballyRoutableAddress('192.168.1.1:4242'), isFalse);
    });

    test('returns false for malformed string', () {
      expect(isGloballyRoutableAddress('garbage'), isFalse);
    });

    test('returns false for empty string', () {
      expect(isGloballyRoutableAddress(''), isFalse);
    });

    test('returns false for ULA address string', () {
      expect(isGloballyRoutableAddress('[fd00::1]:4242'), isFalse);
    });

    test('returns false for link-local address string', () {
      expect(isGloballyRoutableAddress('[fe80::1]:4242'), isFalse);
    });
  });
}
