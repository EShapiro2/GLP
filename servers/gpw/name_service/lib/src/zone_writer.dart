/// Address-record writes into the Knot zone.  A repoint writes the web-name's
/// address record (hostname target -> CNAME, IP literal -> A/AAAA) at a
/// 300-second expiry, per the DNS mechanics of GPW Section 6; a retirement
/// deletes the records or writes a redirect CNAME.
library;

import 'dart:io';

const addressRecordTtl = 300;

abstract class ZoneWriter {
  /// Write the address record for [label]: CNAME if [target] is a hostname,
  /// A/AAAA if an IP literal.  Replaces any existing records at the label.
  Future<void> setAddress(String label, String target);

  /// Delete all records at [label].
  Future<void> clear(String label);
}

/// Writes through `knotc` on the local control socket.
class KnotcZoneWriter implements ZoneWriter {
  KnotcZoneWriter(this.zone, {this.knotc = 'knotc'});

  final String zone;
  final String knotc;

  Future<void> _run(List<List<String>> commands) async {
    await _knotc(['zone-begin', zone]);
    try {
      for (final c in commands) {
        // Unsetting a label that has no records is not a failure.
        await _knotc(c, tolerateNoSuchNode: c.first == 'zone-unset');
      }
      await _knotc(['zone-commit', zone]);
    } catch (e) {
      try {
        await _knotc(['zone-abort', zone]);
      } catch (_) {}
      rethrow;
    }
  }

  Future<void> _knotc(List<String> args,
      {bool tolerateNoSuchNode = false}) async {
    final r = await Process.run(knotc, args);
    if (r.exitCode != 0) {
      final out = '${r.stderr} ${r.stdout}';
      if (tolerateNoSuchNode && out.contains('no such node')) return;
      throw Exception('knotc ${args.join(' ')} failed: ${out.trim()}');
    }
  }

  @override
  Future<void> setAddress(String label, String target) async {
    final ip = InternetAddress.tryParse(target);
    final (type, rdata) = switch (ip?.type) {
      InternetAddressType.IPv4 => ('A', target),
      InternetAddressType.IPv6 => ('AAAA', target),
      _ => ('CNAME', '$target.'),
    };
    await _run([
      ['zone-unset', zone, label],
      ['zone-set', zone, label, '$addressRecordTtl', type, rdata],
    ]);
  }

  @override
  Future<void> clear(String label) async {
    await _run([
      ['zone-unset', zone, label],
    ]);
  }
}

/// In-memory writer for tests: records the zone's address records.
class FakeZoneWriter implements ZoneWriter {
  final Map<String, (String type, String rdata)> records = {};

  @override
  Future<void> setAddress(String label, String target) async {
    final ip = InternetAddress.tryParse(target);
    records[label] = switch (ip?.type) {
      InternetAddressType.IPv4 => ('A', target),
      InternetAddressType.IPv6 => ('AAAA', target),
      _ => ('CNAME', '$target.'),
    };
  }

  @override
  Future<void> clear(String label) async {
    records.remove(label);
  }
}
