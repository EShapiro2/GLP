/// The three signed GPW wire objects — name manifest, repoint, retirement —
/// their envelopes, and field validation.  Formats per the GPW Implementation
/// Notes appendix (decided 2026-07-12).
library;

import 'dart:io' show InternetAddress;

final _label = RegExp(r'^[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$');

bool validLabel(String s) => _label.hasMatch(s);

bool validHostname(String s) {
  if (s.isEmpty || s.length > 253) return false;
  return s.split('.').every(validLabel);
}

bool isIpLiteral(String s) => InternetAddress.tryParse(s) != null;

bool validTimestamp(String s) => DateTime.tryParse(s) != null;

/// A malformed or invalid wire object.
class WireError implements Exception {
  WireError(this.message);
  final String message;
  @override
  String toString() => message;
}

String _string(Map body, String key) {
  final v = body[key];
  if (v is! String) throw WireError('missing or non-string "$key"');
  return v;
}

int _epoch(Map body) {
  final v = body['epoch'];
  if (v is! int || v < 0) throw WireError('missing or invalid "epoch"');
  return v;
}

/// The signed envelope {"body": ..., "signature": ...} common to all three
/// objects.  Signature verification is the service's job (the signer depends
/// on the object and, for repoint/retire, on the stored binding).
class Envelope {
  Envelope(this.body, this.signature);

  final Map<String, Object?> body;
  final String signature;

  static Envelope parse(Object? json) {
    if (json is! Map) throw WireError('not a JSON object');
    final body = json['body'];
    final sig = json['signature'];
    if (body is! Map) throw WireError('missing "body"');
    if (sig is! String) throw WireError('missing "signature"');
    return Envelope(body.cast<String, Object?>(), sig);
  }
}

/// Common fields of all three bodies, validated against the served [zone]
/// and the request's [webName].
class CommonFields {
  CommonFields(this.webName, this.epoch, this.issuedAt);

  final String webName;
  final int epoch;
  final String issuedAt;

  static CommonFields check(
      Map body, String format, String zone, String webName) {
    if (_string(body, 'format') != format) {
      throw WireError('format is not "$format"');
    }
    if (_string(body, 'zone') != zone) {
      throw WireError('zone is not "$zone"');
    }
    final name = _string(body, 'webName');
    if (!validLabel(name)) throw WireError('invalid web-name label');
    if (name != webName) {
      throw WireError('web-name does not match the request path');
    }
    final issuedAt = _string(body, 'issuedAt');
    if (!validTimestamp(issuedAt)) throw WireError('invalid "issuedAt"');
    return CommonFields(name, _epoch(body), issuedAt);
  }
}

/// A custodian signature within a Replace block.
class CustodianSignature {
  CustodianSignature(this.key, this.signature);
  final String key;
  final String signature;
}

/// The Replace block of a rebinding deposit (Replace, per SPM).
class ReplaceBlock {
  ReplaceBlock(this.oldKey, this.custodianSignatures);
  final String oldKey;
  final List<CustodianSignature> custodianSignatures;
}

/// A parsed name-manifest body (gpw/name-manifest/1).
class NameManifest {
  NameManifest(this.common, this.publicKey, this.custodians, this.threshold,
      this.replaces);

  final CommonFields common;
  final String publicKey;
  final List<String> custodians;
  final int threshold;
  final ReplaceBlock? replaces;

  static NameManifest parse(Map body, String zone, String webName) {
    final common =
        CommonFields.check(body, 'gpw/name-manifest/1', zone, webName);
    final publicKey = _string(body, 'publicKey');
    final record = body['identityRecord'];
    if (record is! Map) throw WireError('missing "identityRecord"');
    final custodians = record['custodians'];
    final threshold = record['threshold'];
    if (custodians is! List || custodians.any((c) => c is! String)) {
      throw WireError('missing or invalid "identityRecord.custodians"');
    }
    // Per SPM (Implementation Notes appendix): the threshold is a
    // supermajority — an integer exceeding half the custodian count.
    if (threshold is! int ||
        threshold > custodians.length ||
        threshold * 2 <= custodians.length) {
      throw WireError(
          '"identityRecord.threshold" is not a supermajority of the custodians');
    }
    ReplaceBlock? replaces;
    final rep = body['replaces'];
    if (rep != null) {
      if (rep is! Map) throw WireError('invalid "replaces"');
      final oldKey = _string(rep, 'oldKey');
      final sigs = rep['custodianSignatures'];
      if (sigs is! List) {
        throw WireError('missing "replaces.custodianSignatures"');
      }
      replaces = ReplaceBlock(oldKey, [
        for (final s in sigs)
          if (s is Map)
            CustodianSignature(_string(s, 'key'), _string(s, 'signature'))
          else
            throw WireError('invalid custodian signature'),
      ]);
    }
    return NameManifest(
        common, publicKey, custodians.cast<String>(), threshold, replaces);
  }
}

/// A parsed repoint body (gpw/repoint/1).
class Repoint {
  Repoint(this.common, this.mirror);

  final CommonFields common;
  final String mirror;

  bool get mirrorIsIp => isIpLiteral(mirror);

  static Repoint parse(Map body, String zone, String webName) {
    final common = CommonFields.check(body, 'gpw/repoint/1', zone, webName);
    final mirror = _string(body, 'mirror');
    if (!isIpLiteral(mirror) && !validHostname(mirror)) {
      throw WireError('"mirror" is neither a hostname nor an IP literal');
    }
    return Repoint(common, mirror);
  }
}

/// A parsed retirement body (gpw/retirement/1).
class Retirement {
  Retirement(this.common, this.redirect);

  final CommonFields common;
  final String? redirect;

  static Retirement parse(Map body, String zone, String webName) {
    final common =
        CommonFields.check(body, 'gpw/retirement/1', zone, webName);
    final redirect = body['redirect'];
    if (redirect != null &&
        (redirect is! String || !validHostname(redirect))) {
      throw WireError('invalid "redirect"');
    }
    return Retirement(common, redirect as String?);
  }
}
