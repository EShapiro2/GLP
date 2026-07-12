/// RFC 8785 (JSON Canonicalization Scheme) encoder, restricted to the value
/// space the GPW wire formats use: objects, arrays, strings, booleans, null,
/// and integers.  Floating-point numbers are rejected — no GPW object carries
/// one, and excluding them avoids the ES6 number-serialization algorithm.
library;

import 'dart:convert';

/// Canonical JSON bytes of [value], per RFC 8785.
List<int> jcsBytes(Object? value) => utf8.encode(jcsString(value));

/// Canonical JSON text of [value], per RFC 8785.
String jcsString(Object? value) {
  final out = StringBuffer();
  _write(value, out);
  return out.toString();
}

void _write(Object? value, StringBuffer out) {
  if (value == null) {
    out.write('null');
  } else if (value is bool) {
    out.write(value ? 'true' : 'false');
  } else if (value is int) {
    out.write(value.toString());
  } else if (value is String) {
    _writeString(value, out);
  } else if (value is List) {
    out.write('[');
    for (var i = 0; i < value.length; i++) {
      if (i > 0) out.write(',');
      _write(value[i], out);
    }
    out.write(']');
  } else if (value is Map) {
    // RFC 8785: object keys sorted by UTF-16 code units, no duplicates.
    final keys = value.keys.map((k) => k as String).toList()..sort(_utf16Compare);
    out.write('{');
    for (var i = 0; i < keys.length; i++) {
      if (i > 0) out.write(',');
      _writeString(keys[i], out);
      out.write(':');
      _write(value[keys[i]], out);
    }
    out.write('}');
  } else {
    throw ArgumentError('jcs: unsupported value type ${value.runtimeType} '
        '(GPW objects carry only objects, arrays, strings, booleans, null, '
        'and integers)');
  }
}

int _utf16Compare(String a, String b) {
  final n = a.length < b.length ? a.length : b.length;
  for (var i = 0; i < n; i++) {
    final d = a.codeUnitAt(i) - b.codeUnitAt(i);
    if (d != 0) return d;
  }
  return a.length - b.length;
}

void _writeString(String s, StringBuffer out) {
  // RFC 8785 string serialization: the two-character escapes \" \\ \b \f \n
  // \r \t; other control characters as \u00xx with lowercase hex; everything
  // else literal UTF-8.
  out.write('"');
  for (final unit in s.codeUnits) {
    switch (unit) {
      case 0x22:
        out.write(r'\"');
      case 0x5c:
        out.write(r'\\');
      case 0x08:
        out.write(r'\b');
      case 0x0c:
        out.write(r'\f');
      case 0x0a:
        out.write(r'\n');
      case 0x0d:
        out.write(r'\r');
      case 0x09:
        out.write(r'\t');
      default:
        if (unit < 0x20) {
          out.write('\\u${unit.toRadixString(16).padLeft(4, '0')}');
        } else {
          out.writeCharCode(unit);
        }
    }
  }
  out.write('"');
}
