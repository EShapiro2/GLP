// The runs of the debugging log service (GLP/phones/docs/debugging.md, "The
// log service"): one file a run, <root>/<phone>/<run>.jsonl, <phone> the
// agent's public key in hex and <run> the launch's identifier, the launch
// time in Unix milliseconds.
//
// A batch is appended as it came, one line an event, each line wrapped with
// the batch's arrival time in Unix milliseconds:
//
//   {"arrived":<ms>,"event":<the line as the phone sent it>}
//
// The service never reads the events: a line is split off at its newline and
// copied byte for byte, never decoded, so a line that is not JSON is kept as
// it was sent.  Blank lines are dropped.
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

/// A run as the listing shows it: its size, and its first and last line as
/// stored, decoded where they are JSON and as text where they are not.
class RunInfo {
  final String run;
  final int bytes;
  final Object? first, last;
  RunInfo(this.run, this.bytes, this.first, this.last);
  Map<String, Object?> toJson() => {'run': run, 'bytes': bytes, 'first': first, 'last': last};
}

class LogStore {
  final Directory root;
  LogStore(this.root);

  static final RegExp _phone = RegExp(r'^[0-9a-fA-F]{1,256}$');
  static final RegExp _run = RegExp(r'^[0-9]{1,20}$');

  /// A phone is a key in hex, a run a number: nothing else names a file, so
  /// no request reaches outside the root.
  static bool validPhone(String s) => _phone.hasMatch(s);
  static bool validRun(String s) => _run.hasMatch(s);

  File file(String phone, String run) => File('${root.path}/$phone/$run.jsonl');

  // Appends to one file one at a time, so two batches of one run never
  // interleave.
  final Map<String, Future<void>> _tails = {};

  /// Appends a batch, each non-blank line wrapped with [arrived].  Returns the
  /// number of events appended.
  Future<int> append(String phone, String run, List<int> body, int arrived) {
    final f = file(phone, run);
    final out = BytesBuilder(copy: false);
    final head = utf8.encode('{"arrived":$arrived,"event":');
    var n = 0;
    var start = 0;
    for (var i = 0; i <= body.length; i++) {
      if (i < body.length && body[i] != 0x0A) continue;
      var end = i;
      if (end > start && body[end - 1] == 0x0D) end--;
      if (!_blank(body, start, end)) {
        out.add(head);
        out.add(body.sublist(start, end));
        out.add(const [0x7D, 0x0A]);
        n++;
      }
      start = i + 1;
    }
    if (n == 0) return Future.value(0);
    final bytes = out.takeBytes();
    final prev = _tails[f.path] ?? Future<void>.value();
    final next = prev.then((_) async {
      await f.parent.create(recursive: true);
      final w = await f.open(mode: FileMode.append);
      try {
        await w.writeFrom(bytes);
        await w.flush();
      } finally {
        await w.close();
      }
    });
    _tails[f.path] = next.catchError((_) {});
    return next.then((_) => n);
  }

  static bool _blank(List<int> b, int start, int end) {
    for (var i = start; i < end; i++) {
      final c = b[i];
      if (c != 0x20 && c != 0x09) return false;
    }
    return true;
  }

  /// The phones, each with its runs in the order they were launched.
  Future<Map<String, List<RunInfo>>> list() async {
    final out = <String, List<RunInfo>>{};
    if (!await root.exists()) return out;
    final phones = <String>[];
    await for (final e in root.list()) {
      final name = e.uri.pathSegments.where((s) => s.isNotEmpty).last;
      if (e is Directory && validPhone(name)) phones.add(name);
    }
    phones.sort();
    for (final p in phones) {
      final runs = <RunInfo>[];
      await for (final e in Directory('${root.path}/$p').list()) {
        if (e is! File || !e.path.endsWith('.jsonl')) continue;
        final run = e.uri.pathSegments.last.replaceFirst(RegExp(r'\.jsonl$'), '');
        if (!validRun(run)) continue;
        final len = await e.length();
        runs.add(RunInfo(run, len, _decode(await _firstLine(e)), _decode(await _lastLine(e, len))));
      }
      runs.sort((a, b) => BigInt.parse(a.run).compareTo(BigInt.parse(b.run)));
      out[p] = runs;
    }
    return out;
  }

  static Object? _decode(List<int>? line) {
    if (line == null) return null;
    final s = utf8.decode(line, allowMalformed: true);
    try {
      return jsonDecode(s);
    } on FormatException {
      return s;
    }
  }

  static const int _chunk = 1 << 16;

  static Future<List<int>?> _firstLine(File f) async {
    final r = await f.open();
    try {
      final got = BytesBuilder(copy: false);
      while (true) {
        final b = await r.read(_chunk);
        if (b.isEmpty) break;
        final i = b.indexOf(0x0A);
        if (i >= 0) {
          got.add(b.sublist(0, i));
          return got.takeBytes();
        }
        got.add(b);
      }
      final all = got.takeBytes();
      return all.isEmpty ? null : all;
    } finally {
      await r.close();
    }
  }

  static Future<List<int>?> _lastLine(File f, int len) async {
    if (len == 0) return null;
    final r = await f.open();
    try {
      var end = len;
      // A complete line ends with a newline; the last one is before it.
      await r.setPosition(len - 1);
      if ((await r.read(1)).first == 0x0A) end = len - 1;
      final parts = <List<int>>[];
      var pos = end;
      while (pos > 0) {
        final from = pos > _chunk ? pos - _chunk : 0;
        await r.setPosition(from);
        final b = await r.read(pos - from);
        final i = b.lastIndexOf(0x0A);
        if (i >= 0) {
          parts.insert(0, b.sublist(i + 1));
          break;
        }
        parts.insert(0, b);
        pos = from;
      }
      return [for (final p in parts) ...p];
    } finally {
      await r.close();
    }
  }
}
