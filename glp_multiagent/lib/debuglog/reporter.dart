/// The reporter of a debuggable build of the Grassroots App: every event of
/// the app, one JSON object a line, kept in a file in the app's support
/// directory and posted to the log service (GLP/phones/docs/debugging.md,
/// "The phone side", items 1 to 5).
///
/// A build is debuggable when it is built with the Dart define
/// `GLP_DEBUGLOG=<password>`; a build without it has no reporter, no password
/// and no traffic to the service, since [debugLogOn] is a constant and the app
/// reaches nothing of this module where it is false (main_superapp.dart).  The
/// service's address is the define `GLP_DEBUGLOG_URL`, by default
/// `https://peoplesweb.org/log`.
///
/// A launch is a run, named by the launch time in Unix milliseconds.  Each
/// event carries its time in Unix milliseconds by the phone's clock, its
/// source --- `runtime`, `layer`, `screen` or `app` --- and its kind, and is
/// written whole, as one line, to `<run>.jsonl` before anything else is done
/// with it, so that the buffer survives the termination of the app.  Every
/// two seconds, and at once on an `[ERROR]` line of the runtime, the lines
/// not yet posted are posted, as a batch, to `<base>/<phone>/<run>`, `<phone>`
/// being the agent's public key in hex, with the header `Authorization: Bearer
/// <password>`; the service answers 204.  The bytes posted are recorded in
/// `<run>.sent`, and a run of an earlier launch is posted under its own run at
/// the next launch, and its files deleted once it is posted whole.  The
/// phone's key is recorded in `<run>.phone` once it is known; until then
/// nothing is posted.
///
/// The reporter posts over `dart:io`'s [HttpClient], not over the networking
/// layer, which is what is debugged.  Its own failures go to [syslog], the
/// device's log, and never to the agent.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

/// The password the debug build holds, the Dart define `GLP_DEBUGLOG`; empty
/// in a build without it.
const String debugLogPassword = String.fromEnvironment('GLP_DEBUGLOG');

/// The log service's address, the Dart define `GLP_DEBUGLOG_URL`.
const String debugLogUrl = String.fromEnvironment('GLP_DEBUGLOG_URL',
    defaultValue: 'https://peoplesweb.org/log');

/// Whether this build reports: built with `GLP_DEBUGLOG`.
const bool debugLogOn = debugLogPassword != '';

/// The four sources of an event (debugging.md, "The phone side", item 3).
abstract final class Source {
  static const String runtime = 'runtime';
  static const String layer = 'layer';
  static const String screen = 'screen';
  static const String app = 'app';
}

/// The reporter of one launch.
class DebugReporter {
  /// The directory of the buffer: one `<run>.jsonl` for each run not yet
  /// posted whole, with its `<run>.sent` and `<run>.phone`.
  final Directory dir;

  /// The password the service takes.
  final String password;

  /// The service's address, `<base>/<phone>/<run>` a run's.
  final String base;

  /// This launch's run: the launch time in Unix milliseconds.
  final int run;

  /// How often the lines not yet posted are posted.
  final Duration period;

  /// The phone's clock, in Unix milliseconds.
  final int Function() clock;

  /// Where the reporter's own failures are written: the device's log.
  final void Function(String message) syslog;

  /// The largest batch posted at once, in bytes; a longer buffer is posted in
  /// several, each ending at the end of a line.
  final int batchLimit;

  final RandomAccessFile _out;
  final HttpClient _client = HttpClient()
    ..connectionTimeout = const Duration(seconds: 10);
  Timer? _ticker;
  String? _phone;
  Future<void>? _flushing;
  bool _again = false;
  bool _closed = false;

  DebugReporter._(this.dir, this.password, this.base, this.run, this.period,
      this.clock, this.syslog, this.batchLimit, this._out);

  /// The reporter of a launch now: its run drawn from [clock], its buffer
  /// opened in [dir], and its posting started every [period].
  static Future<DebugReporter> open({
    required Directory dir,
    required String password,
    String base = debugLogUrl,
    Duration period = const Duration(seconds: 2),
    int Function()? clock,
    void Function(String message)? syslog,
    int batchLimit = 512 * 1024,
  }) async {
    final now = clock ?? () => DateTime.now().millisecondsSinceEpoch;
    await dir.create(recursive: true);
    var run = now();
    // Two launches in one millisecond, which a test makes, are two runs.
    while (File('${dir.path}/$run.jsonl').existsSync()) {
      run++;
    }
    final out = File('${dir.path}/$run.jsonl').openSync(mode: FileMode.append);
    final reporter = DebugReporter._(dir, password, base, run, period, now,
        syslog ?? (m) => stderr.writeln(m), batchLimit, out);
    reporter._ticker = Timer.periodic(period, (_) => unawaited(reporter.flush()));
    return reporter;
  }

  /// The phone's name in the service: the agent's public key, 64 hex
  /// characters.  Null until the app has the key.
  String? get phone => _phone;

  /// The agent's public key [key], once the app has it: recorded for this
  /// run, and reported; the lines buffered so far are posted.
  void setPhone(String key) {
    _phone = key;
    try {
      File('${dir.path}/$run.phone').writeAsStringSync(key);
    } on Object catch (e) {
      syslog('debuglog: the phone of run $run is not recorded: $e');
    }
    event(Source.app, 'key', {'key': key});
    unawaited(flush());
  }

  /// Report the event [kind] of [source], with [fields], at the phone's time
  /// now: written at once to the buffer, as one line.
  void event(String source, String kind, [Map<String, Object?> fields = const {}]) {
    if (_closed) return;
    final String line;
    try {
      line = jsonEncode({
        'time': clock(),
        'source': source,
        'kind': kind,
        ...fields,
      }, toEncodable: (Object? o) => o.toString());
    } on Object catch (e) {
      syslog('debuglog: an event $source/$kind is not encoded: $e');
      return;
    }
    try {
      _out.writeStringSync('$line\n');
    } on Object catch (e) {
      syslog('debuglog: an event $source/$kind is not buffered: $e');
    }
  }

  /// A line of the agent's transcript (`AgentRuntime.onOutput`): reported,
  /// and posted at once where it is an `[ERROR]` line.
  void runtimeOutput(String line) {
    event(Source.runtime, 'output', {'line': line});
    if (line.startsWith('[ERROR]')) unawaited(flush());
  }

  /// Post every line not yet posted: the runs of earlier launches first, each
  /// under its own run, then this one's.  One flush at a time; a flush asked
  /// for while one runs is made once that one ends.
  Future<void> flush() {
    final running = _flushing;
    if (running != null) {
      _again = true;
      return running;
    }
    return _flushing = _drain();
  }

  Future<void> _drain() async {
    try {
      do {
        _again = false;
        await _flushOnce();
      } while (_again && !_closed);
    } finally {
      _flushing = null;
    }
  }

  Future<void> _flushOnce() async {
    final List<int> runs;
    try {
      runs = [
        for (final f in dir.listSync())
          if (f is File && f.path.endsWith('.jsonl'))
            int.tryParse(f.uri.pathSegments.last.split('.').first) ?? -1,
      ]..removeWhere((r) => r < 0);
    } on Object catch (e) {
      syslog('debuglog: the buffer is not read: $e');
      return;
    }
    runs.sort();
    // This launch's run last.
    runs
      ..remove(run)
      ..add(run);
    for (final r in runs) {
      final phone = _phoneOf(r);
      if (phone == null) continue;
      final whole = await _postRun(r, phone);
      if (whole == null) return;
      if (whole && r != run) _forget(r);
    }
  }

  /// The phone run [r] was reported from: recorded with the run, or, for a
  /// run that ended before the app had the key, this phone, whose key the
  /// app keeps across launches.
  String? _phoneOf(int r) {
    if (r == run) return _phone;
    final f = File('${dir.path}/$r.phone');
    if (f.existsSync()) return f.readAsStringSync().trim();
    return _phone;
  }

  /// Post the lines of run [r] not yet posted, batch by batch.  True where
  /// the run is now posted whole, false where it is not, and null where a
  /// post failed, so that the flush stops until the next.
  Future<bool?> _postRun(int r, String phone) async {
    final file = File('${dir.path}/$r.jsonl');
    final sent = File('${dir.path}/$r.sent');
    var offset = 0;
    if (sent.existsSync()) offset = int.tryParse(sent.readAsStringSync()) ?? 0;
    while (true) {
      final Uint8List batch;
      try {
        batch = _read(file, offset);
      } on Object catch (e) {
        syslog('debuglog: run $r is not read: $e');
        return null;
      }
      if (batch.isEmpty) return true;
      if (!await _post(phone, r, batch)) return null;
      offset += batch.length;
      try {
        sent.writeAsStringSync('$offset');
      } on Object catch (e) {
        syslog('debuglog: what run $r posted is not recorded: $e');
        return null;
      }
    }
  }

  /// The whole lines of [file] from [offset], at most [batchLimit] bytes of
  /// them, unless a single line is longer.
  Uint8List _read(File file, int offset) {
    final raf = file.openSync();
    try {
      final length = raf.lengthSync();
      if (length <= offset) return Uint8List(0);
      raf.setPositionSync(offset);
      final bytes = raf.readSync(length - offset);
      var end = bytes.lastIndexOf(0x0a) + 1;
      if (end == 0) return Uint8List(0);
      if (end > batchLimit) {
        final cut = bytes.sublist(0, batchLimit).lastIndexOf(0x0a) + 1;
        end = cut > 0 ? cut : bytes.indexOf(0x0a) + 1;
      }
      return Uint8List.sublistView(bytes, 0, end);
    } finally {
      raf.closeSync();
    }
  }

  /// POST [body] to `<base>/<phone>/<run>`; true where the service answers
  /// 204.
  Future<bool> _post(String phone, int r, Uint8List body) async {
    final uri = Uri.parse('$base/$phone/$r');
    try {
      final request = await _client.postUrl(uri);
      request.headers
        ..set(HttpHeaders.authorizationHeader, 'Bearer $password')
        ..contentType = ContentType('application', 'x-ndjson', charset: 'utf-8');
      request.contentLength = body.length;
      request.add(body);
      final response =
          await request.close().timeout(const Duration(seconds: 30));
      await response.drain<void>();
      if (response.statusCode == HttpStatus.noContent) return true;
      syslog('debuglog: $uri answered ${response.statusCode}');
      return false;
    } on Object catch (e) {
      syslog('debuglog: $uri is not reached: $e');
      return false;
    }
  }

  /// Delete the buffer of run [r], posted whole.
  void _forget(int r) {
    for (final ext in const ['jsonl', 'sent', 'phone']) {
      try {
        final f = File('${dir.path}/$r.$ext');
        if (f.existsSync()) f.deleteSync();
      } on Object catch (e) {
        syslog('debuglog: run $r is not deleted: $e');
      }
    }
  }

  /// Stop: post what is buffered, and close the buffer.  The run's files stay
  /// until the next launch posts them whole.
  Future<void> close() async {
    _ticker?.cancel();
    await flush();
    _closed = true;
    try {
      _out.closeSync();
    } on Object catch (_) {}
    _client.close(force: true);
  }
}
