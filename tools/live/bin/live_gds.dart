/// Part 2's flow of the live suite (Code #26's task, Cloud-GDS_inbox.md,
/// 2026-11-08 19:46 UTC): the directory at peoplesweb.org, GDS (GNA Section 7,
/// servers/directory), driven through its agent by three persons off the VPS,
/// each a madGLP agent over its own headless layer with a fresh key pair,
/// declaring the live directory and the live rendezvous server to its layer.
///
/// The agents run `programs/tests/live/gds_live_client.glp`, which sends what
/// it is told and reports every answer and every ask; this program judges
/// each against GNA Section 7 (5040fb6) and prints one line per check:
///
///   RESULT <id> PASS|FAIL|PENDING|INFO <what>
///
/// The checks, 2.2 to 2.10, are the flow of a listing: List and the
/// verification mail's link, the listing kept, the supply of links asked and
/// answered, lookups by name and by email address, an unlisted person found
/// by email address and name, a link handed out and redeemed at the
/// rendezvous server, Unlist, a restart of the directory and the re-listing
/// that asks again, and the mail rate.  Each person lists a fresh address at
/// example.org, which receives no mail: the link of the mail the directory
/// sends is read from the directory's log, where it writes `mailed <address>:
/// <link>` (servers/directory/lib/server.dart), by --log-cmd, a shell command
/// that prints the log; --restart-cmd restarts the directory, and 2.9 is
/// PENDING without it.
///
/// The model is servers/directory/test/agent_test.dart, which runs the same
/// over the loopback; tools/live/test/live_gds_test.dart runs this program
/// against a local directory and rendezvous server.
///
/// Usage:
///   dart run bin/live_gds.dart --base https://peoplesweb.org \
///       --dir-key <hex> --dir-address ip:port \
///       --rv-key <hex> --rv-address ip:port \
///       --log-cmd '<command>' [--restart-cmd '<command>'] \
///       [--glp-root <GLP>/programs] [--wait 30]
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

import 'package:args/args.dart';
import 'package:cryptography/cryptography.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';

int _fails = 0;

void result(String id, String verdict, String what) {
  if (verdict == 'FAIL') _fails++;
  stdout.writeln('RESULT $id $verdict $what');
}

Future<void> main(List<String> argv) async {
  final parser = ArgParser()
    ..addOption('base', mandatory: true, help: 'The address the page is under')
    ..addOption('dir-key', mandatory: true)
    ..addOption('dir-address', mandatory: true)
    ..addOption('rv-key', mandatory: true)
    ..addOption('rv-address', mandatory: true)
    ..addOption('log-cmd', mandatory: true, help: "A shell command printing the directory's log")
    ..addOption('restart-cmd', help: 'A shell command restarting the directory')
    ..addOption('glp-root', defaultsTo: '../../programs')
    ..addOption('wait', defaultsTo: '30', help: 'seconds per answer');
  final args = parser.parse(argv);
  final base = (args['base'] as String).replaceAll(RegExp(r'/+$'), '');
  final dirHex = args['dir-key'] as String;
  final dirAddr = args['dir-address'] as String;
  final rvHex = args['rv-key'] as String;
  final rvAddr = args['rv-address'] as String;
  final logCmd = args['log-cmd'] as String;
  final restartCmd = args['restart-cmd'] as String?;
  final glpRoot = Directory(args['glp-root'] as String).absolute.path;
  final wait = Duration(seconds: int.parse(args['wait'] as String));
  final dirKey = glp.PubKey.fromList(_unhex(dirHex));
  final rvKey = glp.PubKey.fromList(_unhex(rvHex));

  final sodium = await initHeadlessSodium();
  final rng = Random.secure();
  final run = rng.nextInt(1 << 30).toRadixString(36);
  const day = 24 * 3600 * 1000;
  var tag = 0;
  var serial = 1000 + rng.nextInt(1 << 20);

  Future<_Agent> agent(String id) async {
    final seed = Uint8List.fromList(List.generate(32, (_) => rng.nextInt(256)));
    final identity = await GrassrootsIdentity.create(
        keyPair: await Ed25519().newKeyPairFromSeed(seed));
    final layer = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: identity,
      sodium: sodium,
    );
    layer.wan.putRendezvousServer(rvKey.bytes, rvAddr);
    layer.wan.putRendezvousServer(dirKey.bytes, dirAddr);
    final key = glp.PubKey.fromList(identity.publicKey);
    final runtime = AgentRuntime(
      agentId: id,
      program: '$glpRoot/tests/live/gds_live_client.glp',
      rootSelfGlpPath: '$glpRoot/self.glp',
      network: HeadlessGlpNetwork(layer),
    );
    final a = _Agent(id, key, layer, runtime);
    runtime.onUserOutput = a.say;
    await runtime.initialize();
    if (!await layer.start()) {
      result('2.2-start', 'FAIL', '$id: the layer could not bind a UDP port');
      exit(1);
    }
    return a;
  }

  final alice = await agent('alice');
  final bob = await agent('bob');
  final carol = await agent('carol');
  final people = [alice, bob, carol];
  for (final p in people) {
    stdout.writeln('INFO ${p.id} ${p.key.hex}');
  }
  final upAll = await _until(
      () => people.every((p) =>
          p.layer.isPeerReachable(dirKey.bytes) &&
          p.layer.isPeerReachable(rvKey.bytes)),
      wait * 2);
  if (!upAll) {
    result('2.2-session', 'FAIL',
        'the three do not all hold a session with the directory at $dirAddr '
        'and the rendezvous server at $rvAddr within ${wait.inSeconds * 2}s');
    await _down(people);
    exit(1);
  }
  stdout.writeln('INFO the three hold a session with the directory at $dirAddr '
      'and the rendezvous server at $rvAddr');

  final emailA = 'live-$run-alice@example.org';
  final emailB = 'live-$run-bob@example.org';
  final emailC = 'live-$run-carol@example.org';
  final nameA = 'Live Alice $run';
  final nameC = 'Live Carol $run';

  /// The link of the last mail the directory's log says it sent to [email].
  Future<String?> mailedLink(String email) async {
    final end = DateTime.now().add(wait);
    final re = RegExp('mailed ${RegExp.escape(email)}: (\\S+)');
    while (DateTime.now().isBefore(end)) {
      final p = await Process.run('bash', ['-c', logCmd]);
      final ms = re.allMatches('${p.stdout}').toList();
      if (ms.isNotEmpty) return ms.last.group(1);
      await Future.delayed(const Duration(seconds: 1));
    }
    return null;
  }

  Future<int> mailedCount(String email) async {
    final p = await Process.run('bash', ['-c', logCmd]);
    return RegExp('mailed ${RegExp.escape(email)}: ').allMatches('${p.stdout}').length;
  }

  /// List [who] as [email], with [name] or none, listed or not; the list's
  /// tag and the directory's answer.
  Future<(int, String?)> list(_Agent who, String email, String? name, bool listed, {Duration? d}) async {
    final t = ++tag;
    final vis = listed ? 'listed' : 'unlisted';
    if (name == null) {
      await who.act('list_unnamed', [_i(t), _c(dirHex), _c(email), _c(vis)]);
    } else {
      await who.act('list_named', [_i(t), _c(dirHex), _c(email), _c(name), _c(vis)]);
    }
    final r = await who.first((x) => _tagged(x, 'list_reply', t), d ?? wait);
    return (t, r == null ? null : _str(r.args[1]));
  }

  /// List and open the mail's link; whether the listing was verified.
  Future<(int, bool, String)> listAndVerify(_Agent who, String email, String? name, bool listed) async {
    final (t, r) = await list(who, email, name, listed);
    if (r != 'verify') return (t, false, 'answered ${r ?? 'nothing'}');
    final link = await mailedLink(email);
    if (link == null) return (t, false, 'no mail to $email in the directory\'s log');
    final (s, body) = await _get(link);
    final ok = s == 200 && body.contains('is verified');
    return (t, ok, 'answered verify, the mail\'s link opened: $s${ok ? '' : ' ${_brief(body)}'}');
  }

  /// [who] supplies one link, registered with the rendezvous server.
  Future<String?> supply(_Agent who) async {
    final t = ++tag;
    await who.act('supply', [_i(t), _c(dirHex), _c(rvHex), _c(rvAddr), _i(serial++), _i(day)]);
    final s = await who.first((x) => _tagged(x, 'supplied', t), wait);
    return s == null ? null : _str(s.args[1]);
  }

  /// The page's handle for the listed person shown as [shown].
  Future<String?> idOf(String shown) async {
    final (_, body) = await _get('$base/directory');
    for (final m in RegExp(r'name="id" value="([0-9a-f]+)"><button>(.*?)</button>').allMatches(body)) {
      if (_unescape(m.group(2)!) == shown) return m.group(1);
    }
    return null;
  }

  /// The page answers a lookup with the link in a textarea.
  String? linkIn(String body) {
    final m = RegExp(r'<textarea[^>]*>([0-9a-f]+)</textarea>').firstMatch(body);
    return m?.group(1);
  }

  // 2.2  List, the verification mail and its link.
  final (tA, okA, whatA) = await listAndVerify(alice, emailA, nameA, true);
  result('2.2-list-verify', okA ? 'PASS' : 'FAIL', 'alice lists $emailA as "$nameA": $whatA');
  if (!okA) {
    await _skipRest('2.3', 'alice was not listed');
    await _down(people);
    exit(1);
  }

  // 2.3  The listing kept once verified.
  final (s3, page3) = await _get('$base/directory');
  final shownA = page3.contains(_escape(nameA));
  result('2.3-listing-kept', shownA ? 'PASS' : 'FAIL',
      'the page ($s3) ${shownA ? 'shows' : 'does not show'} "$nameA"');

  // 2.4  The supply asked, more(1), and answered.
  final ask1 = await alice.first((x) => _tagged(x, 'asked', tA), wait);
  final linkA1 = ask1 == null ? null : await supply(alice);
  result('2.4-supply', ask1 != null && _int(ask1.args[1]) == 1 && linkA1 != null ? 'PASS' : 'FAIL',
      'alice asked ${ask1 == null ? 'nothing' : 'more(${_int(ask1.args[1])})'} on her Supply, '
      'and ${linkA1 == null ? 'supplied no link' : 'supplied a link, registered with the rendezvous server'}');

  // 2.5  Lookups: alice by name, bob, listed with no name, by email address.
  final (tB, okB, whatB) = await listAndVerify(bob, emailB, null, true);
  final askB = okB ? await bob.first((x) => _tagged(x, 'asked', tB), wait) : null;
  final linkB = askB == null ? null : await supply(bob);
  final idA = await idOf(nameA);
  final idB = await idOf(emailB);
  String? gotA, gotB;
  if (idA != null && linkA1 != null) {
    final (_, b) = await _post('$base/directory/lookup', {'id': idA});
    gotA = linkIn(b);
  }
  if (idB != null && linkB != null) {
    final (_, b) = await _post('$base/directory/lookup', {'id': idB});
    gotB = linkIn(b);
  }
  result('2.5-lookup-name-email', gotA == linkA1 && gotA != null && gotB == linkB && gotB != null ? 'PASS' : 'FAIL',
      'alice, shown by name ${idA == null ? 'not on the page' : 'on the page'}, looked up: '
      '${gotA == null ? 'no link' : gotA == linkA1 ? 'her link' : 'another link'}; '
      'bob ($whatB), shown by email address ${idB == null ? 'not on the page' : 'on the page'}, '
      'looked up: ${gotB == null ? 'no link' : gotB == linkB ? 'his link' : 'another link'}');

  // 2.6  An unlisted person found by email address and name.
  final (tC, okC, whatC) = await listAndVerify(carol, emailC, nameC, false);
  final askC = okC ? await carol.first((x) => _tagged(x, 'asked', tC), wait) : null;
  final linkC = askC == null ? null : await supply(carol);
  final (_, page6) = await _get('$base/directory');
  final hidden = !page6.contains(_escape(nameC));
  String? gotC;
  if (linkC != null) {
    final (_, b) = await _post('$base/directory/find', {'email': emailC, 'name': nameC});
    gotC = linkIn(b);
  }
  result('2.6-unlisted', hidden && gotC != null && gotC == linkC ? 'PASS' : 'FAIL',
      'carol, unlisted ($whatC): ${hidden ? 'not on the page' : 'on the page'}, found by email '
      'address and name: ${gotC == null ? 'no link' : gotC == linkC ? 'her link' : 'another link'}');

  // 2.7  The link the lookup gave, redeemed at the rendezvous server by bob:
  // each told the other's key and address (GRV:3).
  if (gotA != null) {
    final t = ++tag;
    await bob.act('redeem', [_i(t), _c(rvHex), _c(gotA)]);
    final toBob = await bob.first((x) => _tagged(x, 'redeem_reply', t), wait);
    final toAlice = await alice.first((x) => _is(x, 'reg_reply'), wait);
    bool punched(rt.StructTerm? r, _Agent other) =>
        r != null && r.args[1] is rt.StructTerm && (r.args[1] as rt.StructTerm).functor == 'punch' &&
        _str((r.args[1] as rt.StructTerm).args[0]) == other.key.hex;
    result('2.7-link-redeemed', punched(toBob, alice) && punched(toAlice, bob) ? 'PASS' : 'FAIL',
        'bob redeems the link the lookup gave him: told ${_show(toBob?.args[1])}; '
        'alice told ${_show(toAlice?.args[1])}');
  } else {
    result('2.7-link-redeemed', 'FAIL', 'the lookup gave no link of alice\'s to redeem');
  }
  final ask2 = await alice.first((x) => _tagged(x, 'asked', tA) && _int((x as rt.StructTerm).args[1]) == 2, wait);
  result('2.4-supply-after-lookup', ask2 != null ? 'PASS' : 'FAIL',
      'after the lookup took her link, alice asked ${ask2 == null ? 'nothing more' : 'more(2)'}');

  // 2.8  Unlist: answered unlisted, the Supply closed, carol found no more.
  {
    final t = ++tag;
    await carol.act('unlist', [_i(t), _c(dirHex)]);
    final r = await carol.first((x) => _tagged(x, 'unlist_reply', t), wait);
    final closed = await carol.first((x) => _tagged(x, 'closed', tC), wait);
    final (_, b) = await _post('$base/directory/find', {'email': emailC, 'name': nameC});
    final gone = b.contains('Nobody is listed');
    result('2.8-unlist', r != null && _str(r.args[1]) == 'unlisted' && closed != null && gone ? 'PASS' : 'FAIL',
        'carol unlists: answered ${_show(r?.args[1])}, her Supply ${closed == null ? 'open' : 'closed'}, '
        '${gone ? 'found no more' : 'still found'}');
  }

  // 2.9  A restart: the listing and its links kept, the app's re-listing
  // answered listed with no mail, and the asks on its new Supply.
  if (restartCmd == null) {
    result('2.9-restart', 'PENDING', 'no --restart-cmd given');
  } else {
    final a3 = await supply(alice);
    final a4 = await supply(alice);
    final stocked = a3 != null && a4 != null;
    // The two links reach the directory before it stops.
    final linksIn = stocked &&
        await _untilAsync(() async {
          final id = await idOf(nameA);
          return id != null;
        }, wait);
    final mails = await mailedCount(emailA);
    final p = await Process.run('bash', ['-c', restartCmd]);
    await Future.delayed(const Duration(seconds: 3));
    final back = await _untilAsync(
        () async => (await _get('$base/directory')).$1 == 200, wait * 2);
    final (_, page9) = await _get('$base/directory');
    final kept = page9.contains(_escape(nameA));
    // The app lists again once its layer reaches the restarted directory,
    // which may take its heartbeats to notice.
    final (t9b, r9b) = await list(alice, emailA, nameA, true, d: wait * 4);
    final noMail = await mailedCount(emailA) == mails;
    final closedOld = await alice.first((x) => _tagged(x, 'closed', tA), wait);
    final id9 = await idOf(nameA);
    String? got9;
    if (id9 != null) {
      final (_, b) = await _post('$base/directory/lookup', {'id': id9});
      got9 = linkIn(b);
    }
    final ask9 = await alice.first((x) => _tagged(x, 'asked', t9b), wait);
    result('2.9-restart',
        p.exitCode == 0 && back && kept && linksIn && r9b == 'listed' && noMail && closedOld != null &&
                got9 != null && (got9 == a3 || got9 == a4) && ask9 != null
            ? 'PASS'
            : 'FAIL',
        'the directory restarted (exit ${p.exitCode}, ${back ? 'answering again' : 'not answering'}): '
        '"$nameA" ${kept ? 'kept' : 'gone'}; alice lists again: answered ${r9b ?? 'nothing'}, '
        '${noMail ? 'no mail' : 'a mail sent'}, her earlier Supply '
        '${closedOld == null ? 'open' : 'closed'}; a lookup gives '
        '${got9 == null ? 'no link' : (got9 == a3 || got9 == a4) ? 'a link she supplied before the restart' : 'another link'}, '
        'and she is asked ${ask9 == null ? 'nothing' : 'more(${_int(ask9.args[1])})'} on her new Supply');
  }

  // 2.10  The mail rate: bob asks for alice's address within the hour.
  {
    final (_, r) = await list(bob, emailA, null, true);
    result('2.10-mail-rate', r == 'rejected' ? 'PASS' : 'FAIL',
        'bob lists $emailA, mailed within the hour for alice: answered ${r ?? 'nothing'}');
  }

  await _down(people);
  exit(_fails == 0 ? 0 : 1);
}

Future<void> _skipRest(String from, String why) async {
  for (final id in [
    '2.3-listing-kept', '2.4-supply', '2.5-lookup-name-email', '2.6-unlisted',
    '2.7-link-redeemed', '2.8-unlist', '2.9-restart', '2.10-mail-rate'
  ]) {
    result(id, 'FAIL', 'not run: $why');
  }
}

Future<(Object, String)> _get(String url) async {
  final c = HttpClient()..connectionTimeout = const Duration(seconds: 15);
  try {
    final r = await (await c.getUrl(Uri.parse(url))).close();
    return (r.statusCode as Object, await utf8.decodeStream(r));
  } catch (e) {
    return ('$e' as Object, '');
  } finally {
    c.close(force: true);
  }
}

Future<(Object, String)> _post(String url, Map<String, String> form) async {
  final c = HttpClient()..connectionTimeout = const Duration(seconds: 15);
  try {
    final q = await c.postUrl(Uri.parse(url));
    q.headers.contentType = ContentType('application', 'x-www-form-urlencoded');
    q.write(form.entries.map((e) => '${Uri.encodeQueryComponent(e.key)}=${Uri.encodeQueryComponent(e.value)}').join('&'));
    final r = await q.close();
    return (r.statusCode as Object, await utf8.decodeStream(r));
  } catch (e) {
    return ('$e' as Object, '');
  } finally {
    c.close(force: true);
  }
}

String _brief(String body) => body.replaceAll(RegExp(r'<[^>]*>'), ' ').replaceAll(RegExp(r'\s+'), ' ').trim();

String _escape(String s) => const HtmlEscape().convert(s);

String _unescape(String s) => s
    .replaceAll('&lt;', '<')
    .replaceAll('&gt;', '>')
    .replaceAll('&quot;', '"')
    .replaceAll('&#39;', "'")
    .replaceAll('&#x2F;', '/')
    .replaceAll('&amp;', '&');

class _Agent {
  _Agent(this.id, this.key, this.layer, this.runtime);
  final String id;
  final glp.PubKey key;
  final HeadlessGrassrootsNetwork layer;
  final AgentRuntime runtime;
  final List<rt.Term> said = [];
  final _said = StreamController<rt.Term>.broadcast();

  void say(rt.Term t) {
    said.add(t);
    _said.add(t);
  }

  Future<void> act(String f, List<rt.Term> args) =>
      runtime.injectUserInput(rt.StructTerm(f, args));

  Future<rt.StructTerm?> first(bool Function(rt.Term) p, Duration d) async {
    for (final t in said) {
      if (p(t)) return t as rt.StructTerm;
    }
    try {
      return await _said.stream.firstWhere(p).timeout(d) as rt.StructTerm;
    } on TimeoutException {
      return null;
    }
  }
}

Future<bool> _until(bool Function() p, Duration d) async {
  final end = DateTime.now().add(d);
  while (!p()) {
    if (DateTime.now().isAfter(end)) return false;
    await Future.delayed(const Duration(milliseconds: 250));
  }
  return true;
}

Future<bool> _untilAsync(Future<bool> Function() p, Duration d) async {
  final end = DateTime.now().add(d);
  while (!await p()) {
    if (DateTime.now().isAfter(end)) return false;
    await Future.delayed(const Duration(milliseconds: 500));
  }
  return true;
}

Future<void> _down(List<_Agent> as) async {
  for (final a in as) {
    a.runtime.dispose();
    await a.layer.dispose();
  }
}

bool _is(rt.Term? t, String f) =>
    (t is rt.StructTerm && t.functor == f) || (t is rt.ConstTerm && t.value == f);

bool _tagged(rt.Term t, String f, int tag) =>
    t is rt.StructTerm && t.functor == f && t.args.isNotEmpty && _int(t.args[0]) == tag;

String _str(rt.Term t) => (t as rt.ConstTerm).value.toString();
int _int(rt.Term t) => (t as rt.ConstTerm).value as int;
rt.Term _c(String s) => rt.ConstTerm(s);
rt.Term _i(int n) => rt.ConstTerm(n);

String _show(rt.Term? t) {
  if (t == null) return 'nothing';
  if (t is rt.ConstTerm) return '${t.value}';
  if (t is rt.StructTerm) return '${t.functor}(${t.args.map(_show).join(', ')})';
  return t.toString();
}

Uint8List _unhex(String h) => Uint8List.fromList(
    [for (var i = 0; i < h.length; i += 2) int.parse(h.substring(i, i + 2), radix: 16)]);
