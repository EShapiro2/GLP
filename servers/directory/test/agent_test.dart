// The directory's agent (GNA Section 7, programs/examples/rendezvous/
// gds_agent.glp) over the headless layer under the directory's own key,
// hosted with the page and the store as bin/directory_server.dart hosts
// them, against a person's app: a madGLP agent over its own headless layer,
// which declares the directory as a rendezvous server is declared (GNA
// Section 7, "Hosting"), over the loopback, as networking/glue/test/
// rendezvous_server_test.dart runs the rendezvous agent.
//
// The app's program is the smallest stand-in for the Grassroots App's side of
// GDS (Cloud-GAS's): on the test's word it signs a listing and lists, signs
// invitation links and supplies them, and unlists, and it tells the test the
// directory's answer and each ask on its Supply stream.  The links it makes
// are signed terms naming no live rendezvous server: the directory keeps and
// gives them, and redeems none.
//
// Needs a native libsodium, as the glue's tests do; self-skips when absent.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:directory_server/agent.dart';
import 'package:directory_server/mail.dart';
import 'package:directory_server/server.dart';
import 'package:directory_server/store.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_core/src/headless/identity_file.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:test/test.dart';

// The app's program.  Written under the root, programs/ (TGLP modules.tex,
// "Scope construction": "A program lies at or below the root"), in a
// directory of the test's own under programs/tests.
//
//   list_named(D, Email, Name, Vis)  sign listing(Me, Email, name(Name), Vis) and
//                                    list with the directory D
//   list_unnamed(D, Email, Vis)      the same, Name none
//   list_as(D, Other, Email)         sign a listing naming Other, and list
//   supply(D, Rv, Serial, Expiry)    sign invitation(Me, Rv, Addr, Serial,
//                                    Expiry) and supply it to D
//   give(D, Link)                    supply D a link made elsewhere
//   mint(Rv, Serial, Expiry)         sign an invitation and tell the test
//   unlist(D)                        sign unlist(Me) and unlist
//   unlist_as(D, Other)              sign unlist(Other), and unlist
//
// It tells the test replied(R) for the directory's answer to a list,
// asked(N) for each more(N) on the Supply stream, closed when the stream
// closes, minted(L) for a link it minted, and unlist_replied(R) for the
// directory's answer to an unlist.
const appProgram = r'''
AppVis ::= listed ; unlisted.
AppAct ::= list_named(Key, String, String, AppVis)
         ; list_unnamed(Key, String, AppVis)
         ; list_as(Key, Key, String)
         ; supply(Key, Key, Integer, Integer)
         ; give(Key, SignedTerm)
         ; mint(Key, Integer, Integer)
         ; unlist(Key)
         ; unlist_as(Key, Key).
AppActs ::= [] ; [AppAct | AppActs].
AppAsk ::= more(Integer).
AppListReply ::= verify ; listed ; rejected.
AppUnlistReply ::= unlisted ; rejected.
AppLinks ::= [] ; [SignedTerm | AppLinks].
AppMsg ::= list(Key, SignedTerm, Stream(AppAsk)?, AppListReply?)
         ; supply(Key, AppLinks)
         ; unlist(Key, SignedTerm, AppUnlistReply?).
AppSay ::= replied(AppListReply) ; asked(Integer) ; closed ; minted(SignedTerm) ; unlist_replied(AppUnlistReply).

exported procedure agent(Channel(AppActs, Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- self_key(Me), acts(Me?, In?).

procedure acts(Key?, AppActs?).
acts(Me, [list_named(D, Email, Name, Vis) | In]) :- ground(Me?) |
    sign(listing(Me?, Email?, name(Name?), Vis?), Me?, S),
    send_to_net([msg(D?, list(Me?, S?, Supply, Reply))]),
    on_reply(Reply?),
    on_asks(Supply?),
    acts(Me?, In?).
acts(Me, [list_unnamed(D, Email, Vis) | In]) :- ground(Me?) |
    sign(listing(Me?, Email?, none, Vis?), Me?, S),
    send_to_net([msg(D?, list(Me?, S?, Supply, Reply))]),
    on_reply(Reply?),
    on_asks(Supply?),
    acts(Me?, In?).
acts(Me, [list_as(D, Other, Email) | In]) :- ground(Me?) |
    sign(listing(Other?, Email?, none, listed), Me?, S),
    send_to_net([msg(D?, list(Me?, S?, Supply, Reply))]),
    on_reply(Reply?),
    on_asks(Supply?),
    acts(Me?, In?).
acts(Me, [supply(D, Rv, Serial, Expiry) | In]) :- ground(Me?) |
    sign(invitation(Me?, Rv?, '127.0.0.1:9', Serial?, Expiry?), Me?, L),
    send_to_net([msg(D?, supply(Me?, [L?]))]),
    acts(Me?, In?).
acts(Me, [give(D, L) | In]) :- ground(Me?) |
    send_to_net([msg(D?, supply(Me?, [L?]))]),
    acts(Me?, In?).
acts(Me, [mint(Rv, Serial, Expiry) | In]) :- ground(Me?) |
    sign(invitation(Me?, Rv?, '127.0.0.1:9', Serial?, Expiry?), Me?, L),
    send_to_user([minted(L?)]),
    acts(Me?, In?).
acts(Me, [unlist(D) | In]) :- ground(Me?) |
    sign(unlist(Me?), Me?, S),
    send_to_net([msg(D?, unlist(Me?, S?, Reply))]),
    on_unlist(Reply?),
    acts(Me?, In?).
acts(Me, [unlist_as(D, Other) | In]) :- ground(Me?) |
    sign(unlist(Other?), Me?, S),
    send_to_net([msg(D?, unlist(Me?, S?, Reply))]),
    on_unlist(Reply?),
    acts(Me?, In?).
acts(_, []).

procedure on_reply(AppListReply?).
on_reply(verify) :- send_to_user([replied(verify)]).
on_reply(listed) :- send_to_user([replied(listed)]).
on_reply(rejected) :- send_to_user([replied(rejected)]).

procedure on_asks(Stream(AppAsk)?).
on_asks([more(N) | As]) :- send_to_user([asked(N?)]), on_asks(As?).
on_asks([]) :- send_to_user([closed]).

procedure on_unlist(AppUnlistReply?).
on_unlist(unlisted) :- send_to_user([unlist_replied(unlisted)]).
on_unlist(rejected) :- send_to_user([unlist_replied(rejected)]).
''';

void main() {
  String? sodiumUnavailable;
  late Directory dir;
  late String appPath;
  final glpRoot = Directory('../../programs').absolute.path;
  final rootSelf = '$glpRoot/self.glp';
  final gdsAgent = '$glpRoot/examples/rendezvous/gds_agent.glp';
  const day = 24 * 3600 * 1000;
  int now() => DateTime.now().millisecondsSinceEpoch;

  setUpAll(() async {
    try {
      await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping the agent: $e';
    }
    dir = Directory('../../programs/tests').createTempSync('gds_directory_');
    appPath = File('${dir.path}/app.glp').absolute.path;
    File(appPath).writeAsStringSync(appProgram);
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  Future<GrassrootsIdentity> identity(int fill) async {
    final f = File('${dir.path}/id-$fill.json');
    if (!f.existsSync()) {
      f.writeAsStringSync(jsonEncode({'seed': List.filled(32, fill).map((b) => b.toRadixString(16).padLeft(2, '0')).join()}));
    }
    return loadOrCreateIdentity(f.path);
  }

  Future<int> freePort() async {
    final probe = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    final p = probe.port;
    probe.close();
    return p;
  }

  /// The directory: its agent over its layer at UDP [udp], a free port by
  /// default, and its page, over one store in [data], the mail logged.
  Future<_Directory> directory(int fill, File data, {int? udp}) async {
    final state = FileAgentStateStore('${data.path}.agent');
    final port = udp ?? await freePort();
    final mail = LogMailer();
    final web = await HttpServer.bind('127.0.0.1', 0);
    final base = 'http://127.0.0.1:${web.port}';
    final service = DirectoryService(Store(file: data), mail, base);
    web.listen(service.handle);
    final lines = <String>[];
    final agent = await DirectoryAgent.start(
      service: service,
      identity: await identity(fill),
      port: port,
      publicAddress: '127.0.0.1:$port',
      queue: MemoryOutboundQueueStore(),
      state: state,
      program: gdsAgent,
      rootSelf: rootSelf,
    );
    expect(agent, isNotNull, reason: 'the directory binds its UDP port');
    agent!.onLine = lines.add;
    final d = _Directory(agent, web, mail, base, port, lines);
    addTearDown(d.stop);
    addTearDown(() => printOnFailure('the directory said ${lines.join(' | ')}'));
    return d;
  }

  /// A person's app over its own headless layer, which declares the
  /// directory [d].
  Future<_App> app(int fill, _Directory d) async {
    final id = await identity(fill);
    final layer = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: id,
      sodium: await initHeadlessSodium(),
    );
    layer.wan.putRendezvousServer(d.agent.key.bytes, '127.0.0.1:${d.udp}');
    addTearDown(layer.dispose);
    final key = glp.PubKey.fromList(id.publicKey);
    final agent = AgentRuntime(
      agentId: 'app-$fill',
      program: appPath,
      rootSelfGlpPath: rootSelf,
      keyPair: (pub: key, priv: Uint8List.fromList(id.privateKey)),
      network: HeadlessGlpNetwork(layer),
    );
    addTearDown(agent.dispose);
    final a = _App(key, layer, agent);
    agent.onUserOutput = a.say;
    agent.onOutput = a.transcript.add;
    addTearDown(() => printOnFailure('app-$fill said ${a.said.join(' | ')}'));
    addTearDown(() => printOnFailure('app-$fill transcript ${a.transcript.join(' | ')}'));
    await agent.initialize();
    expect(await layer.start(), isTrue);
    return a;
  }

  Future<String> get(String url) async {
    final c = HttpClient();
    final r = await (await c.getUrl(Uri.parse(url))).close();
    final s = await utf8.decodeStream(r);
    c.close();
    return s;
  }

  Future<String> post(String url, Map<String, String> form) async {
    final c = HttpClient();
    final q = await c.postUrl(Uri.parse(url));
    q.headers.contentType = ContentType('application', 'x-www-form-urlencoded');
    q.write(form.entries.map((e) => '${Uri.encodeQueryComponent(e.key)}=${Uri.encodeQueryComponent(e.value)}').join('&'));
    final s = await utf8.decodeStream(await q.close());
    c.close();
    return s;
  }

  /// The verification link of the last mail the directory sent.
  String mailedLink(_Directory d) =>
      RegExp(r'(http\S+/directory/verify\?t=\w+)').firstMatch(d.mail.sent.last.$3)!.group(1)!;

  /// A lists itself as Al, and opens the mail's link.
  Future<void> listAndVerify(_Directory d, _App a, {String email = 'al@example.org', String name = 'Al', String vis = 'listed'}) async {
    final asked = a.count('asked');
    final replied = a.count('replied');
    await a.act('list_named', [d.agent.key.hex, email, name, vis]);
    expect(_arg(await a.nth('replied', replied)), 'verify', reason: 'the directory sent the mail');
    expect(d.mail.sent.last.$1, email);
    expect(await get(mailedLink(d)), contains('is verified'));
    expect(_arg(await a.nth('asked', asked)), 1, reason: 'the buffer asked for, its floor one link');
  }

  test('a person lists by a verified address, is asked for links on the Supply stream, supplies one, and a lookup gives it and asks for two more', () async {
    if (sodiumUnavailable != null) return markTestSkipped(sodiumUnavailable!);
    final tmp = Directory.systemTemp.createTempSync('gds_store_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final d = await directory(0x61, File('${tmp.path}/d.json'));
    final a = await app(0x62, d);

    await a.act('list_named', [d.agent.key.hex, 'al@example.org', 'Al', 'listed']);
    expect(_arg(await a.next('replied')), 'verify');
    expect(d.mail.sent.single.$1, 'al@example.org');
    expect(d.store.listed(), isEmpty, reason: 'nothing is listed until the link is opened');
    expect(await get(mailedLink(d)), contains('is verified and listed'));
    expect(_arg(await a.nth('asked', 0)), 1);
    expect(d.store.byKey(a.key.hex)!.email, 'al@example.org');

    await a.act('supply', [d.agent.key.hex, 'rv', 1, now() + day]);
    await _until(() => d.store.byKey(a.key.hex)!.links.length == 1);

    final page = await get('${d.base}/directory');
    expect(page, contains('Al'));
    final link = d.store.byKey(a.key.hex)!.links.single.text;
    expect(await post('${d.base}/directory/lookup', {'id': d.store.listed().single.id}), contains(link));
    expect(_arg(await a.nth('asked', 1)), 2, reason: 'two more asked for the one taken');
    expect(d.store.byKey(a.key.hex)!.links, isEmpty);
  }, timeout: Timeout.none);

  test('refused: a listing naming another key, a second mail to an address within the hour, a link signed by another, and an expired link', () async {
    if (sodiumUnavailable != null) return markTestSkipped(sodiumUnavailable!);
    final tmp = Directory.systemTemp.createTempSync('gds_store_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final d = await directory(0x63, File('${tmp.path}/d.json'));
    final a = await app(0x64, d);
    final b = await app(0x65, d);

    await a.act('list_as', [d.agent.key.hex, b.key.hex, 'bo@example.org']);
    expect(_arg(await a.next('replied')), 'rejected', reason: 'the listing names B and A signed it');
    expect(await a.next('closed'), isNotNull, reason: 'the Supply of a refused list is closed');
    expect(d.mail.sent, isEmpty);

    await listAndVerify(d, a);
    await b.act('list_named', [d.agent.key.hex, 'al@example.org', 'Bo', 'listed']);
    expect(_arg(await b.next('replied')), 'rejected', reason: "one verification mail per address per hour: B asks for A's address within the hour");
    expect(d.mail.sent.length, 1);

    await b.act('mint', ['rv', 7, now() + day]);
    final forged = _arg(await b.next('minted')) as String;
    await a.act('give', [d.agent.key.hex, forged]);
    await a.act('supply', [d.agent.key.hex, 'rv', 2, now() - 1]);
    await a.act('supply', [d.agent.key.hex, 'rv', 3, now() + day]);
    await _until(() => d.store.byKey(a.key.hex)!.links.isNotEmpty);
    await Future<void>.delayed(const Duration(milliseconds: 500));
    final links = d.store.byKey(a.key.hex)!.links;
    expect(links, hasLength(1), reason: "B's link and the expired one are not kept");
    expect(links.single.text, isNot(forged));
  }, timeout: Timeout.none);

  test('a person listed with no name is shown on the page by email address', () async {
    if (sodiumUnavailable != null) return markTestSkipped(sodiumUnavailable!);
    final tmp = Directory.systemTemp.createTempSync('gds_store_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final d = await directory(0x72, File('${tmp.path}/d.json'));
    final a = await app(0x73, d);
    await a.act('list_unnamed', [d.agent.key.hex, 'nn@example.org', 'listed']);
    expect(_arg(await a.next('replied')), 'verify');
    expect(await get(mailedLink(d)), contains('is verified and listed'));
    final l = d.store.byKey(a.key.hex)!;
    expect(l.name, isNull, reason: 'none is no name');
    expect(l.shown, 'nn@example.org');
    expect(await get('${d.base}/directory'), contains('<button>nn@example.org</button>'));

    // A person who names themselves none is named so: name('none'), not
    // none (GNA 5e276e4, Section 7).
    final b = await app(0x74, d);
    await b.act('list_named', [d.agent.key.hex, 'nm@example.org', 'none', 'listed']);
    expect(_arg(await b.next('replied')), 'verify');
    expect(await get(mailedLink(d)), contains('is verified and listed'));
    expect(d.store.byKey(b.key.hex)!.shown, 'none');
  }, timeout: Timeout.none);

  test('an unlisted person is not on the page and is found by email address and name', () async {
    if (sodiumUnavailable != null) return markTestSkipped(sodiumUnavailable!);
    final tmp = Directory.systemTemp.createTempSync('gds_store_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final d = await directory(0x66, File('${tmp.path}/d.json'));
    final a = await app(0x67, d);
    await listAndVerify(d, a, email: 'ivy@example.org', name: 'Ivy', vis: 'unlisted');
    await a.act('supply', [d.agent.key.hex, 'rv', 1, now() + day]);
    await _until(() => d.store.byKey(a.key.hex)!.links.length == 1);
    expect(await get('${d.base}/directory'), isNot(contains('Ivy')));
    final link = d.store.byKey(a.key.hex)!.links.single.text;
    expect(await post('${d.base}/directory/find', {'email': 'ivy@example.org', 'name': 'Ivy'}), contains(link));
  }, timeout: Timeout.none);

  test("a person's later list takes the place of the earlier one: its Supply carries the asks, and the earlier Supply is closed", () async {
    if (sodiumUnavailable != null) return markTestSkipped(sodiumUnavailable!);
    final tmp = Directory.systemTemp.createTempSync('gds_store_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final d = await directory(0x6d, File('${tmp.path}/d.json'));
    final a = await app(0x6e, d);
    await listAndVerify(d, a);
    await a.act('supply', [d.agent.key.hex, 'rv', 1, now() + day]);
    await _until(() => d.store.byKey(a.key.hex)!.links.length == 1);

    await a.act('list_named', [d.agent.key.hex, 'al@example.net', 'Al N', 'listed']);
    expect(_arg(await a.nth('replied', 1)), 'verify');
    expect(await a.next('closed'), isNotNull, reason: "the earlier list's Supply is closed once the mail is sent");
    expect(d.store.listed().single.email, 'al@example.org', reason: 'nothing changes until the link is opened');
    expect(await get(mailedLink(d)), contains('is verified and listed'));
    expect(d.store.listed().single.email, 'al@example.net');
    expect(d.store.byKey(a.key.hex)!.links, hasLength(1), reason: "the links are the person's, and stay");
    final link = d.store.byKey(a.key.hex)!.links.single.text;
    expect(await post('${d.base}/directory/lookup', {'id': d.store.listed().single.id}), contains(link));
    expect(_arg(await a.nth('asked', 1)), 2, reason: 'the ask on the later Supply');
  }, timeout: Timeout.none);

  test('unlist ends the listing, closes the Supply stream and answers unlisted; an unlist signed for another is rejected', () async {
    if (sodiumUnavailable != null) return markTestSkipped(sodiumUnavailable!);
    final tmp = Directory.systemTemp.createTempSync('gds_store_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final d = await directory(0x68, File('${tmp.path}/d.json'));
    final a = await app(0x69, d);
    final b = await app(0x6f, d);
    await listAndVerify(d, a);
    await b.act('unlist_as', [d.agent.key.hex, a.key.hex]);
    expect(_arg(await b.next('unlist_replied')), 'rejected', reason: "B signed A's unlist");
    expect(d.store.byKey(a.key.hex), isNotNull);
    await a.act('unlist', [d.agent.key.hex]);
    expect(_arg(await a.next('unlist_replied')), 'unlisted');
    expect(await a.next('closed'), isNotNull);
    await _until(() => d.store.byKey(a.key.hex) == null);
    expect(d.store.listed(), isEmpty);
    expect(await get('${d.base}/directory'), contains('Nobody is listed yet'));
  }, timeout: Timeout.none);

  test('the server, built to an executable and run as a process of its own, lists an app by its mailed link and asks it for links', () async {
    if (sodiumUnavailable != null) return markTestSkipped(sodiumUnavailable!);
    final tmp = Directory.systemTemp.createTempSync('gds_server_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    // Built with `dart build cli`, as the rendezvous server is.
    final build = await Process.run(_dartExecutable(),
        ['build', 'cli', '-t', 'bin/directory_server.dart', '-o', '${tmp.path}/out']);
    expect(build.exitCode, 0, reason: '${build.stdout}\n${build.stderr}');
    final udp = await freePort();
    final web = await HttpServer.bind('127.0.0.1', 0);
    final webPort = web.port;
    await web.close();
    final server = await Process.start('${tmp.path}/out/bundle/bin/directory_server', [
      '--identity', '${tmp.path}/id.json',
      '--port', '$udp',
      '--public-address', '127.0.0.1:$udp',
      '--outbound-queue-file', '${tmp.path}/q.json',
      '--state-file', '${tmp.path}/state',
      '--program', gdsAgent,
      '--root-self', rootSelf,
    ], environment: {'PORT': '$webPort', 'DATA': '${tmp.path}/d.json'});
    final out = <String>[];
    final more = StreamController<String>.broadcast();
    server.stdout.transform(utf8.decoder).transform(const LineSplitter()).listen((l) {
      out.add(l);
      more.add(l);
    });
    server.stderr.transform(utf8.decoder).listen((_) {});
    addTearDown(() async {
      server.kill(ProcessSignal.sigterm);
      await server.exitCode;
    });
    addTearDown(() => printOnFailure('the server said ${out.join(' | ')}'));
    Future<String> line(bool Function(String) f) =>
        out.any(f) ? Future.value(out.firstWhere(f)) : more.stream.firstWhere(f);
    final ready = (await line((l) => l.startsWith('READY '))).split(' ');
    expect(ready[1], '$udp');
    expect(ready[3], 'http://127.0.0.1:$webPort/directory');

    // The app declares the directory by the key and address it printed.
    final dirKey = glp.PubKey.fromList([for (var i = 0; i < 64; i += 2) int.parse(ready[2].substring(i, i + 2), radix: 16)]);
    final id = await identity(0x6c);
    final layer = HeadlessGrassrootsNetwork(outboundQueueStore: MemoryOutboundQueueStore(), identity: id, sodium: await initHeadlessSodium());
    layer.wan.putRendezvousServer(dirKey.bytes, '127.0.0.1:$udp');
    addTearDown(layer.dispose);
    final key = glp.PubKey.fromList(id.publicKey);
    final agent = AgentRuntime(agentId: 'app-6c', program: appPath, rootSelfGlpPath: rootSelf,
        keyPair: (pub: key, priv: Uint8List.fromList(id.privateKey)), network: HeadlessGlpNetwork(layer));
    addTearDown(agent.dispose);
    final a = _App(key, layer, agent);
    agent.onUserOutput = a.say;
    await agent.initialize();
    expect(await layer.start(), isTrue);

    await a.act('list_named', [ready[2], 'al@example.org', 'Al', 'listed']);
    expect(_arg(await a.next('replied')), 'verify');
    // No relay is configured: the mail is written to the server's output.
    final verify = RegExp(r'^(http\S+/directory/verify\?t=\w+)$');
    final link = verify.firstMatch(await line(verify.hasMatch))!.group(1)!;
    expect(await get(link), contains('is verified and listed'));
    expect(_arg(await a.next('asked')), 1);
    expect(await get('http://127.0.0.1:$webPort/directory'), contains('Al'));
  }, timeout: Timeout.none);

  test('the listings and links survive a restart of the directory', () async {
    if (sodiumUnavailable != null) return markTestSkipped(sodiumUnavailable!);
    final tmp = Directory.systemTemp.createTempSync('gds_store_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final data = File('${tmp.path}/d.json');
    final d = await directory(0x6a, data);
    final a = await app(0x6b, d);
    await listAndVerify(d, a);
    await a.act('supply', [d.agent.key.hex, 'rv', 1, now() + day]);
    await _until(() => d.store.byKey(a.key.hex)!.links.length == 1);
    final link = d.store.byKey(a.key.hex)!.links.single.text;
    await d.stop();
    // The app's layer finds the directory gone, silent two heartbeats.
    await _until(() => !a.layer.isPeerReachable(d.agent.key.bytes), seconds: 90);

    final d2 = await directory(0x6a, data, udp: d.udp);
    // ... and reaches it again once it is back, at its address and under its
    // key, as the app's layer reconnects to a declared server.
    await _until(() => a.layer.isPeerReachable(d2.agent.key.bytes), seconds: 90);
    expect(d2.agent.key.hex, d.agent.key.hex, reason: 'the same key');
    expect(await get('${d2.base}/directory'), contains('Al'));

    // The agent's state is restored with its sessions: a lookup's ask
    // reaches the app on the Supply of its list before the restart.
    expect(await post('${d2.base}/directory/lookup', {'id': d2.store.listed().single.id}), contains(link));
    expect(_arg(await a.nth('asked', 1)), 2, reason: 'the ask for the link taken, on the Supply kept across the restart');

    // The app lists again, as it does on reconnecting: answered listed, with
    // no mail; the earlier Supply is closed, and what was asked and not
    // supplied is asked again on the new one.
    await a.act('list_named', [d2.agent.key.hex, 'al@example.org', 'Al', 'listed']);
    expect(_arg(await a.nth('replied', 1)), 'listed');
    expect(d2.mail.sent, isEmpty);
    expect(await a.next('closed'), isNotNull);
    expect(_arg(await a.nth('asked', 2)), 2, reason: 'the two asked and not supplied, asked again on the new Supply');
  }, timeout: Timeout.none);

  test('a list for an address already verified for the person is answered listed with no mail: its name and visibility replace the earlier, its Supply the earlier Supply, and the buffer is asked on it at once', () async {
    if (sodiumUnavailable != null) return markTestSkipped(sodiumUnavailable!);
    final tmp = Directory.systemTemp.createTempSync('gds_store_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final d = await directory(0x70, File('${tmp.path}/d.json'));
    final a = await app(0x71, d);
    await listAndVerify(d, a);
    await a.act('list_named', [d.agent.key.hex, 'al@example.org', 'Alice', 'unlisted']);
    expect(_arg(await a.nth('replied', 1)), 'listed');
    expect(d.mail.sent, hasLength(1), reason: 'no second mail');
    expect(await a.next('closed'), isNotNull, reason: 'the earlier Supply is closed');
    expect(_arg(await a.nth('asked', 1)), 1, reason: 'the link asked for and not supplied, asked again on the new Supply');
    final l = d.store.byKey(a.key.hex)!;
    expect((l.name, l.listed), ('Alice', false));
    expect(await get('${d.base}/directory'), isNot(contains('Alice')));
    await a.act('supply', [d.agent.key.hex, 'rv', 1, now() + day]);
    await _until(() => d.store.byKey(a.key.hex)!.links.length == 1);
    final link = d.store.byKey(a.key.hex)!.links.single.text;
    expect(await post('${d.base}/directory/find', {'email': 'al@example.org', 'name': 'Alice'}), contains(link), reason: 'found by the name that replaced the earlier');
  }, timeout: Timeout.none);
}

/// The Dart that builds the server: under `flutter test` the test runs in
/// flutter_tester, whose executable is not Dart's, and the Flutter SDK's own
/// Dart is used; under `dart test`, the Dart running the test.
String _dartExecutable() {
  final root = Platform.environment['FLUTTER_ROOT'];
  if (root != null) {
    final dart = File('$root/bin/dart');
    if (dart.existsSync()) return dart.path;
  }
  return Platform.resolvedExecutable;
}

/// The first argument of a term the app said.
Object? _arg(rt.Term t) => ((t as rt.StructTerm).args[0] as rt.ConstTerm).value;

/// Completes once [holds] holds, checked every 50 ms, within [seconds].
Future<void> _until(bool Function() holds, {int seconds = 30}) async {
  final end = DateTime.now().add(Duration(seconds: seconds));
  while (!holds()) {
    if (DateTime.now().isAfter(end)) fail('timed out waiting');
    await Future<void>.delayed(const Duration(milliseconds: 50));
  }
}

class _Directory {
  final DirectoryAgent agent;
  final HttpServer web;
  final LogMailer mail;
  final String base;
  final int udp;
  final List<String> lines;
  bool _stopped = false;
  _Directory(this.agent, this.web, this.mail, this.base, this.udp, this.lines);
  Store get store => agent.store;
  Future<void> stop() async {
    if (_stopped) return;
    _stopped = true;
    await web.close(force: true);
    await agent.dispose();
  }
}

/// A person's app: its key, its layer, its agent, and what it told the test.
class _App {
  _App(this.key, this.layer, this.agent);
  final glp.PubKey key;
  final HeadlessGrassrootsNetwork layer;
  final AgentRuntime agent;
  final List<rt.Term> said = [];
  final StreamController<rt.Term> _said = StreamController.broadcast();
  final List<String> transcript = [];

  void say(rt.Term t) {
    said.add(t);
    _said.add(t);
  }

  Future<void> act(String functor, List<Object> args) =>
      agent.injectUserInput(rt.StructTerm(functor, [for (final a in args) rt.ConstTerm(a)]));

  bool _named(rt.Term t, String functor) =>
      (t is rt.StructTerm && t.functor == functor) || (t is rt.ConstTerm && t.value == functor);

  int count(String functor) => said.where((t) => _named(t, functor)).length;

  /// The [n]th term named [functor] the app said, counting from 0, once said.
  Future<rt.Term> nth(String functor, int n) async {
    final end = DateTime.now().add(const Duration(seconds: 30));
    while (true) {
      final ts = said.where((t) => _named(t, functor)).toList();
      if (ts.length > n) return ts[n];
      if (DateTime.now().isAfter(end)) fail('app ${agent.agentId} did not say $functor #$n');
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
  }

  Future<rt.Term> next(String functor) => nth(functor, 0);
}
