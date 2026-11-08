// The directory's store and page (GNA Section 7, GDS with its implementation
// pragmas): listing by a verified email address, with a name or none,
// listed or unlisted; one verification mail per address per hour; lookup on
// the page and by email address and name; the buffer of links, the one
// expiring first given, two wanted for each taken, one kept at least,
// expired ones dropped; unlisting; and the store surviving a restart.
import 'dart:convert';
import 'dart:io';

import 'package:directory_server/mail.dart';
import 'package:directory_server/server.dart';
import 'package:directory_server/store.dart';
import 'package:test/test.dart';

void main() {
  var t = 1000000;
  int clock() => t;
  const day = 24 * 3600 * 1000;

  String tokenOf(ListResult r) => (r as MailToSend).token;

  group('store', () {
    test('a listing waits until its address is verified', () {
      final s = Store(now: clock);
      final tok = tokenOf(s.request('kalice', 'Alice@Example.org', name: 'Alice'));
      expect(s.listed(), isEmpty);
      expect(s.verify('no such token'), isNull);
      final l = s.verify(tok)!;
      expect(l.email, 'alice@example.org');
      expect(s.listed().map((l) => l.shown), ['Alice']);
      expect(s.verify(tok), isNull, reason: 'a token is used once');
    });

    test('shown by email address where no name was given', () {
      final s = Store(now: clock);
      s.verify(tokenOf(s.request('kbob', 'bob@example.org')));
      expect(s.listed().single.shown, 'bob@example.org');
    });

    test('one verification mail per address per hour', () {
      final s = Store(now: clock);
      tokenOf(s.request('kcarol', 'carol@example.org'));
      expect(s.request('kcarol', 'carol@example.org'), isA<Refused>());
      t += 3600 * 1000;
      expect(s.request('kcarol', 'carol@example.org'), isA<MailToSend>());
      expect(s.request('kcarol', 'not an address'), isA<Refused>());
    });

    test('an unlisted person is not on the page but is found by email address and name', () {
      final s = Store(now: clock);
      s.verify(tokenOf(s.request('kdave', 'dave@example.org', name: 'Dave', listed: false)));
      expect(s.listed(), isEmpty);
      expect(s.find('DAVE@example.org', 'dave')!.email, 'dave@example.org');
      expect(s.find('dave@example.org', 'David'), isNull);
    });

    test('the buffer: the link expiring first is given, two wanted for it, one kept at least, expired ones dropped', () {
      final s = Store(now: clock);
      final l = s.verify(tokenOf(s.request('kerin', 'erin@example.org', name: 'Erin')))!;
      expect(s.wanted(), {'erin@example.org': 1});
      s.supply('kerin', [Link('late', t + day), Link('early', t + day ~/ 2), Link('gone', t - 1)]);
      expect(s.wanted(), isEmpty);
      expect(s.take(l)!.text, 'early');
      expect(s.wanted(), {'erin@example.org': 2});
      t += day ~/ 2 + day;
      expect(s.take(l), isNull, reason: 'every link has expired: the app has been off for more than a day');
      expect(s.wanted()['erin@example.org'], 4);
    });

    test('unlisting takes the listing and its links away', () {
      final s = Store(now: clock);
      s.verify(tokenOf(s.request('kfay', 'fay@example.org', name: 'Fay')));
      s.supply('kfay', [Link('x', t + day)]);
      expect(s.unlist('kfay'), isTrue);
      expect(s.listed(), isEmpty);
      expect(s.find('fay@example.org', 'Fay'), isNull);
    });

    test('the listings and links survive a restart', () {
      final dir = Directory.systemTemp.createTempSync('gds');
      final f = File('${dir.path}/d.json');
      final s = Store(file: f, now: clock);
      s.verify(tokenOf(s.request('kgil', 'gil@example.org', name: 'Gil')));
      s.supply('kgil', [Link('k', t + day)]);
      final s2 = Store(file: f, now: clock);
      expect(s2.listed().single.shown, 'Gil');
      expect(s2.take(s2.listed().single)!.text, 'k');
      dir.deleteSync(recursive: true);
    });

    test('a request changes nothing until its link is opened, whoever made it', () {
      final s = Store(now: clock);
      s.verify(tokenOf(s.request('kjo', 'jo@example.org', name: 'Jo')));
      t += 3600 * 1000;
      final tok = tokenOf(s.request('kother', 'jo@example.org', name: 'Not Jo', listed: false));
      expect(s.listed().single.shown, 'Jo');
      expect(s.byKey('kjo')!.email, 'jo@example.org');
      expect(s.byKey('kother'), isNull);
      s.verify(tok);
      expect(s.listed(), isEmpty, reason: 'the address verified for the other key, unlisted');
      expect(s.byKey('kother')!.name, 'Not Jo');
      expect(s.byKey('kjo'), isNull);
    });

    test('a person has one listing: a verified listing of a key takes the place of its earlier one, with its links', () {
      final s = Store(now: clock);
      s.verify(tokenOf(s.request('kkim', 'kim@example.org', name: 'Kim')));
      s.supply('kkim', [Link('l1', t + day)]);
      s.verify(tokenOf(s.request('kkim', 'kim@example.net', name: 'Kim N')));
      expect(s.listed().map((l) => l.email), ['kim@example.net']);
      expect(s.byKey('kkim')!.links.single.text, 'l1');
      expect(s.find('kim@example.org', 'Kim'), isNull);
    });

    test('the asks: the floor on verification, two for each lookup, the floor again once links expire', () {
      final asks = <(String, int)>[];
      final s = Store(now: clock, onAsk: (k, n) => asks.add((k, n)));
      final l = s.verify(tokenOf(s.request('klee', 'lee@example.org', name: 'Lee')))!;
      expect(asks, [('klee', 1)]);
      s.supply('klee', [Link('a', t + day), Link('b', t + 2 * day)]);
      s.sweep();
      expect(asks, hasLength(1), reason: 'the buffer is above its floor');
      expect(s.take(l)!.text, 'a');
      expect(asks.last, ('klee', 2));
      t += 3 * day;
      s.sweep();
      expect(asks, hasLength(2), reason: 'two are asked for already, more than the floor');
      s.supply('klee', [Link('c', t + day), Link('d', t + day)]);
      t += 2 * day;
      s.sweep();
      expect(asks.last, ('klee', 1), reason: 'both expired, the floor asked for again');
    });

    test('what was asked for and not supplied is kept across a restart, and asked again when the person lists again', () {
      final dir = Directory.systemTemp.createTempSync('gds');
      final f = File('${dir.path}/d.json');
      final s = Store(file: f, now: clock);
      s.verify(tokenOf(s.request('kmo', 'mo@example.org', name: 'Mo')));
      expect(s.byKey('kmo')!.wanted, 1);
      final asks = <(String, int)>[];
      final s2 = Store(file: f, now: clock, onAsk: (k, n) => asks.add((k, n)));
      expect(s2.byKey('kmo')!.wanted, 1);
      s2.sweep();
      expect(asks, isEmpty, reason: 'one asked already keeps the floor');
      expect(s2.relist('kmo', 'mo@example.org', name: 'Mo M', listed: false), isTrue);
      expect(s2.relist('kother', 'mo@example.org'), isFalse, reason: 'not verified for that key');
      s2.reask('kmo');
      expect(asks, [('kmo', 1)]);
      expect((s2.byKey('kmo')!.name, s2.byKey('kmo')!.listed, s2.byKey('kmo')!.wanted), ('Mo M', false, 1));
      dir.deleteSync(recursive: true);
    });
  });

  group('page', () {
    late HttpServer server;
    late Store store;
    late LogMailer mail;
    late String base;

    setUp(() async {
      store = Store(now: clock);
      mail = LogMailer();
      server = await HttpServer.bind('127.0.0.1', 0);
      base = 'http://127.0.0.1:${server.port}';
      final d = DirectoryService(store, mail, base);
      server.listen(d.handle);
    });
    tearDown(() => server.close(force: true));

    Future<String> get(String path) async {
      final c = HttpClient();
      final r = await (await c.getUrl(Uri.parse('$base$path'))).close();
      final s = await utf8.decodeStream(r);
      c.close();
      return s;
    }

    Future<String> post(String path, Map<String, String> form) async {
      final c = HttpClient();
      final q = await c.postUrl(Uri.parse('$base$path'));
      q.headers.contentType = ContentType('application', 'x-www-form-urlencoded');
      q.write(form.entries.map((e) => '${Uri.encodeQueryComponent(e.key)}=${Uri.encodeQueryComponent(e.value)}').join('&'));
      final s = await utf8.decodeStream(await q.close());
      c.close();
      return s;
    }

    test('list, verify by the mailed link, look up and get a link', () async {
      final d = DirectoryService(store, mail, base);
      expect(await d.list('khal', 'hal@example.org', name: 'Hal <b>'), isNull);
      final body = mail.sent.single.$3;
      final link = RegExp(r'(http\S+/directory/verify\?t=\w+)').firstMatch(body)!.group(1)!;
      expect(await get(link.substring(base.length)), contains('is verified and listed'));
      store.supply('khal', [Link('cafe01', t + day)]);
      final page = await get('/directory');
      expect(page, contains('Hal &lt;b&gt;'), reason: 'names are escaped');
      final id = store.listed().single.id;
      expect(await post('/directory/lookup', {'id': id}), contains('cafe01'));
      expect(await post('/directory/lookup', {'id': id}), contains('has no invitation link here now'));
      expect(await d.list('khal', 'hal@example.org'), contains('within the hour'));
    });

    test('an unlisted person is found by the form', () async {
      store.verify(tokenOf(store.request('kivy', 'ivy@example.org', name: 'Ivy', listed: false)));
      store.supply('kivy', [Link('beef02', t + day)]);
      expect(await get('/directory'), isNot(contains('Ivy')));
      expect(await post('/directory/find', {'email': 'ivy@example.org', 'name': 'Ivy'}), contains('beef02'));
      expect(await post('/directory/find', {'email': 'ivy@example.org', 'name': 'Ive'}), contains('Nobody is listed'));
    });
  });
}
