import 'dart:convert';
import 'dart:io';

import 'package:gpw_name_service/gpw_name_service.dart';
import 'package:test/test.dart';

import 'service_test.dart'
    show signed, manifestBody, retirementBody, zone;

void main() {
  late Directory tmp;
  late Directory mirrors;
  late RecordingRegistrar registrar;
  late NameService service;
  late MirrorWriter writer;
  late SigningKey person;

  setUp(() async {
    tmp = Directory.systemTemp.createTempSync('gpw_push');
    mirrors = Directory('${tmp.path}/mirrors')..createSync();
    registrar = RecordingRegistrar();
    final store = NameStore(tmp.path);
    service = NameService(
      zone: zone,
      store: store,
      zoneWriter: FakeZoneWriter(),
      serverKey: await SigningKey.generate(),
      registrar: registrar,
    );
    writer = MirrorWriter(zone: zone, store: store, mirrorsDir: mirrors);
    person = await SigningKey.generate();
  });

  tearDown(() => tmp.deleteSync(recursive: true));

  Future<Directory> makeArea(String webAddress, int epoch,
      {Map<String, String> pages = const {'/index.html': '<p>hi</p>'},
      SigningKey? key}) async {
    final dir = Directory('${tmp.path}/area-$epoch')..createSync();
    for (final e in pages.entries) {
      final f = File('${dir.path}${e.key}');
      f.createSync(recursive: true);
      f.writeAsStringSync(e.value);
    }
    await signArea(dir, key ?? person, webAddress, epoch);
    return dir;
  }

  group('registration rule', () {
    test('deposit registers, retirement unregisters', () async {
      await service.deposit('me', await signed(person, manifestBody(person)));
      expect(registrar.registered, {person.publicKeyB64});
      await service.retire(
          'me', await signed(person, retirementBody(epoch: 2)));
      expect(registrar.registered, isEmpty);
    });

    test('a key stays registered while it holds another binding', () async {
      await service.deposit('me', await signed(person, manifestBody(person)));
      await service.deposit(
          'us', await signed(person, manifestBody(person, name: 'us')));
      await service.retire(
          'me', await signed(person, retirementBody(epoch: 2)));
      expect(registrar.registered, {person.publicKeyB64});
      await service.retire('us',
          await signed(person, retirementBody(name: 'us', epoch: 2)));
      expect(registrar.registered, isEmpty);
    });

    test('Replace swaps old key for new', () async {
      final custodian = await SigningKey.generate();
      await service.deposit(
          'me',
          await signed(person,
              manifestBody(person, custodians: [custodian.publicKeyB64])));
      final fresh = await SigningKey.generate();
      final body = manifestBody(fresh, epoch: 2);
      final bare = Map<String, Object?>.of(body);
      body['replaces'] = {
        'oldKey': person.publicKeyB64,
        'custodianSignatures': [
          {
            'key': custodian.publicKeyB64,
            'signature': await custodian.signJson(bare),
          },
        ],
      };
      final out = await service.deposit('me', await signed(fresh, body));
      expect(out.status, 200);
      expect(registrar.registered, {fresh.publicKeyB64});
    });

    test('syncRegistrar rebuilds the set from bindings', () async {
      await service.deposit('me', await signed(person, manifestBody(person)));
      registrar.registered.clear();
      service.syncRegistrar();
      expect(registrar.registered, {person.publicKeyB64});
    });
  });

  group('verified push', () {
    test('a valid push lands atomically and serves', () async {
      await service.deposit('me', await signed(person, manifestBody(person)));
      final area = await makeArea('me.$zone', 1,
          pages: {'/index.html': '<p>v1</p>', '/notes.html': '<p>n</p>'});
      final result = await writer.applyPush(
          person.publicKeyB64, buildAreaPush(area, 'me.$zone'));
      expect(result.accepted, isTrue);
      expect(writer.servedAreaEpoch('me.$zone'), 1);
      expect(File('${mirrors.path}/me.$zone/index.html').readAsStringSync(),
          '<p>v1</p>');
      // The landed mirror verifies clean end to end.
      Future<List<int>?> fetch(String path) async {
        final f = File('${mirrors.path}/me.$zone$path');
        return f.existsSync() ? f.readAsBytesSync() : null;
      }

      expect(await verifyArea(fetch, person.publicKeyB64), isEmpty);
    });

    test('a newer area replaces the old atomically; dropped pages vanish',
        () async {
      await service.deposit('me', await signed(person, manifestBody(person)));
      final v1 = await makeArea('me.$zone', 1,
          pages: {'/index.html': '<p>v1</p>', '/old.html': '<p>old</p>'});
      await writer.applyPush(
          person.publicKeyB64, buildAreaPush(v1, 'me.$zone'));
      final v2 =
          await makeArea('me.$zone', 2, pages: {'/index.html': '<p>v2</p>'});
      final result = await writer.applyPush(
          person.publicKeyB64, buildAreaPush(v2, 'me.$zone'));
      expect(result.accepted, isTrue);
      expect(File('${mirrors.path}/me.$zone/index.html').readAsStringSync(),
          '<p>v2</p>');
      expect(File('${mirrors.path}/me.$zone/old.html').existsSync(), isFalse);
    });

    test('stale or equal area epoch is rejected', () async {
      await service.deposit('me', await signed(person, manifestBody(person)));
      final v2 = await makeArea('me.$zone', 2);
      await writer.applyPush(
          person.publicKeyB64, buildAreaPush(v2, 'me.$zone'));
      final again = await writer.applyPush(
          person.publicKeyB64, buildAreaPush(v2, 'me.$zone'));
      expect(again.accepted, isFalse);
      expect(again.reason, contains('areaEpoch'));
      final v1 = await makeArea('me.$zone', 1);
      expect(
          (await writer.applyPush(
                  person.publicKeyB64, buildAreaPush(v1, 'me.$zone')))
              .accepted,
          isFalse);
    });

    test('a push from a non-bound sender is rejected', () async {
      await service.deposit('me', await signed(person, manifestBody(person)));
      final other = await SigningKey.generate();
      final area = await makeArea('me.$zone', 1, key: other);
      final result = await writer.applyPush(
          other.publicKeyB64, buildAreaPush(area, 'me.$zone'));
      expect(result.accepted, isFalse);
      expect(result.reason, contains('sender'));
      expect(Directory('${mirrors.path}/me.$zone').existsSync(), isFalse);
    });

    test('a push for an unbound or retired name is rejected', () async {
      final area = await makeArea('me.$zone', 1);
      expect(
          (await writer.applyPush(
                  person.publicKeyB64, buildAreaPush(area, 'me.$zone')))
              .reason,
          contains('not bound'));
    });

    test('tampered page bytes are rejected and nothing lands', () async {
      await service.deposit('me', await signed(person, manifestBody(person)));
      final area = await makeArea('me.$zone', 1);
      final payload =
          jsonDecode(utf8.decode(buildAreaPush(area, 'me.$zone'))) as Map;
      (payload['pages'] as Map)['/index.html'] =
          base64Encode(utf8.encode('<p>tampered</p>'));
      final result = await writer.applyPush(
          person.publicKeyB64, utf8.encode(jsonEncode(payload)));
      expect(result.accepted, isFalse);
      expect(result.reason, contains('hash'));
      expect(Directory('${mirrors.path}/me.$zone').existsSync(), isFalse);
    });

    test('a listed page missing from the payload is rejected', () async {
      await service.deposit('me', await signed(person, manifestBody(person)));
      final area = await makeArea('me.$zone', 1);
      final payload =
          jsonDecode(utf8.decode(buildAreaPush(area, 'me.$zone'))) as Map;
      (payload['pages'] as Map).remove('/index.html');
      final result = await writer.applyPush(
          person.publicKeyB64, utf8.encode(jsonEncode(payload)));
      expect(result.accepted, isFalse);
      expect(result.reason, contains('not carried'));
    });

    test('a foreign-zone web address is rejected', () async {
      final area = await makeArea('me.example.org', 1);
      expect(
          (await writer.applyPush(
                  person.publicKeyB64, buildAreaPush(area, 'me.example.org')))
              .reason,
          contains('zone'));
    });
  });
}
