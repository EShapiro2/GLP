import 'dart:io';

import 'package:gpw_name_service/gpw_name_service.dart';
import 'package:test/test.dart';

const zone = 'peoplesweb.org';

Future<Map<String, Object?>> signed(
    SigningKey key, Map<String, Object?> body) async {
  return {'body': body, 'signature': await key.signJson(body)};
}

Map<String, Object?> manifestBody(SigningKey key,
    {String name = 'me',
    int epoch = 1,
    List<String>? custodians,
    int threshold = 1,
    Map<String, Object?>? replaces}) {
  return {
    'format': 'gpw/name-manifest/1',
    'zone': zone,
    'webName': name,
    'publicKey': key.publicKeyB64,
    'identityRecord': {
      'custodians': custodians ?? [key.publicKeyB64],
      'threshold': threshold,
    },
    'epoch': epoch,
    'issuedAt': '2026-07-12T00:00:00Z',
    if (replaces != null) 'replaces': replaces,
  };
}

Map<String, Object?> repointBody(
        {String name = 'me', required String mirror, required int epoch}) =>
    {
      'format': 'gpw/repoint/1',
      'zone': zone,
      'webName': name,
      'mirror': mirror,
      'epoch': epoch,
      'issuedAt': '2026-07-12T00:00:00Z',
    };

Map<String, Object?> retirementBody(
        {String name = 'me', required int epoch, String? redirect}) =>
    {
      'format': 'gpw/retirement/1',
      'zone': zone,
      'webName': name,
      if (redirect != null) 'redirect': redirect,
      'epoch': epoch,
      'issuedAt': '2026-07-12T00:00:00Z',
    };

void main() {
  late Directory tmp;
  late FakeZoneWriter zoneWriter;
  late NameService service;
  late SigningKey person;

  setUp(() async {
    tmp = Directory.systemTemp.createTempSync('gpw_test');
    zoneWriter = FakeZoneWriter();
    service = NameService(
      zone: zone,
      store: NameStore(tmp.path),
      zoneWriter: zoneWriter,
      serverKey: await SigningKey.generate(),
      clock: () => DateTime.utc(2026, 7, 12),
    );
    person = await SigningKey.generate();
  });

  tearDown(() => tmp.deleteSync(recursive: true));

  test('deposit on a free label binds it (first deposit wins)', () async {
    final out = await service.deposit('me', await signed(person, manifestBody(person)));
    expect(out.status, 201);
    final counter = out.body['counterSignature'] as Map;
    expect(
      await verifyJson(
        {'body': out.body['body'], 'signature': out.body['signature']},
        counter['signature'] as String,
        counter['serverKey'] as String,
      ),
      isTrue,
    );
    expect(service.get('me').status, 200);
  });

  test('bad signature is rejected', () async {
    final other = await SigningKey.generate();
    final body = manifestBody(person);
    final out = await service
        .deposit('me', {'body': body, 'signature': await other.signJson(body)});
    expect(out.status, 403);
  });

  test('a second key cannot take a bound label', () async {
    await service.deposit('me', await signed(person, manifestBody(person)));
    final thief = await SigningKey.generate();
    final out = await service.deposit(
        'me', await signed(thief, manifestBody(thief, epoch: 5)));
    expect(out.status, 409);
  });

  test('epoch discipline: higher wins, identical redeposit idempotent, '
      'conflict rejected', () async {
    await service.deposit('me', await signed(person, manifestBody(person)));
    expect(
        (await service.deposit(
                'me', await signed(person, manifestBody(person, epoch: 2))))
            .status,
        200);
    expect(
        (await service.deposit(
                'me', await signed(person, manifestBody(person, epoch: 2))))
            .status,
        200); // idempotent
    final conflicting = manifestBody(person, epoch: 2)
      ..['issuedAt'] = '2026-07-12T01:00:00Z';
    expect((await service.deposit('me', await signed(person, conflicting))).status,
        409);
    expect(
        (await service.deposit(
                'me', await signed(person, manifestBody(person, epoch: 1))))
            .status,
        409); // stale
  });

  test('repoint writes the address record and bumps the repoint epoch',
      () async {
    await service.deposit('me', await signed(person, manifestBody(person)));
    final out = await service.repoint('me',
        await signed(person, repointBody(mirror: 'mirror.example.org', epoch: 0)));
    expect(out.status, 200);
    expect(zoneWriter.records['me'], ('CNAME', 'mirror.example.org.'));
    // IP literal becomes an A record.
    await service.repoint('me',
        await signed(person, repointBody(mirror: '162.35.180.99', epoch: 1)));
    expect(zoneWriter.records['me'], ('A', '162.35.180.99'));
    // Stale repoint epoch rejected.
    final stale = await service.repoint('me',
        await signed(person, repointBody(mirror: 'other.example.org', epoch: 1)));
    expect(stale.status, 409);
  });

  test('repoint by a non-bound key is rejected', () async {
    await service.deposit('me', await signed(person, manifestBody(person)));
    final other = await SigningKey.generate();
    final out = await service.repoint('me',
        await signed(other, repointBody(mirror: 'evil.example.org', epoch: 0)));
    expect(out.status, 403);
    expect(zoneWriter.records, isEmpty);
  });

  test('retirement tombstones the name, clears the record, frees the label',
      () async {
    await service.deposit('me', await signed(person, manifestBody(person)));
    await service.repoint('me',
        await signed(person, repointBody(mirror: 'mirror.example.org', epoch: 0)));
    final out = await service.retire(
        'me', await signed(person, retirementBody(epoch: 2)));
    expect(out.status, 200);
    expect(zoneWriter.records, isEmpty);
    expect(service.get('me').status, 410);
    // Replay of the old manifest (epoch 1 <= tombstone 2) cannot resurrect.
    final replay = await service.deposit(
        'me', await signed(person, manifestBody(person, epoch: 1)));
    expect(replay.status, 409);
    // The label returned to the pool: a fresh key binds it.
    final next = await SigningKey.generate();
    final rebind = await service.deposit(
        'me', await signed(next, manifestBody(next, epoch: 1)));
    expect(rebind.status, 201);
  });

  test('retirement with redirect writes a CNAME', () async {
    await service.deposit('me', await signed(person, manifestBody(person)));
    await service.retire('me',
        await signed(person, retirementBody(epoch: 2, redirect: 'me.other.org')));
    expect(zoneWriter.records['me'], ('CNAME', 'me.other.org.'));
  });

  test('Replace: custodians meeting the threshold rebind to a fresh key',
      () async {
    final c1 = await SigningKey.generate();
    final c2 = await SigningKey.generate();
    final c3 = await SigningKey.generate();
    await service.deposit(
        'me',
        await signed(
            person,
            manifestBody(person,
                custodians: [c1.publicKeyB64, c2.publicKeyB64, c3.publicKeyB64],
                threshold: 2)));

    final fresh = await SigningKey.generate();
    Future<Map<String, Object?>> replaceDeposit(List<SigningKey> signers) async {
      final body = manifestBody(fresh, epoch: 2);
      body['replaces'] = {
        'oldKey': person.publicKeyB64,
        'custodianSignatures': [
          for (final c in signers)
            {'key': c.publicKeyB64, 'signature': await c.signJson(body)},
        ],
      };
      return signed(fresh, body);
    }

    // One custodian is below the threshold.
    expect((await service.deposit('me', await replaceDeposit([c1]))).status, 403);
    // Two meet it; the web-name survives the key replacement.
    expect(
        (await service.deposit('me', await replaceDeposit([c1, c3]))).status, 200);
    final served = service.get('me');
    expect((served.body['body'] as Map)['publicKey'], fresh.publicKeyB64);
    // The old key no longer repoints.
    expect(
        (await service.repoint('me',
                await signed(person, repointBody(mirror: 'x.example.org', epoch: 1))))
            .status,
        403);
    // The fresh key does.
    expect(
        (await service.repoint('me',
                await signed(fresh, repointBody(mirror: 'x.example.org', epoch: 1))))
            .status,
        200);
  });

  test('the identity record is immutable within an identity', () async {
    final c1 = await SigningKey.generate();
    await service.deposit('me',
        await signed(person, manifestBody(person, custodians: [c1.publicKeyB64])));
    // Same key, higher epoch, different custodians — the thief attack.
    final c2 = await SigningKey.generate();
    final out = await service.deposit(
        'me',
        await signed(person,
            manifestBody(person, epoch: 2, custodians: [c2.publicKeyB64])));
    expect(out.status, 409);
    // Same record at a higher epoch still updates fine.
    expect(
        (await service.deposit(
                'me',
                await signed(person,
                    manifestBody(person, epoch: 2, custodians: [c1.publicKeyB64]))))
            .status,
        200);
  });

  test('the threshold must be a supermajority of the custodians', () async {
    final c = [for (var i = 0; i < 4; i++) await SigningKey.generate()];
    final keys = c.map((k) => k.publicKeyB64).toList();
    Future<int> status(List<String> custodians, int threshold) async =>
        (await service.deposit(
                'me',
                await signed(
                    person,
                    manifestBody(person,
                        custodians: custodians, threshold: threshold))))
            .status;
    expect(await status(keys.sublist(0, 3), 1), 400); // 1 of 3
    expect(await status(keys, 2), 400); // 2 of 4
    expect(await status(keys, 5), 400); // above the count
    expect(await status(keys.sublist(0, 3), 2), 201); // 2 of 3 binds
  });

  test('mirror-ask tracks bound web-names', () async {
    expect(service.mirrorAsk('me.peoplesweb.org').status, 404); // unbound
    await service.deposit('me', await signed(person, manifestBody(person)));
    expect(service.mirrorAsk('me.peoplesweb.org').status, 200);
    expect(service.mirrorAsk('Me.PeoplesWeb.Org').status, 200); // case-blind
    expect(service.mirrorAsk('other.peoplesweb.org').status, 404);
    expect(service.mirrorAsk('a.me.peoplesweb.org').status, 404); // two labels
    expect(service.mirrorAsk('peoplesweb.org').status, 404); // the apex
    expect(service.mirrorAsk('me.example.org').status, 404); // foreign zone
    expect(service.mirrorAsk(null).status, 404);
    await service.retire(
        'me', await signed(person, retirementBody(epoch: 2)));
    expect(service.mirrorAsk('me.peoplesweb.org').status, 404); // retired
  });

  test('malformed objects are 400', () async {
    expect((await service.deposit('me', null)).status, 400);
    expect((await service.deposit('me', {'body': {}, 'signature': 'x'})).status,
        400);
    final wrongZone = manifestBody(person)..['zone'] = 'example.org';
    expect((await service.deposit('me', await signed(person, wrongZone))).status,
        400);
    final wrongName = manifestBody(person, name: 'you');
    expect((await service.deposit('me', await signed(person, wrongName))).status,
        400);
    expect(service.get('UPPER').status, 400);
    expect(service.get('nosuch').status, 404);
  });
}
