/// The name-server service logic: verified name-manifest deposits, repoints,
/// and retirements; counter-signing; zone writes.  Semantics per the GPW
/// paper (Section 6, Implementation Notes appendix) and
/// GPW/docs/data-types-and-servers.md: every mutable datum has a single
/// writer and carries that writer's epoch; the latest epoch wins; deletion is
/// a tombstone at a higher epoch.  Allocation in this apex zone is
/// first-deposit-wins on a free label (community allocation is a per-zone
/// policy choice).
library;

import 'jcs.dart';
import 'crypto.dart';
import 'objects.dart';
import 'store.dart';
import 'zone_writer.dart';

/// An HTTP-shaped outcome: status code and JSON body.
class Outcome {
  Outcome(this.status, this.body);
  final int status;
  final Map<String, Object?> body;

  static Outcome error(int status, String message) =>
      Outcome(status, {'error': message});
}

class NameService {
  NameService({
    required this.zone,
    required this.store,
    required this.zoneWriter,
    required this.serverKey,
    DateTime Function()? clock,
  }) : _now = clock ?? (() => DateTime.now().toUtc());

  final String zone;
  final NameStore store;
  final ZoneWriter zoneWriter;
  final SigningKey serverKey;
  final DateTime Function() _now;

  Future<Outcome> deposit(String webName, Object? json) async {
    final Envelope env;
    final NameManifest manifest;
    try {
      env = Envelope.parse(json);
      manifest = NameManifest.parse(env.body, zone, webName);
    } on WireError catch (e) {
      return Outcome.error(400, e.message);
    }
    if (!await verifyJson(env.body, env.signature, manifest.publicKey)) {
      return Outcome.error(403, 'signature does not verify');
    }

    var state = store.load(webName);
    var newlyBound = false;

    if (state == null || state.retired) {
      // Free label (never bound, or retired and returned to the pool).
      if (manifest.replaces != null) {
        return Outcome.error(400, 'nothing to replace: the label is free');
      }
      if (state != null) {
        // Replay guard: the retired key may rebind only past its tombstone.
        final tombstoneEpoch =
            (state.retirement!['body'] as Map)['epoch'] as int;
        final sameKey = state.manifestBody['publicKey'] == manifest.publicKey;
        if (sameKey && manifest.common.epoch <= tombstoneEpoch) {
          return Outcome.error(
              409, 'epoch not above the retirement tombstone');
        }
        store.archiveTombstone(webName, state);
      }
      state = null;
      newlyBound = true;
    } else {
      // Bound label: same key at a higher epoch, or Replace (SPM).
      final replaces = manifest.replaces;
      if (replaces != null) {
        if (replaces.oldKey != state.manifestBody['publicKey']) {
          return Outcome.error(409, 'replaces.oldKey is not the bound key');
        }
        if (manifest.common.epoch <= state.manifestEpoch) {
          return Outcome.error(409, 'epoch not above the bound manifest');
        }
        final record =
            (state.manifestBody['identityRecord'] as Map).cast<String, Object?>();
        final custodians = (record['custodians'] as List).cast<String>();
        final threshold = record['threshold'] as int;
        // Custodians sign the new body without the replaces block (which
        // holds their signatures); the person's outer signature covers the
        // full body.
        final bare = Map.of(env.body)..remove('replaces');
        final valid = <String>{};
        for (final cs in replaces.custodianSignatures) {
          if (custodians.contains(cs.key) &&
              await verifyJson(bare, cs.signature, cs.key)) {
            valid.add(cs.key);
          }
        }
        if (valid.length < threshold) {
          return Outcome.error(403,
              'custodian signatures below the identity record\'s threshold');
        }
      } else {
        if (manifest.publicKey != state.manifestBody['publicKey']) {
          return Outcome.error(409, 'web-name is bound to another key');
        }
        if (manifest.common.epoch < state.manifestEpoch) {
          return Outcome.error(409, 'epoch below the bound manifest');
        }
        if (manifest.common.epoch == state.manifestEpoch) {
          if (jcsString(env.body) == jcsString(state.manifestBody)) {
            return Outcome(200, state.served()); // idempotent redeposit
          }
          return Outcome.error(409, 'conflicting deposit at the bound epoch');
        }
      }
    }

    final counterSignature = {
      'serverKey': serverKey.publicKeyB64,
      'signature': await serverKey
          .signJson({'body': env.body, 'signature': env.signature}),
      'signedAt': _now().toIso8601String(),
    };
    final next = NameState(
      manifestBody: env.body,
      manifestSignature: env.signature,
      counterSignature: counterSignature,
      manifestEpoch: manifest.common.epoch,
      repointEpoch: state?.repointEpoch ?? -1,
      mirror: state?.mirror,
    );
    store.save(webName, next);
    return Outcome(newlyBound ? 201 : 200, next.served());
  }

  Outcome get(String webName) {
    if (!validLabel(webName)) return Outcome.error(400, 'invalid web-name');
    final state = store.load(webName);
    if (state == null) return Outcome.error(404, 'no such web-name');
    if (state.retired) {
      return Outcome(410, {'retirement': state.retirement});
    }
    return Outcome(200, state.served());
  }

  Future<Outcome> repoint(String webName, Object? json) async {
    final state = store.load(webName);
    if (state == null || state.retired) {
      return Outcome.error(404, 'no such web-name');
    }
    final Envelope env;
    final Repoint rp;
    try {
      env = Envelope.parse(json);
      rp = Repoint.parse(env.body, zone, webName);
    } on WireError catch (e) {
      return Outcome.error(400, e.message);
    }
    final boundKey = state.manifestBody['publicKey'] as String;
    if (!await verifyJson(env.body, env.signature, boundKey)) {
      return Outcome.error(403, 'signature is not by the bound key');
    }
    if (rp.common.epoch <= state.repointEpoch) {
      return Outcome.error(409, 'repoint epoch not above the last repoint');
    }
    await zoneWriter.setAddress(webName, rp.mirror);
    state.repointEpoch = rp.common.epoch;
    state.mirror = rp.mirror;
    store.save(webName, state);
    return Outcome(200, {
      'status': 'repointed',
      'webName': webName,
      'mirror': rp.mirror,
      'epoch': rp.common.epoch,
    });
  }

  Future<Outcome> retire(String webName, Object? json) async {
    final state = store.load(webName);
    if (state == null || state.retired) {
      return Outcome.error(404, 'no such web-name');
    }
    final Envelope env;
    final Retirement rt;
    try {
      env = Envelope.parse(json);
      rt = Retirement.parse(env.body, zone, webName);
    } on WireError catch (e) {
      return Outcome.error(400, e.message);
    }
    final boundKey = state.manifestBody['publicKey'] as String;
    if (!await verifyJson(env.body, env.signature, boundKey)) {
      return Outcome.error(403, 'signature is not by the bound key');
    }
    if (rt.common.epoch <= state.manifestEpoch) {
      return Outcome.error(
          409, 'retirement epoch not above the bound manifest');
    }
    if (rt.redirect != null) {
      await zoneWriter.setAddress(webName, rt.redirect!);
    } else {
      await zoneWriter.clear(webName);
    }
    state.retirement = {'body': env.body, 'signature': env.signature};
    state.mirror = null;
    store.save(webName, state);
    return Outcome(200, {'status': 'retired', 'webName': webName});
  }

  Outcome serverKeyInfo() => Outcome(200, {'serverKey': serverKey.publicKeyB64});
}
