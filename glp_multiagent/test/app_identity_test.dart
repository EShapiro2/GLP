/// The app installed, as the platform names it, and the attestation that
/// gives the networking layer the version of the app whose program the agent
/// runs (lib/app_identity.dart; GLP paper cce13ce, appendix-guards,
/// "Networking seam": self_app/2; IGLP 59c40e2, Implementation Notes, "The
/// contract": the version the layer attests is "the one the runtime gives
/// it, that of the program the agent runs").
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/app_identity.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show Application;
import 'package:grassroots_networking/grassroots_networking.dart' show NoPlatformAttestation;

/// A platform whose installed version is [installed], counting what it is
/// asked.
class _Platform extends NoPlatformAttestation {
  _Platform(this.installed);
  final int installed;
  int signed = 0;

  @override
  Future<int?> applicationVersion() async => installed;

  @override
  Future<Uint8List?> signSessionDigest(
      {required Uint8List identityPublicKey, required Uint8List digest}) async {
    signed++;
    return Uint8List(0);
  }
}

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  test('the version attested waits for the app the runtime decided on, and is '
      'the running app\'s, not the installed one\'s', () async {
    final platform = _Platform(3);
    final attestation = RunningAppAttestation(platform);
    var asked = false;
    final version = attestation.applicationVersion()..then((_) => asked = true);
    await Future<void>.delayed(Duration.zero);
    expect(asked, isFalse, reason: 'no session is attested before the runtime '
        'has decided which app\'s program the agent runs');

    // After a refused update the agent runs version 2 while 3 is installed.
    await attestation.give(
        const Application(developer: 'TEAM', name: 'app.grassroots', version: 2));
    expect(await version, 2);
    expect(await attestation.applicationVersion(), 2);
    await attestation.give(
        const Application(developer: 'TEAM', name: 'app.grassroots', version: 9));
    expect(await attestation.applicationVersion(), 2,
        reason: 'the runtime decides once a launch');

    await attestation.signSessionDigest(
        identityPublicKey: Uint8List(32), digest: Uint8List(32));
    expect(platform.signed, 1, reason: 'the rest is the platform\'s');
  });

  test('where the runtime knows no app, the platform\'s own version is '
      'attested', () async {
    final attestation = RunningAppAttestation(_Platform(5));
    await attestation.give(null);
    expect(await attestation.applicationVersion(), 5);
  });

  test('the installed app is what the platform names, and none where it names '
      'none or has no channel', () async {
    const channel = MethodChannel('grassroots/app_identity_test');
    final messenger =
        TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger;
    Object? answer = {
      'developer': 'TEAM',
      'name': 'app.grassroots',
      'version': 7,
    };
    messenger.setMockMethodCallHandler(channel, (call) async {
      expect(call.method, 'identity');
      return answer;
    });
    expect(await installedApp(channel: channel),
        const Application(developer: 'TEAM', name: 'app.grassroots', version: 7));
    answer = null;
    expect(await installedApp(channel: channel), isNull);
    messenger.setMockMethodCallHandler(channel, null);
    expect(await installedApp(channel: channel), isNull,
        reason: 'no channel, as on a desktop or in a test');
  });
}
