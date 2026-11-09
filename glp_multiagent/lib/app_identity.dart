/// The app installed on the device, as the platform names it, and the
/// attestation that gives the networking layer the version of the app whose
/// program the agent runs.
///
/// `self_app/2` assigns the app whose program the agent runs and the app
/// installed, which differ after an update the runtime refused (GLP paper
/// cce13ce, appendix-guards, "Networking seam"; IGLP 59c40e2, Definition
/// "Seam Predicates"), and the version the layer attests is "the one the
/// runtime gives it, that of the program the agent runs" (IGLP, Implementation
/// Notes, "The contract").  The host asks the platform for the installed app
/// ([installedApp]) and gives it to the runtime (AgentRuntime.installedApp);
/// the runtime decides at its start which app's program the agent runs
/// (AgentRuntime.runningApp); and the layer signs that app's version in each
/// session's digest, through a [RunningAppAttestation] over the platform's
/// attestation, which attests no session before the runtime has decided.  The
/// API of the layer is unchanged: it asks its attestation for the version, as
/// it always has (GNA, Section 2.6).
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:flutter/services.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show Application;
import 'package:grassroots_networking/grassroots_networking.dart'
    show AttestationEvidence, AttestationVerdict, PlatformAttestation;

/// The channel by which the platform names the app installed: the Runner's
/// (ios/Runner/AppDelegate.swift).
const MethodChannel appIdentityChannel =
    MethodChannel('grassroots/app_identity');

/// The app installed on the device: its developer, its name and its version
/// number, as the platform names them --- on iOS the team identifier, the
/// bundle identifier and the bundle version, as the attestation names them to
/// a peer (GNA, Section 2.6).  Null where the platform names none: a build
/// signed with no team, a bundle version that is no whole number, or a
/// platform with no channel, as a test's.
Future<Application?> installedApp(
    {MethodChannel channel = appIdentityChannel}) async {
  final Map<Object?, Object?>? named;
  try {
    named = await channel.invokeMapMethod<Object?, Object?>('identity');
  } on MissingPluginException {
    return null;
  } on PlatformException {
    return null;
  }
  if (named == null) return null;
  final developer = named['developer'];
  final name = named['name'];
  final version = named['version'];
  if (developer is! String || name is! String || version is! int) {
    return null;
  }
  return Application(developer: developer, name: name, version: version);
}

/// The platform's attestation, with the version of the app whose program the
/// agent runs in place of the installed one: [applicationVersion] waits until
/// the host [give]s the app the runtime decided on at the agent's start, so no
/// session is attested with a version before it.  Everything else is the
/// platform's.
class RunningAppAttestation implements PlatformAttestation {
  RunningAppAttestation(this.platform);

  /// The platform's attestation, the native producer on a phone.
  final PlatformAttestation platform;

  final Completer<int?> _version = Completer<int?>();

  /// Whether [give] has been called.
  bool get given => _version.isCompleted;

  /// Give the layer the app whose program the agent runs, once the runtime
  /// has decided it (AgentRuntime.runningApp, after initialize): its version
  /// is the one attested.  Where the runtime knows no app, the platform's own
  /// version is attested, the program the agent runs being the installed
  /// app's.  A second call does nothing: the runtime decides once a launch.
  Future<void> give(Application? running) async {
    if (_version.isCompleted) return;
    final version = running?.version ?? await platform.applicationVersion();
    if (!_version.isCompleted) _version.complete(version);
  }

  @override
  Future<int?> applicationVersion() => _version.future;

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) =>
      platform.attestationFor(identityPublicKey);

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) =>
      platform.signSessionDigest(
          identityPublicKey: identityPublicKey, digest: digest);

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List handshakeHash,
    required Uint8List peerIdentityKey,
  }) =>
      platform.verify(
          evidence: evidence,
          handshakeHash: handshakeHash,
          peerIdentityKey: peerIdentityKey);
}
