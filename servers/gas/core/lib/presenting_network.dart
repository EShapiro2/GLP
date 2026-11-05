// The person's undertaking, presented (the GSC paper, Section "Signing the
// Contract"): an agent joins a grassroots platform by signing a one-sided
// undertaking to keep its contract towards all its other participants, and
// presents it to every participant they interact with; the code realising the
// contract refuses an interaction with a participant that has not presented
// one.  Presentation is not part of the protocol: the code carries the
// undertaking in the messages of an interaction, and it leaves no atom in a
// state.
//
// This network wraps the one the agent runs over and changes nothing of it.
// The undertaking is a GLP signed term (warranty.dart, signTerm), signed with
// the person's key over the source identity of the contract's unit, of the
// term undertaking(Package), the package's hash naming the contract and the
// code.  Before its first message to a person in a session, the agent's
// network sends that person the undertaking.  A message from a person whose
// undertaking has not arrived is held, not given to the agent, and that
// person is asked for it; it is given to the agent once their undertaking
// arrives and checks: signed by the sender, for the same package and the same
// code.  The undertakings received are kept, in undertakings/<app>.json:
// another participant then holds each, as evidence of who undertook what.
//
// An invitation is the inviter's signed term invitation(Package), over the
// same source identity: it names the contract, and its signer the inviter.
// The invited person checks it, keeps it, and presents their undertaking to
// the inviter before anything else (Session.addFar).
//
// Where the platform attests the peer's code (GNA's attestation: Android's
// hardware key attestation, iOS's App Attest, the application and its version
// named for each session), [admits] decides whose application counts: a
// message from a person whose connection was not attested as an application
// [admits] accepts is held, as one from a person who has not presented their
// undertaking is, until such a connection is reported.
//
// On the wire every payload carries a first byte: 0 the agent's message, 1 an
// undertaking, 2 a request for one.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/identity.dart';

import 'warranty.dart';

const _kindMsg = 0, _kindUndertaking = 1, _kindAsk = 2;

/// The undertaking of [who] to keep the contract of package [package], whose
/// contract unit has source identity [hSrc].
String undertakingOf(PersonIdentity who, String package, String hSrc) =>
    signTerm(who, bytesOfHex(hSrc), strings('undertaking', [package]));

/// Whether [signed] is [signer]'s undertaking to keep the contract of package
/// [package], whose contract unit has source identity [hSrc].
bool isUndertaking(String signed, PubKey signer, String package, String hSrc) {
  final s = readSigned(signed);
  if (s == null || s.key.hex != signer.hex || hexOf(s.hSrc) != hSrc) return false;
  final a = stringArgs(s.term, 'undertaking');
  return a != null && a.length == 1 && a[0] == package;
}

/// [who]'s invitation to the contract of package [package], whose contract
/// unit has source identity [hSrc].
String invitationOf(PersonIdentity who, String package, String hSrc) =>
    signTerm(who, bytesOfHex(hSrc), strings('invitation', [package]));

/// Whether [signed] is [inviter]'s invitation to the contract of package
/// [package], whose contract unit has source identity [hSrc].
bool isInvitation(String? signed, PubKey inviter, String package, String hSrc) {
  if (signed == null) return false;
  final s = readSigned(signed);
  if (s == null || s.key.hex != inviter.hex || hexOf(s.hSrc) != hSrc) return false;
  final a = stringArgs(s.term, 'invitation');
  return a != null && a.length == 1 && a[0] == package;
}

class PresentingNetwork implements GlpNetwork {
  final GlpNetwork inner;

  /// The package's hash, and its contract unit's source identity.
  final String package, hSrc;

  /// This person's undertaking, presented to everyone they interact with.
  final String undertaking;

  /// This person's invitation to the contract, which their invite links carry.
  final String invitation;

  /// Where the undertakings received are kept: [dir]/[app].json.
  final String dir, app;

  /// The undertakings received, by the presenter's key: kept.
  final Map<String, String> received = {};

  final Set<String> _presentedTo = {}, _asked = {};
  final Map<String, List<(Uint8List, String, Underlay)>> _held = {};

  /// A message refused, or an undertaking that did not check.
  void Function(PubKey pk, String why)? onRefused;

  /// An undertaking received and checked.
  void Function(PubKey pk)? onPresented;

  /// The attested applications this person interacts with, where only such
  /// are interacted with: given the application a connection reports (null
  /// where it is unattested), whether it counts.  Null: no attestation is
  /// required.
  bool Function(Object? application)? _admits;
  final Map<String, Set<Underlay>> _admittedOn = {};
  final Set<String> _warned = {};

  bool Function(Object? application)? get admits => _admits;
  set admits(bool Function(Object? application)? a) {
    _admits = a;
    for (final k in [..._held.keys]) {
      _release(PubKey.fromHex(k));
    }
  }

  /// Whether [pk]'s application is attested, on a connection open now, as one
  /// that counts, where it must be.
  bool isAdmitted(PubKey pk) => _admits == null || (_admittedOn[pk.hex]?.isNotEmpty ?? false);

  PresentingNetwork({
    required this.inner,
    required PersonIdentity person,
    required this.package,
    required this.hSrc,
    required this.dir,
    required this.app,
  })  : undertaking = undertakingOf(person, package, hSrc),
        invitation = invitationOf(person, package, hSrc) {
    inner.onPeerConnected = (pk, u, id) {
      final on = _admittedOn.putIfAbsent(pk.hex, () => {});
      if (_admits?.call(id) ?? false) {
        on.add(u);
        _release(pk);
      } else {
        on.remove(u);
      }
      onPeerConnected?.call(pk, u, id);
    };
    inner.onPeerDisconnected = (pk, u) {
      _admittedOn[pk.hex]?.remove(u);
      onPeerDisconnected?.call(pk, u);
    };
    inner.onPeerDiscovered = (pk, u) => onPeerDiscovered?.call(pk, u);
    inner.onMessageReceived = _received;
    try {
      final f = _file;
      if (f.existsSync()) {
        (jsonDecode(f.readAsStringSync()) as Map).forEach((k, v) {
          if (v is String && isUndertaking(v, PubKey.fromHex('$k'), package, hSrc)) received['$k'] = v;
        });
      }
    } on Object {
      // A damaged record: undertakings are asked for again.
    }
  }

  File get _file => File('$dir/$app.json');

  /// Whether [pk]'s undertaking is held here.
  bool hasPresented(PubKey pk) => received.containsKey(pk.hex);

  /// Present this person's undertaking to [pk] now, before anything else:
  /// what an invited person does first, to the inviter.
  Future<void> presentTo(PubKey pk) => _present(pk);

  /// Keep [inviter]'s [invitation], checked, beside the undertakings.
  bool keepInvitation(PubKey inviter, String invitation) {
    if (!isInvitation(invitation, inviter, package, hSrc)) return false;
    try {
      final f = File('$dir/$app-invitations.json');
      final kept = f.existsSync() ? (jsonDecode(f.readAsStringSync()) as Map) : {};
      kept[inviter.hex] = invitation;
      f.parent.createSync(recursive: true);
      f.writeAsStringSync(jsonEncode(kept));
    } on Object {
      return false; // checked, but not kept: not acted on
    }
    return true;
  }

  static Uint8List _frame(int kind, List<int> body) => Uint8List.fromList([kind, ...body]);

  Future<void> _present(PubKey pk) {
    _presentedTo.add(pk.hex);
    return inner.send(pk, _frame(_kindUndertaking, utf8.encode(undertaking)));
  }

  @override
  Future<void> send(PubKey pk, Uint8List payload) async {
    if (!_presentedTo.contains(pk.hex)) await _present(pk);
    return inner.send(pk, _frame(_kindMsg, payload));
  }

  void _received(PubKey sender, Uint8List payload, String id, Underlay u) {
    if (payload.isEmpty) {
      inner.acknowledge(id);
      return;
    }
    final body = Uint8List.sublistView(payload, 1);
    switch (payload[0]) {
      case _kindMsg:
        if (hasPresented(sender) && isAdmitted(sender)) {
          onMessageReceived?.call(sender, Uint8List.fromList(body), id, u);
        } else {
          // Held, unacknowledged, until the sender presents their undertaking
          // and, where it must be, their application is attested.
          _held.putIfAbsent(sender.hex, () => []).add((Uint8List.fromList(body), id, u));
          if (!hasPresented(sender) && _asked.add(sender.hex)) {
            unawaited(inner.send(sender, _frame(_kindAsk, const [])).catchError((Object _) {}));
          }
          if (!isAdmitted(sender) && _warned.add(sender.hex)) {
            onRefused?.call(sender, 'their app is not attested as GAS');
          }
        }
      case _kindUndertaking:
        inner.acknowledge(id);
        final String signed;
        try {
          signed = utf8.decode(body);
        } on Object {
          onRefused?.call(sender, 'an undertaking that is not one');
          return;
        }
        if (!isUndertaking(signed, sender, package, hSrc)) {
          onRefused?.call(sender, 'an undertaking that does not check');
          return;
        }
        if (received[sender.hex] != signed) {
          received[sender.hex] = signed;
          _save();
        }
        onPresented?.call(sender);
        _release(sender);
      case _kindAsk:
        inner.acknowledge(id);
        unawaited(_present(sender).catchError((Object _) {}));
      default:
        inner.acknowledge(id);
        onRefused?.call(sender, 'a message of no kind this network knows');
    }
  }

  /// The messages held from [pk], given to the agent once both hold.
  void _release(PubKey pk) {
    if (!hasPresented(pk) || !isAdmitted(pk)) return;
    for (final (b, mid, under) in _held.remove(pk.hex) ?? const <(Uint8List, String, Underlay)>[]) {
      onMessageReceived?.call(pk, b, mid, under);
    }
  }

  void _save() {
    try {
      _file.parent.createSync(recursive: true);
      final tmp = File('${_file.path}.tmp')..writeAsStringSync(jsonEncode(received));
      tmp.renameSync(_file.path);
    } on Object {
      // Kept in memory; written at the next undertaking.
    }
  }

  @override
  void acknowledge(String messageId) => inner.acknowledge(messageId);

  // --- Everything else is the network's below ---

  @override
  PubKey getIdentity() => inner.getIdentity();
  @override
  void putKnownPeer(PubKey pk) => inner.putKnownPeer(pk);
  @override
  void removeKnownPeer(PubKey pk) => inner.removeKnownPeer(pk);
  @override
  void Function(PubKey pk, Underlay underlay, Object? identity)? onPeerConnected;
  @override
  void Function(PubKey pk, Underlay underlay)? onPeerDisconnected;
  @override
  bool isPeerReachable(PubKey pk) => inner.isPeerReachable(pk);
  @override
  List<Underlay> peerUnderlays(PubKey pk) => inner.peerUnderlays(pk);
  @override
  void Function(PubKey sender, Uint8List payload, String messageId, Underlay underlay)?
      onMessageReceived;
  @override
  Uint8List sign(Uint8List message) => inner.sign(message);
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      inner.verify(signer, message, signature);
  @override
  List<DiscoveredPeer> getPeers() => inner.getPeers();
  @override
  void Function(PubKey pk, ProximityUnderlay underlay)? onPeerDiscovered;
  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) =>
      inner.setTrustLevel(underlay, level);
  @override
  void Function(String? oldAddress, String? newAddress)? get onConnectivityStatusChanged =>
      inner.onConnectivityStatusChanged;
  @override
  set onConnectivityStatusChanged(void Function(String? oldAddress, String? newAddress)? cb) =>
      inner.onConnectivityStatusChanged = cb;
  @override
  String? getPublicAddress() => inner.getPublicAddress();
  @override
  String? networkIdentity() => inner.networkIdentity();
  @override
  void Function(String? oldId, String? newId)? get onNetworkChanged => inner.onNetworkChanged;
  @override
  set onNetworkChanged(void Function(String? oldId, String? newId)? cb) =>
      inner.onNetworkChanged = cb;
  @override
  void putPeerAddress(PubKey pk, String address) => inner.putPeerAddress(pk, address);
  @override
  void putRendezvousServer(PubKey pk, String address) => inner.putRendezvousServer(pk, address);
  @override
  void removeRendezvousServer(PubKey pk) => inner.removeRendezvousServer(pk);
  @override
  String? observedPeerAddress(PubKey pk) => inner.observedPeerAddress(pk);
  @override
  void punchUdp(String address) => inner.punchUdp(address);
  @override
  Future<bool> declarePlace(String place, double radiusMetres) =>
      inner.declarePlace(place, radiusMetres);
  @override
  Future<void> removePlace(String place) => inner.removePlace(place);
  @override
  void Function(String place, PlaceEvent event)? get onPlaceEvent => inner.onPlaceEvent;
  @override
  set onPlaceEvent(void Function(String place, PlaceEvent event)? cb) => inner.onPlaceEvent = cb;
}
