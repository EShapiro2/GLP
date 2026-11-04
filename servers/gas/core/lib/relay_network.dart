// The network the phone's agent runs over: GNA's layer for the people near the
// phone, and talmonlab's relay (gasrelay) for the people who are not.
//
// It wraps the layer behind the same interface the runtime talks to
// (GlpNetwork) and changes nothing of the layer.  A message to a person the
// layer reaches now goes through the layer; any other message, while the relay
// is on, goes through the relay; with the relay off, everything goes through
// the layer, which holds what it cannot deliver yet.
//
// Over the relay each message is encrypted end to end (libsodium crypto_box
// between the two persons' keys, converted to X25519), so the relay sees who
// writes to whom, when, and how much, and nothing else.  Every request to the
// relay is signed with the person's Ed25519 key.  Messages are held by the
// relay until the recipient's runtime acknowledges them: at least once, as
// the layer delivers.
//
// Inside the encryption each message carries the app it is for and the time
// it was sent; the recipient drops a message for another app than the relay
// says, one older than the relay keeps messages, and one it has applied
// before (by its nonce, which the relay cannot change without breaking the
// encryption).  So the relay can drop, delay or replay what it holds, and a
// replay is never applied twice, after a restart too.
//
// People met over the relay are kept in relay/peers.json; messages not yet
// accepted by the relay in relay/<app>-outbox.json; the messages applied, for
// as long as the relay could replay them, in relay/seen.json.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:sodium/sodium_sumo.dart';

/// The relay GAS phones use.
const defaultRelay = 'https://talmonlab.bgu.ac.il/gasrelay';

/// The link a person shares to be added from afar: their key after the '#',
/// and the contract (its hash) they share it from.  contracts.dart's GasLink
/// reads it.
String inviteLink(PubKey pk, String contractHash) =>
    'https://talmonlab.bgu.ac.il/gas/add.html#${pk.hex}&c=$contractHash';

String _hex(List<int> b) => b.map((x) => x.toRadixString(16).padLeft(2, '0')).join();

const _kindMsg = 0, _kindHello = 1;
const _version = 1;

/// How long the relay holds a message (gasrelay's ttl), and a day's margin.
const _maxAge = Duration(days: 8);
const _control = '_';

class RelayGlpNetwork implements GlpNetwork {
  final GlpNetwork inner;
  final SodiumSumo sodium;
  final Uri relay;

  /// This app's name at the relay: messages are held per app, so that one
  /// contract's messages are not delivered to another's agent.
  final String app;
  final String dir;

  late final SecureKey _signKey, _boxKey;
  final _rnd = Random.secure();
  final Set<String> _peers = {};
  final List<Map<String, Object?>> _outbox = [];
  final Set<String> _delivered = {};
  final Map<String, int> _seen = {}; // from:nonce -> sent, ms
  final Map<String, (String, int)> _applying = {}; // messageId -> its seen entry
  Timer? _seenTimer;
  final List<List<String>> _acks = [];
  int _after = 0;
  bool _on = false, _running = false, _pumping = false;
  Timer? _ackTimer;
  HttpClient _http = _client();

  static HttpClient _client() => HttpClient()
    ..connectionTimeout = const Duration(seconds: 15)
    ..findProxy = HttpClient.findProxyFromEnvironment;

  /// For testing the relay between devices that are near each other: every
  /// message goes through the relay, none through the layer.
  bool relayOnly = false;

  /// What the relay is doing, for the screen: 'off', 'connecting', 'online',
  /// or `offline: <why>`.
  String state = 'off';

  /// Someone added this person from afar (their first message over the relay).
  void Function(PubKey pk)? onAddedBy;

  /// The relay's [state] changed.
  void Function()? onState;

  RelayGlpNetwork({
    required this.inner,
    required this.sodium,
    required Uint8List privateKey,
    required this.app,
    required this.dir,
    String relayUrl = defaultRelay,
  }) : relay = Uri.parse(relayUrl.endsWith('/') ? relayUrl : '$relayUrl/') {
    _signKey = SecureKey.fromList(sodium, privateKey);
    _boxKey = sodium.crypto.sign.skToCurve25519(_signKey);
    inner.onPeerConnected = (pk, u, id) => onPeerConnected?.call(pk, u, id);
    inner.onPeerDisconnected = (pk, u) {
      onPeerDisconnected?.call(pk, u);
      if (_relayPeer(pk)) onPeerConnected?.call(pk, Underlay.wan, null);
    };
    inner.onMessageReceived = (s, p, id, u) => onMessageReceived?.call(s, p, id, u);
    inner.onPeerDiscovered = (pk, u) => onPeerDiscovered?.call(pk, u);
    // The WAN's and the places' callbacks are the layer's own, set on it only
    // when the runtime sets them: a layer with no place backend refuses an
    // onPlaceEvent.
    _load();
  }

  File get _peersFile => File('$dir/peers.json');
  File get _outboxFile => File('$dir/$app-outbox.json');
  File get _seenFile => File('$dir/seen.json');

  void _load() {
    try {
      if (_peersFile.existsSync()) {
        _peers.addAll([for (final k in jsonDecode(_peersFile.readAsStringSync()) as List) '$k']);
      }
      if (_seenFile.existsSync()) {
        final cut = DateTime.now().subtract(_maxAge).millisecondsSinceEpoch;
        (jsonDecode(_seenFile.readAsStringSync()) as Map).forEach((k, v) {
          if (v is int && v > cut) _seen['$k'] = v;
        });
      }
      if (_outboxFile.existsSync()) {
        _outbox.addAll([
          for (final m in jsonDecode(_outboxFile.readAsStringSync()) as List)
            (m as Map).cast<String, Object?>()
        ]);
      }
    } on Object {
      // A file that cannot be read starts empty.
    }
  }

  Future<void> _write(File f, Object j) async {
    await f.parent.create(recursive: true);
    final tmp = File('${f.path}.tmp');
    await tmp.writeAsString(jsonEncode(j), flush: true);
    await tmp.rename(f.path);
  }

  // --- The relay's people ---

  /// The people met over the relay, by key.
  List<PubKey> get relayPeers => [for (final k in _peers) PubKey.fromHex(k)];

  bool get on => _on;

  bool _relayPeer(PubKey pk) => _on && _peers.contains(pk.hex);

  /// Add [pk] from afar (an invite link): remember them, tell their phone.
  Future<void> addPerson(PubKey pk) async {
    if (pk == getIdentity()) return;
    final fresh = _peers.add(pk.hex);
    if (fresh) await _write(_peersFile, _peers.toList());
    inner.putKnownPeer(pk);
    if (_on) onPeerConnected?.call(pk, Underlay.wan, null);
    await _enqueue(pk, _kindHello, Uint8List(0), _control);
  }

  Future<void> _met(PubKey pk) async {
    if (!_peers.add(pk.hex)) return;
    await _write(_peersFile, _peers.toList());
    inner.putKnownPeer(pk);
    onPeerConnected?.call(pk, Underlay.wan, null);
    onAddedBy?.call(pk);
  }

  // --- On and off ---

  /// Start or stop using the relay.
  void setOn(bool on) {
    if (on == _on) return;
    _on = on;
    if (on) {
      _http = _client();
      _setState('connecting');
      for (final pk in relayPeers) {
        if (!inner.isPeerReachable(pk)) onPeerConnected?.call(pk, Underlay.wan, null);
      }
      unawaited(_poll());
      unawaited(_pump());
    } else {
      _http.close(force: true);
      _setState('off');
      for (final pk in relayPeers) {
        if (!inner.isPeerReachable(pk)) onPeerDisconnected?.call(pk, Underlay.wan);
      }
    }
  }

  void _setState(String s) {
    if (s == state) return;
    state = s;
    onState?.call();
  }

  // --- Sending ---

  @override
  Future<void> send(PubKey pk, Uint8List payload) {
    if (!_on || (!relayOnly && inner.isPeerReachable(pk))) return inner.send(pk, payload);
    return _enqueue(pk, _kindMsg, payload, app);
  }

  Future<void> _enqueue(PubKey to, int kind, Uint8List payload, String app) async {
    final nonce = sodium.randombytes.buf(sodium.crypto.box.nonceBytes);
    final box = sodium.crypto.box.easy(
      message: _plain(kind, app, payload),
      nonce: nonce,
      publicKey: sodium.crypto.sign.pkToCurve25519(to.bytes),
      secretKey: _boxKey,
    );
    _outbox.add({
      'to': to.hex,
      'app': app,
      'id': '${DateTime.now().microsecondsSinceEpoch.toRadixString(36)}${_rnd.nextInt(1 << 32).toRadixString(36)}',
      'nonce': base64.encode(nonce),
      'box': base64.encode(box),
    });
    await _write(_outboxFile, _outbox);
    unawaited(_pump());
  }

  /// version, kind, sent (8 bytes), app (length and bytes), payload.
  static Uint8List _plain(int kind, String app, Uint8List payload) {
    final a = utf8.encode(app);
    final head = ByteData(11)
      ..setUint8(0, _version)
      ..setUint8(1, kind)
      ..setInt64(2, DateTime.now().millisecondsSinceEpoch)
      ..setUint8(10, a.length);
    return Uint8List.fromList([...head.buffer.asUint8List(), ...a, ...payload]);
  }

  Future<void> _pump() async {
    if (_pumping) return;
    _pumping = true;
    var backoff = 1;
    try {
      while (_on && _outbox.isNotEmpty) {
        final batch = _outbox.take(50).toList();
        try {
          await _call('send', {'msgs': batch});
          _outbox.removeRange(0, batch.length);
          await _write(_outboxFile, _outbox);
          backoff = 1;
        } on Object catch (e) {
          _setState('offline: $e');
          await Future<void>.delayed(Duration(seconds: backoff));
          backoff = min(backoff * 2, 30);
        }
      }
    } finally {
      _pumping = false;
    }
  }

  // --- Receiving ---

  Future<void> _poll() async {
    if (_running) return;
    _running = true;
    var backoff = 1, failures = 0;
    try {
      while (_on) {
        try {
          final out = await _call('inbox', {
            'apps': [app, _control],
            'after': _after,
            'wait': 25,
          });
          _setState('online');
          backoff = 1;
          failures = 0;
          for (final m in (out['msgs'] as List).cast<Map>()) {
            _after = max(_after, m['seq'] as int);
            await _receive(m);
          }
        } on Object catch (e) {
          if (!_on) break;
          // One dropped long poll (a proxy or a network closing an idle
          // connection) is retried at once; offline is two in a row.
          if (++failures == 1) continue;
          _setState('offline: $e');
          await Future<void>.delayed(Duration(seconds: backoff));
          backoff = min(backoff * 2, 30);
        }
      }
    } finally {
      _running = false;
    }
  }

  Future<void> _receive(Map m) async {
    final from = m['from'] as String, app = m['app'] as String, id = m['id'] as String;
    final tag = '$app:$from:$id';
    if (!_delivered.add(tag)) return;
    final pk = PubKey.fromHex(from);
    Uint8List plain;
    try {
      plain = sodium.crypto.box.openEasy(
        cipherText: base64.decode(m['box'] as String),
        nonce: base64.decode(m['nonce'] as String),
        publicKey: sodium.crypto.sign.pkToCurve25519(pk.bytes),
        secretKey: _boxKey,
      );
    } on Object {
      _ack([app, from, id]); // not from who it says, or damaged: dropped
      return;
    }
    // Dropped (and removed from the relay): another format, another app than
    // the relay says, too old or from the future, or applied before.
    final now = DateTime.now().millisecondsSinceEpoch;
    if (plain.length < 11 || plain[0] != _version) return _ack([app, from, id]);
    final head = ByteData.sublistView(plain, 0, 11);
    final kind = head.getUint8(1), sent = head.getInt64(2), alen = head.getUint8(10);
    if (plain.length < 11 + alen) return _ack([app, from, id]);
    final inner = utf8.decode(plain.sublist(11, 11 + alen), allowMalformed: true);
    final seenKey = '$from:${m['nonce']}';
    if (inner != app ||
        sent < now - _maxAge.inMilliseconds ||
        sent > now + const Duration(days: 1).inMilliseconds ||
        _seen.containsKey(seenKey)) {
      return _ack([app, from, id]);
    }
    await _met(pk);
    if (kind == _kindMsg && app == this.app) {
      final messageId = 'relay:$tag';
      _applying[messageId] = (seenKey, sent);
      onMessageReceived?.call(pk, Uint8List.sublistView(plain, 11 + alen), messageId, Underlay.wan);
    } else {
      _markSeen(seenKey, sent);
      _ack([app, from, id]);
    }
  }

  /// [key] applied: never applied again while the relay could replay it.
  void _markSeen(String key, int sent) {
    _seen[key] = sent;
    _seenTimer ??= Timer(const Duration(milliseconds: 500), () async {
      _seenTimer = null;
      final cut = DateTime.now().subtract(_maxAge).millisecondsSinceEpoch;
      _seen.removeWhere((_, t) => t < cut);
      await _write(_seenFile, _seen);
    });
  }

  @override
  void acknowledge(String messageId) {
    if (!messageId.startsWith('relay:')) return inner.acknowledge(messageId);
    final p = messageId.substring(6).split(':');
    final seen = _applying.remove(messageId);
    if (seen != null) _markSeen(seen.$1, seen.$2);
    _ack([p[0], p[1], p[2]]);
  }

  void _ack(List<String> a) {
    _acks.add(a);
    _ackTimer ??= Timer(const Duration(milliseconds: 300), () async {
      _ackTimer = null;
      final batch = List.of(_acks);
      _acks.clear();
      try {
        await _call('ack', {'acks': batch});
      } on Object {
        // Not acknowledged: the relay delivers them again, and they are
        // dropped here as delivered, or redelivered after a restart.
      }
    });
  }

  // --- The relay's protocol ---

  Future<Map> _call(String path, Map body) async {
    final b = jsonEncode(body);
    final key = getIdentity().hex;
    final t = DateTime.now().millisecondsSinceEpoch;
    final sig = sodium.crypto.sign.detached(
      message: Uint8List.fromList(utf8.encode('gasrelay1\n/$path\n$key\n$t\n$b')),
      secretKey: _signKey,
    );
    final req = await _http.postUrl(relay.resolve(path));
    req.headers.contentType = ContentType.json;
    req.write(jsonEncode({'key': key, 't': t, 'b': b, 'sig': _hex(sig)}));
    final res = await req.close().timeout(const Duration(seconds: 40));
    final text = await utf8.decodeStream(res);
    final out = jsonDecode(text);
    if (res.statusCode != 200 || out is! Map) {
      throw out is Map ? '${out['error']}' : 'relay answered ${res.statusCode}';
    }
    return out;
  }

  // --- Everything else is the layer's ---

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
  bool isPeerReachable(PubKey pk) => inner.isPeerReachable(pk) || _relayPeer(pk);
  @override
  List<Underlay> peerUnderlays(PubKey pk) =>
      [...inner.peerUnderlays(pk), if (_relayPeer(pk)) Underlay.wan];
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

  void dispose() {
    _on = false;
    _ackTimer?.cancel();
    if (_seenTimer != null) {
      _seenTimer!.cancel();
      _seenTimer = null;
      _seenFile.parent.createSync(recursive: true);
      _seenFile.writeAsStringSync(jsonEncode(_seen));
    }
    _http.close(force: true);
  }
}
