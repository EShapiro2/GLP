/// A [GlpNetwork] across isolates: the agent's end, [PortGlpNetwork], and the
/// layer's end, [LayerPort].
///
/// "Each agent runs in its own isolate" (IGLP, Implementation Notes,
/// "Event-driven execution"), and on a phone the layer cannot run there: its
/// platform services report through channels, and a background isolate takes
/// no message from the host platform ("Messages from the host platform always
/// go to the root isolate", Flutter's BackgroundIsolateBinaryMessenger).  So
/// the application holds the layer in its root isolate, behind a [LayerPort],
/// and the agent's isolate runs over a [PortGlpNetwork], one more realisation
/// of the one interface (One interface: "transport integration is a backend
/// swap").  Every function passes through to the layer, and nothing is
/// interpreted.
///
/// The interface's calls that return at once in the agent's isolate cannot
/// wait for a port:
///
/// - `getIdentity`, `sign` and `verify` are answered by this end, which holds
///   the agent's key pair, the pair the layer was instantiated with (The
///   contract), Ed25519 being deterministic.
/// - What the layer reports of its peers and its addresses, `isPeerReachable`,
///   `peerUnderlays`, `getPeers`, `observedPeerAddress`, `getPublicAddress`
///   and `networkIdentity`, is answered from a [LayerMirror], read from the
///   layer and sent with each of its events, each of which comes before the
///   callback it goes with.
/// - The functions of a WAN the layer does not run throw [UnsupportedError]
///   here, as the layer does there ([LayerMirror.noWan]).
///
/// `send`, `declarePlace` and `removePlace` return futures, and wait for the
/// layer's answer over the port: "the runtime waits for its answer" (The
/// contract).
///
/// An event comes before the agent is bound, where the layer runs before the
/// agent starts or restarts: a message, a connection, a disconnection or a
/// place event is held until its callback is set, so no message the layer
/// delivers finds no agent; the layer's other reports are dropped where no
/// callback is set, as a layer drops them.
library;

import 'dart:async';
import 'dart:isolate';
import 'dart:typed_data';

import 'agent_state_store.dart' show AgentKeyPair;
import 'glp_network.dart';
import 'identity.dart';

/// What the layer reports of its peers and its addresses, read from it.
class LayerMirror {
  final Map<PubKey, List<Underlay>> underlays;
  final Map<PubKey, String> observed;
  final List<DiscoveredPeer> peers;
  final String? publicAddress;
  final String? networkIdentity;

  /// Why the layer has no WAN functions, as its [UnsupportedError] says, or
  /// null where it has them.
  final String? noWan;

  LayerMirror(this.underlays, this.observed, this.peers, this.publicAddress,
      this.networkIdentity, this.noWan);
}

// The agent's calls to the layer.

/// The agent's end is up, at this port.
class _Attach {
  final SendPort events;
  _Attach(this.events);
}

/// A layer function the agent calls.
enum _Function {
  putKnownPeer,
  removeKnownPeer,
  acknowledge,
  setTrustLevel,
  putPeerAddress,
  putRendezvousServer,
  removeRendezvousServer,
  punchUdp,
  send,
  declarePlace,
  removePlace,
}

/// A call of [function] on the layer; where [id] is given, the agent waits
/// for its answer ([_Answer]).
class _Call {
  final int? id;
  final _Function function;
  final List<Object> args;
  _Call(this.function, this.args, {this.id});
}

// The layer's messages to the agent.

/// The answer to [_Ask] [id]: [value], or the [refusal] of the layer.
class _Answer {
  final int id;
  final Object? value;
  final String? refusal;
  _Answer(this.id, this.value, this.refusal);
}

/// An event of the layer, with the [mirror] read at the event.
sealed class _Event {
  LayerMirror mirror;
  _Event(this.mirror);

  /// Give the event to [net]'s callback; false where it is to wait for one.
  bool deliver(PortGlpNetwork net);
}

class _Connected extends _Event {
  final PubKey pk;
  final Underlay underlay;
  final Object? identity;
  _Connected(super.mirror, this.pk, this.underlay, this.identity);

  @override
  bool deliver(PortGlpNetwork net) {
    final cb = net.onPeerConnected;
    cb?.call(pk, underlay, identity);
    return cb != null;
  }
}

class _Disconnected extends _Event {
  final PubKey pk;
  final Underlay underlay;
  _Disconnected(super.mirror, this.pk, this.underlay);

  @override
  bool deliver(PortGlpNetwork net) {
    final cb = net.onPeerDisconnected;
    cb?.call(pk, underlay);
    return cb != null;
  }
}

class _Received extends _Event {
  final PubKey sender;
  final Uint8List payload;
  final String messageId;
  final Underlay underlay;
  _Received(super.mirror, this.sender, this.payload, this.messageId,
      this.underlay);

  @override
  bool deliver(PortGlpNetwork net) {
    final cb = net.onMessageReceived;
    cb?.call(sender, payload, messageId, underlay);
    return cb != null;
  }
}

class _Place extends _Event {
  final String place;
  final PlaceEvent event;
  _Place(super.mirror, this.place, this.event);

  @override
  bool deliver(PortGlpNetwork net) {
    final cb = net.onPlaceEvent;
    cb?.call(place, event);
    return cb != null;
  }
}

class _Discovered extends _Event {
  final PubKey pk;
  final ProximityUnderlay underlay;
  _Discovered(super.mirror, this.pk, this.underlay);

  @override
  bool deliver(PortGlpNetwork net) {
    net.onPeerDiscovered?.call(pk, underlay);
    return true;
  }
}

class _Connectivity extends _Event {
  final String? oldAddress;
  final String? newAddress;
  _Connectivity(super.mirror, this.oldAddress, this.newAddress);

  @override
  bool deliver(PortGlpNetwork net) {
    net.onConnectivityStatusChanged?.call(oldAddress, newAddress);
    return true;
  }
}

class _NetworkChanged extends _Event {
  final String? oldId;
  final String? newId;
  _NetworkChanged(super.mirror, this.oldId, this.newId);

  @override
  bool deliver(PortGlpNetwork net) {
    net.onNetworkChanged?.call(oldId, newId);
    return true;
  }
}

/// The layer's end: [layer]'s callbacks taken and sent to the agent's end,
/// and the agent's calls run on [layer].  Made before the layer starts, so
/// that it misses none of the layer's events; those that come before the
/// agent's end attaches are held for it.
class LayerPort {
  final GlpNetwork layer;
  final ReceivePort _calls = ReceivePort();
  final Set<PubKey> _connected = {};
  final List<Object> _unsent = [];
  SendPort? _agent;

  LayerPort(this.layer) {
    _calls.listen(_take);
    layer.onPeerConnected = (pk, underlay, identity) {
      _connected.add(pk);
      _post(_Connected(_mirror(), pk, underlay, identity));
    };
    layer.onPeerDisconnected = (pk, underlay) {
      final mirror = _mirror();
      if (!layer.isPeerReachable(pk)) _connected.remove(pk);
      _post(_Disconnected(mirror, pk, underlay));
    };
    layer.onMessageReceived = (sender, payload, messageId, underlay) =>
        _post(_Received(_mirror(), sender, payload, messageId, underlay));
    layer.onPeerDiscovered = (pk, underlay) =>
        _post(_Discovered(_mirror(), pk, underlay));
    layer.onConnectivityStatusChanged = (oldAddress, newAddress) =>
        _post(_Connectivity(_mirror(), oldAddress, newAddress));
    layer.onNetworkChanged =
        (oldId, newId) => _post(_NetworkChanged(_mirror(), oldId, newId));
    layer.onPlaceEvent =
        (place, event) => _post(_Place(_mirror(), place, event));
  }

  /// The port the agent's end calls the layer on.
  SendPort get sendPort => _calls.sendPort;

  /// The agent's end is gone; [layer]'s disposal is the application's.
  void close() => _calls.close();

  LayerMirror _mirror() {
    String? noWan;
    String? publicAddress;
    String? networkIdentity;
    try {
      publicAddress = layer.getPublicAddress();
      networkIdentity = layer.networkIdentity();
    } on UnsupportedError catch (e) {
      noWan = '${e.message}';
    }
    return LayerMirror(
      {for (final pk in _connected) pk: layer.peerUnderlays(pk)},
      noWan != null
          ? const {}
          : {
              for (final pk in _connected)
                if (layer.observedPeerAddress(pk) case final address?)
                  pk: address,
            },
      layer.getPeers(),
      publicAddress,
      networkIdentity,
      noWan,
    );
  }

  void _post(Object event) {
    final agent = _agent;
    if (agent == null) {
      _unsent.add(event);
    } else {
      agent.send(event);
    }
  }

  void _take(Object? call) {
    switch (call) {
      case _Attach(:final events):
        // The mirror the agent's end starts from comes in its own right: the
        // layer may report nothing before the agent asks.
        events.send(_mirror());
        _agent = events;
        _unsent
          ..forEach(events.send)
          ..clear();
      case _Call(:final id?, :final function, :final args):
        _answer(function, args).then(
          (value) => _agent?.send(_Answer(id, value, null)),
          onError: (Object e) => _agent?.send(_Answer(id, null, '$e')),
        );
      case _Call(:final function, :final args):
        _answer(function, args);
    }
  }

  /// Run the layer's [function] on [a]: its answer, null for a function that
  /// has none.
  Future<Object?> _answer(_Function function, List<Object> a) async {
    switch (function) {
      case _Function.putKnownPeer:
        layer.putKnownPeer(a[0] as PubKey);
      case _Function.removeKnownPeer:
        layer.removeKnownPeer(a[0] as PubKey);
      case _Function.acknowledge:
        layer.acknowledge(a[0] as String);
      case _Function.setTrustLevel:
        layer.setTrustLevel(a[0] as ProximityUnderlay, a[1] as TrustLevel);
      case _Function.putPeerAddress:
        layer.putPeerAddress(a[0] as PubKey, a[1] as String);
      case _Function.putRendezvousServer:
        layer.putRendezvousServer(a[0] as PubKey, a[1] as String);
      case _Function.removeRendezvousServer:
        layer.removeRendezvousServer(a[0] as PubKey);
      case _Function.punchUdp:
        layer.punchUdp(a[0] as String);
      case _Function.send:
        await layer.send(a[0] as PubKey, a[1] as Uint8List);
      case _Function.declarePlace:
        return layer.declarePlace(a[0] as String, a[1] as double);
      case _Function.removePlace:
        await layer.removePlace(a[0] as String);
    }
    return null;
  }
}

/// The agent's end: [GlpNetwork] over a [LayerPort], in the agent's isolate.
class PortGlpNetwork implements GlpNetwork {
  final SendPort _layer;
  final PersonIdentity _identity;
  final ReceivePort _events = ReceivePort();
  final Map<int, Completer<Object?>> _asked = {};
  final List<_Event> _held = [];
  bool _delivering = false;
  int _nextAsk = 0;
  LayerMirror _mirror =
      LayerMirror(const {}, const {}, const [], null, null, null);

  PortGlpNetwork(this._layer, AgentKeyPair keyPair)
      : _identity = PersonIdentity(keyPair.pub, keyPair.priv) {
    _events.listen(_take);
    _layer.send(_Attach(_events.sendPort));
  }

  /// The agent is gone: its end of the port closes.
  void close() => _events.close();

  void _take(Object? message) {
    switch (message) {
      case LayerMirror():
        _mirror = message;
      case _Answer(:final id, :final value, :final refusal):
        final asked = _asked.remove(id)!;
        if (refusal == null) {
          asked.complete(value);
        } else {
          asked.completeError(StateError(refusal));
        }
      case _Event():
        _mirror = message.mirror;
        _held.add(message);
        _deliver();
    }
  }

  /// Give each held event to its callback, in order of arrival.
  void _deliver() {
    if (_delivering) return;
    _delivering = true;
    var i = 0;
    while (i < _held.length) {
      if (_held[i].deliver(this)) {
        _held.removeAt(i);
      } else {
        i++;
      }
    }
    _delivering = false;
  }

  void _call(_Function function, List<Object> args) =>
      _layer.send(_Call(function, args));

  Future<Object?> _ask(_Function function, List<Object> args) {
    final id = _nextAsk++;
    final asked = Completer<Object?>();
    _asked[id] = asked;
    _layer.send(_Call(function, args, id: id));
    return asked.future;
  }

  void _wan() {
    final why = _mirror.noWan;
    if (why != null) throw UnsupportedError(why);
  }

  // --- Identity ---

  @override
  PubKey getIdentity() => _identity.pub;

  // --- Known peers ---

  @override
  void putKnownPeer(PubKey pk) => _call(_Function.putKnownPeer, [pk]);

  @override
  void removeKnownPeer(PubKey pk) => _call(_Function.removeKnownPeer, [pk]);

  // --- Connection and reachability ---

  void Function(PubKey pk, Underlay underlay, Object? identity)?
      _onPeerConnected;

  @override
  void Function(PubKey pk, Underlay underlay, Object? identity)?
      get onPeerConnected => _onPeerConnected;

  @override
  set onPeerConnected(
      void Function(PubKey pk, Underlay underlay, Object? identity)? cb) {
    _onPeerConnected = cb;
    _deliver();
  }

  void Function(PubKey pk, Underlay underlay)? _onPeerDisconnected;

  @override
  void Function(PubKey pk, Underlay underlay)? get onPeerDisconnected =>
      _onPeerDisconnected;

  @override
  set onPeerDisconnected(void Function(PubKey pk, Underlay underlay)? cb) {
    _onPeerDisconnected = cb;
    _deliver();
  }

  @override
  bool isPeerReachable(PubKey pk) => peerUnderlays(pk).isNotEmpty;

  @override
  List<Underlay> peerUnderlays(PubKey pk) => _mirror.underlays[pk] ?? const [];

  // --- Point-to-point communication ---

  @override
  Future<void> send(PubKey pk, Uint8List payload) =>
      _ask(_Function.send, [pk, payload]);

  void Function(PubKey sender, Uint8List payload, String messageId,
      Underlay underlay)? _onMessageReceived;

  @override
  void Function(PubKey sender, Uint8List payload, String messageId,
      Underlay underlay)? get onMessageReceived => _onMessageReceived;

  @override
  set onMessageReceived(
      void Function(PubKey sender, Uint8List payload, String messageId,
              Underlay underlay)?
          cb) {
    _onMessageReceived = cb;
    _deliver();
  }

  @override
  void acknowledge(String messageId) =>
      _call(_Function.acknowledge, [messageId]);

  // --- Cryptography ---

  @override
  Uint8List sign(Uint8List message) => _identity.sign(message);

  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      PersonIdentity.verify(signer, message, signature);

  // --- Proximity underlays ---

  @override
  List<DiscoveredPeer> getPeers() => _mirror.peers;

  @override
  void Function(PubKey pk, ProximityUnderlay underlay)? onPeerDiscovered;

  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) =>
      _call(_Function.setTrustLevel, [underlay, level]);

  // --- WAN ---

  @override
  void Function(String? oldAddress, String? newAddress)?
      onConnectivityStatusChanged;

  @override
  String? getPublicAddress() {
    _wan();
    return _mirror.publicAddress;
  }

  @override
  String? networkIdentity() {
    _wan();
    return _mirror.networkIdentity;
  }

  @override
  void Function(String? oldId, String? newId)? onNetworkChanged;

  @override
  void putPeerAddress(PubKey pk, String address) {
    _wan();
    _call(_Function.putPeerAddress, [pk, address]);
  }

  @override
  void putRendezvousServer(PubKey pk, String address) {
    _wan();
    _call(_Function.putRendezvousServer, [pk, address]);
  }

  @override
  void removeRendezvousServer(PubKey pk) {
    _wan();
    _call(_Function.removeRendezvousServer, [pk]);
  }

  @override
  String? observedPeerAddress(PubKey pk) {
    _wan();
    return _mirror.observed[pk];
  }

  @override
  void punchUdp(String address) {
    _wan();
    _call(_Function.punchUdp, [address]);
  }

  // --- Places ---

  @override
  Future<bool> declarePlace(String place, double radiusMetres) async =>
      await _ask(_Function.declarePlace, [place, radiusMetres]) as bool;

  @override
  Future<void> removePlace(String place) =>
      _ask(_Function.removePlace, [place]);

  void Function(String place, PlaceEvent event)? _onPlaceEvent;

  @override
  void Function(String place, PlaceEvent event)? get onPlaceEvent =>
      _onPlaceEvent;

  @override
  set onPlaceEvent(void Function(String place, PlaceEvent event)? cb) {
    _onPlaceEvent = cb;
    _deliver();
  }
}
