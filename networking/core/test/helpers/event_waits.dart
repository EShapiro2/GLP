/// Waiting on the layer's events, with no clock: a test waits on the event it
/// tests, and the suite's watchdog bounds a hang (networking/CLAUDE.md, rule
/// 4).
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:redux/redux.dart';

/// Completes once [holds] is true: checked now, and again after every
/// reachability event of each of [layers] (`onPeerConnected`,
/// `onPeerDisconnected`) and every message delivered to a layer whose test
/// set `onMessageReceived`.  While it waits, the layers' callbacks are
/// wrapped, each calling the test's own first; they are restored once
/// [holds] is true.  A layer whose test set no `onMessageReceived` keeps
/// none, so that the kept messages a callback would be handed at once are
/// not handed to the wait.
Future<void> until(
  List<HeadlessGrassrootsNetwork> layers,
  bool Function() holds,
) async {
  if (holds()) return;
  final done = Completer<void>();
  void check() {
    if (!done.isCompleted && holds()) done.complete();
  }

  final restore = <void Function()>[];
  for (final layer in layers) {
    final connected = layer.onPeerConnected;
    final disconnected = layer.onPeerDisconnected;
    final received = layer.onMessageReceived;
    layer.onPeerConnected = (pk, underlay, application) {
      connected?.call(pk, underlay, application);
      check();
    };
    layer.onPeerDisconnected = (pk, underlay) {
      disconnected?.call(pk, underlay);
      check();
    };
    if (received != null) {
      layer.onMessageReceived = (Uint8List sender, Uint8List payload,
          String id, String senderMessageId, Underlay u) {
        received(sender, payload, id, senderMessageId, u);
        check();
      };
    }
    restore.add(() {
      layer.onPeerConnected = connected;
      layer.onPeerDisconnected = disconnected;
      if (received != null) layer.onMessageReceived = received;
    });
  }
  await done.future;
  for (final undo in restore) {
    undo();
  }
}

/// Completes once [a] and [b] each hold the other reachable.
Future<void> untilBothReachable(
        HeadlessGrassrootsNetwork a, HeadlessGrassrootsNetwork b) =>
    until([a, b], () =>
        a.isPeerReachable(b.getIdentity()) &&
        b.isPeerReachable(a.getIdentity()));

/// A queue store that tells each save of a queue: [inner] holds the queue,
/// and [untilSaved] waits for the save the test waits on.
class WatchedQueueStore implements OutboundQueueStore {
  WatchedQueueStore(this.inner);

  final OutboundQueueStore inner;

  /// The queue each owner last saved, once the save completed.
  final Map<String, List<OutboundQueueEntry>> _lastSaved = {};
  final List<(String, bool Function(List<OutboundQueueEntry>), Completer<void>)>
      _waits = [];

  /// Completes once a save of [ownerHex]'s queue has completed whose entries
  /// [holds] is true of, the last save already completed included.
  Future<void> untilSaved(
      String ownerHex, bool Function(List<OutboundQueueEntry>) holds) {
    final last = _lastSaved[ownerHex];
    if (last != null && holds(last)) return Future.value();
    final done = Completer<void>();
    _waits.add((ownerHex, holds, done));
    return done.future;
  }

  @override
  Future<List<OutboundQueueEntry>> load(String ownerHex) =>
      inner.load(ownerHex);

  @override
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries) async {
    final saved = List.of(entries);
    await inner.save(ownerHex, entries);
    _lastSaved[ownerHex] = saved;
    for (final wait in _waits.toList()) {
      if (wait.$1 == ownerHex && wait.$2(saved)) {
        _waits.remove(wait);
        wait.$3.complete();
      }
    }
  }

  @override
  Future<List<DeliveredQueueEntry>> loadDelivered(String ownerHex) =>
      inner.loadDelivered(ownerHex);

  @override
  Future<void> saveDelivered(
          String ownerHex, List<DeliveredQueueEntry> entries) =>
      inner.saveDelivered(ownerHex, entries);
}

/// Completes once [holds] is true of [store]'s state: checked now, and at
/// each change of the store.
Future<void> stateWhere<S>(Store<S> store, bool Function(S state) holds) async {
  if (holds(store.state)) return;
  await store.onChange.firstWhere(holds);
}
