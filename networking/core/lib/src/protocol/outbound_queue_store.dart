import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import '../models/underlay.dart';
import '../platform/compat.dart';

/// A message the layer accepted from `send` and holds until its recipient
/// acknowledges it: the fair-delivery queue of GNA §2.11, "Messages to
/// temporarily-unreachable peers are queued and delivered when connectivity
/// is restored; the queue survives the termination of the application."
/// Only the sender's own messages are ever held (GNA, Appendix, Rules of
/// the Code, "Local queueing, no relaying").
@immutable
class OutboundQueueEntry {
  OutboundQueueEntry({
    required this.recipientHex,
    required this.messageId,
    required Uint8List payload,
  }) : payload = Uint8List.fromList(payload);

  /// The recipient's public key, 64 hex characters.
  final String recipientHex;

  /// The message identifier `send` assigned, kept across a restart so that a
  /// replay is the same message (GNA §2.9: a re-send may deliver a message
  /// multiple times, and madGLP dedupes repeated deliveries).
  final String messageId;

  /// The byte payload passed to `send`.
  final Uint8List payload;

  Map<String, Object?> toJson() => {
        'recipient': recipientHex,
        'messageId': messageId,
        'payload': base64Encode(payload),
      };

  /// Reads an entry written by [toJson]. Every field is required: an entry
  /// that lacks one, or carries a recipient that is not a 32-byte key, is
  /// malformed and throws [FormatException] (GNA, Appendix, Rules of the
  /// Code, "No legacy or compatibility code").
  factory OutboundQueueEntry.fromJson(Object? json) {
    if (json is! Map) {
      throw const FormatException('a queued message is not a JSON object');
    }
    final recipient = json['recipient'];
    final messageId = json['messageId'];
    final payload = json['payload'];
    if (recipient is! String || !_isKeyHex(recipient)) {
      throw FormatException('a queued message has no valid recipient: '
          '$recipient');
    }
    if (messageId is! String || messageId.isEmpty) {
      throw FormatException('a queued message has no message id: $messageId');
    }
    if (payload is! String) {
      throw const FormatException('a queued message has no payload');
    }
    return OutboundQueueEntry(
      recipientHex: recipient,
      messageId: messageId,
      payload: base64Decode(payload),
    );
  }

  static final RegExp _keyHex = RegExp(r'^[0-9a-f]{64}$');
  static bool _isKeyHex(String s) => _keyHex.hasMatch(s);
}

/// A message the layer delivered to the runtime and keeps until the runtime
/// acknowledges it (GNA §2.9, `acknowledge(messageId)`: "the layer keeps a
/// delivered message until it is acknowledged, and delivers it again at
/// restart otherwise"; §2.11: "the receiving layer keeps a delivered message
/// until it is acknowledged, across the termination of the application, and
/// delivers it again when the application restarts").
@immutable
class DeliveredQueueEntry {
  DeliveredQueueEntry({
    required this.senderHex,
    required this.messageId,
    required Uint8List payload,
    required this.underlay,
  }) : payload = Uint8List.fromList(payload);

  /// The sender's public key, 64 hex characters.
  final String senderHex;

  /// The layer-assigned id the message was delivered under (GNA §2.9), and is
  /// delivered under again.
  final String messageId;

  /// The byte payload delivered.
  final Uint8List payload;

  /// The underlay the message was delivered over, which a delivery after a
  /// restart reports again.
  final Underlay underlay;

  Map<String, Object?> toJson() => {
        'sender': senderHex,
        'messageId': messageId,
        'payload': base64Encode(payload),
        'underlay': underlay.name,
      };

  /// Reads an entry written by [toJson].  Every field is required: an entry
  /// that lacks one, or carries a sender that is not a 32-byte key or an
  /// underlay that is not one of the three, is malformed and throws
  /// [FormatException] (GNA, Appendix, Rules of the Code, "No legacy or
  /// compatibility code").
  factory DeliveredQueueEntry.fromJson(Object? json) {
    if (json is! Map) {
      throw const FormatException('a kept message is not a JSON object');
    }
    final sender = json['sender'];
    final messageId = json['messageId'];
    final payload = json['payload'];
    final underlay = json['underlay'];
    if (sender is! String || !OutboundQueueEntry._isKeyHex(sender)) {
      throw FormatException('a kept message has no valid sender: $sender');
    }
    if (messageId is! String || messageId.isEmpty) {
      throw FormatException('a kept message has no message id: $messageId');
    }
    if (payload is! String) {
      throw const FormatException('a kept message has no payload');
    }
    final named = Underlay.values.where((u) => u.name == underlay);
    if (named.isEmpty) {
      throw FormatException('a kept message has no underlay: $underlay');
    }
    return DeliveredQueueEntry(
      senderHex: sender,
      messageId: messageId,
      payload: base64Decode(payload),
      underlay: named.single,
    );
  }
}

/// Encodes a queue as the JSON text the stores keep.
String encodeOutboundQueue(List<OutboundQueueEntry> entries) =>
    jsonEncode([for (final entry in entries) entry.toJson()]);

/// Decodes the JSON text [encodeOutboundQueue] wrote; throws
/// [FormatException] on anything else.
List<OutboundQueueEntry> decodeOutboundQueue(String text) {
  final decoded = jsonDecode(text);
  if (decoded is! List) {
    throw const FormatException('a persisted queue is not a JSON list');
  }
  return [for (final item in decoded) OutboundQueueEntry.fromJson(item)];
}

/// Encodes the kept delivered messages as the JSON text the stores keep.
String encodeDeliveredQueue(List<DeliveredQueueEntry> entries) =>
    jsonEncode([for (final entry in entries) entry.toJson()]);

/// Decodes the JSON text [encodeDeliveredQueue] wrote; throws
/// [FormatException] on anything else.
List<DeliveredQueueEntry> decodeDeliveredQueue(String text) {
  final decoded = jsonDecode(text);
  if (decoded is! List) {
    throw const FormatException('a persisted queue is not a JSON list');
  }
  return [for (final item in decoded) DeliveredQueueEntry.fromJson(item)];
}

/// Where the layer keeps its fair-delivery queue from one instantiation to
/// the next, so that the queue survives the termination of the application
/// (GNA §2.11).  The queue has two parts.  The layer saves a message here
/// before `send` returns and removes it once the recipient has acknowledged
/// it ([load], [save]); and it saves a message it delivers before it
/// delivers it, and removes it once the runtime has acknowledged it with
/// `acknowledge(messageId)` ([loadDelivered], [saveDelivered]; GNA §2.9).
/// It reads both back when it is instantiated again with the same identity,
/// replaying the first and delivering the second again.  A queue is one
/// agent's: it is keyed by the public key of the agent that sent or received
/// its messages, and is never replayed under another identity.
///
/// The Flutter profile keeps it in the application's SharedPreferences (the
/// Flutter package's `SharedPreferencesOutboundQueueStore`); the headless
/// profile in a file its embedder names ([FileOutboundQueueStore]), or in
/// memory ([MemoryOutboundQueueStore]), which lasts only as long as the
/// process.
abstract class OutboundQueueStore {
  /// The queue persisted for the agent whose public key is [ownerHex], in the
  /// order `send` accepted its messages; empty when none is persisted.
  Future<List<OutboundQueueEntry>> load(String ownerHex);

  /// Persist [entries] as the whole of [ownerHex]'s queue, replacing what
  /// was persisted before.
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries);

  /// The messages persisted as delivered to [ownerHex]'s runtime and not yet
  /// acknowledged, in the order they were delivered; empty when none is.
  Future<List<DeliveredQueueEntry>> loadDelivered(String ownerHex);

  /// Persist [entries] as the whole of [ownerHex]'s delivered messages,
  /// replacing what was persisted before.
  Future<void> saveDelivered(
      String ownerHex, List<DeliveredQueueEntry> entries);
}

/// A store that lasts as long as the process. One instance passed to two
/// layers in turn carries the queue from the first to the second, as a
/// restart does; it does not survive the termination of the application.
class MemoryOutboundQueueStore implements OutboundQueueStore {
  final Map<String, List<OutboundQueueEntry>> _queues = {};
  final Map<String, List<DeliveredQueueEntry>> _delivered = {};

  @override
  Future<List<OutboundQueueEntry>> load(String ownerHex) async =>
      List.of(_queues[ownerHex] ?? const []);

  @override
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries) async {
    if (entries.isEmpty) {
      _queues.remove(ownerHex);
    } else {
      _queues[ownerHex] = List.of(entries);
    }
  }

  @override
  Future<List<DeliveredQueueEntry>> loadDelivered(String ownerHex) async =>
      List.of(_delivered[ownerHex] ?? const []);

  @override
  Future<void> saveDelivered(
      String ownerHex, List<DeliveredQueueEntry> entries) async {
    if (entries.isEmpty) {
      _delivered.remove(ownerHex);
    } else {
      _delivered[ownerHex] = List.of(entries);
    }
  }
}

/// A JSON file holding one agent's queue, `{"owner": <hex>, "messages":
/// [...], "delivered": [...]}`, the messages it sent and the messages it
/// delivered: the headless profile's store, as [FileKnownPeersStore] is its
/// known-peer set's.  Writes are atomic (a temporary file, then a rename), so
/// a termination in the middle of a write leaves the previous queue whole.
///
/// The file belongs to the agent that first wrote it: loading or saving
/// another agent's queue through it throws [StateError], since a message
/// replayed under another identity would be a message that agent never sent.
class FileOutboundQueueStore implements OutboundQueueStore {
  FileOutboundQueueStore(this.path);

  final String path;

  @override
  Future<List<OutboundQueueEntry>> load(String ownerHex) async =>
      _read(ownerHex)?.messages ?? [];

  @override
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries) async =>
      _write(ownerHex, messages: entries,
          delivered: _read(ownerHex)?.delivered ?? const []);

  @override
  Future<List<DeliveredQueueEntry>> loadDelivered(String ownerHex) async =>
      _read(ownerHex)?.delivered ?? [];

  @override
  Future<void> saveDelivered(
          String ownerHex, List<DeliveredQueueEntry> entries) async =>
      _write(ownerHex,
          messages: _read(ownerHex)?.messages ?? const [], delivered: entries);

  /// The file's two parts, after checking it is [ownerHex]'s; null when there
  /// is no file.
  ({List<OutboundQueueEntry> messages, List<DeliveredQueueEntry> delivered})?
      _read(String ownerHex) {
    final file = File(path);
    if (!file.existsSync()) return null;
    final decoded = jsonDecode(file.readAsStringSync());
    if (decoded is! Map) {
      throw FormatException('$path: a persisted queue is not a JSON object');
    }
    final owner = decoded['owner'];
    final messages = decoded['messages'];
    final delivered = decoded['delivered'];
    if (owner is! String || messages is! List || delivered is! List) {
      throw FormatException('$path: a persisted queue needs an owner, its '
          'messages and its delivered messages');
    }
    if (owner != ownerHex) {
      throw StateError('$path holds the queue of ${owner.substring(0, 8)}…, '
          'not of ${ownerHex.substring(0, 8)}…');
    }
    return (
      messages: [for (final item in messages) OutboundQueueEntry.fromJson(item)],
      delivered: [
        for (final item in delivered) DeliveredQueueEntry.fromJson(item)
      ],
    );
  }

  void _write(
    String ownerHex, {
    required List<OutboundQueueEntry> messages,
    required List<DeliveredQueueEntry> delivered,
  }) {
    final file = File(path);
    file.parent.createSync(recursive: true);
    final tmp = File('$path.tmp');
    tmp.writeAsStringSync(
      jsonEncode({
        'owner': ownerHex,
        'messages': [for (final entry in messages) entry.toJson()],
        'delivered': [for (final entry in delivered) entry.toJson()],
      }),
      flush: true,
    );
    tmp.renameSync(path);
  }
}

/// The layer's side of the store: the outstanding messages in the order
/// `send` accepted them, and the delivered messages the runtime has not yet
/// acknowledged in the order they were delivered, each saved whole on every
/// change, one write after another, so that a later queue is never
/// overwritten by an earlier one.
///
/// [restored] completes once the persisted queue has been read; nothing is
/// added or saved before it, so a save never overwrites a queue not yet read.
class PersistentOutboundQueue {
  PersistentOutboundQueue({
    required OutboundQueueStore store,
    required String ownerHex,
  })  : _store = store,
        _ownerHex = ownerHex {
    restored = _restore();
    // A failed read is reported to whoever awaits [restored] (`send`, and
    // the layer's start), not as an error no one handles.
    restored.ignore();
  }

  final OutboundQueueStore _store;
  final String _ownerHex;

  /// Completes with the restored outstanding messages once the persisted
  /// queue is read, its delivered messages then in [delivered]; fails if it
  /// cannot be read.
  late final Future<List<OutboundQueueEntry>> restored;

  final Map<String, OutboundQueueEntry> _entries = {};
  final Map<String, DeliveredQueueEntry> _delivered = {};
  Future<void> _writes = Future.value();

  Future<List<OutboundQueueEntry>> _restore() async {
    final entries = await _store.load(_ownerHex);
    final delivered = await _store.loadDelivered(_ownerHex);
    for (final entry in entries) {
      _entries[entry.messageId] = entry;
    }
    for (final entry in delivered) {
      _delivered[entry.messageId] = entry;
    }
    _isRestored = true;
    return entries;
  }

  /// Whether [restored] has completed with the queue read.
  bool _isRestored = false;

  /// The outstanding messages, oldest first.
  List<OutboundQueueEntry> get entries => List.unmodifiable(_entries.values);

  /// Whether [messageId] is outstanding.
  bool contains(String messageId) => _entries.containsKey(messageId);

  /// The delivered messages the runtime has not acknowledged, oldest first.
  List<DeliveredQueueEntry> get delivered =>
      List.unmodifiable(_delivered.values);

  /// Record [entry] as outstanding and save the queue; completes once the
  /// queue holding it is saved, and fails if the save fails.
  Future<void> add(OutboundQueueEntry entry) async {
    await restored;
    _entries[entry.messageId] = entry;
    await _save();
  }

  /// [messageId] was acknowledged: drop it and save the queue.  A failed
  /// save is logged and leaves the message persisted, so a restart may
  /// deliver it again, which at-least-once delivery allows (GNA §2.9); the
  /// next save that succeeds drops it.
  Future<void> remove(String messageId) async {
    if (_entries.remove(messageId) == null) return;
    await _save().catchError((Object _) {});
  }

  /// Whether the message delivered under [messageId] is kept and the runtime
  /// has not acknowledged it.  A message whose [keep] failed is not.
  bool isKept(String messageId) => _delivered.containsKey(messageId);

  /// Keep [entry], a message about to be delivered, until the runtime
  /// acknowledges it (GNA §2.9); completes once the delivered messages
  /// holding it are saved, and fails if the save fails, the message then not
  /// kept ([isKept]), so that the sender's retransmission saves it again.  A
  /// message kept already, delivered again after a re-send, is kept once.
  Future<void> keep(DeliveredQueueEntry entry) async {
    await restored;
    final wasKept = _delivered.containsKey(entry.messageId);
    _delivered[entry.messageId] = entry;
    try {
      await _saveDelivered();
    } catch (_) {
      if (!wasKept) _delivered.remove(entry.messageId);
      rethrow;
    }
  }

  /// The runtime acknowledged [messageId] (GNA §2.9): drop it and save.  A
  /// failed save is logged and leaves the message persisted, so a restart
  /// delivers it again, which at-least-once delivery allows (GNA §2.10:
  /// "madGLP dedupes repeated deliveries"); the next save that succeeds
  /// drops it.  A message not kept does nothing.
  ///
  /// Once the queue is read, the message is dropped and its save begun in
  /// the call itself, so that a [flush] that follows waits for that save.
  Future<void> acknowledge(String messageId) {
    if (!_isRestored) return restored.then((_) => acknowledge(messageId));
    if (_delivered.remove(messageId) == null) return Future.value();
    return _saveDelivered().catchError((Object _) {});
  }

  /// Completes once every save begun so far has completed, and every save an
  /// [acknowledge] made before the queue was read begins once it is.
  Future<void> flush() async {
    if (!_isRestored) await restored.then((_) {}, onError: (Object _) {});
    await _writes;
  }

  Future<void> _save() {
    final snapshot = List<OutboundQueueEntry>.of(_entries.values);
    return _chain(() => _store.save(_ownerHex, snapshot));
  }

  Future<void> _saveDelivered() {
    final snapshot = List<DeliveredQueueEntry>.of(_delivered.values);
    return _chain(() => _store.saveDelivered(_ownerHex, snapshot));
  }

  Future<void> _chain(Future<void> Function() write) {
    final next = _writes.then((_) => write());
    _writes = next.catchError((Object e) {
      debugPrint('[queue] Failed to save the queue: $e');
    });
    return next;
  }
}
