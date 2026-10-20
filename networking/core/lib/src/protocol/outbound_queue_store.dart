import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import '../platform/compat.dart';

/// A message the layer accepted from `send` and holds until its recipient
/// acknowledges it: the fair-delivery queue of GNA §2.10, "Messages to
/// temporarily-unreachable peers are queued and delivered when connectivity
/// is restored; the queue survives the termination of the application."
/// Only the sender's own messages are ever held (networking `CLAUDE.md`,
/// "Local Queueing, No Relaying").
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
  /// malformed and throws [FormatException] (networking `CLAUDE.md`, "No
  /// Legacy or Compatibility Code").
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

/// Where the layer keeps its fair-delivery queue from one instantiation to
/// the next, so that the queue survives the termination of the application
/// (GNA §2.10).  The layer saves a message here before `send` returns and
/// removes it once the recipient has acknowledged it, and reads the queue
/// back when it is instantiated again with the same identity.  A queue is
/// one agent's: it is keyed by the public key of the agent that sent its
/// messages, and is never replayed under another identity.
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
}

/// A store that lasts as long as the process. One instance passed to two
/// layers in turn carries the queue from the first to the second, as a
/// restart does; it does not survive the termination of the application.
class MemoryOutboundQueueStore implements OutboundQueueStore {
  final Map<String, List<OutboundQueueEntry>> _queues = {};

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
}

/// A JSON file holding one agent's queue, `{"owner": <hex>, "messages":
/// [...]}`: the headless profile's store, as [FileKnownPeersStore] is its
/// known-peer set's. Writes are atomic (a temporary file, then a rename), so
/// a termination in the middle of a write leaves the previous queue whole.
///
/// The file belongs to the agent that first wrote it: loading or saving
/// another agent's queue through it throws [StateError], since a message
/// replayed under another identity would be a message that agent never sent.
class FileOutboundQueueStore implements OutboundQueueStore {
  FileOutboundQueueStore(this.path);

  final String path;

  @override
  Future<List<OutboundQueueEntry>> load(String ownerHex) async {
    final file = File(path);
    if (!file.existsSync()) return [];
    final (owner, entries) = _decode(file.readAsStringSync());
    _checkOwner(owner, ownerHex);
    return entries;
  }

  @override
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries) async {
    final file = File(path);
    if (file.existsSync()) {
      final (owner, _) = _decode(file.readAsStringSync());
      _checkOwner(owner, ownerHex);
    }
    file.parent.createSync(recursive: true);
    final tmp = File('$path.tmp');
    tmp.writeAsStringSync(
      jsonEncode({
        'owner': ownerHex,
        'messages': [for (final entry in entries) entry.toJson()],
      }),
      flush: true,
    );
    tmp.renameSync(path);
  }

  (String, List<OutboundQueueEntry>) _decode(String text) {
    final decoded = jsonDecode(text);
    if (decoded is! Map) {
      throw FormatException('$path: a persisted queue is not a JSON object');
    }
    final owner = decoded['owner'];
    final messages = decoded['messages'];
    if (owner is! String || messages is! List) {
      throw FormatException('$path: a persisted queue needs an owner and '
          'its messages');
    }
    return (
      owner,
      [for (final item in messages) OutboundQueueEntry.fromJson(item)],
    );
  }

  void _checkOwner(String owner, String ownerHex) {
    if (owner == ownerHex) return;
    throw StateError('$path holds the queue of ${owner.substring(0, 8)}…, '
        'not of ${ownerHex.substring(0, 8)}…');
  }
}

/// The layer's side of the store: the outstanding messages in the order
/// `send` accepted them, saved whole on every change, one write after
/// another, so that a later queue is never overwritten by an earlier one.
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

  /// Completes with the restored entries once the persisted queue is read;
  /// fails if it cannot be read.
  late final Future<List<OutboundQueueEntry>> restored;

  final Map<String, OutboundQueueEntry> _entries = {};
  Future<void> _writes = Future.value();

  Future<List<OutboundQueueEntry>> _restore() async {
    final entries = await _store.load(_ownerHex);
    for (final entry in entries) {
      _entries[entry.messageId] = entry;
    }
    return entries;
  }

  /// The outstanding messages, oldest first.
  List<OutboundQueueEntry> get entries => List.unmodifiable(_entries.values);

  /// Whether [messageId] is outstanding.
  bool contains(String messageId) => _entries.containsKey(messageId);

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

  /// Completes once every save begun so far has completed.
  Future<void> flush() => _writes;

  Future<void> _save() {
    final snapshot = List<OutboundQueueEntry>.of(_entries.values);
    final write = _writes.then((_) => _store.save(_ownerHex, snapshot));
    _writes = write.catchError((Object e) {
      debugPrint('[queue] Failed to save the outbound queue: $e');
    });
    return write;
  }
}
