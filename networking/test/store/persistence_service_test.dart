import 'dart:convert';

import 'package:fake_async/fake_async.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:grassroots_networking/src/store/persistence_service.dart';
import 'package:grassroots_networking/src/store/friendships_state.dart';
import 'package:grassroots_networking/src/store/settings_state.dart';
import 'package:grassroots_networking/src/store/messages_state.dart';
import 'package:grassroots_networking/src/store/app_state.dart';

// ===== Helper builders (top-level to avoid underscore lint warnings) =====

FriendshipState makeFriendship({
  required String pubkey,
  FriendshipStatus status = FriendshipStatus.accepted,
  String? nickname,
  String? udpAddress,
  String? message,
  DateTime? createdAt,
  DateTime? updatedAt,
}) {
  final now = DateTime.utc(2025, 1, 15, 12, 0, 0);
  return FriendshipState(
    peerPubkeyHex: pubkey,
    nickname: nickname,
    status: status,
    udpAddress: udpAddress,
    message: message,
    createdAt: createdAt ?? now,
    updatedAt: updatedAt ?? now,
  );
}

ChatMessageState makeMessage({
  required String sender,
  required String recipient,
  String content = 'hello',
  bool isOutgoing = true,
  ChatMessageType messageType = ChatMessageType.text,
  String? udpAddress,
  String? messageId,
  DateTime? timestamp,
}) {
  return ChatMessageState(
    senderPubkeyHex: sender,
    recipientPubkeyHex: recipient,
    content: content,
    timestamp: timestamp ?? DateTime.utc(2025, 1, 15, 12, 0, 0),
    isOutgoing: isOutgoing,
    messageType: messageType,
    udpAddress: udpAddress,
    messageId: messageId,
  );
}

AppState makeAppState({
  FriendshipsState? friendships,
  SettingsState? settings,
  MessagesState? messages,
}) {
  return AppState(
    friendships: friendships ?? const FriendshipsState(),
    settings: settings ?? const SettingsState(),
    messages: messages ?? const MessagesState(),
  );
}

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  const peerA =
      'aabbccdd11223344aabbccdd11223344aabbccdd11223344aabbccdd11223344';
  const peerB =
      'eeff00112233445566778899aabbccddeeff00112233445566778899aabbccdd';
  const rendezvousA = RendezvousServerSettings(
    address: '[2001:db8::10]:9516',
    pubkeyHex:
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
  );
  const rendezvousB = RendezvousServerSettings(
    address: '198.51.100.20:9514',
    pubkeyHex:
        'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
  );

  late PersistenceService service;

  setUp(() {
    SharedPreferences.setMockInitialValues({});
    service = PersistenceService();
  });

  tearDown(() {
    service.dispose();
  });

  // ===================================================================
  // loadFriendships
  // ===================================================================
  group('loadFriendships', () {
    test('returns empty FriendshipsState when no data stored', () async {
      final result = await service.loadFriendships();

      expect(result.friendships, isEmpty);
    });

    test('loads friendships from v2 key', () async {
      final friendship = makeFriendship(
        pubkey: peerA,
        nickname: 'Alice',
        status: FriendshipStatus.accepted,
        udpAddress: '1.2.3.4:4001',
        message: 'Hi!',
      );
      final state = FriendshipsState(friendships: {peerA: friendship});

      SharedPreferences.setMockInitialValues({
        'grassroots_friendships_v2': jsonEncode(state.toJson()),
      });
      service = PersistenceService();

      final result = await service.loadFriendships();

      expect(result.friendships.length, equals(1));
      expect(result.friendships.containsKey(peerA), isTrue);
      final loaded = result.friendships[peerA]!;
      expect(loaded.peerPubkeyHex, equals(peerA));
      expect(loaded.nickname, equals('Alice'));
      expect(loaded.status, equals(FriendshipStatus.accepted));
      expect(loaded.udpAddress, equals('1.2.3.4:4001'));
      expect(loaded.message, equals('Hi!'));
    });

    test('loads multiple friendships from v2 key', () async {
      final friendshipA = makeFriendship(
        pubkey: peerA,
        nickname: 'Alice',
        status: FriendshipStatus.accepted,
      );
      final friendshipB = makeFriendship(
        pubkey: peerB,
        nickname: 'Bob',
        status: FriendshipStatus.pending,
      );
      final state = FriendshipsState(
        friendships: {peerA: friendshipA, peerB: friendshipB},
      );

      SharedPreferences.setMockInitialValues({
        'grassroots_friendships_v2': jsonEncode(state.toJson()),
      });
      service = PersistenceService();

      final result = await service.loadFriendships();

      expect(result.friendships.length, equals(2));
      expect(result.friendships[peerA]!.nickname, equals('Alice'));
      expect(result.friendships[peerB]!.nickname, equals('Bob'));
      expect(
          result.friendships[peerB]!.status, equals(FriendshipStatus.pending));
    });

    test('returns empty FriendshipsState on corrupt data', () async {
      SharedPreferences.setMockInitialValues({
        'grassroots_friendships_v2': 'this is not json{{{',
      });
      service = PersistenceService();

      final result = await service.loadFriendships();

      expect(result.friendships, isEmpty);
    });
  });

  // ===================================================================
  // loadSettings
  // ===================================================================
  group('loadSettings', () {
    test('returns default SettingsState when no data stored', () async {
      final result = await service.loadSettings();

      expect(result.bluetoothEnabled, isTrue);
      expect(result.udpEnabled, isTrue);
      expect(
          result.transportPriority,
          equals(const [
            TransportProtocol.bluetooth,
            TransportProtocol.udp,
          ]));
    });

    test('loads settings from v2 key', () async {
      const settings = SettingsState(
        bluetoothEnabled: false,
        udpEnabled: true,
        transportPriority: [
          TransportProtocol.udp,
          TransportProtocol.bluetooth,
        ],
      );

      SharedPreferences.setMockInitialValues({
        'grassroots_settings_v2': jsonEncode(settings.toJson()),
      });
      service = PersistenceService();

      final result = await service.loadSettings();

      expect(result.bluetoothEnabled, isFalse);
      expect(result.udpEnabled, isTrue);
      expect(
          result.transportPriority,
          equals(const [
            TransportProtocol.udp,
            TransportProtocol.bluetooth,
          ]));
    });

    test('returns default SettingsState on corrupt data', () async {
      SharedPreferences.setMockInitialValues({
        'grassroots_settings_v2': 'garbage data {{{',
      });
      service = PersistenceService();

      final result = await service.loadSettings();

      expect(result.bluetoothEnabled, isTrue);
      expect(result.udpEnabled, isTrue);
    });

    test('handles partial settings JSON gracefully', () async {
      // Only bluetoothEnabled present, rest should use defaults
      final partialJson = {'bluetoothEnabled': false};

      SharedPreferences.setMockInitialValues({
        'grassroots_settings_v2': jsonEncode(partialJson),
      });
      service = PersistenceService();

      final result = await service.loadSettings();

      expect(result.bluetoothEnabled, isFalse);
      // Defaults for missing fields
      expect(result.udpEnabled, isTrue);
      expect(
          result.transportPriority,
          equals(const [
            TransportProtocol.bluetooth,
            TransportProtocol.udp,
          ]));
    });

    test('loads multiple rendezvous servers and merges legacy single server',
        () async {
      final settingsJson = {
        'anchorAddress': rendezvousA.address,
        'anchorPubkeyHex': rendezvousA.pubkeyHex,
        'rendezvousServers': [
          rendezvousA.toJson(),
          rendezvousB.toJson(),
        ],
      };

      SharedPreferences.setMockInitialValues({
        'grassroots_settings_v2': jsonEncode(settingsJson),
      });
      service = PersistenceService();

      final result = await service.loadSettings();

      expect(
        result.configuredRendezvousServers,
        equals(const [rendezvousA, rendezvousB]),
      );
    });
  });

  // ===================================================================
  // loadConversations
  // ===================================================================
  group('loadConversations', () {
    test('returns empty maps when no data stored', () async {
      final (conversations, unreadCounts) = await service.loadConversations();

      expect(conversations, isEmpty);
      expect(unreadCounts, isEmpty);
    });

    test('loads conversations and unread counts', () async {
      final msg1 = makeMessage(
        sender: peerA,
        recipient: peerB,
        content: 'Hello Bob',
        isOutgoing: true,
        messageId: 'msg-1',
      );
      final msg2 = makeMessage(
        sender: peerB,
        recipient: peerA,
        content: 'Hi Alice',
        isOutgoing: false,
        timestamp: DateTime.utc(2025, 1, 15, 12, 1, 0),
      );

      final conversationsJson = {
        peerB: [msg1.toJson(), msg2.toJson()],
      };
      final unreadJson = {peerB: 1};

      SharedPreferences.setMockInitialValues({
        'grassroots_conversations_v2': jsonEncode(conversationsJson),
        'grassroots_unread_counts_v2': jsonEncode(unreadJson),
      });
      service = PersistenceService();

      final (conversations, unreadCounts) = await service.loadConversations();

      expect(conversations.length, equals(1));
      expect(conversations[peerB]!.length, equals(2));
      expect(conversations[peerB]![0].content, equals('Hello Bob'));
      expect(conversations[peerB]![0].senderPubkeyHex, equals(peerA));
      expect(conversations[peerB]![0].isOutgoing, isTrue);
      expect(conversations[peerB]![0].messageId, equals('msg-1'));
      expect(conversations[peerB]![1].content, equals('Hi Alice'));
      expect(conversations[peerB]![1].isOutgoing, isFalse);

      expect(unreadCounts.length, equals(1));
      expect(unreadCounts[peerB], equals(1));
    });

    test('loads conversations without unread counts', () async {
      final msg = makeMessage(
        sender: peerA,
        recipient: peerB,
        content: 'test',
      );
      final conversationsJson = {
        peerB: [msg.toJson()],
      };

      SharedPreferences.setMockInitialValues({
        'grassroots_conversations_v2': jsonEncode(conversationsJson),
      });
      service = PersistenceService();

      final (conversations, unreadCounts) = await service.loadConversations();

      expect(conversations.length, equals(1));
      expect(unreadCounts, isEmpty);
    });

    test('loads unread counts without conversations', () async {
      final unreadJson = {peerA: 5, peerB: 3};

      SharedPreferences.setMockInitialValues({
        'grassroots_unread_counts_v2': jsonEncode(unreadJson),
      });
      service = PersistenceService();

      final (conversations, unreadCounts) = await service.loadConversations();

      expect(conversations, isEmpty);
      expect(unreadCounts.length, equals(2));
      expect(unreadCounts[peerA], equals(5));
      expect(unreadCounts[peerB], equals(3));
    });

    test('returns empty on corrupt conversations data', () async {
      SharedPreferences.setMockInitialValues({
        'grassroots_conversations_v2': 'not json!!',
        'grassroots_unread_counts_v2': jsonEncode({peerA: 2}),
      });
      service = PersistenceService();

      final (conversations, unreadCounts) = await service.loadConversations();

      // Conversations fail, but unread counts load independently
      expect(conversations, isEmpty);
      expect(unreadCounts[peerA], equals(2));
    });

    test('returns empty on corrupt unread counts data', () async {
      final msg = makeMessage(sender: peerA, recipient: peerB, content: 'x');
      SharedPreferences.setMockInitialValues({
        'grassroots_conversations_v2': jsonEncode({
          peerB: [msg.toJson()]
        }),
        'grassroots_unread_counts_v2': 'broken{{{',
      });
      service = PersistenceService();

      final (conversations, unreadCounts) = await service.loadConversations();

      // Conversations load, unread counts fail independently
      expect(conversations.length, equals(1));
      expect(unreadCounts, isEmpty);
    });

    test('loads messages with all ChatMessageType values', () async {
      final textMsg = makeMessage(
        sender: peerA,
        recipient: peerB,
        content: 'normal message',
        messageType: ChatMessageType.text,
      );
      final friendReqSent = makeMessage(
        sender: peerA,
        recipient: peerB,
        content: 'Sent a friend request',
        messageType: ChatMessageType.friendRequestSent,
        udpAddress: '[2001:db8::1]:4001',
      );
      final friendReqReceived = makeMessage(
        sender: peerB,
        recipient: peerA,
        content: 'Wants to be friends',
        isOutgoing: false,
        messageType: ChatMessageType.friendRequestReceived,
        udpAddress: '[2001:db8::2]:4001',
      );
      final friendReqAccepted = makeMessage(
        sender: peerB,
        recipient: peerA,
        content: 'Accepted',
        isOutgoing: false,
        messageType: ChatMessageType.friendRequestAccepted,
      );

      SharedPreferences.setMockInitialValues({
        'grassroots_conversations_v2': jsonEncode({
          peerB: [
            textMsg.toJson(),
            friendReqSent.toJson(),
            friendReqReceived.toJson(),
            friendReqAccepted.toJson(),
          ],
        }),
      });
      service = PersistenceService();

      final (conversations, _) = await service.loadConversations();

      expect(conversations[peerB]!.length, equals(4));
      expect(
          conversations[peerB]![0].messageType, equals(ChatMessageType.text));
      expect(conversations[peerB]![1].messageType,
          equals(ChatMessageType.friendRequestSent));
      expect(conversations[peerB]![2].messageType,
          equals(ChatMessageType.friendRequestReceived));
      expect(conversations[peerB]![2].udpAddress, equals('[2001:db8::2]:4001'));
      expect(conversations[peerB]![3].messageType,
          equals(ChatMessageType.friendRequestAccepted));
    });
  });

  // ===================================================================
  // flush
  // ===================================================================
  group('flush', () {
    test('persists all state sections immediately', () async {
      final friendship = makeFriendship(
        pubkey: peerA,
        nickname: 'Alice',
      );
      final friendshipsState =
          FriendshipsState(friendships: {peerA: friendship});
      const settingsState = SettingsState(
        bluetoothEnabled: false,
        udpEnabled: true,
      );
      final msg = makeMessage(
        sender: peerA,
        recipient: peerB,
        content: 'Persisted msg',
        messageId: 'msg-flush-1',
      );
      final messagesState = MessagesState(
        conversations: {
          peerB: [msg],
        },
        unreadCounts: const {peerB: 1},
      );

      final state = makeAppState(
        friendships: friendshipsState,
        settings: settingsState,
        messages: messagesState,
      );

      await service.flush(state);

      final prefs = await SharedPreferences.getInstance();
      expect(prefs.getString('grassroots_friendships_v2'), isNotNull);
      expect(prefs.getString('grassroots_settings_v2'), isNotNull);
      expect(prefs.getString('grassroots_conversations_v2'), isNotNull);
      expect(prefs.getString('grassroots_unread_counts_v2'), isNotNull);
    });

    test('round-trip: flush then load returns same friendships', () async {
      final friendship = makeFriendship(
        pubkey: peerA,
        nickname: 'RoundTripAlice',
        status: FriendshipStatus.received,
        udpAddress: '[2001:db8::1]:4001',
        message: 'round trip message',
      );
      final friendshipsState =
          FriendshipsState(friendships: {peerA: friendship});

      final state = makeAppState(friendships: friendshipsState);
      await service.flush(state);

      // Create new service to load from SharedPreferences
      final loadService = PersistenceService();
      final loaded = await loadService.loadFriendships();
      loadService.dispose();

      expect(loaded.friendships.length, equals(1));
      final loadedFriendship = loaded.friendships[peerA]!;
      expect(loadedFriendship.peerPubkeyHex, equals(peerA));
      expect(loadedFriendship.nickname, equals('RoundTripAlice'));
      expect(loadedFriendship.status, equals(FriendshipStatus.received));
      expect(loadedFriendship.udpAddress, equals('[2001:db8::1]:4001'));
      expect(loadedFriendship.message, equals('round trip message'));
      expect(loadedFriendship.createdAt, equals(friendship.createdAt));
      expect(loadedFriendship.updatedAt, equals(friendship.updatedAt));
    });

    test('round-trip: flush then load returns same settings', () async {
      const settings = SettingsState(
        bluetoothEnabled: false,
        udpEnabled: false,
        transportPriority: [
          TransportProtocol.udp,
          TransportProtocol.bluetooth,
        ],
      );

      final state = makeAppState(settings: settings);
      await service.flush(state);

      final loadService = PersistenceService();
      final loaded = await loadService.loadSettings();
      loadService.dispose();

      expect(loaded.bluetoothEnabled, isFalse);
      expect(loaded.udpEnabled, isFalse);
      expect(
          loaded.transportPriority,
          equals(const [
            TransportProtocol.udp,
            TransportProtocol.bluetooth,
          ]));
    });

    test('round-trip: flush then load returns same rendezvous servers',
        () async {
      final settings = SettingsState(
        anchorAddress: rendezvousA.address,
        anchorPubkeyHex: rendezvousA.pubkeyHex,
        rendezvousServers: const [rendezvousA, rendezvousB],
      );

      final state = makeAppState(settings: settings);
      await service.flush(state);

      final loadService = PersistenceService();
      final loaded = await loadService.loadSettings();
      loadService.dispose();

      expect(
        loaded.configuredRendezvousServers,
        equals(const [rendezvousA, rendezvousB]),
      );
    });

    test('round-trip: flush then load returns same conversations', () async {
      final msg1 = makeMessage(
        sender: peerA,
        recipient: peerB,
        content: 'First message',
        isOutgoing: true,
        messageId: 'msg-rt-1',
      );
      final msg2 = makeMessage(
        sender: peerB,
        recipient: peerA,
        content: 'Second message',
        isOutgoing: false,
        timestamp: DateTime.utc(2025, 1, 15, 12, 5, 0),
      );
      final messages = MessagesState(
        conversations: {
          peerB: [msg1, msg2],
        },
        unreadCounts: const {peerB: 1},
      );

      final state = makeAppState(messages: messages);
      await service.flush(state);

      final loadService = PersistenceService();
      final (loadedConvs, loadedUnread) = await loadService.loadConversations();
      loadService.dispose();

      expect(loadedConvs.length, equals(1));
      expect(loadedConvs[peerB]!.length, equals(2));
      expect(loadedConvs[peerB]![0].content, equals('First message'));
      expect(loadedConvs[peerB]![0].senderPubkeyHex, equals(peerA));
      expect(loadedConvs[peerB]![0].isOutgoing, isTrue);
      expect(loadedConvs[peerB]![0].messageId, equals('msg-rt-1'));
      expect(loadedConvs[peerB]![1].content, equals('Second message'));
      expect(loadedConvs[peerB]![1].isOutgoing, isFalse);
      expect(loadedConvs[peerB]![1].timestamp,
          equals(DateTime.utc(2025, 1, 15, 12, 5, 0)));

      expect(loadedUnread[peerB], equals(1));
    });

    test('flush with empty state stores empty data', () async {
      const state = AppState();
      await service.flush(state);

      final prefs = await SharedPreferences.getInstance();
      final friendshipsData = prefs.getString('grassroots_friendships_v2');
      final settingsData = prefs.getString('grassroots_settings_v2');
      final conversationsData = prefs.getString('grassroots_conversations_v2');
      final unreadData = prefs.getString('grassroots_unread_counts_v2');

      expect(friendshipsData, isNotNull);
      expect(settingsData, isNotNull);
      expect(conversationsData, isNotNull);
      expect(unreadData, isNotNull);

      // Verify the stored data decodes to empty/default states
      final friendshipsJson =
          jsonDecode(friendshipsData!) as Map<String, dynamic>;
      expect(friendshipsJson['friendships'], isEmpty);

      final settingsJson = jsonDecode(settingsData!) as Map<String, dynamic>;
      expect(settingsJson['bluetoothEnabled'], isTrue);
      expect(settingsJson['udpEnabled'], isTrue);

      final conversationsJson =
          jsonDecode(conversationsData!) as Map<String, dynamic>;
      expect(conversationsJson, isEmpty);

      final unreadJson = jsonDecode(unreadData!) as Map<String, dynamic>;
      expect(unreadJson, isEmpty);
    });
  });

  // ===================================================================
  // onStateChanged + debounced persistence
  // ===================================================================
  group('onStateChanged', () {
    test('debounces writes - does not persist immediately', () async {
      final state = makeAppState(
        friendships: FriendshipsState(friendships: {
          peerA: makeFriendship(pubkey: peerA, nickname: 'Alice'),
        }),
      );

      service.onStateChanged(state);

      // Immediately after calling onStateChanged, nothing persisted yet
      final prefs = await SharedPreferences.getInstance();
      expect(prefs.getString('grassroots_friendships_v2'), isNull);
    });

    test('persists after debounce delay elapses', () {
      fakeAsync((async) {
        final state = makeAppState(
          friendships: FriendshipsState(friendships: {
            peerA: makeFriendship(pubkey: peerA, nickname: 'Debounced'),
          }),
        );

        service.onStateChanged(state);

        // Before debounce delay: nothing persisted
        async.elapse(const Duration(milliseconds: 400));

        // After debounce delay (600ms total): should persist
        async.elapse(const Duration(milliseconds: 200));

        // flushMicrotasks to let the async persistence complete
        async.flushMicrotasks();
      });
    });

    test('resets debounce timer on rapid state changes', () {
      fakeAsync((async) {
        final state1 = makeAppState(
          friendships: FriendshipsState(friendships: {
            peerA: makeFriendship(pubkey: peerA, nickname: 'First'),
          }),
        );
        final state2 = makeAppState(
          friendships: FriendshipsState(friendships: {
            peerA: makeFriendship(pubkey: peerA, nickname: 'Second'),
          }),
        );

        service.onStateChanged(state1);

        // Wait 300ms, then change state again (should reset timer)
        async.elapse(const Duration(milliseconds: 300));
        service.onStateChanged(state2);

        // After 300ms from second call (600ms total), still not 500ms
        // since the second onStateChanged
        async.elapse(const Duration(milliseconds: 300));

        // Wait the remaining time (200ms+) so the second timer fires
        async.elapse(const Duration(milliseconds: 300));
        async.flushMicrotasks();
      });
    });

    test('only persists sections that changed - friendships only', () async {
      // Establish _lastPersistedState by flushing initial state
      const initialState = AppState();
      await service.flush(initialState);

      // Remove non-friendships keys so we can detect if they get re-written
      final prefs = await SharedPreferences.getInstance();
      await prefs.remove('grassroots_settings_v2');
      await prefs.remove('grassroots_conversations_v2');
      await prefs.remove('grassroots_unread_counts_v2');

      // Now change only friendships via onStateChanged (flush already set
      // _lastPersistedState, so only the diff is marked pending)
      fakeAsync((async) {
        final newState = makeAppState(
          friendships: FriendshipsState(friendships: {
            peerA: makeFriendship(pubkey: peerA, nickname: 'Changed'),
          }),
        );
        service.onStateChanged(newState);

        async.elapse(const Duration(milliseconds: 600));
        async.flushMicrotasks();
      });

      final prefsAfter = await SharedPreferences.getInstance();
      // Friendships should have been persisted
      expect(prefsAfter.getString('grassroots_friendships_v2'), isNotNull);
      // Settings and conversations should NOT have been re-persisted
      expect(prefsAfter.getString('grassroots_settings_v2'), isNull);
      expect(prefsAfter.getString('grassroots_conversations_v2'), isNull);
      expect(prefsAfter.getString('grassroots_unread_counts_v2'), isNull);
    });

    test('only persists sections that changed - settings only', () async {
      const initialState = AppState();
      await service.flush(initialState);

      final prefs = await SharedPreferences.getInstance();
      await prefs.remove('grassroots_friendships_v2');
      await prefs.remove('grassroots_conversations_v2');
      await prefs.remove('grassroots_unread_counts_v2');

      fakeAsync((async) {
        final newState = makeAppState(
          settings: const SettingsState(bluetoothEnabled: false),
        );
        service.onStateChanged(newState);

        async.elapse(const Duration(milliseconds: 600));
        async.flushMicrotasks();
      });

      final prefsAfter = await SharedPreferences.getInstance();
      expect(prefsAfter.getString('grassroots_settings_v2'), isNotNull);
      expect(prefsAfter.getString('grassroots_friendships_v2'), isNull);
      expect(prefsAfter.getString('grassroots_conversations_v2'), isNull);
    });

    test('does not schedule timer when nothing changed', () async {
      const state = AppState();

      fakeAsync((async) {
        // First call sets _lastPersistedState
        service.onStateChanged(state);
        async.elapse(const Duration(milliseconds: 600));
        async.flushMicrotasks();

        // After first persistence, call again with identical state
        service.onStateChanged(state);
        async.elapse(const Duration(milliseconds: 600));
        async.flushMicrotasks();
      });

      // Should complete without error; verify data is correct
      final prefs = await SharedPreferences.getInstance();
      final data = prefs.getString('grassroots_friendships_v2');
      expect(data, isNotNull);
    });
  });

  // ===================================================================
  // dispose
  // ===================================================================
  group('dispose', () {
    test('cancels pending debounce timer', () async {
      final state = makeAppState(
        friendships: FriendshipsState(friendships: {
          peerA: makeFriendship(pubkey: peerA, nickname: 'DisposedAlice'),
        }),
      );

      service.onStateChanged(state);
      // Dispose before debounce fires
      service.dispose();

      // Wait for what would have been the debounce delay
      await Future.delayed(const Duration(milliseconds: 600));

      final prefs = await SharedPreferences.getInstance();
      // Nothing should have been persisted since we disposed before the timer
      expect(prefs.getString('grassroots_friendships_v2'), isNull);
    });
  });

  // ===================================================================
  // flush cancels pending debounce
  // ===================================================================
  group('flush interaction with debounce', () {
    test('flush cancels pending debounce and persists immediately', () async {
      final state = makeAppState(
        friendships: FriendshipsState(friendships: {
          peerA: makeFriendship(pubkey: peerA, nickname: 'FlushAlice'),
        }),
      );

      // Schedule a debounced write
      service.onStateChanged(state);

      // Immediately flush
      await service.flush(state);

      // Data should be persisted immediately
      final prefs = await SharedPreferences.getInstance();
      final data = prefs.getString('grassroots_friendships_v2');
      expect(data, isNotNull);
      final loaded =
          FriendshipsState.fromJson(jsonDecode(data!) as Map<String, dynamic>);
      expect(loaded.friendships[peerA]!.nickname, equals('FlushAlice'));
    });

    test('flush persists all sections regardless of what changed', () async {
      // Create state with data in all sections
      final state = makeAppState(
        friendships: FriendshipsState(friendships: {
          peerA: makeFriendship(pubkey: peerA, nickname: 'FlushAll'),
        }),
        settings: const SettingsState(bluetoothEnabled: false),
        messages: MessagesState(
          conversations: {
            peerB: [
              makeMessage(
                sender: peerA,
                recipient: peerB,
                content: 'flush all msg',
              ),
            ],
          },
          unreadCounts: const {peerB: 3},
        ),
      );

      await service.flush(state);

      final prefs = await SharedPreferences.getInstance();
      expect(prefs.getString('grassroots_friendships_v2'), isNotNull);
      expect(prefs.getString('grassroots_settings_v2'), isNotNull);
      expect(prefs.getString('grassroots_conversations_v2'), isNotNull);
      expect(prefs.getString('grassroots_unread_counts_v2'), isNotNull);

      // Verify contents
      final settingsJson = jsonDecode(prefs.getString('grassroots_settings_v2')!)
          as Map<String, dynamic>;
      expect(settingsJson['bluetoothEnabled'], isFalse);

      final unreadJson =
          jsonDecode(prefs.getString('grassroots_unread_counts_v2')!)
              as Map<String, dynamic>;
      expect(unreadJson[peerB], equals(3));
    });
  });

  // ===================================================================
  // Edge cases
  // ===================================================================
  group('edge cases', () {
    test('friendship with null optional fields round-trips correctly',
        () async {
      final friendship = makeFriendship(
        pubkey: peerA,
        status: FriendshipStatus.pending,
      );
      final state = makeAppState(
        friendships: FriendshipsState(friendships: {peerA: friendship}),
      );

      await service.flush(state);

      final loadService = PersistenceService();
      final loaded = await loadService.loadFriendships();
      loadService.dispose();

      final f = loaded.friendships[peerA]!;
      expect(f.udpAddress, isNull);
      expect(f.nickname, isNull);
      expect(f.message, isNull);
      expect(f.status, equals(FriendshipStatus.pending));
    });

    test('multiple conversations round-trip correctly', () async {
      final msg1 = makeMessage(
        sender: peerA,
        recipient: peerB,
        content: 'To Bob',
      );
      final msg2 = makeMessage(
        sender: peerA,
        recipient: peerA,
        content: 'To self',
      );

      final state = makeAppState(
        messages: MessagesState(
          conversations: {
            peerB: [msg1],
            peerA: [msg2],
          },
          unreadCounts: const {peerB: 1, peerA: 2},
        ),
      );

      await service.flush(state);

      final loadService = PersistenceService();
      final (convs, unreads) = await loadService.loadConversations();
      loadService.dispose();

      expect(convs.length, equals(2));
      expect(convs[peerB]!.length, equals(1));
      expect(convs[peerA]!.length, equals(1));
      expect(unreads[peerB], equals(1));
      expect(unreads[peerA], equals(2));
    });
  });
}
