import 'package:flutter/foundation.dart';
import 'package:grassroots_networking/grassroots_networking.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:flutter_local_notifications/flutter_local_notifications.dart';
import 'package:logger/logger.dart' show Logger, Level;
import 'src/debug/log_buffer.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:redux/redux.dart';
import 'package:flutter_redux/flutter_redux.dart';
import 'package:connectivity_plus/connectivity_plus.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';
import 'dart:async';
import 'chat_screen.dart';
import 'chat_models.dart';
import 'settings_screen.dart';

// Global notification plugin instance
final FlutterLocalNotificationsPlugin flutterLocalNotificationsPlugin =
    FlutterLocalNotificationsPlugin();

// Global key for navigation from notification
final GlobalKey<NavigatorState> navigatorKey = GlobalKey<NavigatorState>();

final Logger _log = Logger();

// Pending chat to open from notification
String? _pendingChatPeerHex;

// Global redux store
late final Store<AppState> appStore;

// Global persistence service
late final PersistenceService persistenceService;

// Global libsodium handle, initialized at app startup. Used by ProtocolHandler
// for native Ed25519 sign on the main isolate; verifier worker isolates
// initialize their own Sodium handles independently.
late final SodiumSumo appSodium;

Future<GrassrootsIdentity> _initIdentity() async {
  // Spec putIdentity/getIdentity (docs/GLP_Networking_API/sections/api.tex
  // §Identity): restore the persisted identity, or generate-and-persist one on
  // first launch so the same Ed25519 key pair is reused every session.
  final existing = await IdentityStore.getIdentity();
  if (existing != null) {
    debugPrint('Identity found in secure storage.');
    debugPrint('Private Key Bytes (Seed): ${existing.privateKey.length} bytes');
    debugPrint('Public Key Bytes: ${existing.publicKey.length} bytes');
    debugPrint('Nickname: ${existing.nickname}');
    return existing;
  }

  debugPrint('No identity found, generating new one.');
  final identity = await GrassrootsIdentity.generate();
  await IdentityStore.putIdentity(identity);
  debugPrint('Private Key Bytes (Seed): ${identity.privateKey.length} bytes');
  debugPrint('Public Key Bytes: ${identity.publicKey.length} bytes');
  debugPrint('Nickname: ${identity.nickname}');
  return identity;
}

Map<String, dynamic> _serializeAppState(AppState state) {
  return {
    'bleTransportState': state.transports.bleState.name,
    'udpTransportState': state.transports.udpState.name,
    'peers': {
      'discoveredBlePeers': {
        for (final e in state.peers.discoveredBlePeers.entries)
          e.key: {
            'transportId': e.value.transportId,
            'displayName': e.value.displayName,
            'rssi': e.value.rssi,
            'isConnecting': e.value.isConnecting,
            'isConnected': e.value.isConnected,
            'lastSeen': e.value.lastSeen.toIso8601String(),
          },
      },
      'peers': {
        for (final e in state.peers.peers.entries)
          e.key: {
            'nickname': e.value.nickname,
            'connectionState': e.value.connectionState.name,
            'transport': e.value.transport.name,
            'activeTransport': e.value.activeTransport.name,
            'rssi': e.value.rssi,
            'bleDeviceId': e.value.bleDeviceId,
            'udpAddress': e.value.udpAddress,
            'isFriend': e.value.isFriend,
            'lastSeen': e.value.lastSeen?.toIso8601String(),
          },
      },
    },
    'messages': {
      'conversationCount': state.messages.conversations.length,
      'unreadCounts': state.messages.unreadCounts,
      'outgoingCount': state.messages.outgoingMessages.length,
      'incomingCount': state.messages.incomingMessages.length,
    },
    'friendships': {
      for (final e in state.friendships.friendships.entries)
        e.key: {
          'nickname': e.value.nickname,
          'status': e.value.status.name,
          'udpAddress': e.value.udpAddress,
        },
    },
    'settings': state.settings.toJson(),
  };
}

/// Set up debug log capture by intercepting debugPrint.
///
/// Adds a `[HH:MM:SS.fff]` timestamp to every console line (matching the
/// rendezvous-server format so client/server logs can be cross-referenced),
/// and feeds the in-memory LogBuffer that drives the Debug Logs screen.
void _setupDebugLogCapture() {
  final originalDebugPrint = debugPrint;
  debugPrint = (String? message, {int? wrapWidth}) {
    // Prepend a wall-clock timestamp so console output is correlatable with
    // the rendezvous-server logs. The LogBuffer entry below uses its own
    // structured timestamp; only the console line gets the prefix.
    final stamped = message == null ? null : '[${_logTimestamp()}] $message';
    originalDebugPrint(stamped, wrapWidth: wrapWidth);

    // Parse the (untimestamped) log line to extract level and feed the buffer
    if (message != null && message.isNotEmpty) {
      final entry = _parseLogLine(message);
      if (entry != null) {
        LogBuffer.instance.addEntry(entry);
      }
    }
  };
}

String _logTimestamp() {
  final now = DateTime.now();
  String two(int v) => v.toString().padLeft(2, '0');
  String three(int v) => v.toString().padLeft(3, '0');
  return '${two(now.hour)}:${two(now.minute)}:${two(now.second)}.${three(now.millisecond)}';
}

/// Parse a logger output line to extract the level and clean message.
LogEntry? _parseLogLine(String line) {
  // Strip ANSI codes
  final clean = line.replaceAll(RegExp(r'\x1B\[[0-9;]*m'), '').trim();
  if (clean.isEmpty) return null;

  // Skip box-drawing borders (┌ ├ └ │ alone)
  if (RegExp(r'^[┌├└─┄]+$').hasMatch(clean)) return null;

  // Skip Flutter framework noise
  if (clean.startsWith('I/flutter') ||
      clean.startsWith('D/') ||
      clean.startsWith('W/')) {
    return null;
  }

  // Detect level from emoji markers.
  // Logger package uses: ⛔=error, ⚠️=warning, 💡=info, 🐛=debug
  // App code uses: 📨📦🤝=debug (message parsing), Persisted=debug
  Level level = Level.debug;
  String message = clean;

  if (clean.contains('⛔')) {
    level = Level.error;
  } else if (clean.contains('⚠️')) {
    level = Level.warning;
  } else if (clean.contains('💡')) {
    level = Level.info;
  } else if (clean.contains('🐛') ||
      clean.contains('📨') ||
      clean.contains('📦') ||
      clean.contains('🤝')) {
    level = Level.debug;
  }

  // Strip the box-drawing prefix (│ )
  message = message.replaceFirst(RegExp(r'^│\s*'), '');

  return LogEntry(
    level: level,
    message: message,
    timestamp: DateTime.now(),
  );
}

/// Redux middleware that handles two file-system side effects:
///
/// 1. **View-once cleanup on delivery**: when an outgoing view-once picture
///    transitions to `MessageStatus.delivered`, delete the sender's local
///    copy and dispatch `MarkPictureViewedAction` to drop the path from state.
///    The recipient's copy is deleted separately when they tap to view.
///
/// 2. **Conversation deletion cleanup**: when a `DeleteConversationAction`
///    fires, snapshot the media paths in that conversation BEFORE the reducer
///    runs (the conversation is gone after `next(action)`), then delete those
///    files asynchronously.
void _mediaCleanupMiddleware(
    Store<AppState> store, dynamic action, NextDispatcher next) {
  // Snapshot before reducing — we need the conversation's media paths before
  // it disappears.
  List<String>? pathsToDeleteOnConversationDrop;
  if (action is DeleteConversationAction) {
    final conv = store.state.messages.conversations[action.peerHex];
    if (conv != null) {
      pathsToDeleteOnConversationDrop = [
        for (final m in conv)
          if (m.mediaPath != null) m.mediaPath!,
      ];
    }
  }

  next(action);

  if (pathsToDeleteOnConversationDrop != null) {
    for (final p in pathsToDeleteOnConversationDrop) {
      unawaited(deleteMediaFile(p));
    }
    return;
  }

  if (action is MessageDeliveredAction) {
    // Find the matching outgoing chat message by messageId across all
    // conversations. Pictures are infrequent; this scan is cheap relative to
    // the actual message arrival.
    final messageId = action.messageId;
    for (final entry in store.state.messages.conversations.entries) {
      final peerHex = entry.key;
      for (final msg in entry.value) {
        if (msg.messageId == messageId &&
            msg.isOutgoing &&
            msg.viewOnce &&
            msg.mediaPath != null) {
          final pathToDelete = msg.mediaPath!;
          unawaited(deleteMediaFile(pathToDelete));
          store.dispatch(MarkPictureViewedAction(
            peerHex: peerHex,
            messageId: messageId,
          ));
          return;
        }
      }
    }
  }
}

void main() async {
  WidgetsFlutterBinding.ensureInitialized();

  // Capture all Flutter print output (including Logger) into the debug log buffer.
  // This feeds the Debug Logs screen in Settings.
  _setupDebugLogCapture();

  // Initialize libsodium (SUMO) once for the main isolate. SUMO is required for
  // the Ed25519↔X25519 conversion used to derive/verify Noise static keys.
  // Verifier worker isolates init their own (verify-only) handles — the native
  // binary loads once per process, but each isolate needs its own FFI handle.
  appSodium = await SodiumSumoInit.init();

  // Create persistence service and load persisted state
  persistenceService = PersistenceService();
  final friendships = await persistenceService.loadFriendships();
  final settings = await persistenceService.loadSettings();
  final (conversations, unreadCounts) =
      await persistenceService.loadConversations();

  // Initialize redux store with hydrated state
  appStore = Store<AppState>(
    appReducer,
    initialState: AppState(
      friendships: friendships,
      settings: settings,
      messages: MessagesState(
        conversations: conversations,
        unreadCounts: unreadCounts,
      ),
    ),
    middleware: [_mediaCleanupMiddleware],
  );

  // Subscribe to persist changes (debounced)
  appStore.onChange.listen((state) => persistenceService.onStateChanged(state));

  // Initialize notifications
  const AndroidInitializationSettings initializationSettingsAndroid =
      AndroidInitializationSettings('@mipmap/ic_launcher');
  const DarwinInitializationSettings initializationSettingsDarwin =
      DarwinInitializationSettings(
    requestAlertPermission: true,
    requestBadgePermission: true,
    requestSoundPermission: true,
  );
  const InitializationSettings initializationSettings = InitializationSettings(
    android: initializationSettingsAndroid,
    iOS: initializationSettingsDarwin,
  );

  await flutterLocalNotificationsPlugin.initialize(
    initializationSettings,
    onDidReceiveNotificationResponse: (NotificationResponse response) {
      // Handle notification tap - store the peer to open chat with
      if (response.payload != null) {
        _pendingChatPeerHex = response.payload;
      }
    },
  );

  // Request notification permission (Android 13+)
  await Permission.notification.request();

  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return StoreProvider<AppState>(
      store: appStore,
      child: MaterialApp(
        navigatorKey: navigatorKey,
        theme: ThemeData.dark(),
        home: const GrassrootsHome(),
      ),
    );
  }
}

class GrassrootsHome extends StatefulWidget {
  const GrassrootsHome({super.key});

  @override
  State<GrassrootsHome> createState() => _GrassrootsHomeState();
}

class _GrassrootsHomeState extends State<GrassrootsHome>
    with TickerProviderStateMixin, WidgetsBindingObserver {
  GrassrootsIdentity? _identity;
  GrassrootsNetwork? _grassroots;
  Timer? _refreshTimer;
  StreamSubscription<List<ConnectivityResult>>? _connectivitySubscription;
  int _currentIndex = 1; // Start on "Around" tab (center)

  // Track nickname changes for animation
  final Map<String, _NicknameChange> _nicknameChanges = {};

  // Transport availability derived from Redux store
  bool get _bleAvailable => appStore.state.transports.bleState.isUsable;
  bool get _udpAvailable => appStore.state.transports.udpState.isUsable;

  /// Get our UDP address for friend communication
  String? get _myUdpAddress => _grassroots?.udpAddress;

  /// Get nearby peers from Redux store (BLE-connected peers in physical proximity).
  /// For the "Nearby" section - only peers reachable via Bluetooth.
  Map<String, PeerState> get _peers {
    final peersState = appStore.state.peers;
    return {for (var p in peersState.nearbyBlePeers) p.pubkeyHex: p};
  }

  // /// Get discovered but unconnected nearby devices
  // List<DiscoveredPeerState> get _unconnectedDevices {
  //   final peersState = appStore.state.peers;
  //   // Map of currently connected device IDs
  //   final connectedDeviceIds = peersState.peersList
  //       .where((p) => p.hasBleConnection)
  //       .expand((p) => [p.bleCentralDeviceId, p.blePeripheralDeviceId])
  //       .where((id) => id != null)
  //       .toSet();

  //   return peersState.discoveredBlePeersList
  //       .where((d) =>
  //           !connectedDeviceIds.contains(d.transportId) && !d.isConnected)
  //       .toList();
  // }

  @override
  void initState() {
    super.initState();
    WidgetsBinding.instance.addObserver(this);
    // Subscribe to Redux store changes - this handles all state updates
    appStore.onChange.listen((_) {
      if (mounted) setState(() {});
    });
    // Subscribe to connectivity changes
    _connectivitySubscription =
        Connectivity().onConnectivityChanged.listen(_onConnectivityChanged);
    _initialize();
    // Refresh UI every second to update "seconds ago" display
    _refreshTimer = Timer.periodic(const Duration(seconds: 1), (_) {
      if (mounted) {
        setState(() {});
        // Check for pending chat from notification
        _checkPendingChat();
      }
    });
  }

  void _onConnectivityChanged(List<ConnectivityResult> results) {
    debugPrint('🌐 Connectivity changed: $results');
  }

  void _checkPendingChat() {
    if (_pendingChatPeerHex != null &&
        _grassroots != null &&
        _identity != null) {
      final peerHex = _pendingChatPeerHex!;
      _pendingChatPeerHex = null;

      // Find the peer
      final peer = _peers.values
          .where((p) => ChatMessage.pubkeyToHex(p.publicKey) == peerHex)
          .firstOrNull;

      if (peer != null) {
        _openChat(peer);
      }
    }
  }

  @override
  void dispose() {
    WidgetsBinding.instance.removeObserver(this);
    _connectivitySubscription?.cancel();
    _refreshTimer?.cancel();
    _grassroots?.dispose();
    // Flush persistence on exit
    persistenceService.flush(appStore.state);
    super.dispose();
  }

  @override
  void didChangeAppLifecycleState(AppLifecycleState state) {
    super.didChangeAppLifecycleState(state);
    debugPrint('[lifecycle] App state -> $state');
    if (state == AppLifecycleState.resumed) {
      unawaited(_grassroots?.onAppResumed() ?? Future.value());
    }
  }

  Future<void> _initialize() async {
    try {
      final identity = await _initIdentity();

      final grassroots = GrassrootsNetwork(
        identity: identity,
        store: appStore,
        sodium: appSodium,
      );

      grassroots.onMessageReceived =
          (messageId, senderPubkey, payload, transport) {
        _handleIncomingMessage(messageId, senderPubkey, payload, transport);
      };

      // Friend presence is handled at the transport layer; no app-layer
      // callback needed for UDP initialization.

      // grassroots.onPeerConnected = (peer) {
      //   print('Peer connected: ${peer.displayName}');
      //   // PeerStore already has the peer - just track nickname changes
      // };

      // grassroots.onPeerUpdated = (peer) {
      //   print('Peer updated: ${peer.displayName}');

      //   // Check if nickname changed - use peerStore to get the previous state
      //   // Note: Since peerStore already updated, we track changes via the _nicknameChanges map
      //   // The peer object passed here is from peerStore, so we can't compare old/new directly
      //   // This callback is mainly for nickname change animations
      // };

      // grassroots.onPeerDisconnected = (peer) {
      //   print('Peer disconnected: ${peer.displayName}');
      //   // PeerStore already updated - UI will refresh via _onPeersChanged
      // };

      setState(() {
        _identity = identity;
        _grassroots = grassroots;
      });

      final success = await grassroots.initialize();
      if (!success) {
        debugPrint('Grassroots initialization failed');
        return;
      }

      // Hydrate Redux store with existing friends from FriendshipStore
      await _hydrateFriendsFromStore();
    } catch (e) {
      debugPrint('Initialization error: $e');
    }
  }

  /// Hydrate Redux store with friends from persistent FriendshipStore
  Future<void> _hydrateFriendsFromStore() async {
    for (final friendship in appStore.state.friendships.friends) {
      final pubkey = ChatMessage.hexToPubkey(friendship.peerPubkeyHex);

      // Establish friendship in Redux
      appStore.dispatch(FriendEstablishedAction(
        publicKey: pubkey,
        nickname: friendship.nickname,
      ));

      // If friend has UDP info, associate it
      if (friendship.udpAddress != null && friendship.udpAddress!.isNotEmpty) {
        appStore.dispatch(AssociateUdpAddressAction(
          publicKey: pubkey,
          address: friendship.udpAddress!,
        ));
      }
    }
  }

  // Friend presence is handled at the transport layer via unified ANNOUNCE
  // messages. BLE and UDP broadcasts include address for friends automatically.

  Future<void> _handleIncomingMessage(String messageId, Uint8List senderPubkey,
      Uint8List payload, MessageTransport transport) async {
    final senderHex = ChatMessage.pubkeyToHex(senderPubkey);
    final myHex = ChatMessage.pubkeyToHex(_identity!.publicKey);

    final block = Block.tryDeserialize(payload);

    if (block != null) {
      await _handleBlock(
          block, senderHex, myHex, messageId, senderPubkey, transport);
    } else {
      debugPrint(
          '📨 Failed to parse block (${payload.length} bytes from $senderHex) - dropping');
    }
  }

  Future<void> _handleBlock(
      Block block,
      String senderHex,
      String myHex,
      String messageId,
      Uint8List senderPubkey,
      MessageTransport transport) async {
    // Find sender name
    final peer = _peers.values
        .where((p) => ChatMessage.pubkeyToHex(p.publicKey) == senderHex)
        .firstOrNull;
    final senderName = peer?.displayName ?? 'Unknown';
    final transportName = transport == MessageTransport.udp ? 'UDX' : 'BLE';

    switch (block.type) {
      case BlockType.say:
        final sayBlock = block as SayBlock;
        if (sayBlock is TextSayBlock) {
          debugPrint(
              '💬 [$transportName] Message from $senderName: "${sayBlock.content}"');
          await _handleTextMessage(
              senderHex, myHex, sayBlock.content, messageId, senderPubkey);
        } else if (sayBlock is PictureSayBlock) {
          debugPrint(
              '📷 [$transportName] Picture from $senderName (${sayBlock.imageBytes.length} bytes, viewOnce=${sayBlock.viewOnce})');
          await _handlePictureMessage(
              senderHex, myHex, sayBlock, messageId, senderPubkey);
        }

      case BlockType.friendshipOffer:
        debugPrint(
            'Hansdling FriendshipOfferBlock from $senderName ($senderHex)');
        final offerBlock = block as FriendshipOfferBlock;
        await _handleFriendshipOffer(senderHex, myHex, offerBlock, senderName);

      case BlockType.friendshipAccept:
        debugPrint(
            'Handling FriendshipAcceptBlock from $senderName ($senderHex)');
        final acceptBlock = block as FriendshipAcceptBlock;
        await _handleFriendshipAccept(
            senderHex, myHex, acceptBlock, senderName);

      case BlockType.friendshipRevoke:
        debugPrint(
            'Handling FriendshipRevokeBlock from $senderName ($senderHex)');
        await _handleFriendshipRevoke(senderHex);
    }
  }

  Future<void> _handleTextMessage(String senderHex, String myHex,
      String content, String messageId, Uint8List senderPubkey) async {
    // Save message to Redux store
    appStore.dispatch(SaveChatMessageAction(
      senderPubkeyHex: senderHex,
      recipientPubkeyHex: myHex,
      content: content,
      isOutgoing: false,
      messageId: messageId,
    ));
    // Read receipt sent when user opens the chat (see ChatScreen._sendReadReceipts)

    // Find sender name
    final peer = _peers.values
        .where((p) => ChatMessage.pubkeyToHex(p.publicKey) == senderHex)
        .firstOrNull;
    final senderName = peer?.displayName ?? 'Unknown';

    // Show notification
    await _showMessageNotification(senderHex, senderName, content);
  }

  Future<void> _handlePictureMessage(String senderHex, String myHex,
      PictureSayBlock block, String messageId, Uint8List senderPubkey) async {
    // Persist the image bytes to disk under a SHA-256-named file. dedupes
    // identical images naturally.
    final file = await writeMediaFile(block.imageBytes, block.mime);

    appStore.dispatch(SaveChatMessageAction(
      senderPubkeyHex: senderHex,
      recipientPubkeyHex: myHex,
      // No caption support in v1; keep content empty so chat-list previews
      // can still render a short subtitle.
      content: '',
      isOutgoing: false,
      messageId: messageId,
      messageType: ChatMessageType.picture.index,
      mediaPath: file.path,
      mediaMime: block.mime,
      viewOnce: block.viewOnce,
    ));

    final peer = _peers.values
        .where((p) => ChatMessage.pubkeyToHex(p.publicKey) == senderHex)
        .firstOrNull;
    final senderName = peer?.displayName ?? 'Unknown';
    await _showMessageNotification(
      senderHex,
      senderName,
      block.viewOnce ? '🔥 Sent a 1-time photo' : '📷 Sent a photo',
    );
  }

  Future<void> _handleFriendshipOffer(
    String senderHex,
    String myHex,
    FriendshipOfferBlock block,
    String senderName,
  ) async {
    // Record the friend request
    appStore.dispatch(ReceiveFriendRequestAction(
      peerPubkeyHex: senderHex,
      nickname: senderName,
      message: block.message,
    ));

    // Get the updated friendship state
    final friendship = appStore.state.friendships.getFriendship(senderHex);
    final pubkey = ChatMessage.hexToPubkey(senderHex);

    // If auto-accepted (mutual friend requests), establish friendship in Redux
    if (friendship != null && friendship.isAccepted) {
      appStore.dispatch(FriendEstablishedAction(
        publicKey: pubkey,
        nickname: senderName,
      ));
    }

    // Save as a chat message
    appStore.dispatch(SaveChatMessageAction(
      senderPubkeyHex: senderHex,
      recipientPubkeyHex: myHex,
      content: block.message ?? 'Wants to be friends',
      isOutgoing: false,
      messageType: ChatMessageType.friendRequestReceived.index,
    ));

    // Show notification if friendship is new
    if (friendship?.status == FriendshipStatus.received) {
      await _showFriendRequestNotification(senderHex, senderName);
    }

    // UDP connection will be established when ANNOUNCE is received
  }

  Future<void> _handleFriendshipAccept(
    String senderHex,
    String myHex,
    FriendshipAcceptBlock block,
    String senderName,
  ) async {
    debugPrint('🤝 _handleFriendshipAccept from $senderName ($senderHex)');

    // Update friendship status
    appStore.dispatch(ProcessFriendshipAcceptAction(
      peerPubkeyHex: senderHex,
      nickname: senderName,
    ));
    debugPrint('🤝 Friendship status updated');

    // Establish friendship in Redux store
    final pubkey = ChatMessage.hexToPubkey(senderHex);
    appStore.dispatch(FriendEstablishedAction(
      publicKey: pubkey,
      nickname: senderName,
    ));

    // Save as a chat message
    appStore.dispatch(SaveChatMessageAction(
      senderPubkeyHex: senderHex,
      recipientPubkeyHex: myHex,
      content: 'Accepted your friend request',
      isOutgoing: false,
      messageType: ChatMessageType.friendRequestAccepted.index,
    ));
    debugPrint('🤝 Chat message saved');

    // UDP connection will be established when ANNOUNCE is received
  }

  /// Handle being unfriended by someone
  Future<void> _handleFriendshipRevoke(String senderHex) async {
    // Silently remove them from our friend list (Redux handles both friendships and peers)
    appStore.dispatch(HandleUnfriendedByAction(senderHex));
    final pubkey = ChatMessage.hexToPubkey(senderHex);
    appStore.dispatch(FriendRemovedAction(pubkey));

    // We don't show any notification to the user - they will just
    // notice the person is no longer in their friends list
  }

  /// Unfriend someone - removes them from our list and notifies them
  Future<void> _unfriend(String peerHex) async {
    if (_grassroots == null) return;

    final pubkey = ChatMessage.hexToPubkey(peerHex);

    // Send the revoke message so they remove us too
    final block = FriendshipRevokeBlock();
    await _grassroots!.send(pubkey, block.serialize());

    // Remove from our friend list (Redux handles both friendships and peers)
    appStore.dispatch(RemoveFriendshipAction(peerHex));
    appStore.dispatch(FriendRemovedAction(pubkey));

    if (mounted) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Text('Removed from friends'),
          duration: Duration(seconds: 2),
        ),
      );
    }
  }

  Future<void> _showMessageNotification(
      String senderHex, String senderName, String content) async {
    const AndroidNotificationDetails androidDetails =
        AndroidNotificationDetails(
      'grassroots_messages',
      'Messages',
      channelDescription: 'Grassroots message notifications',
      importance: Importance.high,
      priority: Priority.high,
    );
    const DarwinNotificationDetails iosDetails = DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
      presentSound: true,
    );
    const NotificationDetails notificationDetails = NotificationDetails(
      android: androidDetails,
      iOS: iosDetails,
    );

    await flutterLocalNotificationsPlugin.show(
      senderHex.hashCode,
      'Message from $senderName',
      content.length > 50 ? '${content.substring(0, 50)}...' : content,
      notificationDetails,
      payload: senderHex,
    );
  }

  Future<void> _showFriendRequestNotification(
      String senderHex, String senderName) async {
    const AndroidNotificationDetails androidDetails =
        AndroidNotificationDetails(
      'grassroots_friend_requests',
      'Friend Requests',
      channelDescription: 'Grassroots friend request notifications',
      importance: Importance.high,
      priority: Priority.high,
    );
    const DarwinNotificationDetails iosDetails = DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
      presentSound: true,
    );
    const NotificationDetails notificationDetails = NotificationDetails(
      android: androidDetails,
      iOS: iosDetails,
    );

    await flutterLocalNotificationsPlugin.show(
      'friend_$senderHex'.hashCode,
      'Friend Request from $senderName',
      '$senderName wants to be friends with you',
      notificationDetails,
      payload: senderHex,
    );
  }

  void _openChat(PeerState peer) {
    final peerHex = peer.pubkeyHex;
    Navigator.of(context).push(
      MaterialPageRoute(
        builder: (context) => ChatScreen(
          peer: peer,
          grassroots: _grassroots!,
          myPubkey: _identity!.publicKey,
          store: appStore,
          onSendFriendRequest: () => _sendFriendRequest(peer),
          onAcceptFriendRequest: () => _acceptFriendRequest(peer),
          onUnfriend: () => _unfriend(peerHex),
          myUdpAddress: _myUdpAddress,
        ),
      ),
    );
  }

  Future<void> _sendFriendRequest(PeerState peer) async {
    if (_grassroots == null || _identity == null) {
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text('Cannot send friend request'),
            duration: Duration(seconds: 2),
          ),
        );
      }
      return;
    }

    final peerHex = peer.pubkeyHex;
    final myHex = ChatMessage.pubkeyToHex(_identity!.publicKey);

    // Create the friendship offer block
    final block = FriendshipOfferBlock(
      message: 'Hey, let\'s be friends!',
    );

    // Send via Grassroots
    final messageId =
        await _grassroots!.send(peer.publicKey, block.serialize());
    if (messageId == null) {
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content:
                Text('Failed to send friend request to ${peer.displayName}'),
            duration: const Duration(seconds: 2),
          ),
        );
      }
      return;
    }

    // Create and record the friend request in Redux
    appStore.dispatch(CreateFriendRequestAction(
      peerPubkeyHex: peerHex,
      nickname: peer.displayName,
    ));

    // Save as a chat message in Redux
    appStore.dispatch(SaveChatMessageAction(
      senderPubkeyHex: myHex,
      recipientPubkeyHex: peerHex,
      content: 'Sent a friend request',
      isOutgoing: true,
      messageId: messageId,
      messageType: ChatMessageType.friendRequestSent.index,
    ));

    if (mounted) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text('Friend request sent to ${peer.displayName}'),
          duration: const Duration(seconds: 2),
        ),
      );
    }
  }

  Future<void> _acceptFriendRequest(PeerState peer) async {
    if (_grassroots == null || _identity == null) {
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text('Cannot accept friend request'),
            duration: Duration(seconds: 2),
          ),
        );
      }
      return;
    }

    final peerHex = peer.pubkeyHex;
    final myHex = ChatMessage.pubkeyToHex(_identity!.publicKey);

    // Accept the friend request in Redux
    appStore.dispatch(AcceptFriendRequestAction(peerHex));

    // Establish friendship in Redux
    appStore.dispatch(FriendEstablishedAction(
      publicKey: peer.publicKey,
      nickname: peer.displayName,
    ));

    // Create the friendship accept block
    final block = FriendshipAcceptBlock();

    // Send via Grassroots (works over BLE)
    final messageId =
        await _grassroots!.send(peer.publicKey, block.serialize());
    if (messageId == null) {
      debugPrint('⚠️ Failed to send friendship accept to ${peer.displayName}');
    }

    // Save as a chat message in Redux
    appStore.dispatch(SaveChatMessageAction(
      senderPubkeyHex: myHex,
      recipientPubkeyHex: peerHex,
      content: 'You accepted the friend request',
      isOutgoing: true,
      messageType: ChatMessageType.friendRequestAcceptedByUs.index,
    ));

    // UDP connection will be established when ANNOUNCE is received
  }

  Future<void> _declineFriendRequest(String peerHex) async {
    appStore.dispatch(DeclineFriendRequestAction(peerHex));

    if (mounted) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Text('Friend request declined'),
          duration: Duration(seconds: 2),
        ),
      );
    }
  }

  String _formatSecondsAgo(DateTime time) {
    final seconds = DateTime.now().difference(time).inSeconds;
    if (seconds < 60) {
      return '${seconds}s ago';
    } else if (seconds < 3600) {
      return '${seconds ~/ 60}m ago';
    } else {
      return '${seconds ~/ 3600}h ago';
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SafeArea(
        child: IndexedStack(
          index: _currentIndex,
          children: [
            _buildChatsTab(),
            _buildAroundTab(),
            _buildProfileTab(),
          ],
        ),
      ),
      bottomNavigationBar: _buildBottomNavBar(),
    );
  }

  Widget _buildBottomNavBar() {
    return Container(
      decoration: BoxDecoration(
        color: const Color(0xFF1B3D2F), // Dark green background
        boxShadow: [
          BoxShadow(
            color: Colors.black.withOpacity(0.2),
            blurRadius: 10,
            offset: const Offset(0, -2),
          ),
        ],
      ),
      child: SafeArea(
        child: Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
          child: Row(
            mainAxisAlignment: MainAxisAlignment.spaceAround,
            children: [
              _buildNavItem(
                icon: Icons.chat_bubble_outline,
                label: 'Chats',
                index: 0,
                badge: _getTotalUnreadCount(),
              ),
              _buildNavItem(
                icon: Icons.radar,
                label: 'Around',
                index: 1,
                isCenter: true,
              ),
              _buildNavItem(
                icon: Icons.person_outline,
                label: 'Profile',
                index: 2,
              ),
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildNavItem({
    required IconData icon,
    required String label,
    required int index,
    int badge = 0,
    bool isCenter = false,
  }) {
    final isSelected = _currentIndex == index;

    // Use orange highlight for selected item
    final Color bgColor =
        isSelected ? const Color(0xFFE8A33C) : Colors.transparent;
    final Color iconColor =
        isSelected ? (isCenter ? Colors.black : Colors.black) : Colors.white54;
    final Color textColor =
        isSelected ? (isCenter ? Colors.black : Colors.black) : Colors.white54;

    return GestureDetector(
      onTap: () => setState(() => _currentIndex = index),
      child: AnimatedContainer(
        duration: const Duration(milliseconds: 200),
        padding: EdgeInsets.symmetric(
          horizontal: isSelected ? 16 : 12,
          vertical: 8,
        ),
        decoration: BoxDecoration(
          color: bgColor,
          borderRadius: BorderRadius.circular(20),
          boxShadow: isSelected
              ? [
                  BoxShadow(
                    color: const Color(0xFFE8A33C).withOpacity(0.4),
                    blurRadius: 8,
                    offset: const Offset(0, 2),
                  ),
                ]
              : null,
        ),
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            Stack(
              clipBehavior: Clip.none,
              children: [
                Icon(
                  icon,
                  color: iconColor,
                  size: 24,
                ),
                if (badge > 0)
                  Positioned(
                    right: -8,
                    top: -4,
                    child: Container(
                      padding: const EdgeInsets.all(4),
                      decoration: const BoxDecoration(
                        color: Colors.red,
                        shape: BoxShape.circle,
                      ),
                      constraints:
                          const BoxConstraints(minWidth: 16, minHeight: 16),
                      child: Text(
                        badge > 99 ? '99+' : badge.toString(),
                        style: const TextStyle(
                          color: Colors.white,
                          fontSize: 10,
                          fontWeight: FontWeight.bold,
                        ),
                        textAlign: TextAlign.center,
                      ),
                    ),
                  ),
              ],
            ),
            const SizedBox(height: 4),
            Text(
              label,
              style: TextStyle(
                color: textColor,
                fontSize: 12,
                fontWeight: isSelected ? FontWeight.w600 : FontWeight.normal,
              ),
            ),
          ],
        ),
      ),
    );
  }

  int _getTotalUnreadCount() {
    return appStore.state.messages.totalUnreadCount;
  }

  // ===== CHATS TAB =====
  Widget _buildChatsTab() {
    final chatsWithMessages = _getChatsWithMessages();
    final pendingRequests = appStore.state.friendships.pendingIncoming;

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        const Padding(
          padding: EdgeInsets.all(16),
          child: Text(
            'Chats',
            style: TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
          ),
        ),
        // Pending friend requests section
        if (pendingRequests.isNotEmpty) ...[
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16),
            child: Row(
              children: [
                const Icon(Icons.person_add,
                    size: 18, color: Color(0xFFE8A33C)),
                const SizedBox(width: 8),
                Text(
                  'Friend Requests (${pendingRequests.length})',
                  style: const TextStyle(
                    fontSize: 14,
                    fontWeight: FontWeight.bold,
                    color: Color(0xFFE8A33C),
                  ),
                ),
              ],
            ),
          ),
          const SizedBox(height: 8),
          SizedBox(
            height: 100,
            child: ListView.builder(
              scrollDirection: Axis.horizontal,
              padding: const EdgeInsets.symmetric(horizontal: 12),
              itemCount: pendingRequests.length,
              itemBuilder: (context, index) {
                final request = pendingRequests[index];
                return _buildFriendRequestCard(request);
              },
            ),
          ),
          const Divider(height: 24),
        ],
        Expanded(
          child: chatsWithMessages.isEmpty
              ? const Center(
                  child: Column(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      Icon(Icons.chat_bubble_outline,
                          size: 64, color: Colors.grey),
                      SizedBox(height: 16),
                      Text(
                        'No chats yet',
                        style: TextStyle(color: Colors.grey, fontSize: 16),
                      ),
                      SizedBox(height: 8),
                      Text(
                        'Start a conversation from\nthe Around tab',
                        textAlign: TextAlign.center,
                        style: TextStyle(color: Colors.grey),
                      ),
                    ],
                  ),
                )
              : ListView.builder(
                  itemCount: chatsWithMessages.length,
                  itemBuilder: (context, index) {
                    final chat = chatsWithMessages[index];
                    return _buildChatListItem(chat);
                  },
                ),
        ),
      ],
    );
  }

  Widget _buildFriendRequestCard(FriendshipState request) {
    return Card(
      margin: const EdgeInsets.symmetric(horizontal: 4),
      color: const Color(0xFF1B3D2F),
      child: Container(
        width: 160,
        padding: const EdgeInsets.all(12),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                CircleAvatar(
                  radius: 16,
                  backgroundColor: Colors.blueGrey,
                  child: Text(
                    request.displayName.isNotEmpty
                        ? request.displayName[0].toUpperCase()
                        : '?',
                    style: const TextStyle(color: Colors.white, fontSize: 14),
                  ),
                ),
                const SizedBox(width: 8),
                Expanded(
                  child: Text(
                    request.displayName,
                    overflow: TextOverflow.ellipsis,
                    style: const TextStyle(fontWeight: FontWeight.bold),
                  ),
                ),
              ],
            ),
            const Spacer(),
            Row(
              mainAxisAlignment: MainAxisAlignment.spaceEvenly,
              children: [
                TextButton(
                  onPressed: () => _declineFriendRequest(request.peerPubkeyHex),
                  style: TextButton.styleFrom(
                    padding: const EdgeInsets.symmetric(horizontal: 8),
                    minimumSize: Size.zero,
                  ),
                  child: const Text('Decline',
                      style: TextStyle(color: Colors.grey)),
                ),
                ElevatedButton(
                  onPressed: () async {
                    // Find peer to accept
                    var peer = _peers.values
                        .where((p) =>
                            ChatMessage.pubkeyToHex(p.publicKey) ==
                            request.peerPubkeyHex)
                        .firstOrNull;

                    if (peer != null) {
                      await _acceptFriendRequest(peer);
                    }
                  },
                  style: ElevatedButton.styleFrom(
                    backgroundColor: const Color(0xFFE8A33C),
                    padding: const EdgeInsets.symmetric(horizontal: 12),
                    minimumSize: Size.zero,
                  ),
                  child: const Text('Accept',
                      style: TextStyle(color: Colors.black)),
                ),
              ],
            ),
          ],
        ),
      ),
    );
  }

  List<_ChatPreview> _getChatsWithMessages() {
    final chats = <_ChatPreview>[];

    // Walk every conversation that has messages — friends, BT peers, UDP-only
    // peers, or any peer we've ever exchanged a message with. The keys are
    // Ed25519 pubkey hexes, so a peer who is BOTH a friend and a BT peer
    // appears once.
    final messagesState = appStore.state.messages;
    final peersMap = appStore.state.peers.peers;
    final friendshipsState = appStore.state.friendships;

    for (final peerHex in messagesState.conversationPeers) {
      final messages = messagesState.getConversation(peerHex);
      if (messages.isEmpty) continue;

      final lastMessage = messages.last;
      // Prefer the full peer record (carries connection state + nickname),
      // fall back to the friendship record for offline friends so the chat
      // list shows the right name even when the peer isn't currently seen.
      final peer = peersMap[peerHex];
      final friendship = friendshipsState.getFriendship(peerHex);

      chats.add(_ChatPreview(
        peerHex: peerHex,
        peer: peer,
        friendship: friendship,
        lastMessage: lastMessage,
        unreadCount: messagesState.getUnreadCount(peerHex),
      ));
    }

    // Sort by last message time (newest first)
    chats.sort(
        (a, b) => b.lastMessage.timestamp.compareTo(a.lastMessage.timestamp));
    return chats;
  }

  Widget _buildChatListItem(_ChatPreview chat) {
    final displayName = chat.displayName;
    final isFriend = chat.isFriend;
    final isOnline = chat.peer?.isReachable ?? false;

    return Dismissible(
      key: ValueKey('chat-${chat.peerHex}'),
      direction: DismissDirection.endToStart,
      background: Container(
        color: Colors.red,
        alignment: Alignment.centerRight,
        padding: const EdgeInsets.symmetric(horizontal: 24),
        child: const Row(
          mainAxisAlignment: MainAxisAlignment.end,
          mainAxisSize: MainAxisSize.min,
          children: [
            Icon(Icons.delete, color: Colors.white),
            SizedBox(width: 8),
            Text(
              'Delete',
              style:
                  TextStyle(color: Colors.white, fontWeight: FontWeight.bold),
            ),
          ],
        ),
      ),
      confirmDismiss: (_) => _confirmDeleteChat(chat),
      onDismissed: (_) {
        appStore.dispatch(DeleteConversationAction(chat.peerHex));
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(
              content: Text('Deleted chat with $displayName'),
              duration: const Duration(seconds: 2),
            ),
          );
        }
      },
      child: ListTile(
        leading: Stack(
          children: [
            CircleAvatar(
              backgroundColor: isFriend ? Colors.blue : Colors.blueGrey,
              child: Text(
                displayName.isNotEmpty ? displayName[0].toUpperCase() : '?',
                style: const TextStyle(color: Colors.white),
              ),
            ),
            if (isOnline)
              Positioned(
                right: 0,
                bottom: 0,
                child: Container(
                  width: 12,
                  height: 12,
                  decoration: BoxDecoration(
                    color: Colors.green,
                    shape: BoxShape.circle,
                    border: Border.all(color: Colors.black, width: 2),
                  ),
                ),
              ),
          ],
        ),
        title: Row(
          children: [
            Expanded(
              child: Row(
                children: [
                  Flexible(
                    child: Text(displayName, overflow: TextOverflow.ellipsis),
                  ),
                  if (isFriend) ...[
                    const SizedBox(width: 4),
                    const Icon(Icons.people, size: 14, color: Colors.blue),
                  ],
                ],
              ),
            ),
            if (chat.unreadCount > 0)
              Container(
                padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 2),
                decoration: BoxDecoration(
                  color: Colors.red,
                  borderRadius: BorderRadius.circular(10),
                ),
                child: Text(
                  chat.unreadCount.toString(),
                  style: const TextStyle(color: Colors.white, fontSize: 12),
                ),
              ),
          ],
        ),
        subtitle: Text(
          _chatPreviewText(chat.lastMessage),
          maxLines: 1,
          overflow: TextOverflow.ellipsis,
          style: TextStyle(
            color: chat.unreadCount > 0 ? Colors.white : Colors.grey,
            fontWeight:
                chat.unreadCount > 0 ? FontWeight.w500 : FontWeight.normal,
          ),
        ),
        trailing: Text(
          _formatMessageTime(chat.lastMessage.timestamp),
          style: const TextStyle(color: Colors.grey, fontSize: 12),
        ),
        onTap: () {
          // Live peer wins; otherwise synthesize a stub from the friendship so
          // the chat opens for offline friends. ChatScreen + grassroots.send
          // only need the pubkey + nickname; if no transport is live, the send
          // path queues messages until a connection resumes.
          final peer = chat.peer ?? _stubPeerFromChatPreview(chat);
          if (peer != null) {
            _openChat(peer);
          }
        },
      ),
    );
  }

  /// Confirm chat deletion. Returns true if the user accepted; the dispatch
  /// happens in `onDismissed` so the snackbar fires after the list animates.
  Future<bool> _confirmDeleteChat(_ChatPreview chat) async {
    final accepted = await showDialog<bool>(
      context: context,
      builder: (dialogCtx) => AlertDialog(
        title: const Text('Delete chat?'),
        content: Text(
          'Delete the entire chat with ${chat.displayName}? '
          'Messages and any photos in this chat will be removed from this device.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(dialogCtx, false),
            child: const Text('Cancel'),
          ),
          TextButton(
            onPressed: () => Navigator.pop(dialogCtx, true),
            style: TextButton.styleFrom(foregroundColor: Colors.red),
            child: const Text('Delete'),
          ),
        ],
      ),
    );
    return accepted ?? false;
  }

  /// Build a synthetic [PeerState] for opening a chat from the chat list when
  /// no live peer record exists (e.g. an offline friend on app startup).
  /// Returns null only when neither a peer record nor a friendship is on file
  /// — in that case there's nothing to talk to.
  PeerState? _stubPeerFromChatPreview(_ChatPreview chat) {
    final friendship = chat.friendship;
    if (friendship == null) return null;
    return PeerState(
      publicKey: _hexStringToBytes(chat.peerHex),
      nickname: friendship.nickname ?? '',
      isFriend: friendship.isAccepted,
      udpAddress: friendship.udpAddress,
    );
  }

  /// One-line preview text for the chat list. Substitutes a short label for
  /// picture messages instead of trying to show the empty `content` field.
  String _chatPreviewText(ChatMessageState m) {
    if (m.messageType == ChatMessageType.picture) {
      if (m.viewOnce) return '🔥 1-time photo';
      return '📷 Photo';
    }
    return m.content;
  }

  /// Decode a 64-char hex pubkey into 32 bytes.
  Uint8List _hexStringToBytes(String hex) {
    return Uint8List.fromList(
      List.generate(
        hex.length ~/ 2,
        (i) => int.parse(hex.substring(i * 2, i * 2 + 2), radix: 16),
      ),
    );
  }

  String _formatMessageTime(DateTime time) {
    final now = DateTime.now();
    final diff = now.difference(time);

    if (diff.inMinutes < 1) return 'now';
    if (diff.inHours < 1) return '${diff.inMinutes}m';
    if (diff.inDays < 1) return '${diff.inHours}h';
    if (diff.inDays < 7) return '${diff.inDays}d';
    return '${time.day}/${time.month}';
  }

  // ===== AROUND TAB =====
  Widget _buildAroundTab() {
    final onlineFriends = appStore.state.peers.onlineFriends;

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Padding(
          padding: const EdgeInsets.all(16),
          child: Row(
            children: [
              const Text(
                'Around',
                style: TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
              ),
              const Spacer(),
              IconButton(
                icon: const Icon(Icons.search),
                onPressed: _showPeerLookupDialog,
                tooltip: 'Look up peer by public key',
              ),
            ],
          ),
        ),
        // Status bar - using StoreConnector to listen to redux state
        StoreConnector<AppState, AppState>(
          converter: (store) => store.state,
          builder: (context, state) {
            return Container(
              margin: const EdgeInsets.symmetric(horizontal: 16),
              padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
              decoration: BoxDecoration(
                color: state.isHealthy
                    ? Colors.green.withOpacity(0.2)
                    : Colors.orange.withOpacity(0.2),
                borderRadius: BorderRadius.circular(8),
              ),
              child: Row(
                children: [
                  Container(
                    width: 8,
                    height: 8,
                    decoration: BoxDecoration(
                      shape: BoxShape.circle,
                      color: state.isHealthy ? Colors.green : Colors.orange,
                    ),
                  ),
                  const SizedBox(width: 8),
                  Text(
                    state.statusDisplayString,
                    style: const TextStyle(fontSize: 13),
                  ),
                  const Spacer(),
                  Text(
                    '${_peers.length} nearby • ${onlineFriends.length} friends online',
                    style: const TextStyle(fontSize: 13, color: Colors.grey),
                  ),
                ],
              ),
            );
          },
        ),
        const SizedBox(height: 16),

        // Online friends section
        if (onlineFriends.isNotEmpty) ...[
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16),
            child: Row(
              children: [
                const Icon(Icons.wifi, size: 18, color: Colors.blue),
                const SizedBox(width: 8),
                Text(
                  'Friends Online (${onlineFriends.length})',
                  style: const TextStyle(
                    fontSize: 14,
                    fontWeight: FontWeight.bold,
                    color: Colors.blue,
                  ),
                ),
              ],
            ),
          ),
          const SizedBox(height: 8),
          SizedBox(
            height: 100,
            child: ListView.builder(
              scrollDirection: Axis.horizontal,
              padding: const EdgeInsets.symmetric(horizontal: 12),
              itemCount: onlineFriends.length,
              itemBuilder: (context, index) {
                final friend = onlineFriends[index];
                return _buildOnlineFriendChip(friend);
              },
            ),
          ),
          const Divider(height: 24),
        ],

        // Nearby peers section header
        Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16),
          child: Row(
            children: [
              const Icon(Icons.bluetooth, size: 18, color: Colors.blueGrey),
              const SizedBox(width: 8),
              Text(
                'Nearby (${_peers.length})',
                style: const TextStyle(
                  fontSize: 14,
                  fontWeight: FontWeight.bold,
                  color: Colors.blueGrey,
                ),
              ),
            ],
          ),
        ),
        const SizedBox(height: 8),

        Expanded(
          child: (_peers.isEmpty)
              ? const Center(
                  child: Column(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      Icon(Icons.radar, size: 64, color: Colors.grey),
                      SizedBox(height: 16),
                      Text(
                        'No peers nearby',
                        style: TextStyle(color: Colors.grey, fontSize: 16),
                      ),
                      SizedBox(height: 8),
                      Text(
                        'Make sure Bluetooth is enabled\non both devices',
                        textAlign: TextAlign.center,
                        style: TextStyle(color: Colors.grey),
                      ),
                    ],
                  ),
                )
              : ListView(
                  children: [
                    if (_peers.isNotEmpty) ...[
                      const Padding(
                        padding:
                            EdgeInsets.symmetric(horizontal: 16, vertical: 8),
                        child: Text('Connected Peers',
                            style: TextStyle(
                                fontWeight: FontWeight.bold,
                                color: Colors.grey,
                                fontSize: 12)),
                      ),
                      ...(_peers.values.toList()
                            ..sort((a, b) =>
                                (b.rssi ?? -100).compareTo(a.rssi ?? -100)))
                          .map((peer) => _buildPeerListItem(peer)),
                    ],
                  ],
                ),
        ),
      ],
    );
  }

  Widget _buildOnlineFriendChip(PeerState friend) {
    return GestureDetector(
      onTap: () {
        _openChat(friend);
      },
      child: Container(
        margin: const EdgeInsets.symmetric(horizontal: 4),
        padding: const EdgeInsets.all(12),
        decoration: BoxDecoration(
          color: Colors.blue.withOpacity(0.1),
          borderRadius: BorderRadius.circular(12),
          border: Border.all(color: Colors.blue.withOpacity(0.3)),
        ),
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            Stack(
              children: [
                CircleAvatar(
                  radius: 20,
                  backgroundColor: Colors.blueGrey,
                  child: Text(
                    friend.displayName.isNotEmpty
                        ? friend.displayName[0].toUpperCase()
                        : '?',
                    style: const TextStyle(color: Colors.white),
                  ),
                ),
                Positioned(
                  right: 0,
                  bottom: 0,
                  child: Container(
                    width: 12,
                    height: 12,
                    decoration: BoxDecoration(
                      color: Colors.green,
                      shape: BoxShape.circle,
                      border: Border.all(color: Colors.black, width: 2),
                    ),
                  ),
                ),
              ],
            ),
            const SizedBox(height: 4),
            Text(
              friend.displayName,
              style: const TextStyle(fontSize: 12),
              overflow: TextOverflow.ellipsis,
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildPeerListItem(PeerState peer) {
    final peerHex = peer.pubkeyHex;
    final unreadCount = appStore.state.messages.getUnreadCount(peerHex);
    final friendship = appStore.state.friendships.getFriendship(peerHex);
    final isFriend = friendship?.isAccepted ?? false;
    final hasPendingRequest =
        appStore.state.friendships.hasPendingRequest(peerHex);

    // RSSI is unavailable for some peripheral-role BLE links because the OS
    // does not expose remote signal strength to the GATT server side.
    final rssiDbm = peer.rssi;
    final IconData signalIcon;
    final Color signalColor;
    if (rssiDbm == null) {
      signalIcon = Icons.signal_cellular_alt;
      signalColor = Colors.grey;
    } else if (rssiDbm < -80) {
      signalIcon = Icons.signal_cellular_alt_1_bar;
      signalColor = Colors.red;
    } else if (rssiDbm < -60) {
      signalIcon = Icons.signal_cellular_alt_2_bar;
      signalColor = Colors.orange;
    } else {
      signalIcon = Icons.signal_cellular_alt;
      signalColor = Colors.green;
    }

    // Check if this peer has a recent nickname change
    final nicknameChange = _nicknameChanges[peerHex];
    final isChanging = nicknameChange != null;

    return Card(
      margin: const EdgeInsets.symmetric(horizontal: 16, vertical: 4),
      child: AnimatedContainer(
        duration: const Duration(milliseconds: 300),
        decoration: BoxDecoration(
          borderRadius: BorderRadius.circular(12),
          border: isChanging
              ? Border.all(color: const Color(0xFFE8A33C), width: 2)
              : null,
        ),
        child: ListTile(
          leading: Stack(
            children: [
              CircleAvatar(
                backgroundColor: isFriend ? Colors.blue : Colors.blueGrey,
                child: AnimatedSwitcher(
                  duration: const Duration(milliseconds: 300),
                  child: Text(
                    peer.displayName.isNotEmpty
                        ? peer.displayName[0].toUpperCase()
                        : '?',
                    key: ValueKey(peer.displayName),
                    style: const TextStyle(color: Colors.white),
                  ),
                ),
              ),
              if (unreadCount > 0)
                Positioned(
                  right: 0,
                  top: 0,
                  child: Container(
                    padding: const EdgeInsets.all(2),
                    decoration: BoxDecoration(
                      color: Colors.red,
                      borderRadius: BorderRadius.circular(10),
                    ),
                    constraints:
                        const BoxConstraints(minWidth: 16, minHeight: 16),
                    child: Text(
                      unreadCount > 99 ? '99+' : unreadCount.toString(),
                      style: const TextStyle(
                          color: Colors.white,
                          fontSize: 10,
                          fontWeight: FontWeight.bold),
                      textAlign: TextAlign.center,
                    ),
                  ),
                ),
              if (isFriend)
                Positioned(
                  right: 0,
                  bottom: 0,
                  child: Container(
                    padding: const EdgeInsets.all(2),
                    decoration: const BoxDecoration(
                      color: Colors.blue,
                      shape: BoxShape.circle,
                    ),
                    child:
                        const Icon(Icons.star, color: Colors.white, size: 10),
                  ),
                ),
            ],
          ),
          title: Row(
            children: [
              Expanded(
                child: AnimatedSwitcher(
                  duration: const Duration(milliseconds: 500),
                  transitionBuilder: (child, animation) {
                    return FadeTransition(
                      opacity: animation,
                      child: SlideTransition(
                        position: Tween<Offset>(
                          begin: const Offset(0, 0.5),
                          end: Offset.zero,
                        ).animate(animation),
                        child: child,
                      ),
                    );
                  },
                  child: Row(
                    key: ValueKey(peer.displayName),
                    children: [
                      Flexible(
                        child: Text(
                          peer.displayName,
                          style: TextStyle(
                            color: isChanging ? const Color(0xFFE8A33C) : null,
                            fontWeight: isChanging
                                ? FontWeight.bold
                                : FontWeight.normal,
                          ),
                          overflow: TextOverflow.ellipsis,
                        ),
                      ),
                      if (isFriend) ...[
                        const SizedBox(width: 4),
                        const Icon(Icons.people, size: 14, color: Colors.blue),
                      ],
                    ],
                  ),
                ),
              ),
              peer.activeTransport.icon,
            ],
          ),
          subtitle: Text(
            peer.lastBleSeen != null
                ? 'Last seen: ${_formatSecondsAgo(peer.lastBleSeen!)}'
                : 'Connecting...',
          ),
          trailing: Row(
            mainAxisSize: MainAxisSize.min,
            children: [
              Column(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Icon(signalIcon, color: signalColor, size: 20),
                  Text(
                    rssiDbm == null ? '-- dBm' : '$rssiDbm dBm',
                    style: TextStyle(fontSize: 10, color: signalColor),
                  ),
                ],
              ),
              const SizedBox(width: 8),
              // Friend request / Chat button
              if (!isFriend && !hasPendingRequest)
                IconButton(
                  icon: const Icon(Icons.person_add_outlined),
                  color: const Color(0xFFE8A33C),
                  tooltip: 'Send friend request',
                  onPressed: () => _sendFriendRequest(peer),
                )
              else if (hasPendingRequest)
                const Padding(
                  padding: EdgeInsets.all(8.0),
                  child:
                      Icon(Icons.hourglass_empty, color: Colors.grey, size: 20),
                ),
              IconButton(
                icon: const Icon(Icons.chat_bubble_outline),
                color: Colors.blue,
                onPressed: () => _openChat(peer),
              ),
            ],
          ),
          onTap: () => _openChat(peer),
        ),
      ),
    );
  }

  void _showPeerLookupDialog() {
    final controller = TextEditingController();

    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Look up Peer'),
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            const Text(
              'Enter public key (hex) to check if peer is reachable:',
              style: TextStyle(fontSize: 14),
            ),
            const SizedBox(height: 16),
            TextField(
              controller: controller,
              decoration: const InputDecoration(
                hintText: 'Public key (64 hex chars)',
                border: OutlineInputBorder(),
              ),
              maxLines: 2,
            ),
          ],
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: const Text('Cancel'),
          ),
          ElevatedButton(
            onPressed: () {
              Navigator.pop(context);
              _lookupPeer(controller.text.trim());
            },
            child: const Text('Look up'),
          ),
        ],
      ),
    );
  }

  void _lookupPeer(String hexPubkey) {
    if (hexPubkey.isEmpty || _grassroots == null) return;

    try {
      // Convert hex to bytes
      final pubkeyBytes = Uint8List.fromList(
        List.generate(
          hexPubkey.length ~/ 2,
          (i) => int.parse(hexPubkey.substring(i * 2, i * 2 + 2), radix: 16),
        ),
      );

      final isReachable = _grassroots!.isPeerReachable(pubkeyBytes);
      final peer = _grassroots!.getPeer(pubkeyBytes);

      showDialog(
        context: context,
        builder: (context) => AlertDialog(
          title:
              Text(isReachable ? '✅ Peer Reachable' : '❌ Peer Not Reachable'),
          content: Column(
            mainAxisSize: MainAxisSize.min,
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              if (peer != null) ...[
                Text('Nickname: ${peer.displayName}'),
                Text('Status: ${peer.connectionState.name}'),
                if (peer.rssi != null) Text('Signal: ${peer.rssi} dBm'),
                if (peer.lastSeen != null)
                  Text('Last seen: ${_formatSecondsAgo(peer.lastSeen!)}'),
              ] else
                const Text('Peer not found in known peers list.'),
            ],
          ),
          actions: [
            TextButton(
              onPressed: () => Navigator.pop(context),
              child: const Text('OK'),
            ),
            if (peer != null)
              ElevatedButton(
                onPressed: () {
                  Navigator.pop(context);
                  _openChat(peer);
                },
                child: const Text('Open Chat'),
              ),
          ],
        ),
      );
    } catch (e) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('Invalid public key format: $e')),
      );
    }
  }

  // ===== PROFILE TAB =====
  Widget _buildProfileTab() {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              const Text(
                'Profile',
                style: TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
              ),
              const Spacer(),
              IconButton(
                icon: const Icon(Icons.settings),
                onPressed: _openSettings,
                tooltip: 'Settings',
              ),
            ],
          ),
          const SizedBox(height: 24),

          // Avatar
          Center(
            child: CircleAvatar(
              radius: 50,
              backgroundColor: Colors.blueGrey,
              child: Text(
                _identity?.nickname.isNotEmpty == true
                    ? _identity!.nickname[0].toUpperCase()
                    : '?',
                style: const TextStyle(fontSize: 40, color: Colors.white),
              ),
            ),
          ),
          const SizedBox(height: 16),

          // Nickname with edit button
          Center(
            child: Row(
              mainAxisSize: MainAxisSize.min,
              children: [
                Text(
                  _identity?.nickname ?? 'Loading...',
                  style: const TextStyle(
                      fontSize: 24, fontWeight: FontWeight.bold),
                ),
                const SizedBox(width: 8),
                IconButton(
                  icon: const Icon(Icons.edit, size: 20),
                  onPressed: _showEditNicknameDialog,
                  tooltip: 'Edit nickname',
                ),
              ],
            ),
          ),
          const SizedBox(height: 32),

          // Info cards
          _buildInfoCard(
            title: 'Fingerprint',
            value: _identity?.shortFingerprint ?? '...',
            icon: Icons.fingerprint,
            onCopy: () => _copyToClipboard(_identity?.shortFingerprint ?? ''),
          ),
          const SizedBox(height: 12),

          _buildInfoCard(
            title: 'Service UUID',
            value: _identity?.bleServiceUuid ?? '...',
            icon: Icons.bluetooth,
            onCopy: () => _copyToClipboard(_identity?.bleServiceUuid ?? ''),
          ),
          const SizedBox(height: 12),

          _buildInfoCard(
            title: 'Public Key',
            value: _identity != null
                ? ChatMessage.pubkeyToHex(_identity!.publicKey)
                : '...',
            icon: Icons.key,
            onCopy: () => _copyToClipboard(_identity != null
                ? ChatMessage.pubkeyToHex(_identity!.publicKey)
                : ''),
            maxLines: 2,
          ),
          const SizedBox(height: 12),

          // Regenerate identity (dev/testing affordance).
          // Re-rolls the Ed25519 keypair so peers treat this install as a new
          // device without requiring uninstall + reinstall.
          OutlinedButton.icon(
            icon: const Icon(Icons.refresh),
            label: const Text('Regenerate Identity'),
            style: OutlinedButton.styleFrom(
              foregroundColor: Colors.red,
              side: const BorderSide(color: Colors.red),
              minimumSize: const Size.fromHeight(44),
            ),
            onPressed: _identity == null ? null : _regenerateIdentity,
          ),
          const SizedBox(height: 12),

          // Transport status card
          _buildTransportStatusCard(),
          const SizedBox(height: 12),

          // Settings shortcut card
          _buildSettingsCard(),
        ],
      ),
    );
  }

  Widget _buildTransportStatusCard() {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                StoreConnector<AppState, bool>(
                  converter: (store) => store.state.isHealthy,
                  builder: (context, isHealthy) => Icon(
                    isHealthy ? Icons.check_circle : Icons.error,
                    color: isHealthy ? Colors.green : Colors.orange,
                  ),
                ),
                const SizedBox(width: 8),
                const Text(
                  'Transport Status',
                  style: TextStyle(fontWeight: FontWeight.bold),
                ),
              ],
            ),
            const SizedBox(height: 12),

            // BLE status
            _buildTransportStatusRow(
              icon: Icons.bluetooth,
              iconColor: Colors.blue,
              name: 'Bluetooth',
              enabled: appStore.state.settings.bluetoothEnabled,
              available: _bleAvailable,
            ),
            const SizedBox(height: 8),

            // UDP status
            _buildTransportStatusRow(
              icon: Icons.public,
              iconColor: Colors.green,
              name: 'Internet (UDP)',
              enabled: appStore.state.settings.udpEnabled,
              available: _udpAvailable,
            ),

            const Divider(height: 24),

            Text(
              '${_peers.length} connected peers',
              style: const TextStyle(color: Colors.grey),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildTransportStatusRow({
    required IconData icon,
    required Color iconColor,
    required String name,
    required bool enabled,
    required bool available,
  }) {
    final isActive = enabled && available;

    return Row(
      children: [
        Icon(
          icon,
          size: 18,
          color: isActive ? iconColor : Colors.grey,
        ),
        const SizedBox(width: 8),
        Expanded(
          child: Text(
            name,
            style: TextStyle(
              color: isActive ? Colors.white : Colors.grey,
            ),
          ),
        ),
        Container(
          padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 2),
          decoration: BoxDecoration(
            color: isActive
                ? Colors.green.withOpacity(0.2)
                : (enabled
                    ? Colors.orange.withOpacity(0.2)
                    : Colors.grey.withOpacity(0.2)),
            borderRadius: BorderRadius.circular(4),
          ),
          child: Text(
            isActive ? 'Active' : (enabled ? 'Unavailable' : 'Disabled'),
            style: TextStyle(
              fontSize: 11,
              color: isActive
                  ? Colors.green
                  : (enabled ? Colors.orange : Colors.grey),
            ),
          ),
        ),
      ],
    );
  }

  Widget _buildSettingsCard() {
    return Card(
      child: ListTile(
        leading: const Icon(Icons.settings, color: Color(0xFFE8A33C)),
        title: const Text('Transport Settings'),
        subtitle: const Text('Configure Bluetooth and Internet protocols'),
        trailing: const Icon(Icons.chevron_right),
        onTap: _openSettings,
      ),
    );
  }

  void _openSettings() {
    Navigator.of(context).push(
      MaterialPageRoute(
        builder: (context) => SettingsScreen(
          store: appStore,
          onSettingsChanged: () {
            setState(() {});
          },
          onAddRendezvousServer: _grassroots == null
              ? null
              : (address, pubkeyHex) => _grassroots!.addRendezvousServer(
                    address: address,
                    pubkeyHex: pubkeyHex,
                  ),
          onRemoveRendezvousServer: _grassroots == null
              ? null
              : (address, pubkeyHex) => _grassroots!.removeRendezvousServer(
                    address: address,
                    pubkeyHex: pubkeyHex,
                  ),
          onBleRoleModeChanged: _grassroots == null
              ? null
              : (mode) => _grassroots!.setBleRoleMode(mode),
          onRetryPublicAddressDiscovery: _grassroots == null
              ? null
              : () => _grassroots!.retryPublicAddressDiscovery(),
        ),
      ),
    );
  }

  Widget _buildInfoCard({
    required String title,
    required String value,
    required IconData icon,
    VoidCallback? onCopy,
    int maxLines = 1,
  }) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                Icon(icon, size: 20, color: Colors.grey),
                const SizedBox(width: 8),
                Text(
                  title,
                  style: const TextStyle(color: Colors.grey, fontSize: 12),
                ),
                const Spacer(),
                if (onCopy != null)
                  IconButton(
                    icon: const Icon(Icons.copy, size: 18),
                    onPressed: onCopy,
                    tooltip: 'Copy',
                    padding: EdgeInsets.zero,
                    constraints: const BoxConstraints(),
                  ),
              ],
            ),
            const SizedBox(height: 4),
            Text(
              value,
              style: const TextStyle(fontSize: 14, fontFamily: 'monospace'),
              maxLines: maxLines,
              overflow: TextOverflow.ellipsis,
            ),
          ],
        ),
      ),
    );
  }

  void _copyToClipboard(String text) {
    Clipboard.setData(ClipboardData(text: text));
    ScaffoldMessenger.of(context).showSnackBar(
      const SnackBar(content: Text('Copied to clipboard')),
    );
  }

  void _showEditNicknameDialog() {
    if (_identity == null) return;

    final controller = TextEditingController(text: _identity!.nickname);

    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Edit Nickname'),
        content: TextField(
          controller: controller,
          decoration: const InputDecoration(
            hintText: 'Enter new nickname',
            border: OutlineInputBorder(),
          ),
          autofocus: true,
          maxLength: 30,
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: const Text('Cancel'),
          ),
          ElevatedButton(
            onPressed: () async {
              final newNickname = controller.text.trim();
              if (newNickname.isNotEmpty && _grassroots != null) {
                Navigator.pop(context);
                await _updateNickname(newNickname);
              }
            },
            child: const Text('Save'),
          ),
        ],
      ),
    );
  }

  Future<void> _updateNickname(String newNickname) async {
    if (_grassroots == null || _identity == null) return;

    // Update nickname via Grassroots (broadcasts ANNOUNCE)
    await _grassroots!.updateNickname(newNickname);

    // Persist to secure storage
    await IdentityStore.putIdentity(_identity!);

    setState(() {});

    ScaffoldMessenger.of(context).showSnackBar(
      const SnackBar(content: Text('Nickname updated!')),
    );
  }

  /// Generate a brand-new Ed25519 keypair and restart Grassroots under the new
  /// identity. Useful for testing discovery and identity reset behavior
  /// without an uninstall+reinstall cycle.
  Future<void> _regenerateIdentity() async {
    final confirmed = await showDialog<bool>(
      context: context,
      builder: (ctx) => AlertDialog(
        title: const Text('Regenerate Identity?'),
        content: const Text(
          'This generates a fresh Ed25519 keypair and rebuilds the Grassroots '
          'transport under the new identity. Your peers will see you as a '
          'new device — existing friendships, discovered peers and BLE '
          'connection state are dropped.\n\n'
          'Useful for testing identity reset behavior without uninstall + '
          'reinstall.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(ctx, false),
            child: const Text('Cancel'),
          ),
          ElevatedButton(
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.red,
              foregroundColor: Colors.white,
            ),
            onPressed: () => Navigator.pop(ctx, true),
            child: const Text('Regenerate'),
          ),
        ],
      ),
    );
    if (confirmed != true) return;
    if (!mounted) return;

    // Tear down the current Grassroots instance before swapping identities.
    // dispose() stops advertising, closes GATT, cancels timers, and flushes
    // streams — without this we'd run two transports concurrently with
    // overlapping characteristic UUIDs, which gets confusing fast.
    final oldGrassroots = _grassroots;
    setState(() {
      _grassroots = null;
      _identity = null;
    });
    await oldGrassroots?.dispose();

    // Fresh keypair (same shape as _initIdentity for a clean install).
    final newIdentity = await GrassrootsIdentity.generate();

    // Persist immediately so a crash mid-restart doesn't leave us with a
    // stored identity that doesn't match anything in memory.
    await IdentityStore.putIdentity(newIdentity);

    // Rebuild the transport with the new identity (mirrors _initialize).
    final newGrassroots = GrassrootsNetwork(
      identity: newIdentity,
      store: appStore,
      sodium: appSodium,
    );
    newGrassroots.onMessageReceived =
        (messageId, senderPubkey, payload, transport) {
      _handleIncomingMessage(messageId, senderPubkey, payload, transport);
    };

    if (!mounted) return;
    setState(() {
      _identity = newIdentity;
      _grassroots = newGrassroots;
    });

    final ok = await newGrassroots.initialize();
    if (!ok) {
      debugPrint('Grassroots re-initialization failed after identity regen');
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text('Identity regenerated but Grassroots init failed'),
            backgroundColor: Colors.red,
          ),
        );
      }
      return;
    }

    if (!mounted) return;
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text('New identity: ${newIdentity.nickname} '
            '(${newIdentity.shortFingerprint})'),
      ),
    );
  }

  void _showNicknameChangeAnimation(
      String oldName, String newName, String peerId) {
    // Store the nickname change for UI animation
    _nicknameChanges[peerId] = _NicknameChange(
      oldName: oldName,
      newName: newName,
      timestamp: DateTime.now(),
    );

    // Show a snackbar notification
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Row(
          children: [
            const Icon(Icons.person, color: Colors.white),
            const SizedBox(width: 12),
            Expanded(
              child: RichText(
                text: TextSpan(
                  style: const TextStyle(color: Colors.white),
                  children: [
                    TextSpan(
                      text: oldName.isEmpty ? 'Unknown' : oldName,
                      style: const TextStyle(
                        fontWeight: FontWeight.bold,
                        decoration: TextDecoration.lineThrough,
                        color: Colors.white70,
                      ),
                    ),
                    const TextSpan(text: ' → '),
                    TextSpan(
                      text: newName,
                      style: const TextStyle(fontWeight: FontWeight.bold),
                    ),
                  ],
                ),
              ),
            ),
          ],
        ),
        duration: const Duration(seconds: 3),
        behavior: SnackBarBehavior.floating,
        backgroundColor: const Color(0xFF1B3D2F),
      ),
    );

    // Clear the animation after a delay
    Future.delayed(const Duration(seconds: 3), () {
      if (mounted) {
        setState(() {
          _nicknameChanges.remove(peerId);
        });
      }
    });
  }
}

/// Helper class for tracking nickname changes
class _NicknameChange {
  final String oldName;
  final String newName;
  final DateTime timestamp;

  _NicknameChange({
    required this.oldName,
    required this.newName,
    required this.timestamp,
  });
}

/// Helper class for chat list preview
class _ChatPreview {
  final String peerHex;

  /// Full peer record, when one exists. Null for an offline friend whose
  /// `PeerState` hasn't been re-hydrated yet (e.g. fresh app start, no live
  /// connection, but the conversation history is loaded from disk).
  final PeerState? peer;

  /// Friendship record, when this peer is on our friends list. Used as the
  /// nickname source when [peer] is null.
  final FriendshipState? friendship;

  final ChatMessageState lastMessage;
  final int unreadCount;

  _ChatPreview({
    required this.peerHex,
    required this.peer,
    required this.friendship,
    required this.lastMessage,
    required this.unreadCount,
  });

  /// Display name from peer (live), then friendship (offline friend),
  /// then a truncated pubkey as last resort.
  String get displayName {
    if (peer != null && peer!.displayName.isNotEmpty) return peer!.displayName;
    final fNick = friendship?.nickname;
    if (fNick != null && fNick.isNotEmpty) return fNick;
    return 'Peer ${peerHex.substring(0, 8)}...';
  }

  bool get isFriend => friendship?.isAccepted ?? false;
}
