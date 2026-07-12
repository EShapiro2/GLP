import 'dart:async';
import 'dart:io';
import 'dart:typed_data';
import 'dart:ui';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:grassroots_networking/grassroots_networking.dart';
import 'package:image_picker/image_picker.dart';
import 'package:redux/redux.dart';
import 'chat_models.dart';
import 'package:logger/logger.dart';
import 'package:uuid/uuid.dart';

/// Chat screen for a conversation with a specific peer
class ChatScreen extends StatefulWidget {
  final GrassrootsNetwork grassroots;
  final PeerState peer;
  final Uint8List myPubkey;
  final Store<AppState> store;
  final VoidCallback? onSendFriendRequest;
  final VoidCallback? onAcceptFriendRequest;
  final VoidCallback? onUnfriend;
  final String? myUdpAddress;

  const ChatScreen({
    super.key,
    required this.grassroots,
    required this.peer,
    required this.myPubkey,
    required this.store,
    this.onSendFriendRequest,
    this.onAcceptFriendRequest,
    this.onUnfriend,
    this.myUdpAddress,
  });

  @override
  State<ChatScreen> createState() => _ChatScreenState();
}

class _ChatScreenState extends State<ChatScreen> {
  final Logger _log = Logger();

  final TextEditingController _messageController = TextEditingController();
  final ScrollController _scrollController = ScrollController();
  StreamSubscription<AppState>? _storeSubscription;
  final ImagePicker _imagePicker = ImagePicker();

  /// True while a picture is being compressed and queued for send. Drives the
  /// indeterminate progress bar above the composer (BLE fragmentation can
  /// take seconds per image).
  bool _sendingMedia = false;

  /// True when the list is scrolled far enough up that the latest message is
  /// off-screen — drives the floating "jump to latest" button at the bottom
  /// right. We only auto-scroll on outgoing sends; otherwise an incoming
  /// message or an unrelated Redux dispatch (RSSI tick, status change, …)
  /// would yank the user back to the bottom while they're reading history.
  bool _showScrollDownButton = false;

  /// How many pixels from the bottom we still treat as "at the bottom".
  /// Tolerates a small gap so the button doesn't flicker on inertia.
  static const double _atBottomThreshold = 80.0;

  /// Message ids we've already fired a read receipt for during this screen's
  /// lifetime. Read receipts are best-effort UX info, not delivery-critical,
  /// so we only track in-memory: closing and reopening the chat will re-send
  /// receipts (the peer dedupes by messageId).
  final Set<String> _sentReadReceiptIds = {};

  String get _peerHex => ChatMessage.pubkeyToHex(widget.peer.publicKey);
  String get _myHex => ChatMessage.pubkeyToHex(widget.myPubkey);

  FriendshipState? get _friendship =>
      widget.store.state.friendships.getFriendship(_peerHex);
  bool get _isFriend => _friendship?.isAccepted ?? false;
  bool get _hasPendingIncoming => _friendship?.isPendingIncoming ?? false;
  bool get _hasPendingOutgoing => _friendship?.isPendingOutgoing ?? false;

  @override
  void initState() {
    super.initState();
    // Listen to Redux store for all state updates
    _storeSubscription = widget.store.onChange.listen((_) => _onStoreChanged());
    // Track whether we're at the bottom so we can toggle the jump-to-latest
    // button. The ListView gives us pixels-from-the-bottom via the controller.
    _scrollController.addListener(_onScrollChanged);
    // Mark every already-loaded incoming message as read and fire receipts.
    // Subsequent incoming messages are receipted from `_onStoreChanged`.
    _flushReadReceipts();
    // Android can kill the Flutter Activity while the camera app is in the
    // foreground (memory pressure). When we come back, the original
    // pickImage future is dead but the captured file is recoverable here.
    unawaited(_recoverLostPickedMedia());
  }

  /// Android-only: recover a picked image whose pickImage future was killed
  /// when the Flutter Activity was reclaimed during camera capture. iOS does
  /// not need this — the UIImagePickerController is hosted in our process.
  Future<void> _recoverLostPickedMedia() async {
    if (!Platform.isAndroid) return;
    final LostDataResponse lost;
    try {
      lost = await _imagePicker.retrieveLostData();
    } catch (e) {
      debugPrint('[picture-send] retrieveLostData failed: $e');
      return;
    }
    if (lost.isEmpty) return;
    if (lost.exception != null) {
      debugPrint('[picture-send] lost data exception: ${lost.exception}');
      return;
    }
    final file = lost.file;
    if (file == null) return;
    debugPrint('[picture-send] recovered lost file: ${file.path}');
    if (!mounted) return;
    // The viewOnce flag was lost when the Activity died. Confirm with the
    // user before sending so a recovery doesn't silently downgrade privacy.
    final proceed = await showDialog<bool>(
      context: context,
      builder: (ctx) => AlertDialog(
        title: const Text('Recovered photo'),
        content: const Text(
          'Your last photo capture was interrupted. Send it now? '
          '(1-time view will not be applied to the recovered photo.)',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(ctx, false),
            child: const Text('Discard'),
          ),
          TextButton(
            onPressed: () => Navigator.pop(ctx, true),
            child: const Text('Send'),
          ),
        ],
      ),
    );
    if (proceed == true) {
      await _sendPicture(file, viewOnce: false);
    }
  }

  /// Send a read receipt for every incoming message in the conversation we
  /// haven't already receipted during this screen's lifetime, and clear the
  /// per-peer unread badge.
  ///
  /// Called on initial open AND on every Redux store change while the screen
  /// is mounted. The dedupe set ensures we only send one receipt per
  /// messageId per session — without it every dispatch (~once a second
  /// between RSSI ticks, status updates, etc.) would re-send a receipt for
  /// every old message.
  ///
  /// The semantics are "while this chat is open, every visible incoming
  /// message has been read by the user" — a small generosity (it counts
  /// messages off-screen too) that matches the user's expectation that
  /// staying in the conversation means they're caught up.
  void _flushReadReceipts() {
    final messages = widget.store.state.messages.getConversation(_peerHex);
    final senderPubkey = widget.peer.publicKey;

    for (final message in messages) {
      if (message.isOutgoing) continue;
      final id = message.messageId;
      if (id == null) continue;
      if (!_sentReadReceiptIds.add(id)) continue;
      widget.grassroots.sendReadReceipt(
        messageId: id,
        senderPubkey: senderPubkey,
      );
    }

    // Keep the unread counter at zero while the chat is open so a freshly
    // arrived incoming message doesn't briefly show a badge on the
    // conversation list. Guarded to avoid a dispatch loop: only fire when
    // there's actually something to clear.
    if (widget.store.state.messages.getUnreadCount(_peerHex) > 0) {
      widget.store.dispatch(MarkMessagesReadAction(_peerHex));
    }
  }

  @override
  void dispose() {
    _storeSubscription?.cancel();
    _scrollController.removeListener(_onScrollChanged);
    _messageController.dispose();
    _scrollController.dispose();
    super.dispose();
  }

  void _onStoreChanged() {
    // Rebuild to update messages, friendships, and status checkmarks.
    // Intentionally do NOT auto-scroll here: store changes fire on every
    // dispatch (RSSI ticks, status updates, queue events, …) and snapping
    // to the bottom would prevent the user from reading older messages.
    // Scrolling-on-send is handled explicitly by `_sendMessage` /
    // `_sendPicture`. For incoming messages the user discovers them via
    // the floating jump-to-latest button instead.
    if (!mounted) return;
    setState(() {});
    // While the chat is open, every incoming message counts as read. The
    // dedupe set in `_flushReadReceipts` keeps us from re-sending a receipt
    // every dispatch (peer-RSSI ticks etc.), so this is cheap.
    _flushReadReceipts();
  }

  void _onScrollChanged() {
    if (!_scrollController.hasClients) return;
    final position = _scrollController.position;
    // `maxScrollExtent - pixels` is the gap below the current viewport.
    // 0 means flush at the bottom; >threshold means the latest message is
    // off-screen.
    final atBottom =
        (position.maxScrollExtent - position.pixels) <= _atBottomThreshold;
    if (atBottom == !_showScrollDownButton) return; // no flip needed
    if (!mounted) return;
    setState(() => _showScrollDownButton = !atBottom);
  }

  static const _uuid = Uuid();

  void _sendMessage() {
    final text = _messageController.text.trim();
    if (text.isEmpty) return;

    _messageController.clear();

    // Generate message ID upfront so we can show the message immediately
    // Full UUID — messageId is encoded as a 16-byte packet id on the wire,
// and as a 36-char string in the FRAGMENT_START header. An 8-char prefix
// would corrupt either format.
    final messageId = _uuid.v4();

    // Save to conversation immediately so the message appears in the UI
    widget.store.dispatch(SaveChatMessageAction(
      senderPubkeyHex: _myHex,
      recipientPubkeyHex: _peerHex,
      content: text,
      isOutgoing: true,
      messageId: messageId,
    ));

    _scrollToBottom();

    // Send in the background — status updates (sending → sent/delivered/failed)
    // will be dispatched by GrassrootsNetwork.send() and reflected via the status icon
    final block = TextSayBlock(content: text);
    debugPrint("Sending '$text' to peer ${widget.peer.displayName}");
    widget.grassroots
        .send(widget.peer.publicKey, block.serialize(), messageId: messageId);
  }

  /// Open a small bottom sheet to pick an image source (camera or gallery)
  /// and an optional 1-time view toggle, then send the chosen photo.
  Future<void> _openAttachmentSheet() async {
    bool viewOnce = false;
    final source = await showModalBottomSheet<ImageSource>(
      context: context,
      builder: (sheetContext) {
        return SafeArea(
          child: StatefulBuilder(
            builder: (sbContext, setSheet) => Column(
              mainAxisSize: MainAxisSize.min,
              children: [
                ListTile(
                  leading: const Icon(Icons.camera_alt),
                  title: const Text('Camera'),
                  onTap: () => Navigator.pop(sheetContext, ImageSource.camera),
                ),
                ListTile(
                  leading: const Icon(Icons.photo_library),
                  title: const Text('Photo library'),
                  onTap: () => Navigator.pop(sheetContext, ImageSource.gallery),
                ),
                const Divider(height: 1),
                SwitchListTile(
                  secondary: const Icon(Icons.local_fire_department),
                  title: const Text('Send as 1-time view'),
                  subtitle: const Text(
                      'Recipient sees a blurred preview; deletes after one view'),
                  value: viewOnce,
                  onChanged: (v) => setSheet(() => viewOnce = v),
                ),
              ],
            ),
          ),
        );
      },
    );
    if (source == null) {
      debugPrint('[picture-send] attachment sheet dismissed without choice');
      return;
    }
    debugPrint('[picture-send] source chosen: $source, viewOnce=$viewOnce');

    // No app-level permission request: image_picker handles camera permission
    // natively on both platforms.
    //  - iOS: UIImagePickerController triggers the OS prompt the first time;
    //    if the user denied previously, iOS shows its standard
    //    "Camera access is denied — Settings" alert.
    //  - Android: ACTION_IMAGE_CAPTURE runs in the system camera app's own
    //    process, so the calling app doesn't need a runtime CAMERA grant.

    XFile? picked;
    try {
      picked = await _imagePicker.pickImage(source: source);
    } catch (e, st) {
      _log.e('[picture-send] pickImage threw', error: e, stackTrace: st);
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(content: Text('Image picker failed: $e')),
        );
      }
      return;
    }
    if (picked == null) {
      debugPrint('[picture-send] pickImage returned null — user cancelled, OR '
          'Activity was killed while in $source. Will retry retrieveLostData on next entry.');
      return;
    }
    debugPrint('[picture-send] picker returned: ${picked.path}');

    await _sendPicture(picked, viewOnce: viewOnce);
  }

  Future<void> _sendPicture(XFile picked, {required bool viewOnce}) async {
    if (_sendingMedia) return;
    setState(() => _sendingMedia = true);
    try {
      debugPrint('[picture-send] reading bytes from ${picked.path}');
      final raw = await picked.readAsBytes();
      debugPrint('[picture-send] raw bytes: ${raw.length}');

      // Aggressive compression: BLE moves ~15 KB/s with 20ms inter-fragment
      // delays, so unconstrained photos take minutes. Target ~100 KB.
      final compressed = await compressForBle(raw);
      debugPrint(
          '[picture-send] compressed: ${compressed.length} bytes (was ${raw.length})');

      final mime = picked.mimeType ??
          'image/jpeg'; // imagePicker may omit on some platforms
      final mediaFile = await writeMediaFile(compressed, mime);
      debugPrint('[picture-send] wrote ${mediaFile.path}');

      // Full UUID — messageId is encoded as a 16-byte packet id on the wire,
// and as a 36-char string in the FRAGMENT_START header. An 8-char prefix
// would corrupt either format.
      final messageId = _uuid.v4();

      widget.store.dispatch(SaveChatMessageAction(
        senderPubkeyHex: _myHex,
        recipientPubkeyHex: _peerHex,
        content: '',
        isOutgoing: true,
        messageId: messageId,
        messageType: ChatMessageType.picture.index,
        mediaPath: mediaFile.path,
        mediaMime: mime,
        viewOnce: viewOnce,
      ));
      _scrollToBottom();

      final block = PictureSayBlock(
        viewOnce: viewOnce,
        mime: mime,
        imageBytes: compressed,
      );
      final wireBytes = block.serialize();
      debugPrint('[picture-send] block serialized: ${wireBytes.length} bytes; '
          'dispatching to grassroots.send for peer ${widget.peer.displayName}');

      final sentMessageId = await widget.grassroots
          .send(widget.peer.publicKey, wireBytes, messageId: messageId);

      if (sentMessageId == null) {
        // send() returns null only for invalid input; offline peers are queued
        // by GrassrootsNetwork and retain a clock-style status until reachable.
        debugPrint(
            '[picture-send] grassroots.send returned null for ${widget.peer.displayName}');
        widget.store.dispatch(MessageFailedAction(messageId: messageId));
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(
              content: Text(
                'Could not queue picture for ${widget.peer.displayName}.',
              ),
              duration: const Duration(seconds: 3),
            ),
          );
        }
      } else {
        debugPrint('[picture-send] sent ok, messageId=$sentMessageId');
      }
    } catch (e, st) {
      _log.e('Failed to send picture', error: e, stackTrace: st);
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Failed to send picture: $e'),
            duration: const Duration(seconds: 3),
          ),
        );
      }
    } finally {
      if (mounted) setState(() => _sendingMedia = false);
    }
  }

  void _scrollToBottom() {
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (_scrollController.hasClients) {
        _scrollController.animateTo(
          _scrollController.position.maxScrollExtent,
          duration: const Duration(milliseconds: 300),
          curve: Curves.easeOut,
        );
      }
    });
  }

  void _copyMessage(ChatMessageState message) {
    Clipboard.setData(ClipboardData(text: message.content));
    ScaffoldMessenger.of(context).showSnackBar(
      const SnackBar(
        content: Text('Message copied'),
        duration: Duration(seconds: 2),
      ),
    );
  }

  void _forwardMessage(ChatMessageState message) {
    // Show dialog to select a peer to forward to
    final peers = widget.grassroots.getPeers();

    if (peers.isEmpty) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Text('No connected peers to forward to'),
          duration: Duration(seconds: 2),
        ),
      );
      return;
    }

    showModalBottomSheet(
      context: context,
      backgroundColor: Theme.of(context).colorScheme.surface,
      shape: const RoundedRectangleBorder(
        borderRadius: BorderRadius.vertical(top: Radius.circular(20)),
      ),
      builder: (context) => _ForwardSheet(
        message: message,
        peers: peers,
        onForward: (peer) async {
          Navigator.pop(context);
          final forwardedMessageId = await widget.grassroots.send(
            peer.publicKey,
            Uint8List.fromList(message.content.codeUnits),
          );

          if (forwardedMessageId == null) {
            if (!mounted) return;
            ScaffoldMessenger.of(this.context).showSnackBar(
              SnackBar(
                content:
                    Text('Failed to forward message to ${peer.displayName}'),
                duration: const Duration(seconds: 2),
              ),
            );
            return;
          }

          // Also save to Redux store as outgoing
          widget.store.dispatch(SaveChatMessageAction(
            senderPubkeyHex: _myHex,
            recipientPubkeyHex: ChatMessage.pubkeyToHex(peer.publicKey),
            content: message.content,
            isOutgoing: true,
            messageId: forwardedMessageId,
          ));

          if (!mounted) return;
          ScaffoldMessenger.of(this.context).showSnackBar(
            SnackBar(
              content: Text('Message forwarded to ${peer.displayName}'),
              duration: const Duration(seconds: 2),
            ),
          );
        },
      ),
    );
  }

  void _showMessageOptions(ChatMessageState message, Offset tapPosition) {
    showMenu(
      context: context,
      position: RelativeRect.fromLTRB(
        tapPosition.dx,
        tapPosition.dy,
        tapPosition.dx + 1,
        tapPosition.dy + 1,
      ),
      shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(12)),
      items: [
        PopupMenuItem(
          onTap: () => _copyMessage(message),
          child: const Row(
            children: [
              Icon(Icons.copy, size: 20),
              SizedBox(width: 12),
              Text('Copy'),
            ],
          ),
        ),
        PopupMenuItem(
          onTap: () => Future.delayed(
            const Duration(milliseconds: 10),
            () => _forwardMessage(message),
          ),
          child: const Row(
            children: [
              Icon(Icons.forward, size: 20),
              SizedBox(width: 12),
              Text('Forward'),
            ],
          ),
        ),
      ],
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Row(
          children: [
            Expanded(
              child: Text(
                widget.peer.nickname.isNotEmpty
                    ? widget.peer.nickname
                    : 'Peer ${_peerHex.substring(0, 8)}...',
                overflow: TextOverflow.ellipsis,
              ),
            ),
            if (_isFriend)
              const Padding(
                padding: EdgeInsets.only(left: 4),
                child: Icon(Icons.people, size: 18, color: Colors.blue),
              ),
          ],
        ),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        actions: _buildAppBarActions(),
      ),
      body: Column(
        children: [
          // Friend request banner if pending incoming
          if (_hasPendingIncoming) _buildFriendRequestBanner(),
          Expanded(
            child: Builder(builder: (context) {
              final messages =
                  widget.store.state.messages.getConversation(_peerHex);
              return Stack(
                children: [
                  ListView.builder(
                    controller: _scrollController,
                    padding: const EdgeInsets.all(16),
                    itemCount: messages.length,
                    itemBuilder: (context, index) {
                      final message = messages[index];
                      return _buildMessageWidget(message);
                    },
                  ),
                  // Jump-to-latest button. Visible whenever the user is more
                  // than `_atBottomThreshold` pixels above `maxScrollExtent`.
                  Positioned(
                    right: 16,
                    bottom: 16,
                    child: AnimatedOpacity(
                      opacity: _showScrollDownButton ? 1.0 : 0.0,
                      duration: const Duration(milliseconds: 150),
                      child: IgnorePointer(
                        ignoring: !_showScrollDownButton,
                        child: FloatingActionButton.small(
                          heroTag: 'chat-scroll-down',
                          onPressed: _scrollToBottom,
                          child: const Icon(Icons.arrow_downward),
                        ),
                      ),
                    ),
                  ),
                ],
              );
            }),
          ),
          _buildMessageInput(),
        ],
      ),
    );
  }

  List<Widget> _buildAppBarActions() {
    final actions = <Widget>[];

    if (!_isFriend && !_hasPendingOutgoing && !_hasPendingIncoming) {
      // Can send friend request
      actions.add(
        IconButton(
          icon: const Icon(Icons.person_add),
          tooltip: 'Send friend request',
          onPressed: widget.onSendFriendRequest,
        ),
      );
    } else if (_hasPendingOutgoing) {
      // Waiting for response
      actions.add(
        const Padding(
          padding: EdgeInsets.symmetric(horizontal: 16),
          child: Row(
            mainAxisSize: MainAxisSize.min,
            children: [
              Icon(Icons.hourglass_empty, size: 16),
              SizedBox(width: 4),
              Text('Pending', style: TextStyle(fontSize: 12)),
            ],
          ),
        ),
      );
    }

    // Menu
    actions.add(
      PopupMenuButton<String>(
        onSelected: (value) {
          switch (value) {
            case 'info':
              _showPeerInfo();
              break;
            case 'unfriend':
              _confirmUnfriend();
              break;
          }
        },
        itemBuilder: (context) => [
          const PopupMenuItem(
            value: 'info',
            child: Row(
              children: [
                Icon(Icons.info_outline),
                SizedBox(width: 12),
                Text('Peer Info'),
              ],
            ),
          ),
          if (_isFriend)
            const PopupMenuItem(
              value: 'unfriend',
              child: Row(
                children: [
                  Icon(Icons.person_remove, color: Colors.red),
                  SizedBox(width: 12),
                  Text('Unfriend', style: TextStyle(color: Colors.red)),
                ],
              ),
            ),
        ],
      ),
    );

    return actions;
  }

  void _confirmUnfriend() {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Unfriend'),
        content: Text(
          'Are you sure you want to remove ${widget.peer.displayName} from your friends?\n\n'
          'You will only be able to contact them via Bluetooth when they are nearby.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: const Text('Cancel'),
          ),
          TextButton(
            onPressed: () {
              Navigator.pop(context);
              widget.onUnfriend?.call();
              Navigator.pop(context); // Close chat screen
            },
            style: TextButton.styleFrom(foregroundColor: Colors.red),
            child: const Text('Unfriend'),
          ),
        ],
      ),
    );
  }

  Widget _buildFriendRequestBanner() {
    return Container(
      padding: const EdgeInsets.all(12),
      color: const Color(0xFF1B3D2F),
      child: Row(
        children: [
          const Icon(Icons.person_add, color: Color(0xFFE8A33C)),
          const SizedBox(width: 12),
          Expanded(
            child: Text(
              '${widget.peer.displayName} wants to be friends!',
              style: const TextStyle(fontWeight: FontWeight.w500),
            ),
          ),
          TextButton(
            onPressed: () {
              // Decline
              widget.store.dispatch(DeclineFriendRequestAction(_peerHex));
            },
            child: const Text('Decline'),
          ),
          const SizedBox(width: 8),
          ElevatedButton(
            onPressed: widget.onAcceptFriendRequest,
            style: ElevatedButton.styleFrom(
              backgroundColor: const Color(0xFFE8A33C),
            ),
            child: const Text('Accept', style: TextStyle(color: Colors.black)),
          ),
        ],
      ),
    );
  }

  Widget _buildMessageWidget(ChatMessageState message) {
    if (message.isFriendshipMessage) {
      return _FriendshipMessageBubble(
        message: message,
        onAccept: message.canAccept ? widget.onAcceptFriendRequest : null,
      );
    }
    return _MessageBubble(
      message: message,
      onLongPress: (position) => _showMessageOptions(message, position),
      messagesState: widget.store.state.messages,
      onResend: message.isOutgoing ? () => _resendMessage(message) : null,
      store: widget.store,
    );
  }

  Future<void> _resendMessage(ChatMessageState message) async {
    debugPrint(
        "Resending '${message.content}' to peer ${widget.peer.displayName}");

    final existingMessageId = message.messageId;
    if (existingMessageId == null) {
      if (!mounted) return;
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Text('Cannot resend message without message ID'),
          duration: Duration(seconds: 2),
        ),
      );
      return;
    }

    // Send via Grassroots using SayBlock. Picture resend is not yet wired —
    // for now resend treats the message as text. (The bubble's resend affordance
    // is only shown for failed messages, and pictures over BLE rarely fail
    // partway because fragmentation is in-process.)
    final block = TextSayBlock(content: message.content);
    final messageId = await widget.grassroots.send(
      widget.peer.publicKey,
      block.serialize(),
      messageId: existingMessageId,
    );

    if (!mounted) return;

    if (messageId != null) {
      debugPrint("Resend successful for messageId: $messageId");
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Text('Message resent'),
          duration: Duration(seconds: 2),
        ),
      );
    } else {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Text('Failed to resend message'),
          duration: Duration(seconds: 2),
        ),
      );
    }
  }

  void _showPeerInfo() {
    // Refresh from store so the dialog reflects the latest udpAddress /
    // connection state, not just the snapshot widget.peer was built with.
    // RV-list lives on the friendship record (persisted, friendship-scoped).
    final peer =
        widget.store.state.peers.getPeerByPubkeyHex(_peerHex) ?? widget.peer;
    final friendship = widget.store.state.friendships.getFriendship(_peerHex);
    final knownRvServers =
        friendship?.knownRvServers ?? const <String, String>{};
    final rvEntries = knownRvServers.entries.toList()
      ..sort((a, b) => a.key.compareTo(b.key));

    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text(peer.displayName),
        content: SingleChildScrollView(
          child: Column(
            mainAxisSize: MainAxisSize.min,
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              _buildInfoRow('Public Key', _peerHex),
              const SizedBox(height: 8),
              _buildInfoRow(
                  'Bluetooth',
                  peer.hasBleConnection
                      ? 'Connected (${peer.bleCentralDeviceId != null && peer.blePeripheralDeviceId != null ? 'central + peripheral' : peer.bleCentralDeviceId != null ? 'central' : 'peripheral'})'
                      : 'Not connected'),
              const SizedBox(height: 8),
              _buildInfoRow('Internet',
                  peer.udpAddress != null ? peer.udpAddress! : 'No address'),
              if (_isFriend) ...[
                const SizedBox(height: 8),
                _buildInfoRow('Friendship', 'Friends ✓'),
              ],
              const SizedBox(height: 16),
              Text(
                'Rendezvous Servers',
                style: Theme.of(context).textTheme.titleSmall,
              ),
              const SizedBox(height: 4),
              if (rvEntries.isEmpty)
                const Text(
                  'None advertised by this peer yet.',
                  style: TextStyle(color: Colors.grey, fontSize: 12),
                )
              else
                ...rvEntries.map((entry) => _buildRvServerRow(
                      address: entry.value,
                      pubkeyHex: entry.key,
                    )),
            ],
          ),
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: const Text('Close'),
          ),
          TextButton(
            onPressed: () {
              Clipboard.setData(ClipboardData(text: _peerHex));
              ScaffoldMessenger.of(context).showSnackBar(
                const SnackBar(content: Text('Public key copied')),
              );
            },
            child: const Text('Copy Key'),
          ),
        ],
      ),
    );
  }

  Widget _buildRvServerRow({
    required String address,
    required String pubkeyHex,
  }) {
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 4),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              const SizedBox(
                width: 80,
                child: Text('Address:',
                    style: TextStyle(color: Colors.grey, fontSize: 12)),
              ),
              Expanded(
                child: Text(
                  address,
                  style: const TextStyle(fontFamily: 'monospace', fontSize: 12),
                ),
              ),
              IconButton(
                icon: const Icon(Icons.copy, size: 16),
                tooltip: 'Copy address',
                visualDensity: VisualDensity.compact,
                padding: EdgeInsets.zero,
                constraints: const BoxConstraints(),
                onPressed: () {
                  Clipboard.setData(ClipboardData(text: address));
                  ScaffoldMessenger.of(context).showSnackBar(
                    SnackBar(content: Text('Address copied: $address')),
                  );
                },
              ),
            ],
          ),
          Row(
            children: [
              const SizedBox(
                width: 80,
                child: Text('Pubkey:',
                    style: TextStyle(color: Colors.grey, fontSize: 12)),
              ),
              Expanded(
                child: Text(
                  pubkeyHex,
                  style: const TextStyle(fontFamily: 'monospace', fontSize: 11),
                  overflow: TextOverflow.ellipsis,
                ),
              ),
              IconButton(
                icon: const Icon(Icons.copy, size: 16),
                tooltip: 'Copy pubkey',
                visualDensity: VisualDensity.compact,
                padding: EdgeInsets.zero,
                constraints: const BoxConstraints(),
                onPressed: () {
                  Clipboard.setData(ClipboardData(text: pubkeyHex));
                  ScaffoldMessenger.of(context).showSnackBar(
                    const SnackBar(content: Text('Rendezvous pubkey copied')),
                  );
                },
              ),
            ],
          ),
        ],
      ),
    );
  }

  Widget _buildInfoRow(String label, String value) {
    return Row(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        SizedBox(
          width: 80,
          child: Text(
            '$label:',
            style: const TextStyle(color: Colors.grey),
          ),
        ),
        Expanded(
          child: Text(
            value,
            style: const TextStyle(fontFamily: 'monospace', fontSize: 12),
          ),
        ),
      ],
    );
  }

  Widget _buildMessageInput() {
    return Container(
      decoration: BoxDecoration(
        color: Theme.of(context).colorScheme.surface,
        boxShadow: [
          BoxShadow(
            color: Colors.black.withValues(alpha: 0.1),
            blurRadius: 4,
            offset: const Offset(0, -2),
          ),
        ],
      ),
      child: Column(
        mainAxisSize: MainAxisSize.min,
        children: [
          if (_sendingMedia) const LinearProgressIndicator(minHeight: 2),
          Padding(
            padding: const EdgeInsets.all(8),
            child: Row(
              children: [
                IconButton(
                  icon: const Icon(Icons.add_photo_alternate_outlined),
                  tooltip: 'Send a picture',
                  onPressed: _sendingMedia ? null : _openAttachmentSheet,
                ),
                Expanded(
                  child: TextField(
                    controller: _messageController,
                    decoration: const InputDecoration(
                      hintText: 'Type a message...',
                      border: OutlineInputBorder(),
                      contentPadding:
                          EdgeInsets.symmetric(horizontal: 16, vertical: 8),
                    ),
                    onSubmitted: (_) => _sendMessage(),
                  ),
                ),
                const SizedBox(width: 8),
                IconButton(
                  icon: const Icon(Icons.send),
                  onPressed: _sendMessage,
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }
}

class _MessageBubble extends StatelessWidget {
  final ChatMessageState message;
  final void Function(Offset position)? onLongPress;
  final MessagesState? messagesState;
  final VoidCallback? onResend;
  final Store<AppState>? store;

  const _MessageBubble({
    required this.message,
    this.onLongPress,
    this.messagesState,
    this.onResend,
    this.store,
  });

  @override
  Widget build(BuildContext context) {
    return Align(
      alignment:
          message.isOutgoing ? Alignment.centerRight : Alignment.centerLeft,
      child: GestureDetector(
        onLongPressStart: (details) {
          onLongPress?.call(details.globalPosition);
        },
        child: Container(
          margin: const EdgeInsets.symmetric(vertical: 4),
          padding: message.isPicture
              ? const EdgeInsets.all(6)
              : const EdgeInsets.symmetric(horizontal: 16, vertical: 10),
          constraints: BoxConstraints(
            maxWidth: MediaQuery.of(context).size.width * 0.75,
          ),
          decoration: BoxDecoration(
            color: message.isOutgoing
                ? Theme.of(context).colorScheme.primary
                : Theme.of(context).colorScheme.surfaceContainerHighest,
            borderRadius: BorderRadius.circular(16),
          ),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.end,
            children: [
              if (message.isPicture)
                _buildPictureContent(context)
              else
                Text(
                  message.content,
                  style: TextStyle(
                    color: message.isOutgoing ? Colors.white : null,
                  ),
                ),
              const SizedBox(height: 4),
              Padding(
                padding: message.isPicture
                    ? const EdgeInsets.symmetric(horizontal: 6)
                    : EdgeInsets.zero,
                child: Row(
                  mainAxisSize: MainAxisSize.min,
                  children: [
                    Text(
                      _formatTime(message.timestamp),
                      style: TextStyle(
                        fontSize: 10,
                        color: message.isOutgoing
                            ? Colors.white70
                            : Theme.of(context)
                                .colorScheme
                                .onSurface
                                .withValues(alpha: 0.5),
                      ),
                    ),
                    if (message.isOutgoing) ...[
                      const SizedBox(width: 4),
                      _buildStatusIcon(context),
                    ],
                  ],
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildPictureContent(BuildContext context) {
    final mediaPath = message.mediaPath;

    // Expired view-once (recipient already viewed, or sender's copy was
    // deleted on delivery).
    if (mediaPath == null || message.viewed) {
      return _buildExpiredPlaceholder(context);
    }

    if (message.viewOnce && !message.isOutgoing) {
      return _buildViewOncePreview(context, mediaPath);
    }

    return _buildInlinePicture(context, mediaPath);
  }

  Widget _buildInlinePicture(BuildContext context, String mediaPath) {
    return GestureDetector(
      onTap: () => _openFullscreen(context, mediaPath, viewOnce: false),
      child: ClipRRect(
        borderRadius: BorderRadius.circular(12),
        child: Image.file(
          File(mediaPath),
          width: 240,
          fit: BoxFit.cover,
          cacheWidth: 600,
          errorBuilder: (_, __, ___) => _buildExpiredPlaceholder(context),
        ),
      ),
    );
  }

  Widget _buildViewOncePreview(BuildContext context, String mediaPath) {
    return GestureDetector(
      onTap: () => _openFullscreen(context, mediaPath, viewOnce: true),
      child: ClipRRect(
        borderRadius: BorderRadius.circular(12),
        child: Stack(
          alignment: Alignment.center,
          children: [
            Image.file(
              File(mediaPath),
              width: 240,
              height: 240,
              fit: BoxFit.cover,
              cacheWidth: 600,
              errorBuilder: (_, __, ___) => _buildExpiredPlaceholder(context),
            ),
            BackdropFilter(
              filter: ImageFilter.blur(sigmaX: 18, sigmaY: 18),
              child: Container(
                width: 240,
                height: 240,
                color: Colors.black.withValues(alpha: 0.15),
                alignment: Alignment.center,
                child: const Column(
                  mainAxisSize: MainAxisSize.min,
                  children: [
                    Icon(Icons.local_fire_department,
                        color: Colors.white, size: 36),
                    SizedBox(height: 8),
                    Text(
                      'Tap to view once',
                      style: TextStyle(
                        color: Colors.white,
                        fontWeight: FontWeight.bold,
                      ),
                    ),
                  ],
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildExpiredPlaceholder(BuildContext context) {
    return Container(
      width: 240,
      height: 120,
      decoration: BoxDecoration(
        borderRadius: BorderRadius.circular(12),
        color: Colors.black26,
      ),
      alignment: Alignment.center,
      child: const Column(
        mainAxisSize: MainAxisSize.min,
        children: [
          Icon(Icons.visibility_off, color: Colors.white70),
          SizedBox(height: 4),
          Text(
            'View-once photo expired',
            style: TextStyle(color: Colors.white70, fontSize: 12),
          ),
        ],
      ),
    );
  }

  Future<void> _openFullscreen(
    BuildContext context,
    String mediaPath, {
    required bool viewOnce,
  }) async {
    await Navigator.of(context).push(
      MaterialPageRoute(
        fullscreenDialog: true,
        builder: (_) => _FullscreenImageView(path: mediaPath),
      ),
    );

    // After the viewer dismisses, if this was an incoming view-once photo,
    // mark it consumed and delete the local file. The middleware in main.dart
    // handles the outgoing-side deletion separately on delivery.
    if (viewOnce && !message.isOutgoing && message.messageId != null) {
      final s = store;
      if (s != null) {
        unawaited(deleteMediaFile(mediaPath));
        s.dispatch(MarkPictureViewedAction(
          peerHex: message.senderPubkeyHex,
          messageId: message.messageId!,
        ));
      }
    }
  }

  Widget _buildStatusIcon(BuildContext context) {
    // Get message status from store
    MessageStatus status = MessageStatus.sent;
    if (message.messageId != null && messagesState != null) {
      final outgoing = messagesState!.getOutgoingMessage(message.messageId!);
      status = outgoing?.status ?? MessageStatus.sent;
    }

    switch (status) {
      case MessageStatus.sending:
        // Clock icon (sending)
        return const Icon(
          Icons.access_time,
          size: 14,
          color: Colors.white70,
        );
      case MessageStatus.queued:
        // Clock icon (queued until the peer is reachable)
        return const Icon(
          Icons.schedule,
          size: 14,
          color: Colors.white70,
        );
      case MessageStatus.failed:
        // Red exclamation (failed) - clickable for resend
        return GestureDetector(
          onTap: onResend,
          child: const Icon(
            Icons.error_outline,
            size: 14,
            color: Colors.redAccent,
          ),
        );
      case MessageStatus.sent:
        // 1 check (sent)
        return const Icon(
          Icons.check,
          size: 14,
          color: Colors.white70,
        );
      case MessageStatus.delivered:
        // 2 checks (delivered)
        return const Icon(
          Icons.done_all,
          size: 14,
          color: Colors.white70,
        );
      case MessageStatus.read:
        // 2 blue checks (read)
        return const Icon(
          Icons.done_all,
          size: 14,
          color: Colors.blueAccent,
        );
    }
  }

  String _formatTime(DateTime time) {
    final hour = time.hour.toString().padLeft(2, '0');
    final minute = time.minute.toString().padLeft(2, '0');
    return '$hour:$minute';
  }
}

/// Fullscreen viewer for picture messages: pinch-zoom, drag, dismiss with
/// the back button or the close icon. Used for both regular and view-once
/// pictures; the caller decides what to do on dismiss.
class _FullscreenImageView extends StatelessWidget {
  final String path;
  const _FullscreenImageView({required this.path});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.black,
      appBar: AppBar(
        backgroundColor: Colors.black,
        iconTheme: const IconThemeData(color: Colors.white),
      ),
      body: Center(
        child: InteractiveViewer(
          minScale: 0.5,
          maxScale: 4,
          child: Image.file(
            File(path),
            errorBuilder: (_, __, ___) => const Center(
              child: Text(
                'Image unavailable',
                style: TextStyle(color: Colors.white70),
              ),
            ),
          ),
        ),
      ),
    );
  }
}

/// Bottom sheet for selecting a peer to forward message to
class _ForwardSheet extends StatelessWidget {
  final ChatMessageState message;
  final List<PeerState> peers;
  final void Function(PeerState peer) onForward;

  const _ForwardSheet({
    required this.message,
    required this.peers,
    required this.onForward,
  });

  @override
  Widget build(BuildContext context) {
    return Container(
      padding: const EdgeInsets.all(16),
      child: Column(
        mainAxisSize: MainAxisSize.min,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          // Handle bar
          Center(
            child: Container(
              width: 40,
              height: 4,
              margin: const EdgeInsets.only(bottom: 16),
              decoration: BoxDecoration(
                color: Colors.grey[400],
                borderRadius: BorderRadius.circular(2),
              ),
            ),
          ),
          const Text(
            'Forward to',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
            ),
          ),
          const SizedBox(height: 8),
          // Message preview
          Container(
            padding: const EdgeInsets.all(12),
            decoration: BoxDecoration(
              color: Theme.of(context).colorScheme.surfaceContainerHighest,
              borderRadius: BorderRadius.circular(8),
            ),
            child: Text(
              message.content.length > 100
                  ? '${message.content.substring(0, 100)}...'
                  : message.content,
              style: TextStyle(
                color: Theme.of(context).colorScheme.onSurface.withOpacity(0.7),
                fontStyle: FontStyle.italic,
              ),
            ),
          ),
          const SizedBox(height: 16),
          const Text(
            'Select peer:',
            style: TextStyle(
              fontSize: 14,
              color: Colors.grey,
            ),
          ),
          const SizedBox(height: 8),
          // Peer list
          ConstrainedBox(
            constraints: BoxConstraints(
              maxHeight: MediaQuery.of(context).size.height * 0.4,
            ),
            child: ListView.builder(
              shrinkWrap: true,
              itemCount: peers.length,
              itemBuilder: (context, index) {
                final peer = peers[index];
                return ListTile(
                  leading: CircleAvatar(
                    backgroundColor: Colors.blueGrey,
                    child: Text(
                      peer.displayName.isNotEmpty
                          ? peer.displayName[0].toUpperCase()
                          : '?',
                      style: const TextStyle(color: Colors.white),
                    ),
                  ),
                  title: Text(peer.displayName),
                  subtitle: Text(
                    peer.connectionState == PeerConnectionState.connected
                        ? 'Online'
                        : 'Offline',
                    style: TextStyle(
                      color:
                          peer.connectionState == PeerConnectionState.connected
                              ? Colors.green
                              : Colors.grey,
                      fontSize: 12,
                    ),
                  ),
                  trailing: const Icon(Icons.send, size: 20),
                  onTap: () => onForward(peer),
                );
              },
            ),
          ),
        ],
      ),
    );
  }
}

/// Special message bubble for friendship-related messages
class _FriendshipMessageBubble extends StatelessWidget {
  final ChatMessageState message;
  final VoidCallback? onAccept;

  const _FriendshipMessageBubble({
    required this.message,
    this.onAccept,
  });

  @override
  Widget build(BuildContext context) {
    final isOutgoing = message.isOutgoing;

    IconData icon;
    Color iconColor;
    String title;

    switch (message.messageType) {
      case ChatMessageType.friendRequestSent:
        icon = Icons.person_add;
        iconColor = const Color(0xFFE8A33C);
        title = 'Friend Request Sent';
        break;
      case ChatMessageType.friendRequestReceived:
        icon = Icons.person_add;
        iconColor = const Color(0xFFE8A33C);
        title = 'Friend Request';
        break;
      case ChatMessageType.friendRequestAccepted:
        icon = Icons.check_circle;
        iconColor = Colors.green;
        title = 'Friend Request Accepted';
        break;
      case ChatMessageType.friendRequestAcceptedByUs:
        icon = Icons.check_circle;
        iconColor = Colors.green;
        title = 'You Accepted';
        break;
      default:
        icon = Icons.info;
        iconColor = Colors.grey;
        title = 'System Message';
    }

    return Align(
      alignment: isOutgoing ? Alignment.centerRight : Alignment.centerLeft,
      child: Container(
        margin: const EdgeInsets.symmetric(vertical: 8),
        constraints: BoxConstraints(
          maxWidth: MediaQuery.of(context).size.width * 0.75,
        ),
        decoration: BoxDecoration(
          color: const Color(0xFF1B3D2F),
          borderRadius: BorderRadius.circular(16),
          border: Border.all(color: iconColor.withOpacity(0.5)),
        ),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.stretch,
          children: [
            // Header
            Container(
              padding: const EdgeInsets.all(12),
              decoration: BoxDecoration(
                color: iconColor.withOpacity(0.1),
                borderRadius:
                    const BorderRadius.vertical(top: Radius.circular(15)),
              ),
              child: Row(
                mainAxisSize: MainAxisSize.min,
                children: [
                  Icon(icon, color: iconColor, size: 20),
                  const SizedBox(width: 8),
                  Expanded(
                    child: Text(
                      title,
                      style: TextStyle(
                        fontWeight: FontWeight.bold,
                        color: iconColor,
                      ),
                    ),
                  ),
                ],
              ),
            ),
            // Content
            Padding(
              padding: const EdgeInsets.all(12),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    message.content,
                    style: const TextStyle(fontSize: 14),
                  ),
                  const SizedBox(height: 8),
                  // Accept button for pending incoming requests
                  if (message.canAccept && onAccept != null) ...[
                    const SizedBox(height: 8),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton.icon(
                        onPressed: onAccept,
                        icon: const Icon(Icons.check, size: 18),
                        label: const Text('Accept Friend Request'),
                        style: ElevatedButton.styleFrom(
                          backgroundColor: const Color(0xFFE8A33C),
                          foregroundColor: Colors.black,
                        ),
                      ),
                    ),
                  ],
                  // Timestamp
                  Align(
                    alignment: Alignment.bottomRight,
                    child: Text(
                      _formatTime(message.timestamp),
                      style: TextStyle(
                        fontSize: 10,
                        color: Colors.white.withOpacity(0.5),
                      ),
                    ),
                  ),
                ],
              ),
            ),
          ],
        ),
      ),
    );
  }

  String _formatTime(DateTime time) {
    final hour = time.hour.toString().padLeft(2, '0');
    final minute = time.minute.toString().padLeft(2, '0');
    return '$hour:$minute';
  }
}
