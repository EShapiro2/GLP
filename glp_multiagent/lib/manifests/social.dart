/// Unified social manifest for the running demo: ONE platform, ONE connection
/// type (Friend), ONE request (befriend). Once two people are friends they can
/// talk; not before. The app shows this one world through two views — a Friends
/// list (the Social Graph view) and a Chats list (the GrassApp view) — over a
/// single shared inbox, so a request is accepted once and is gone from both.
///
/// (The paper's separate GSG and GrassApp manifests, with their richer
/// connection vocabulary, live in gsg.dart / grassapp.dart for the figures.)
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

final Manifest socialManifest = Manifest(
  title: 'Grassroots',

  // Two surfaces: Chats (the friends you can talk to) and Requests (the inbox).
  chat: const ChatView(threadKey: 'chats', label: 'Chats', sendCtor: 'send'),
  state: const [
    StateView('chats', 'Chats', StateKind.thread),
  ],

  // The one outbox request the person composes: offer friendship.
  commands: const [
    CommandDesc(
      ctor: 'connect',
      label: 'Add friend',
      args: [FieldDesc('target', FieldType.person, 'Person to connect')],
    ),
  ],

  // The one inbox request: a friend offer.
  inbox: [
    InboxDesc(
      notifyCtor: 'befriend',
      args: const ['from', 'req'],
      title: '{from} wants to connect',
      answers: const [
        AnswerDesc(
          label: 'Accept',
          cmdCtor: 'decision',
          fill: [ConstFill(GAtom('yes')), FromField('from'), FromField('req')],
        ),
        AnswerDesc(
          label: 'Decline',
          cmdCtor: 'decision',
          fill: [ConstFill(GAtom('no')), FromField('from'), FromField('req')],
        ),
      ],
    ),
  ],

  // Activity rules land in their target surface.
  activity: const [
    // Becoming friends opens a conversation — the friend appears in Chats.
    ActivityDesc(
      notifyCtor: 'connected',
      args: ['who'],
      effect: OpenChat('chats', 'who'),
    ),
    // A message from a friend extends that conversation.
    ActivityDesc(
      notifyCtor: 'received',
      args: ['from', 'text'],
      effect: PushChat('chats', 'from', 'text', outgoing: false),
    ),
    // A refused offer leaves no friend.
    ActivityDesc(notifyCtor: 'rejected', args: ['who']),
    ActivityDesc(notifyCtor: 'rejected', args: []),
  ],
);
