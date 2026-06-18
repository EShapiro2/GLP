/// Unified social manifest for the running demo: ONE connection type (Friend),
/// ONE request (befriend). Once two people are friends they can talk; not
/// before. As panels (paper §7.3) it is GrassApp without Coins — a **Friends**
/// panel (the social graph: friend offers, the friends list) and a **Chats**
/// panel (the conversations friendship opens) — over one shared activity store,
/// so a friend offer is answered once on its Friends row and is then gone.
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

final Manifest socialManifest = Manifest(
  title: 'Grassroots',

  panels: [
    // Friends: the social graph. A friend offer alerts the offering person's
    // row; the "+" offers friendship.
    Panel(
      id: 'friends',
      name: 'Friends',
      friends: const FriendsView(listKey: 'friends', label: 'Friends'),
      commands: const [
        CommandDesc(
          ctor: 'connect',
          label: 'Add friend',
          args: [FieldDesc('target', FieldType.person, 'Person to connect')],
        ),
      ],
      inbox: [
        InboxDesc(
          notifyCtor: 'befriend',
          args: const ['from', 'req'],
          itemKey: 'from',
          title: '{from} wants to connect',
          answers: const [
            AnswerDesc(
              label: 'Accept',
              cmdCtor: 'decision',
              fill: [
                ConstFill(GAtom('yes')),
                FromField('from'),
                FromField('req')
              ],
            ),
            AnswerDesc(
              label: 'Decline',
              cmdCtor: 'decision',
              fill: [
                ConstFill(GAtom('no')),
                FromField('from'),
                FromField('req')
              ],
            ),
          ],
        ),
      ],
    ),

    // Chats: the conversations friendship opens. Sending happens inside the open
    // conversation, so the panel has no "+".
    const Panel(
      id: 'chats',
      name: 'Chats',
      chat: ChatView(threadKey: 'chats', label: 'Chats', sendCtor: 'send'),
    ),
  ],

  state: const [
    StateView('friends', 'Friends', StateKind.list),
    StateView('chats', 'Chats', StateKind.thread),
  ],

  // Becoming friends adds the friend (Friends) and opens a conversation (Chats);
  // a message from a friend extends it; a refused offer leaves no friend.
  activity: const [
    ActivityDesc(
      notifyCtor: 'connected',
      args: ['who'],
      effects: [AppendTo('friends', 'who'), OpenChat('chats', 'who')],
    ),
    ActivityDesc(
      notifyCtor: 'received',
      args: ['from', 'text'],
      effects: [PushChat('chats', 'from', 'text', outgoing: false)],
    ),
    ActivityDesc(notifyCtor: 'rejected', args: ['who']),
    ActivityDesc(notifyCtor: 'rejected', args: []),
  ],
);
