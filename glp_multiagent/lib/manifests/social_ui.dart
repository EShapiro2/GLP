/// Social-graph manifest: the graph platform's UI contract, mirroring the
/// UserCmd/UserNotify vocabulary of programs/social/graph/ui/mediator.glp —
/// the manual compilation of the platform's volition-guarded clauses. As
/// panels it is GrassApp without Currencies — a **Friends** panel (the social
/// graph: friend offers, introductions, the friends list) and a **Chats**
/// panel (the conversations friendship opens) — over one shared activity
/// store, so a friend offer is answered once on its Friends row and is then
/// gone.
///
/// The compose forms are the Request-shaped volition-guarded clauses (offer
/// friendship, end a friendship, offer an introduction, message a friend);
/// the inbox cards are the Respond-shaped asks (a friend offer, an
/// introduction), their buttons the sibling clauses; everything else is
/// screen state.
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

final Manifest socialManifest = Manifest(
  title: 'Grassroots',

  panels: [
    // Friends: the social graph. A friend offer or an introduction alerts the
    // relevant person's row; the "+" composes the Request-shaped clauses.
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
        // End a friendship: a unilateral compose command — no one else
        // answers; the other side's agent integrates it.
        CommandDesc(
          ctor: 'unfriend',
          label: 'End friendship',
          args: [FieldDesc('friend', FieldType.person, 'Friend to remove')],
        ),
        // Offer an introduction of two of your friends to each other; each of
        // them gets an introduction card.
        CommandDesc(
          ctor: 'introduce',
          label: 'Introduce friends',
          args: [
            FieldDesc('p', FieldType.person, 'Introduce'),
            FieldDesc('q', FieldType.person, 'To'),
          ],
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
        InboxDesc(
          notifyCtor: 'befriend_intro',
          args: const ['from', 'other', 'req'],
          itemKey: 'other',
          title: '{from} introduces you to {other}',
          answers: const [
            AnswerDesc(
              label: 'Accept',
              cmdCtor: 'accept_intro',
              fill: [FromField('other'), FromField('req')],
            ),
            AnswerDesc(
              label: 'Decline',
              cmdCtor: 'reject_intro',
              fill: [FromField('other'), FromField('req')],
            ),
          ],
        ),
      ],
    ),

    // Chats: the conversations friendship opens. Sending happens inside the
    // open conversation, so the panel has no "+".
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

  // Becoming friends adds the friend (Friends) and opens a conversation
  // (Chats); a message from a friend extends it; an ended friendship removes
  // the friend; a refused offer leaves no friend.
  activity: const [
    ActivityDesc(
      notifyCtor: 'connected',
      args: ['who'],
      effects: [AppendTo('friends', 'who'), OpenChat('chats', 'who')],
    ),
    ActivityDesc(
      notifyCtor: 'unfriended',
      args: ['who'],
      effects: [RemoveFrom('friends', 'who')],
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
