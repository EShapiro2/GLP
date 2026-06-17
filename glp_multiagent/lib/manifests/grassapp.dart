/// GrassApp (social network) UI manifest — paper §7.3 (sec:ui-whatsapp),
/// Figure fig:grassapp.
///
/// GrassApp adds one-to-one and group messaging to the social graph: element
/// for element WhatsApp, with one difference — the inbox. The two surfaces:
///
///   • Requests (inbox): a friend offer (`befriend`) or a group invitation
///     (`group_invite`), each accepted or declined before contact begins.
///   • Chats (the state the outbox leaves): the chat list and, on tap, the open
///     conversation; messaging is the outbox (`send`), its delivery ticks the
///     response (`sent`/`received`).
///
/// The vocabulary extends GSG (`befriend`/`decision`, `send`/`received`) with
/// group invitations. Rendered by the same generic interpreter — no new Dart.
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

final Manifest grassappManifest = Manifest(
  title: 'GrassApp',
  stateTabLabel: 'Chats',

  // The state screen is a chat list; tapping a row opens the conversation,
  // whose input sends send(peer, text).
  chat: const ChatView(threadKey: 'chats', label: 'Chats', sendCtor: 'send'),
  state: const [
    StateView('chats', 'Chats', StateKind.thread),
  ],

  // Outbox: free UserCmds (no ReqId). 'send' is issued from the conversation
  // input; 'connect'/'introduce' compose via the "+".
  commands: const [
    CommandDesc(
      ctor: 'connect',
      label: 'Add friend',
      args: [FieldDesc('target', FieldType.person, 'Person to connect')],
    ),
    CommandDesc(
      ctor: 'introduce',
      label: 'Introduce',
      args: [
        FieldDesc('p', FieldType.person, 'Person'),
        FieldDesc('q', FieldType.person, 'Introduce to'),
      ],
    ),
  ],

  // Inbox: the consent step before contact — a friend offer or a group invite.
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
    InboxDesc(
      notifyCtor: 'group_invite',
      args: const ['group', 'from', 'req'],
      title: '{group}',
      subtitle: '{from} invited you',
      answers: const [
        AnswerDesc(
          label: 'Accept',
          cmdCtor: 'accept_group',
          fill: [FromField('group'), FromField('req')],
        ),
        // decline carries no req id — just the group.
        AnswerDesc(
          label: 'Decline',
          cmdCtor: 'decline_group',
          fill: [FromField('group')],
        ),
      ],
    ),
  ],

  // Activity rules: wholly-ground notifies land in their target surface.
  activity: const [
    // An incoming message extends its conversation (left bubble).
    ActivityDesc(
      notifyCtor: 'received',
      args: ['from', 'text'],
      effect: PushChat('chats', 'from', 'text', outgoing: false),
    ),
    // The echoed outgoing message + its delivery tick (right bubble).
    ActivityDesc(
      notifyCtor: 'sent',
      args: ['to', 'text', 'tick'],
      effect: PushChat('chats', 'to', 'text', outgoing: true, tickField: 'tick'),
    ),
    // A new friendship opens a conversation — recognised; the first message
    // populates the chat row.
    ActivityDesc(notifyCtor: 'connected', args: ['who']),
  ],
);
