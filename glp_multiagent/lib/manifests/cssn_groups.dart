/// CSSN social-network manifest (paper §7.2): the child-safe social network's
/// UI contract, mirroring the UserCmd/UserNotify vocabulary of
/// programs/cssn/ui/mediator.glp — the manual compilation of the platform's
/// volition-guarded clauses. As panels: **Friends** (the social graph), **Groups**
/// (the social network's groups), and **Chats** (one-to-one conversations), over
/// one shared activity store.
///
/// Groups are the new surface: a group is a `GroupId`-keyed multi-party
/// conversation. Create-group is the Groups panel's "+"; a group invitation is
/// an inbox card (the Respond-shaped ask); opening a group shows its
/// author-labeled messages, an input that posts (`send_group`), and per-group
/// actions (invite a friend, leave). Backed live by programs/cssn/ (agent.glp +
/// ui/mediator.glp), booted by play_ui_boot.glp.
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

final Manifest cssnGroupsManifest = Manifest(
  title: 'Grassroots',

  panels: [
    // --- Friends: the social graph -----------------------------------------
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

    // --- Groups: the social network's groups -------------------------------
    // The "+" creates a group; a group invitation alerts as a row; opening a
    // group shows its messages, the input posts, and the app-bar actions invite
    // a friend or leave.
    Panel(
      id: 'groups',
      name: 'Groups',
      groups: const GroupChatView(
        threadKey: 'groups',
        label: 'Groups',
        sendCtor: 'send_group',
        groupField: 'group',
        actions: [
          CommandDesc(
            ctor: 'invite_group',
            label: 'Invite',
            args: [
              FieldDesc('group', FieldType.text, 'Group'),
              FieldDesc('invitee', FieldType.person, 'Friend to invite'),
            ],
          ),
          CommandDesc(
            ctor: 'leave_group',
            label: 'Leave',
            args: [FieldDesc('group', FieldType.text, 'Group')],
          ),
        ],
      ),
      commands: const [
        CommandDesc(
          ctor: 'create_group',
          label: 'Create group',
          args: [FieldDesc('name', FieldType.text, 'Group name')],
        ),
      ],
      inbox: [
        InboxDesc(
          notifyCtor: 'group_invite',
          args: const ['group', 'req'],
          itemKey: 'group',
          title: 'Invitation to join a group',
          answers: const [
            AnswerDesc(
              label: 'Accept',
              cmdCtor: 'accept_group',
              fill: [FromField('group'), FromField('req')],
            ),
            AnswerDesc(
              label: 'Decline',
              cmdCtor: 'reject_group',
              fill: [FromField('group'), FromField('req')],
            ),
          ],
        ),
      ],
    ),

    // --- Chats: one-to-one conversations -----------------------------------
    const Panel(
      id: 'chats',
      name: 'Chats',
      chat: ChatView(threadKey: 'chats', label: 'Chats', sendCtor: 'send'),
    ),
  ],

  state: const [
    StateView('friends', 'Friends', StateKind.list),
    StateView('groups', 'Groups', StateKind.thread),
    StateView('chats', 'Chats', StateKind.thread),
  ],

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
    // Groups.
    ActivityDesc(
      notifyCtor: 'group_joined',
      args: ['group'],
      effects: [OpenGroup('groups', 'group')],
    ),
    ActivityDesc(
      notifyCtor: 'group_joined_member',
      args: ['group', 'member'],
      effects: [Toast('{member} joined the group')],
    ),
    ActivityDesc(
      notifyCtor: 'group_received',
      args: ['group', 'author', 'text'],
      effects: [PushGroupChat('groups', 'group', 'author', 'text')],
    ),
    ActivityDesc(
      notifyCtor: 'group_left',
      args: ['group'],
      effects: [CloseGroup('groups', 'group')],
    ),
    ActivityDesc(
      notifyCtor: 'group_removed',
      args: ['group'],
      effects: [CloseGroup('groups', 'group')],
    ),
    ActivityDesc(notifyCtor: 'rejected', args: ['who']),
    ActivityDesc(notifyCtor: 'rejected', args: []),
  ],
);
