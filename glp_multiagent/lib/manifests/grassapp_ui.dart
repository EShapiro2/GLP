/// GrassApp manifest (paper §7): one app, one panel per platform —
/// **Friends** (the social graph), **Currencies** (coins and bonds among friends), and
/// **Chats** (the social network). The bottom bar is these three panels; each
/// carries its own compose forms and its own per-item alerts. One mediator
/// (programs/book/grassapp/grassapp_mediator.glp) feeds them all; the Dart side
/// routes each ask to its platform's panel by notify constructor — a friend
/// offer to Friends, a proposed swap to Currencies, a group invitation to Chats.
///
/// Backed live by the GLP GrassApp scenario in programs/book/grassapp/
/// (grassapp_agent + grassapp_mediator), whose UserCmd/UserNotify vocabulary
/// this manifest mirrors.
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

final Manifest grassrootsManifest = Manifest(
  title: 'GrassApp',

  panels: [
    // --- Friends: the social graph -----------------------------------------
    // The friends list; a friend offer alerts the offering person's row (first
    // contact is gated here). The "+" offers friendship.
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
        // End a friendship (paper §5 Request; Table 1 "end a friendship"). A
        // unilateral compose command — no one else answers; the other side's
        // agent integrates it and both lists drop the friend.
        CommandDesc(
          ctor: 'unfriend',
          label: 'End friendship',
          args: [FieldDesc('friend', FieldType.person, 'Friend to remove')],
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
        // Introductions are the social-graph platform's
        // (programs/social/graph, rendered by the social manifest); the
        // grassapp mediator's vocabulary carries no befriend_intro ask.
      ],
    ),

    // --- Currencies: the wallet, organised by friend -----------------------
    // A currency unit is a bond; a coin is one mature by the holder's own clock,
    // so a holding is keyed by (issuer, maturity) and the drill-down splits Cash
    // (mature) from Loans (dated). People = self + friends + holdings owners;
    // tapping one drills into their holdings and the actions against them. A swap
    // a friend proposes alerts that friend's row. Actions live in the drill-down,
    // so the panel has no "+". Compose forms carry scalar fields only; the
    // mediator assembles the lot specs (see grassapp_mediator.glp).
    Panel(
      id: 'currencies',
      name: 'Currencies',
      wallet: WalletView(
        storeKey: 'holdings',
        label: 'Currencies',
        selfKey: 'bob',
        friendsList: 'friends',
        friendField: 'friend',
        cashLabel: 'Cash',
        loansLabel: 'Loans',
        selfActions: const [
          CommandDesc(
            ctor: 'mint',
            label: 'Mint',
            args: [
              FieldDesc('amount', FieldType.integer, 'How many to mint'),
              FieldDesc('maturity', FieldType.integer, 'Maturity date (0 = cash)'),
            ],
          ),
          CommandDesc(
            ctor: 'advance_date',
            label: 'Advance date',
            args: [FieldDesc('date', FieldType.integer, 'New local date')],
          ),
        ],
        friendActions: const [
          CommandDesc(
            ctor: 'pay',
            label: 'Pay',
            args: [
              FieldDesc('friend', FieldType.person, 'To'),
              FieldDesc('coin', FieldType.person, 'Coin (issuer)'),
              FieldDesc('maturity', FieldType.integer, 'Maturity (0 = cash)'),
              FieldDesc('amount', FieldType.integer, 'Amount'),
            ],
          ),
          CommandDesc(
            ctor: 'trade',
            label: 'Propose swap',
            args: [
              FieldDesc('friend', FieldType.person, 'With'),
              FieldDesc('give_coin', FieldType.person, 'You give (issuer)'),
              FieldDesc('give_maturity', FieldType.integer, 'You give (maturity)'),
              FieldDesc('give_amount', FieldType.integer, 'You give (amount)'),
              FieldDesc('want_coin', FieldType.person, 'You want (issuer)'),
              FieldDesc('want_maturity', FieldType.integer, 'You want (maturity)'),
              FieldDesc('want_amount', FieldType.integer, 'You want (amount)'),
            ],
          ),
          CommandDesc(
            ctor: 'redeem',
            label: 'Redeem',
            args: [
              FieldDesc('friend', FieldType.person, 'From'),
              FieldDesc('give_coin', FieldType.person, 'Present (issuer)'),
              FieldDesc('give_maturity', FieldType.integer, 'Present (maturity)'),
              FieldDesc('give_amount', FieldType.integer, 'Present (amount)'),
              FieldDesc('want_coin', FieldType.person, 'You want (issuer)'),
              FieldDesc('want_maturity', FieldType.integer, 'You want (maturity)'),
              FieldDesc('want_amount', FieldType.integer, 'You want (amount)'),
            ],
          ),
        ],
      ),
      inbox: [
        InboxDesc(
          notifyCtor: 'trade_proposed',
          args: const [
            'from',
            'want_coin',
            'want_maturity',
            'want_amount',
            'req'
          ],
          itemKey: 'from',
          title: '{from} proposes a swap',
          subtitle:
              'Wants {want_amount} {want_coin} maturing {want_maturity}',
          answers: const [
            AnswerDesc(
              label: 'Accept',
              cmdCtor: 'accept_trade',
              fill: [FromField('from'), FromField('req')],
            ),
            AnswerDesc(
              label: 'Decline',
              cmdCtor: 'reject_trade',
              fill: [FromField('from'), FromField('req')],
            ),
          ],
        ),
      ],
    ),

    // --- Chats: the social network — one-to-one AND groups (paper §8) -------
    // One chat list holds one-to-one conversations and groups. The "+" creates a
    // group; a group invitation is a card (accepted before the group joins the
    // list); opening a group shows its author-labeled thread, its input posts,
    // and its actions add / remove a member / leave.
    Panel(
      id: 'chats',
      name: 'Chats',
      chat: const ChatView(threadKey: 'chats', label: 'Chats', sendCtor: 'send'),
      groups: const GroupChatView(
        threadKey: 'groups',
        label: 'Chats',
        sendCtor: 'send_group',
        groupField: 'group',
        actions: [
          CommandDesc(
            ctor: 'invite_group',
            label: 'Add member',
            args: [
              FieldDesc('group', FieldType.text, 'Group'),
              FieldDesc('invitee', FieldType.person, 'Friend to add'),
            ],
          ),
          CommandDesc(
            ctor: 'remove_from_group',
            label: 'Remove member',
            args: [
              FieldDesc('group', FieldType.text, 'Group'),
              FieldDesc('member', FieldType.person, 'Member to remove'),
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
          label: 'New group',
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
              opensItem: true,
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
  ],

  // Declared state, shared across panels and rendered as their views.
  state: const [
    StateView('friends', 'Friends', StateKind.list),
    StateView('chats', 'Chats', StateKind.thread),
    StateView('groups', 'Groups', StateKind.thread),
  ],

  // Activity rules: a friendship lands in BOTH Friends (adds the friend) and
  // Chats (opens the conversation); messages extend it; a balance report sets a
  // keyed (issuer, maturity) holding. The trade acks (completed/failed/returned)
  // and date_advanced show a transient notice (no lasting state).
  activity: const [
    ActivityDesc(
      notifyCtor: 'connected',
      args: ['who'],
      effects: [AppendTo('friends', 'who'), OpenChat('chats', 'who')],
    ),
    // A friendship ended (paper §4 Integrate unfriend; §5 "unfriended removes
    // one"): drop the person from the Friends list (and the wallet, which lists
    // friends). The chat history is left in place.
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
    ActivityDesc(
      notifyCtor: 'balance_report',
      args: ['owner', 'issuer', 'maturity', 'count'],
      effects: [
        SetBalance('holdings', 'owner', 'issuer', 'count',
            maturityField: 'maturity')
      ],
    ),
    // The local date advanced (Def. 2 item 2) — an ack, no lasting state.
    ActivityDesc(
        notifyCtor: 'date_advanced',
        args: ['date'],
        effects: [Toast('Local date advanced to {date}')]),
    // Swap / redeem settlement acks. trade_returned: the offer was declined;
    // trade_returned_menu: a redemption the issuer could not fill (menu is M2).
    ActivityDesc(
        notifyCtor: 'trade_completed',
        args: ['who'],
        effects: [Toast('Swap with {who} completed')]),
    ActivityDesc(
        notifyCtor: 'trade_failed',
        args: ['who'],
        effects: [Toast('Swap with {who} failed')]),
    ActivityDesc(
        notifyCtor: 'trade_returned',
        args: ['who'],
        effects: [Toast('{who} declined — your bonds are back')]),
    ActivityDesc(
        notifyCtor: 'trade_returned_menu',
        args: ['who', 'menu'],
        effects: [Toast('{who} could not redeem — bonds returned')]),
    // Groups (in the Chats panel).
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
