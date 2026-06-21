/// GrassApp manifest (paper §7.3): one app, one panel per platform —
/// **Friends** (the social graph), **Coins** (coins among friends), and
/// **Chats** (the social network). The bottom bar is these three panels; each
/// carries its own outbox and its own per-item alerts. One mediator
/// (programs/book/grassapp/grassapp_mediator.glp) feeds them all; the Dart side
/// routes each volition to its platform's panel by notify constructor — a friend
/// offer to Friends, a proposed swap to Coins, a group invitation to Chats.
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
        // unilateral outbox command — no one else answers; the other side's
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
        // An introduction from a friend — the same generic interpreter renders
        // it as a per-item alert, accepted or declined. The introduction itself
        // is the social-graph platform's (programs/social/graph), which the
        // paper cites; here the interpreter only renders the volition.
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

    // --- Coins: the wallet, organised by friend ----------------------------
    // People = self + friends + holdings owners; tapping one drills into their
    // coins and the actions against them. A swap a friend proposes alerts that
    // friend's row. The actions live in the drill-down, so the panel has no "+".
    Panel(
      id: 'coins',
      name: 'Coins',
      wallet: WalletView(
        storeKey: 'holdings',
        label: 'Coins',
        selfKey: 'bob',
        friendsList: 'friends',
        friendField: 'friend',
        selfActions: const [
          CommandDesc(
            ctor: 'mint',
            label: 'Mint',
            args: [FieldDesc('amount', FieldType.integer, 'How many to mint')],
          ),
        ],
        friendActions: const [
          CommandDesc(
            ctor: 'pay',
            label: 'Pay',
            args: [
              FieldDesc('friend', FieldType.person, 'To'),
              FieldDesc('coin', FieldType.person, 'Coin (issuer)'),
              FieldDesc('amount', FieldType.integer, 'Amount'),
            ],
          ),
          CommandDesc(
            ctor: 'redeem',
            label: 'Redeem',
            args: [
              FieldDesc('friend', FieldType.person, 'From'),
              FieldDesc('amount', FieldType.integer, 'How many of their coins'),
            ],
          ),
          CommandDesc(
            ctor: 'propose_swap',
            label: 'Propose swap',
            args: [
              FieldDesc('friend', FieldType.person, 'With'),
              FieldDesc('give_coin', FieldType.person, 'You give (coin)'),
              FieldDesc('give_amount', FieldType.integer, 'You give (amount)'),
              FieldDesc('want_coin', FieldType.person, 'You want (coin)'),
              FieldDesc('want_amount', FieldType.integer, 'You want (amount)'),
            ],
          ),
        ],
      ),
      inbox: [
        InboxDesc(
          notifyCtor: 'swap_offer',
          args: const [
            'from',
            'give_coin',
            'give_amount',
            'want_coin',
            'want_amount',
            'req'
          ],
          itemKey: 'from',
          title: '{from} proposes a swap',
          subtitle:
              'Gives {give_amount} {give_coin}-coins for {want_amount} {want_coin}-coins',
          answers: const [
            AnswerDesc(
              label: 'Accept',
              cmdCtor: 'accept_swap',
              fill: [FromField('from'), FromField('req')],
            ),
            AnswerDesc(
              label: 'Decline',
              cmdCtor: 'decline_swap',
              fill: [FromField('from'), FromField('req')],
            ),
          ],
        ),
      ],
    ),

    // --- Chats: the social network -----------------------------------------
    // The chat list; a conversation opens once the friendship is made. The one
    // alert is a group invitation (none in this scenario yet). Sending happens
    // inside the open conversation, so the panel has no "+".
    const Panel(
      id: 'chats',
      name: 'Chats',
      chat: ChatView(threadKey: 'chats', label: 'Chats', sendCtor: 'send'),
    ),
  ],

  // Declared state, shared across panels and rendered as their views.
  state: const [
    StateView('friends', 'Friends', StateKind.list),
    StateView('chats', 'Chats', StateKind.thread),
  ],

  // Activity rules: a friendship lands in BOTH Friends (adds the friend) and
  // Chats (opens the conversation); messages extend it; a balance report sets a
  // coin's holding. swap_done/swap_failed are acknowledged (no state).
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
      args: ['owner', 'coin', 'amount'],
      effects: [SetBalance('holdings', 'owner', 'coin', 'amount')],
    ),
    ActivityDesc(
        notifyCtor: 'swap_done',
        args: ['who'],
        effects: [Toast('Swap with {who} completed')]),
    ActivityDesc(
        notifyCtor: 'swap_failed',
        args: ['who'],
        effects: [Toast('Swap with {who} failed — not enough coins')]),
    ActivityDesc(notifyCtor: 'rejected', args: ['who']),
    ActivityDesc(notifyCtor: 'rejected', args: []),
  ],
);
