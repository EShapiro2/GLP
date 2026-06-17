/// Unified Grassroots manifest for the running demo: ONE platform with three
/// surfaces over ONE shared inbox — Chats (messaging), Wallet (coins among
/// friends), and Requests (friend offers + swap proposals). Friends established
/// by befriending can both talk and transact coins. Rendered by the one generic
/// interpreter; all specifics are here, none in Dart.
///
/// Backed live by the GLP coins scenario in programs/book/coins/ (coins_agent +
/// coins_mediator), whose UserCmd/UserNotify vocabulary this manifest mirrors.
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

final Manifest grassrootsManifest = Manifest(
  title: 'GrassApp',

  // Surface 1: Chats (the friends you can talk to).
  chat: const ChatView(threadKey: 'chats', label: 'Chats', sendCtor: 'send'),

  // Surface 2: Wallet (the coins you and your friends hold). People are derived
  // from chat peers and holdings owners; self is the live agent, bob.
  wallet: WalletView(
    storeKey: 'holdings',
    label: 'Wallet',
    selfKey: 'bob',
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

  state: const [
    StateView('chats', 'Chats', StateKind.thread),
  ],

  // The Chats FAB composes the one outbox request: offer friendship.
  commands: const [
    CommandDesc(
      ctor: 'connect',
      label: 'Add friend',
      args: [FieldDesc('target', FieldType.person, 'Person to connect')],
    ),
  ],

  // Surface 3: Requests — friend offers and swap proposals share one inbox.
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
      notifyCtor: 'swap_offer',
      args: const [
        'from',
        'give_coin',
        'give_amount',
        'want_coin',
        'want_amount',
        'req'
      ],
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

  // Activity rules: a friend opens a chat; messages extend it; a balance report
  // sets a coin's holding. swap_done/swap_failed are acknowledged (no state).
  activity: const [
    ActivityDesc(
      notifyCtor: 'connected',
      args: ['who'],
      effect: OpenChat('chats', 'who'),
    ),
    ActivityDesc(
      notifyCtor: 'received',
      args: ['from', 'text'],
      effect: PushChat('chats', 'from', 'text', outgoing: false),
    ),
    ActivityDesc(
      notifyCtor: 'balance_report',
      args: ['owner', 'coin', 'amount'],
      effect: SetBalance('holdings', 'owner', 'coin', 'amount'),
    ),
    ActivityDesc(
        notifyCtor: 'swap_done',
        args: ['who'],
        effect: Toast('Swap with {who} completed')),
    ActivityDesc(
        notifyCtor: 'swap_failed',
        args: ['who'],
        effect: Toast('Swap with {who} failed — not enough coins')),
    ActivityDesc(notifyCtor: 'rejected', args: ['who']),
    ActivityDesc(notifyCtor: 'rejected', args: []),
  ],
);
