/// Coins-among-friends manifest (paper §7.4 sec:ui-coins, Appendix app:coins).
///
/// The restricted grassroots coins model: coins only (no maturity), holdings and
/// transactions confined to the social graph — a person holds and trades only
/// their friends' coins and their own. The four guarded transactions of the
/// appendix become the two UI primitives:
///
///   • Mint, Pay, Redeem — self-guarded Requests (outbox), composed from a
///     friend's (or your own) wallet so they point at actual coins.
///   • Swap — Propose is a Request (outbox), Accept/Decline a Response (inbox).
///
/// The state the outbox leaves is the wallet: each person and the coins they
/// hold. Rendering a real wallet needs the keyed-balance view (`WalletView` +
/// `SetBalance`) the chat/list/value views do not provide — so the coins app is
/// the fourth instance of the one interface, and the one that required a new
/// generic view. No app-specific Dart: only this manifest.
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

final Manifest coinsManifest = Manifest(
  title: 'Coins',
  stateTabLabel: 'Wallet',

  // The state the outbox leaves: a person and the coins they hold. Friends come
  // from the social graph (`connected`); you hold and trade only their coins.
  wallet: WalletView(
    storeKey: 'holdings',
    label: 'Wallet',
    selfKey: 'you',
    friendsList: 'friends',
    friendField: 'friend',
    // Mint — make k of your own coins. Self-guarded; on your own tile.
    selfActions: const [
      CommandDesc(
        ctor: 'mint',
        label: 'Mint',
        args: [FieldDesc('amount', FieldType.integer, 'How many to mint')],
      ),
    ],
    // On a friend's tile, pointing at their actual coins.
    friendActions: const [
      // Pay — hand some coins you hold to this friend.
      CommandDesc(
        ctor: 'pay',
        label: 'Pay',
        args: [
          FieldDesc('friend', FieldType.person, 'To'),
          FieldDesc('coin', FieldType.person, 'Coin (issuer)'),
          FieldDesc('amount', FieldType.integer, 'Amount'),
        ],
      ),
      // Redeem — return this friend's coins to them; the issuer fills.
      CommandDesc(
        ctor: 'redeem',
        label: 'Redeem',
        args: [
          FieldDesc('friend', FieldType.person, 'From'),
          FieldDesc('amount', FieldType.integer, 'How many of their coins'),
        ],
      ),
      // Propose swap — your coins for theirs; they Accept or Decline (inbox).
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

  // Friends shown in the wallet; populated by the social graph.
  state: const [
    StateView('friends', 'Friends', StateKind.list),
  ],

  // The outbox actions are wallet actions, so there is no generic "+" form.
  commands: const [],

  // The one inbox request: a swap a friend proposes (the Response side).
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

  // Activity rules: a new friend joins the wallet; a balance report sets a coin.
  activity: const [
    ActivityDesc(
      notifyCtor: 'connected',
      args: ['who'],
      effect: AppendTo('friends', 'who'),
    ),
    ActivityDesc(
      notifyCtor: 'balance_report',
      args: ['owner', 'coin', 'amount'],
      effect: SetBalance('holdings', 'owner', 'coin', 'amount'),
    ),
  ],
);
