/// The currency mini-app's screen, the image of the display declarations of
/// `programs/coins/currency/coins_agent.vglp`.
///
/// Nothing here is invented. Every element is derived from one declaration
/// under vGLP's Definitions "Display Declaration", "Manifest" and "Canonical
/// Compilation", by the derivation vGLP wrote out in
/// `/Grassroots/vGLP/docs/currency-manifest-derivation.md`. The declarations,
/// carried through into the compiled `coins_agent.glp`, are:
///
/// ```
/// display agent *(Amount) : panel(coins), label("Mint"),
///     field(Amount, number), persistent.
/// display agent *(Friend, GiveCoin, GiveAmount, WantCoin, WantAmount) :
///     panel(coins), label("Swap"), field(Friend, peer), field(GiveCoin, peer),
///     field(GiveAmount, number), field(WantCoin, peer),
///     field(WantAmount, number), persistent.
/// display agent *(Friend, Amount) : panel(coins), label("Pay"),
///     field(Friend, peer), field(Amount, number), persistent.
/// display agent *(Friend, WantCoin) : panel(coins), label("Redeem"),
///     field(Friend, peer), field(WantCoin, peer), persistent.
/// display respond_swap *(no, From?, Want?, Offered?) : panel(coins),
///     label("Decline"), transient.
/// display respond_swap *(yes, From?, Want?, Offered?) : panel(coins),
///     label("Accept"), transient.
/// display msg(agent, person, holdings(Lots)) : panel(coins), view(balances).
/// ```
///
/// A field's label is its writer's name: a display declaration's `field(X, W)`
/// item gives the widget and not a label, so the writer is what the person
/// sees, and Currencies named the writers on 2026-09-09 so that the forms
/// label themselves. The clause names and answer functors are the compilation's — the
/// four request clauses of `agent` are `agent_1` to `agent_4` in the order
/// they are written, and the two of `respond_swap` are `respond_swap_1`
/// (decline) and `respond_swap_2` (accept).
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

/// The four request clauses of `agent`, perpetually pending: each a compose
/// form bound to the standing card of its clause.
const _mint = CommandDesc(
  ctor: '',
  clause: 'agent_1',
  answerCtor: 'xs_agent_1',
  label: 'Mint',
  args: [FieldDesc('Amount', FieldType.number, 'Amount')],
);

const _swap = CommandDesc(
  ctor: '',
  clause: 'agent_2',
  answerCtor: 'xs_agent_2',
  label: 'Swap',
  args: [
    FieldDesc('Friend', FieldType.peer, 'Friend'),
    FieldDesc('GiveCoin', FieldType.peer, 'GiveCoin'),
    FieldDesc('GiveAmount', FieldType.number, 'GiveAmount'),
    FieldDesc('WantCoin', FieldType.peer, 'WantCoin'),
    FieldDesc('WantAmount', FieldType.number, 'WantAmount'),
  ],
);

const _pay = CommandDesc(
  ctor: '',
  clause: 'agent_3',
  answerCtor: 'xs_agent_3',
  label: 'Pay',
  args: [
    FieldDesc('Friend', FieldType.peer, 'Friend'),
    FieldDesc('Amount', FieldType.number, 'Amount'),
  ],
);

const _redeem = CommandDesc(
  ctor: '',
  clause: 'agent_4',
  answerCtor: 'xs_agent_4',
  label: 'Redeem',
  args: [
    FieldDesc('Friend', FieldType.peer, 'Friend'),
    FieldDesc('WantCoin', FieldType.peer, 'WantCoin'),
  ],
);

/// The two clauses of the swap responder, transient and sharing the context
/// `ctx_respond_swap_j(From, Want, Offered)`: one card, a button each, and
/// whichever the person taps the other ask is aborted by the reduction and
/// retired by `closed`.
///
/// The accept clause carries an else-branch, which until 2026-09-15 the
/// mediator's deadline selected — the machine declining for a person who had
/// not answered. With the deadline gone it is selected only by a decline, and
/// this program does not need one: the decline is its own volition-guarded
/// clause here, `respond_swap_1`, and a will for that sibling reaches the same
/// answer (vGLP, Section "Elicitation"). So the card declares no
/// [InboxDesc.elseBranch] and offers two buttons rather than a second decline
/// that says what the first says. The else-branch of `coins_agent.vglp` is
/// what is now redundant, and removing it is Currencies' and vGLP's.
const _respondSwap = InboxDesc(
  clauses: ['respond_swap_1', 'respond_swap_2'],
  args: ['From', 'Want', 'Offered'],
  itemKey: 'From',
  title: '',
  answers: [
    AnswerDesc(
      label: 'Accept',
      cmdCtor: '',
      clause: 'respond_swap_2',
      answerCtor: 'xs_respond_swap_2',
      fill: [ConstFill(GAtom('yes'))],
    ),
    AnswerDesc(
      label: 'Decline',
      cmdCtor: '',
      clause: 'respond_swap_1',
      answerCtor: 'xs_respond_swap_1',
      fill: [ConstFill(GAtom('no'))],
    ),
  ],
);

/// Panel `coins`: the four forms, the swap card, the balances view, and the
/// default display — a list in the program's panel — which takes the sixteen
/// other alternatives of `ScreenContent`.
const _coinsPanel = Panel(
  id: 'coins',
  name: 'coins',
  commands: [_mint, _swap, _pay, _redeem],
  inbox: [_respondSwap],
  views: [
    ScreenView(
      pattern: 'msg(agent, person, holdings(Lots))',
      content: 'Lots',
      kind: ViewKind.balances,
      label: 'Balances',
      store: 'balances',
    ),
    ScreenView(
      pattern: 'msg(agent, person, S)',
      content: 'S',
      kind: ViewKind.list,
      label: 'Screen',
      store: 'screen',
    ),
  ],
);

/// The currency mini-app's manifest.
const Manifest coinsManifest = Manifest(
  title: 'Coins',
  panels: [_coinsPanel],
  activity: [],
);
